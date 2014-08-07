#include <stdlib.h>
#include <string.h>
#include "dht.h"

#include <R.h>

/* #### #### #### #### #### #### ####
 * #### #### #### #### #### #### ####
 *
 * BEGIN Dynamic Hash Table Module.
 *
 * #### #### #### #### #### #### ####
 * #### #### #### #### #### #### ####
 */

/* This function was written by Paul Hsieh and obtained from http://www.azillionmonkeys.com/qed/hash.html
 * under the LGPL 2.1 license.  I modified it very slightly to accept the initial hash value as a
 * parameter.
 */
/*#include "pstdint.h"*/ /* Replace with <stdint.h> if appropriate */
#include <stdint.h>
#undef get16bits
#if (defined(__GNUC__) && defined(__i386__)) || defined(__WATCOMC__) \
  || defined(_MSC_VER) || defined (__BORLANDC__) || defined (__TURBOC__)
#define get16bits(d) (*((const uint16_t *) (d)))
#endif

#if !defined (get16bits)
#define get16bits(d) ((((uint32_t)(((const uint8_t *)(d))[1])) << 8)\
                       +(uint32_t)(((const uint8_t *)(d))[0]) )
#endif

static uint32_t SuperFastHashModified (const char * data, int len, uint32_t hash) {
uint32_t tmp;
int rem;

    if (len <= 0 || data == NULL) return 0;

    rem = len & 3;
    len >>= 2;

    /* Main loop */
    for (;len > 0; len--) {
        hash  += get16bits (data);
        tmp    = (get16bits (data+2) << 11) ^ hash;
        hash   = (hash << 16) ^ tmp;
        data  += 2*sizeof (uint16_t);
        hash  += hash >> 11;
    }

    /* Handle end cases */
    switch (rem) {
        case 3: hash += get16bits (data);
                hash ^= hash << 16;
                hash ^= ((signed char)data[sizeof (uint16_t)]) << 18;
                hash += hash >> 11;
                break;
        case 2: hash += get16bits (data);
                hash ^= hash << 11;
                hash += hash >> 17;
                break;
        case 1: hash += (signed char)*data;
                hash ^= hash << 10;
                hash += hash >> 1;
    }

    /* Force "avalanching" of final 127 bits */
    hash ^= hash << 3;
    hash += hash >> 5;
    hash ^= hash << 4;
    hash += hash >> 17;
    hash ^= hash << 25;
    hash += hash >> 6;

    return hash;
}

/* This function further modifies an initial hash with a hash of the input data.
 * The parameters are:
 * 1. A pointer to the start of the data to hash.
 * 2. The number of bytes in that data.
 * 3. The initial hash value.
 */
static inline unsigned long rehash (const char *str, long len, unsigned long h)
{
    return SuperFastHashModified (str, len, h);
}

/* This function computes a hash of the input data.
 * The parameters are:
 * 1. A pointer to the start of the data to hash.
 * 2. The number of bytes in that data.
 */
static inline unsigned long hash (const char *str, long len)
{
    return SuperFastHashModified (str, len, len);
}


/* For each hash table slot, we maintain 3 fields.
 */
typedef struct {
    long order;		/* Number of strings inserted before this one. */
    const char *str;	/* Address of string in this slot (needed for rehashing). */
    long len;		/* Length of string in this slot. */
} dhtSlot;

/* A free slot is indicated by a special value of the order field. */
#define FREESLOT	-1

/* This structure maintains the representation of a dynamic hash table.
 */
struct _dynhashtab {
    /* Standard dynamic hash table fields: */
    long size;		/* Number of slots in hash table. */
    long count;		/* Number of slots in use. */
    long loadLimit;	/* When count reaches this limit, we grow the table. */
    dhtSlot *slot;	/* Hash table slots. */
};


/* This function allocates a DHT with an initial size given by the parameter.
 */
dynHashTab *
newDynHashTab (long isize)
{
    dynHashTab *dht = malloc (sizeof (*dht));
    long ii;

    /* Set initial DHT size, load limit, and number of entries. */
    dht->size = isize;
    dht->loadLimit = (isize * 3) / 4;
    dht->count = 0;

    /* Allocate and initialize slots. */
    dht->slot = malloc (sizeof(dhtSlot) * isize);
    for (ii = 0; ii < isize; ii++) {
        dht->slot[ii].order = FREESLOT;
        dht->slot[ii].str = NULL;
        dht->slot[ii].len = 0;
    }
    return dht;
}

void
insertStr (dynHashTab *dht, const char *str, long len)
{
    unsigned long h = hash (str, len);
    dhtSlot *newslot;
    long newsize;
    long ii, idx, iters;

    /* Search hash table until we encounter either the desired string
     * or an empty slot.
     */
    iters = 0;
    while (dht->slot[idx = (h % dht->size)].order != FREESLOT) {
	if ((dht->slot[idx].len == len) && (strncmp (dht->slot[idx].str, str, len) == 0))
	    return;
        h = rehash (str, len, h);
	if (iters++ > 1000) {
	    warning ("dht.insertStr: excessive looping in hash.\n");
	    return;
	}
    }
    /* Put new entry into empty slot and increment number of entries. */
    dht->slot[idx].order = dht->count++;
    dht->slot[idx].str = str;
    dht->slot[idx].len = len;

    /* Check load and grow DHT if required. */
    if (dht->count >= dht->loadLimit) {
	/* We will double the number of slots. */
        newsize = dht->size * 2;
	/* Create stores for remapped slots. */
	newslot = malloc (sizeof(dhtSlot) * newsize);
	for (ii = 0; ii < newsize; ii++) {
	    newslot[ii].order = FREESLOT;
	    newslot[ii].str = NULL;
	    newslot[ii].len = 0;
	}
	/* Copy existing slots to new locations. */
	for (ii = 0; ii < dht->size; ii++) {
	    if (dht->slot[ii].order != FREESLOT) {
		/* Find new location. */
		h = hash (dht->slot[ii].str, dht->slot[ii].len);
		iters = 0;
		while (newslot[idx = (h % newsize)].order != FREESLOT) {
		    h = rehash (dht->slot[ii].str, dht->slot[ii].len, h);
		    if (iters++ > 1000) {
			warning ("dht.insertStr: excessive looping in hash.\n");
			return;
		    }
		}
		/* Copy element to new location. */
		newslot[idx] = dht->slot[ii];
	    }
	}
	/* Release old slots and replace with the new ones. */
	free (dht->slot);
	dht->slot = newslot;
	/* Set new DHT size and load limit. */
	dht->size = newsize;
	dht->loadLimit = (newsize * 3) / 4;
    };
}

/* This function returns the insertion index of the string given as a parameter.
 * Parameters:
 * 1. Pointer to DHT containing the string.
 * 2. Address of the string (the string is not modified)/
 * 3. Number of bytes in the string.
 * Returns -1 if the string is not already in the DHT.
 */
long
getStringIndex (const dynHashTab *dht, const char *str, long len)
{
    unsigned long h = hash (str, len);
    long iters = 0;
    long idx;

    while (dht->slot[idx = (h % dht->size)].order != FREESLOT) {
	if ((dht->slot[idx].len == len) && (strncmp (dht->slot[idx].str, str, len) == 0))
	    return dht->slot[idx].order;
        h = rehash (str, len, h);
	if (iters++ > 1000) {
	    warning ("dht.getStringIndex: excessive looping in hash.\n");
	    return -1;
	}
    }
    return -1;
}

/* This function destroys the DHT and releases any storage allocated for it by this module.
 * Releasing backing storage for the strings inserted into the table is the responsibility
 * of the caller.
 *
 * After this function returns, the DHT and any memory associated with it is not valid. 
 */
void
freeDynHashTab (dynHashTab *dht)
{
    free (dht->slot);
    free (dht);
}

/* #### #### #### #### #### #### ####
 * #### #### #### #### #### #### ####
 *
 * END Dynamic Hash Table Module.
 *
 * #### #### #### #### #### #### ####
 * #### #### #### #### #### #### ####
 */

