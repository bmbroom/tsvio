
/* This module implements a dynamic hash table that grows
 * as necessary to hold the inserted strings.  The order in which strings are first inserted
 * is recorded, and the insertion index of each inserted string can be retrieved.
 *
 * An additional user-specified value can associated with each inserted string and updated as
 * needed.
 *
 * Strings are specified by a pointer to the first character of the string and its maximum
 * length.  Shorter strings are NUL-terminated.
 *
 * If flag DHT_STRDUP is specified when the DHT is created, all strings will be duplicated on
 * insertion and freed when the DHT is destroyed.
 * Otherwise, the memory backing the strings inserted into the hash table must be maintained
 * by the caller(s) until the table is destroyed (at least).
 *
 * Summary of operations:
 */

typedef struct _dynhashtab dynHashTab;

extern dynHashTab *newDynHashTab (long initial_size, long flags);
#define DHT_STRDUP 1	/* Iff set, the dht will duplicate all inserted strings. */
extern void freeDynHashTab (dynHashTab *dht);
			/* Iff DHT_STRDUP is set, all inserted strings will also be freed. */

/* If string is not in dht, insert it with initial value 0. */
extern void insertStr (dynHashTab *dht, const char *str, long len);

/* Insert string into dht if not present. Associate value with string (always). */
extern void insertStrVal (dynHashTab *dht, const char *str, long len, long value);

/* If string is in dht, associated value with it. */
extern void changeStrVal (dynHashTab *dht, const char *str, long len, long value);

/* Associate value with all strings in dht. */
extern void setAllValues (dynHashTab *dht, long value);

/* Returns the number of strings in dht. */
extern long dhtNumStrings (const dynHashTab *dht);

/* Returns the number of strings in dht associated with value. */
extern long countValues (const dynHashTab *dht, long value);

/* Returns the number of strings in dht not associated with value. */
extern long countNotValues (const dynHashTab *dht, long value);

/* Returns the value associated with string in dht. Returns -1L if string not in dht. */
extern long getStringValue (const dynHashTab *dht, const char *str, long len);

/* Returns the number of strings inserted into the dht before string.
 * Returns -1L if the string is not in the dht.
 */
extern long getStringIndex (const dynHashTab *dht, const char *str, long len);

/* Initializes an iterator for iterating over the strings contained in the dht. */
extern void initIterator (const dynHashTab *dht, long *iter);

/* Advances the iterator to the next string in dht.
 *
 * If there is no next string, the function returns 0 and the iterator is invalidated.
 *
 * Otherwise the function returns 1 and sets the values associated with the string:
 *   *strp = address of the first character in the (duplicated) string
 *   *lenp = maximum length of the string
 *   *orderp = number of strings inserted before this string
 *   *valuep = value associated with this string.
 * If any of the above pointers is NULL, the corresponding value is not returned.
 *
 * The behavior of the iterator is undefined if strings are inserted into the table during
 * the iteration.
 */
extern int getNextStr (const dynHashTab *dht, long *iter, const char **strp, long *lenp, long *orderp, long *valuep);

