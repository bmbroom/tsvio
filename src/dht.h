
/* This module implements a dynamic hash table that grows
 * as necessary to hold the inserted labels.  The order in which labels are inserted
 * is recorded, and the insertion index of each inserted label can be retrieved.
 *
 * The memory backing the strings inserted into the hash table must be maintained
 * until the table is destroyed.
 *
 * Summary of operations:
 */
typedef struct _dynhashtab dynHashTab;
extern dynHashTab *newDynHashTab (long initial_size);
extern void insertStr (dynHashTab *tab, const char *str, long len);
extern long getStringIndex (const dynHashTab *dht, const char *str, long len);
extern void freeDynHashTab (dynHashTab *dht);

