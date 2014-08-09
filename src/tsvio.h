

enum status { OK, EMPTY_FILE, WRITE_ERROR, INCOMPLETE_LAST_LINE, NO_LABEL_ERROR, LABEL_NOT_FOUND, NO_INDEX, LABEL_TOO_LONG, INDEX_TOO_LONG, NON_NUMERIC_IN_INDEX, SEEK_FAILED };

extern enum status generate_index (FILE *ip, FILE *op);
extern enum status scan_index_file (FILE *indexp, dynHashTab *dht, long insertall);
extern enum status find_col_indices (char *buffer, long buflen, long findany, long nindex, const char *labels[], long *index, void (*warn)(char *msg,...));
extern int get_tsv_line_buffer (char *buffer, size_t bufsize, FILE *tsvp, long posn);
extern long num_columns (char *buffer, long buflen);

