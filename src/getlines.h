
enum status { OK, EMPTY_FILE, WRITE_ERROR, INCOMPLETE_LAST_LINE, LABEL_NOT_FOUND, NO_INDEX, LABEL_TOO_LONG, INDEX_TOO_LONG, NON_NUMERIC_IN_INDEX, SEEK_FAILED };

extern enum status
find_indices (FILE *indexp, long findany, long nindex, char *labels[], long *index);

