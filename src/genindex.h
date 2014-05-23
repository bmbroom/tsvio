
enum status { OK, EMPTY_FILE, NO_LABEL_ERROR, WRITE_ERROR, INCOMPLETE_LAST_LINE };
extern enum status generate_index (FILE *ip, FILE *op);
