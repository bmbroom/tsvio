#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "tsvio.h"

enum status
generate_index (FILE *ip, FILE *op)
{
	long	position = 0;	/* Number of bytes read. */
	int	ch;		/* Current input byte. */

	/* Skip header line. */
	while ((ch = getc (ip)) != EOF) {
	    position++;
	    if (ch == '\n')
	        break;
	}
	if (ch == EOF) {
	    return position == 0 ? EMPTY_FILE : INCOMPLETE_LAST_LINE;
	}

	/* Assert: ip is positioned just before either EOF or an input line. */
	while ((ch = getc (ip)) != EOF) {
	    long start = position++;
	    if (ch != '\n') {	/* Quietly ignore blank lines. */

		if (ch == '\t')
		    return NO_LABEL_ERROR;

		/* Read label and write to output. */
		if (putc(ch,op) < 0) return WRITE_ERROR;
		while ((ch = getc (ip)) != EOF) {
		    position++;
		    if (ch == '\t' || ch == '\n')
		        break;
		    if (putc(ch,op) < 0) return WRITE_ERROR;
		}

		/* Skip over what remains of current line. */
		if (ch == '\t') {
		    while ((ch = getc (ip)) != EOF) {
			position++;
		        if (ch == '\n')
			    break;
		    }
		}

		if (ch == EOF)
		    return INCOMPLETE_LAST_LINE;

		if (fprintf (op, "\t%ld\n", start) < 0)
		    return WRITE_ERROR;
	    }
	}
	return OK;
}

