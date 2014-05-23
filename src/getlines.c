#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#include "tsvio.h"

enum status
find_indices (FILE *indexp, long findany, long nindex, const char *labels[], long *index, void (*warn)(char *msg,...))
{
	long	nfound = 0;
	long	ii;
	int	ch;
	char label[1024]; 
	long len;
	char posn[64]; 

	/* Mark all indices as not found. */
	for (ii = 0; ii < nindex; ii++) index[ii] = -1;

	/* Assert: indexp is positioned just before either EOF or an input line. */
	while ((nfound < nindex) && (ch = getc (indexp)) != EOF) {

	    /* Read label. */
	    label[0] = (unsigned char)ch;
	    len = 1;
	    while ((ch = getc (indexp)) != EOF && ch != '\t' && ch != '\n') {
		if (len == sizeof(label)-1)
		    return LABEL_TOO_LONG;
		label[len++] = (unsigned char)ch;
	    }
	    label[len] = '\0';

	    if (ch != '\t')
	        return ch == EOF ? INCOMPLETE_LAST_LINE : NO_INDEX;

	    /* Read index. */
	    posn[0] = (unsigned char)ch;
	    len = 1;
	    while ((ch = getc (indexp)) != EOF && ch != '\n') {
		if (len == sizeof(posn)-1)
		    return INDEX_TOO_LONG;
		if (!isdigit((unsigned char)ch))
		    return NON_NUMERIC_IN_INDEX;
		posn[len++] = (unsigned char)ch;
	    }
	    posn[len] = '\0';

	    if (ch == EOF)
		return INCOMPLETE_LAST_LINE;

	    /* See if label matches any of the ones we're looking for.*/
	    for (ii = 0; ii < nindex; ii++) {
	        if (strcmp (labels[ii], label) == 0) {
		    if (index[ii] >= 0)
		        warn ("duplicate entry for label %s ignored\n", label);
		    else {
		        index[ii] = atol (posn);
			nfound++;
		    }
		}
	    }
	}

	if (findany)
	    return nfound > 0 ? OK : LABEL_NOT_FOUND;

	if (nfound < nindex) {
	    for (ii = 0; ii < nindex; ii++) {
		if (index[ii] < 0)
		    warn ("no matching entry for label %s\n", labels[ii]);
	    }
	    return LABEL_NOT_FOUND;
	}

	return OK;
}

