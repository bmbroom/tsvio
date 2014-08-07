#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#include "tsvio.h"
#include "dht.h"

enum status
find_indices (FILE *indexp, long findany, long nindex, const char *labels[], long *index, void (*warn)(char *msg, ...))
{
	long	nfound = 0;
	long	ii;
	int	ch;
	char label[1024]; 
	long lablen, len;
	char posn[64]; 
	dynHashTab *dht;

	/* Create temporary hash table of labels we're looking for. */
	dht = newDynHashTab (1024);
	for (ii = 0; ii < nindex; ii++)
	    insertStr (dht, labels[ii], strlen (labels[ii]));

	/* Mark all indices as not found. */
	for (ii = 0; ii < nindex; ii++) index[ii] = -1;

	/* Assert: indexp is positioned just before either EOF or an input line. */
	while ((nfound < nindex) && (ch = getc (indexp)) != EOF) {

	    /* Read label. */
	    label[0] = (unsigned char)ch;
	    lablen = 1;
	    while ((ch = getc (indexp)) != EOF && ch != '\t' && ch != '\n') {
		if (lablen == sizeof(label)-1)
		    return LABEL_TOO_LONG;
		label[lablen++] = (unsigned char)ch;
	    }
	    label[lablen] = '\0';

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
	    ii = getStringIndex (dht, label, lablen);
	    if (ii >= 0) {
		if (index[ii] >= 0)
		    warn ("duplicate entry for label %s ignored\n", label);
		else {
		    index[ii] = atol (posn);
		    nfound++;
		}
	    }
	}
	freeDynHashTab (dht);

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

/* Return the number of tab-separated columns in the line buffer.
 * The number of columns is *defined* to be the number of tabs plus one.
 * So, an empty line has 1 column (the empty string).
 */
long
num_columns (char *buffer, long buflen)
{
    long n = 1;
    long ii;

    for (ii = 0; ii < buflen; ii++)
        if (buffer[ii] == '\t')
	    n++;
    return n;
}

enum status
find_col_indices (char *buffer, long buflen, long findany, long nindex, const char *labels[], long *index, void (*warn)(char *msg,...))
{
	long	nfound = 0;
	long	ii;
	int	ch;
	char label[1024]; 
	long len;
	char posn[64]; 
	char *ptr;
	long indexp = 0;
	long fstart;
	long fieldnum;


	/* Mark all indices as not found. */
	for (ii = 0; ii < nindex; ii++) index[ii] = -1;

	/* Assert: indexp is positioned at start of a field or immediately following buffer contents. */
	fieldnum = 0;
	while ((nfound < nindex) && (indexp < buflen)) {

	    /* Read field. */
	    fieldnum++;
	    fstart = indexp;
	    ptr = label;
	    while ((indexp < buflen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
		*ptr++ = buffer[indexp++];
	    }
	    *ptr = '\0';
#ifdef DEBUG
	    Rprintf ("Found column header start=%ld indexp=%ld len=%ld: %s\n", fstart, indexp, indexp-fstart, label);
#endif
	    if (indexp < buflen) indexp++; /* Advance over field-terminator, if any. */

	    /* See if field label matches any of the ones we're looking for.*/
	    for (ii = 0; ii < nindex; ii++) {
	        if (strcmp (labels[ii], label) == 0) {
		    if (index[ii] >= 0)
		        warn ("duplicate entry for label %s ignored\n", label);
		    else {
		        index[ii] = fieldnum;
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
		    warn ("no matching entry for col label %s\n", labels[ii]);
	    }
	    return LABEL_NOT_FOUND;
	}

	return OK;
}

