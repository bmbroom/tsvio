/* Copyright 2013 UT MD Anderson Cancer Center.
 *
 * Author : Bradley Broom
 */

/* This is the C language component of the R tsvio library.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>

#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>

#include "tsvio.h"

static SEXP
add_dims (SEXP svec, long nrows, long ncols)
{
    SEXP sdim;
    int *dim;

    PROTECT (svec);
    PROTECT (sdim = allocVector(INTSXP, 2));
    dim = INTEGER_POINTER(sdim);
    dim[0] = nrows;
    dim[1] = ncols;
    setAttrib (svec, R_DimSymbol, sdim);
    UNPROTECT (2);
    return svec;
}

static SEXP
add_score_names (SEXP orig, SEXP mat)
{
    SEXP dimNames;
    SEXP colNames;
    SEXP size;
    SEXP class;
    long nprotect;
    long nvar = 2;

    PROTECT (orig);
    PROTECT (mat);
    PROTECT (dimNames = getAttrib (orig, R_DimNamesSymbol));
    nprotect = 3;

    if (dimNames != R_NilValue) {
	PROTECT (colNames = VECTOR_ELT (dimNames, 1));
	nprotect++;
	setAttrib (mat, install("Labels"), colNames);
    }

    PROTECT (size = allocVector(INTSXP, 1));
    nprotect++;
    INTEGER(size)[0] = nvar;
    setAttrib (mat, install("Size"), size);

    UNPROTECT (nprotect);
    return mat;
}

SEXP
tsvGenIndex (SEXP dataFile, SEXP indexFile)
{
    FILE *tsvp, *indexp;
    enum status res;

    PROTECT (dataFile = AS_CHARACTER(dataFile));
    PROTECT (indexFile = AS_CHARACTER(indexFile));

    if (dataFile == R_NilValue || indexFile == R_NilValue) {
        error ("tsvGenIndex: parameter cannot be NULL\n");
    }

    tsvp = fopen (CHAR(STRING_ELT(dataFile,0)), "rb");
    if (tsvp == NULL) {
        error ("tsvGenIndex: unable to open datafile '%s' for reading\n", CHAR(STRING_ELT(dataFile,0)));
    }
    indexp = fopen (CHAR(STRING_ELT(indexFile,0)), "wb");
    if (indexp == NULL) {
        error ("tsvGenIndex: unable to open indexfile '%s' for writing\n", CHAR(STRING_ELT(indexFile,0)));
    }

    res = generate_index (tsvp, indexp);
    if (res == EMPTY_FILE)
	warning ("tsvGenIndex: Warning: tsvfile '%s' is empty\n", CHAR(STRING_ELT(dataFile,0)));
    else if (res != OK) {
	if (res == WRITE_ERROR)
	    error ("tsvGenIndex: error writing to indexfile '%s'\n", CHAR(STRING_ELT(indexFile,0)));
	else if (res == INCOMPLETE_LAST_LINE)
	    error ("tsvGenIndex: last line of tsvfile '%s' is incomplete\n", CHAR(STRING_ELT(dataFile,0)));
	else if (res == NO_LABEL_ERROR)
	    error ("tsvGenIndex: line of tsvfile '%s' does not contain a label\n", CHAR(STRING_ELT(dataFile,0)));
	else
	    error ("tsvGenIndex: unknown internal error\n");
    }
    fclose (tsvp);
    fclose (indexp);
    UNPROTECT (2);
    return R_NilValue;
}

SEXP
get_tsv_line (FILE *tsvp, long posn)
{
    int	ch, len;
    char buffer[1024*1024];

    if (fseek (tsvp, posn, SEEK_SET) < 0)
	error ("get_tsv_line: error seeking to line starting at %ld\n", posn);

    len = 0;
    while ((ch = getc (tsvp)) != EOF && ch != '\n') {
	if (len >= (sizeof(buffer)-1)) {
	    error ("get_tsv_line: line starting at %ld longer than buffer length (%d bytes)\n", posn, sizeof(buffer));
	}
	buffer[len++] = ch;
    }

    if (ch == EOF) {
	error ("get_tsv_line: line starting at %ld is prematurely terminated by EOF\n", posn);
    }
	
    buffer[len++] = '\n'; /* Check above ensures space for this. */
    return mkCharLen(buffer,len);
}

void
warn (char *msg, ...)
{
    va_list argptr;
    va_start (argptr, msg);
    warning (msg, argptr);
    va_end (argptr);
}

SEXP
tsvGetLines (SEXP dataFile, SEXP indexFile, SEXP patterns, SEXP findany)
{
    long nprotect = 0;
    FILE *tsvp, *indexp;
    long Npattern, Nresult;
    SEXP results;
    long *posns;
    const char **pats;
    long ii;
    enum status res;
    
#ifdef DEBUG
    Rprintf ("> tsvGetLines\n");
#endif

    /* Convert, if necessary, data into expected format. */
    PROTECT (dataFile = AS_CHARACTER(dataFile));
    PROTECT (indexFile = AS_CHARACTER(indexFile));
    PROTECT (patterns = AS_CHARACTER(patterns));
    PROTECT (findany = AS_LOGICAL(findany));
    nprotect += 4;

    if (dataFile == R_NilValue || indexFile == R_NilValue || patterns == R_NilValue) {
        error ("tsvGetLines: parameter cannot be NULL\n");
    }

    indexp = fopen (CHAR(STRING_ELT(indexFile,0)), "rb");
    if (indexp == NULL) {
        error ("tsvGetLines: unable to open indexfile '%s' for reading\n", CHAR(STRING_ELT(indexFile,0)));
    }

    Npattern = length(patterns);
#ifdef DEBUG
    Rprintf ("  tsvGetLines: received %d patterns\n", Npattern);
#endif

    posns = (long *)R_alloc (Npattern, sizeof(long));
    if (posns == NULL) {
        error ("tsgGetLines: ERROR: unable to allocate working memory\n");
    }
    pats = (const char **)R_alloc (Npattern, sizeof(char *));
    if (pats == NULL) {
        error ("tsgGetLines: ERROR: unable to allocate working memory\n");
    }
    for (ii = 0; ii < Npattern; ii++) {
	pats[ii] = CHAR(STRING_ELT(patterns,ii));
    }
    res = find_indices (indexp, LOGICAL(findany)[0], Npattern, pats, posns, warn);
    fclose (indexp);

    /* Return TSV header and selected lines. */
    if (res == OK) {
	Nresult = 0;
	for (ii = 0; ii < Npattern; ii++) {
	    if (posns[ii] >= 0) {
		Nresult++;
	    }
	}
#ifdef DEBUG
	Rprintf ("  tsvGetLines: found %d matches\n", Nresult);
#endif
	PROTECT (results = allocVector(STRSXP, Nresult+1)); /* Includes header. */
	nprotect++;

	tsvp = fopen (CHAR(STRING_ELT(dataFile,0)), "rb");
	if (tsvp == NULL) {
	    error ("tsvGetLines: unable to open datafile '%s' for reading\n", CHAR(STRING_ELT(dataFile,0)));
	}
	Nresult = 0;
	SET_STRING_ELT (results, Nresult++, get_tsv_line (tsvp, 0L));
	for (ii = 0; ii < Npattern; ii++) {
	    if (posns[ii] >= 0) {
		SET_STRING_ELT (results, Nresult++, get_tsv_line (tsvp, posns[ii]));
	    }
	}
	fclose (tsvp);
    } else {
#ifdef DEBUG
	Rprintf ("  tsvGetLines: error finding matches\n");
	results = R_NilValue;
#endif
	error ("tsvGetLines: match not found");
    }

#ifdef DEBUG
    Rprintf ("< tsvGetLines\n");
#endif
    UNPROTECT (nprotect);
    return results;
}
