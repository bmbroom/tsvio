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
    fclose (tsvp);
    fclose (indexp);
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
    UNPROTECT (2);
    return R_NilValue;
}

int
get_tsv_line_buffer (char *buffer, size_t bufsize, FILE *tsvp, long posn)
{
    int	ch, len;

    if (fseek (tsvp, posn, SEEK_SET) < 0)
	error ("get_tsv_line: error seeking to line starting at %ld\n", posn);

    len = 0;
    while ((ch = getc (tsvp)) != EOF && ch != '\n') {
	if (len >= (bufsize-1)) {
	    error ("get_tsv_line: line starting at %ld longer than buffer length (%d bytes)\n", posn, bufsize);
	}
	buffer[len++] = ch;
    }

    if (ch == EOF) {
	error ("get_tsv_line: line starting at %ld is prematurely terminated by EOF\n", posn);
    }
	
    buffer[len++] = '\n'; /* Check above ensures space for this. */
    return len;
}

SEXP
get_tsv_line (FILE *tsvp, long posn)
{
    char buffer[1024*1024];
    int len;
    len = get_tsv_line_buffer (buffer, sizeof(buffer), tsvp, posn);
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
	fclose (indexp);
        error ("tsgGetLines: ERROR: unable to allocate working memory\n");
    }
    pats = (const char **)R_alloc (Npattern, sizeof(char *));
    if (pats == NULL) {
	fclose (indexp);
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

/* R matrix is laid out in column-major order.
 */
void
get_tsv_fields (SEXP strvec, long nrows, long rowid, FILE *tsvp, long rowposn, long maxFieldWanted, long *fieldWanted)
{
    long len;
    long buflen;
    char buffer[1024*1024];
    long indexp;
    long fstart;
    SEXP element;
    long infield, outfield;


    /* Read line into buffer. */
    buflen = get_tsv_line_buffer (buffer, sizeof(buffer), tsvp, rowposn);

    infield = 0;
    indexp = 0;
    /* Assert: infield fields in the buffer have been processed. */
    /* Assert: indexp is positioned at start of a field or immediately following buffer contents. */
    while ((infield <= maxFieldWanted) && (indexp < buflen)) {

	/* Read field. */
	fstart = indexp;
	while ((indexp < buflen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
	    indexp++;
	}

	/* Insert field into output matrix if required. */
	if (infield <= maxFieldWanted) {
	    outfield = fieldWanted[infield];
	    if (outfield >= 0) {
		element = mkCharLen(buffer+fstart, indexp-fstart);
		SET_STRING_ELT (strvec, outfield*nrows+rowid, element);
	    }
	}

	if (indexp < buflen) indexp++; /* Advance over field-terminator, if any. */
	infield++;
    }
}
SEXP
tsvGetData (SEXP dataFile, SEXP indexFile, SEXP rowpatterns, SEXP colpatterns, SEXP findany)
{
    long nprotect = 0;
    FILE *tsvp, *indexp;
    long NrowPattern, NrowResult;
    long NcolPattern, NcolResult;
    SEXP results, dimnames, rownames, colnames;
    long *rowposns, *colposns;
    const char **rowpats, **colpats;
    long ii, ff, rowid, colid;
    enum status res;
    long maxColWanted;
    long *wantedFields;
    
#ifdef DEBUG
    Rprintf ("> tsvGetData\n");
#endif

    /* Convert, if necessary, data into expected format. */
    PROTECT (dataFile = AS_CHARACTER(dataFile));
    PROTECT (indexFile = AS_CHARACTER(indexFile));
    PROTECT (rowpatterns = AS_CHARACTER(rowpatterns));
    PROTECT (colpatterns = AS_CHARACTER(colpatterns));
    PROTECT (findany = AS_LOGICAL(findany));
    nprotect += 5;

    if (dataFile == R_NilValue || indexFile == R_NilValue || rowpatterns == R_NilValue || colpatterns == R_NilValue) {
        error ("tsvGetData: parameter cannot be NULL\n");
    }

    indexp = fopen (CHAR(STRING_ELT(indexFile,0)), "rb");
    if (indexp == NULL) {
        error ("tsvGetData: unable to open indexfile '%s' for reading\n", CHAR(STRING_ELT(indexFile,0)));
    }

    NrowPattern = length(rowpatterns);
    NcolPattern = length(colpatterns);
#ifdef DEBUG
    Rprintf ("  tsvGetData: received %d rowpatterns, %d colpatterns\n", NrowPattern, NcolPattern);
#endif

    rowposns = (long *)R_alloc (NrowPattern, sizeof(long));
    if (rowposns == NULL) {
	fclose (indexp);
        error ("tsgGetData: ERROR: unable to allocate working memory: rowposns\n");
    }
    rowpats = (const char **)R_alloc (NrowPattern, sizeof(char *));
    if (rowpats == NULL) {
	fclose (indexp);
        error ("tsgGetData: ERROR: unable to allocate working memory: rowpats\n");
    }
    for (ii = 0; ii < NrowPattern; ii++) {
	rowpats[ii] = CHAR(STRING_ELT(rowpatterns,ii));
    }
    res = find_indices (indexp, LOGICAL(findany)[0], NrowPattern, rowpats, rowposns, warn);
    fclose (indexp);

    /* Return TSV header and selected lines. */
    if (res != OK) {
#ifdef DEBUG
	Rprintf ("  tsvGetData: error finding row matches\n");
	results = R_NilValue;
#endif
	error ("tsvGetData: row match not found");
    }
    NrowResult = 0;
    for (ii = 0; ii < NrowPattern; ii++) {
	if (rowposns[ii] >= 0) {
	    NrowResult++;
	}
    }
#ifdef DEBUG
    Rprintf ("  tsvGetData: found %d row matches\n", NrowResult);
#endif
    tsvp = fopen (CHAR(STRING_ELT(dataFile,0)), "rb");
    if (tsvp == NULL) {
	error ("tsvGetData: unable to open datafile '%s' for reading\n", CHAR(STRING_ELT(dataFile,0)));
    }

    colposns = (long *)R_alloc (NcolPattern, sizeof(long));
    if (colposns == NULL) {
	fclose (tsvp);
	error ("tsgGetData: ERROR: unable to allocate working memory: colposns\n");
    }
    colpats = (const char **)R_alloc (NcolPattern, sizeof(char *));
    if (colpats == NULL) {
	fclose (tsvp);
	error ("tsgGetData: ERROR: unable to allocate working memory: colpats\n");
    }
    for (ii = 0; ii < NcolPattern; ii++) {
	colpats[ii] = CHAR(STRING_ELT(colpatterns,ii));
    }
    res = find_col_indices (tsvp, LOGICAL(findany)[0], NcolPattern, colpats, colposns, warn);
    if (res != OK) {
#ifdef DEBUG
	Rprintf ("  tsvGetData: error finding col matches\n");
	results = R_NilValue;
#endif
	fclose (tsvp);
	error ("tsvGetData: col match not found");
    }

    maxColWanted = -1;
    NcolResult = 0;
    for (ii = 0; ii < NcolPattern; ii++) {
	ff = colposns[ii];
	if (ff >= 0) {
	    if (ff > maxColWanted) maxColWanted = ff;
	    NcolResult++;
	}
    }
    PROTECT (colnames = allocVector (STRSXP, NcolResult)); nprotect++;
    wantedFields = (long *)R_alloc (maxColWanted+1, sizeof(long));
    for (ff = 0; ff <= maxColWanted; ff++) {
	wantedFields[ff] = -1;
    }
    // Three column name orders:
    // 1. Order of names in request list
    // 2. Order of names in tsv file
    // 3. Order of names in output matrix (which is a subset of 1).

    // wantedFields: order of names in tsv file -> order of names in output matrix.
    // colnames: order of names in request list -> order of names in output matrix.
    colid = 0;
    for (ii = 0; ii < NcolPattern; ii++) { // Iterate over names in request list.
	ff = colposns[ii];	// Get field number of name in tsv file, if any.
	if (ff >= 0) {
	    wantedFields[ff] = colid;
	    SET_STRING_ELT (colnames, colid, STRING_ELT(colpatterns, ii));
	    colid++;
	}
    }
#ifdef DEBUG
    Rprintf ("  tsvGetData: found %d column matches\n", NcolResult);
#endif

    PROTECT (rownames = allocVector(STRSXP, NrowResult)); nprotect++;
    PROTECT (results = allocVector(STRSXP, NrowResult*NcolResult)); nprotect++;

    rowid = 0;
    for (ii = 0; ii < NrowPattern; ii++) {
	if (rowposns[ii] >= 0) {
	    SET_STRING_ELT (rownames, rowid, STRING_ELT(rowpatterns, ii));
	    get_tsv_fields (results, NrowResult, rowid++, tsvp, rowposns[ii], maxColWanted, wantedFields);
	}
    }
    fclose (tsvp);

    PROTECT (results = add_dims (results, NrowResult, NcolResult));
    nprotect++;

    PROTECT (dimnames = allocVector (VECSXP, 2)); nprotect++;
    SET_VECTOR_ELT(dimnames, 0, rownames);
    SET_VECTOR_ELT(dimnames, 1, colnames);

    setAttrib (results, R_DimNamesSymbol, dimnames);
#ifdef DEBUG
    Rprintf ("< tsvGetData\n");
#endif
    UNPROTECT (nprotect);
    return results;
}

