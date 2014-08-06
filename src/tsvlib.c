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

void
report_genindex_errors (enum status res, char *name, SEXP dataFile, SEXP indexFile)
{
    if (res == EMPTY_FILE)
	warning ("%s: Warning: tsvfile '%s' is empty\n", name, CHAR(STRING_ELT(dataFile,0)));
    else if (res != OK) {
	if (res == WRITE_ERROR)
	    error ("%s: error writing to indexfile '%s'\n", name, CHAR(STRING_ELT(indexFile,0)));
	else if (res == INCOMPLETE_LAST_LINE)
	    error ("%s: last line of tsvfile '%s' is incomplete\n", name, CHAR(STRING_ELT(dataFile,0)));
	else if (res == NO_LABEL_ERROR)
	    error ("%s: line of tsvfile '%s' does not contain a label\n", name, CHAR(STRING_ELT(dataFile,0)));
	else
	    error ("%s: unknown internal error\n", name);
    }
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
    report_genindex_errors (res, "tsvGenIndex", dataFile, indexFile);
    UNPROTECT (2);
    return R_NilValue;
}

int
get_tsv_line_buffer (char *buffer, size_t bufsize, FILE *tsvp, long posn)
{
    int	ch, len;

#ifdef DEBUG
    Rprintf ("> get_tsv_line_buffer (posn=%ld)\n", posn);
#endif
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
#ifdef DEBUG
    Rprintf ("< get_tsv_line_buffer (len=%d)\n", len);
#endif
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
get_tsv_fields (char *buffer, long buffer_size, long headercols, SEXP strvec, long nrows, long rowid, FILE *tsvp, long rowposn, long maxFieldWanted, long *fieldWanted)
{
    long len;
    long linelen;
    long linecols;
    long indexp;
    long fstart;
    SEXP element;
    long infield, outfield;


    /* Read line into buffer. */
    linelen = get_tsv_line_buffer (buffer, buffer_size, tsvp, rowposn);
    linecols = num_columns (buffer, linelen);

    /* Validate number of columns and accommodate 'R-style' tsv files with one less header column. */
    if (headercols == linecols) {
        /* first header field is not a column label, so one less column header. */
	/* But first check we didn't match that first label. */
	if (fieldWanted[0] >= 0) {
	    error ("tsvGetData: for row %ld: number of columns (%ld) equals number of header columns, but first column header has matched a column pattern\n", rowid, linecols);
	}
	/* advance over unwanted field. */
	fieldWanted++;
	maxFieldWanted--;
    } else if (headercols == (linecols-1)) {
        /* No need to adjust fieldWanted. */
    } else {
        error ("tsvGetData: for row %ld: number of columns (%ld) does not match number of header columns (%ld)\n", rowid, linecols, headercols);
    }

    infield = 0;
    indexp = 0;
    /* Assert: infield fields in the buffer have been processed. */
    /* Assert: indexp is positioned at start of a field or immediately following buffer contents. */
    while ((infield <= maxFieldWanted) && (indexp < linelen)) {

	/* Read field. */
	fstart = indexp;
	while ((indexp < linelen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
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

	if (indexp < linelen) indexp++; /* Advance over field-terminator, if any. */
	infield++;
    }
}

SEXP
autoColPatterns (FILE *tsvp, long rowposn)
{
    char buffer[1024*1024];
    long linelen, headercols, rowcols, numpats;
    SEXP element, pats;
    long indexp;
    long fstart;

    if (rowposn < 0) {
	linelen = get_tsv_line_buffer (buffer, sizeof(buffer), tsvp, 0L);
	headercols = num_columns (buffer, linelen);
	rowcols = headercols + 1; /* Assume R-style. */
    } else {
	linelen = get_tsv_line_buffer (buffer, sizeof(buffer), tsvp, rowposn);
	rowcols = num_columns (buffer, linelen);
	linelen = get_tsv_line_buffer (buffer, sizeof(buffer), tsvp, 0L);
	headercols = num_columns (buffer, linelen);
    }
    Rprintf  ("headercols: %ld, rowcols: %ld\n", headercols, rowcols);

    PROTECT (pats = allocVector(STRSXP, rowcols-1));
    numpats = 0;
    indexp = 0;
    /* Assert: numpats fields have been copied into pats. */
    /* Assert: indexp is positioned at start of a field or immediately following buffer contents. */
    while (indexp < linelen) {

	/* Read field (aka pattern). */
	fstart = indexp;
	while ((indexp < linelen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
	    indexp++;
	}

	Rprintf ("fstart: %ld, indexp: %ld, numpats: %ld\n", fstart, indexp, numpats);
	/* Insert field into list of patterns if not first non-R-style column header. */
	if ((fstart > 0) || (rowcols != headercols)) {
	    element = mkCharLen(buffer+fstart, indexp-fstart);
	    SET_STRING_ELT (pats, numpats++, element);
	}

	if (indexp < linelen) indexp++; /* Advance over field-terminator, if any. */
    }
    Rprintf ("indexp: %ld, linelen: %ld, numpats: %ld\n", indexp, linelen, numpats);
    if (numpats != (rowcols-1)) {
        error ("autoColPatterns: program bug detected: number of patterns (%ld) differs from number of data columns (%ld)\n", numpats, rowcols-1);
    }
    UNPROTECT (1);
    return pats;
}

SEXP
autoRowPatterns (FILE *indexfile)
{
    char buffer[1024*1024];
    long linelen, numpats, indexp;
    SEXP element, pats;

    /* Count number of row index entries, allocate vector. */
    rewind (indexfile);
    numpats = 0;
    while (fgets (buffer, sizeof(buffer), indexfile)) {
        numpats++;
    }
    PROTECT (pats = allocVector(STRSXP, numpats));

    /* Read each row label and assign to list of patterns. */
    rewind (indexfile);
    numpats = 0;
    while (fgets (buffer, sizeof(buffer), indexfile)) {
	linelen = strlen (buffer);
	indexp = 0;
	while ((indexp < linelen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
	    indexp++;
	}
	element = mkCharLen(buffer, indexp);
	SET_STRING_ELT (pats, numpats, element);
        numpats++;
    }

    UNPROTECT (1);
    return pats;
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
    char buffer[1024*1024];
    long tsvheaderlen;	   /* Number of bytes in tsv header line. */
    long tsvheadercols;    /* Number of columns in tsv header line. */
    
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

    if (dataFile == R_NilValue || indexFile == R_NilValue) {
        error ("tsvGetData: parameter cannot be NULL\n");
    }

    tsvp = fopen (CHAR(STRING_ELT(dataFile,0)), "rb");
    if (tsvp == NULL) {
	error ("tsvGetData: unable to open datafile '%s' for reading\n", CHAR(STRING_ELT(dataFile,0)));
    }

    indexp = fopen (CHAR(STRING_ELT(indexFile,0)), "rb");
    if (indexp == NULL) {
	warning ("tsvGetData: Warning: unable to read index file '%s': attempting to create\n", CHAR(STRING_ELT(indexFile,0)));
	indexp = fopen (CHAR(STRING_ELT(indexFile,0)), "wb+");
	if (indexp == NULL) {
	    fclose (tsvp);
	    error ("tsvGetData: unable to open indexfile '%s' for writing\n", CHAR(STRING_ELT(indexFile,0)));
	}
	res = generate_index (tsvp, indexp);
	if ((res != EMPTY_FILE) && (res != OK)) {
	    fclose (tsvp);
	    fclose (indexp);
	}
	report_genindex_errors (res, "tsvGetData", dataFile, indexFile);
	rewind (tsvp);
	rewind (indexp);
    }

    if (length (rowpatterns) == 0) {
	rowpatterns = autoRowPatterns (indexp);
	PROTECT (rowpatterns);
	rewind (indexp);
	nprotect++;
    }

    NrowPattern = length(rowpatterns);
    if (NrowPattern == 0) {
	fclose (tsvp);
	fclose (indexp);
        error ("tsvGetData: parameter rowpatterns cannot be empty\n");
    }
#ifdef DEBUG
    Rprintf ("  tsvGetData: received %d rowpatterns\n", NrowPattern);
#endif

    rowposns = (long *)R_alloc (NrowPattern, sizeof(long));
    if (rowposns == NULL) {
	fclose (tsvp);
	fclose (indexp);
        error ("tsgGetData: ERROR: unable to allocate working memory: rowposns\n");
    }
    rowpats = (const char **)R_alloc (NrowPattern, sizeof(char *));
    if (rowpats == NULL) {
	fclose (tsvp);
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
	fclose (tsvp);
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

    if (length(colpatterns) == 0) {
	colpatterns = autoColPatterns (tsvp, NrowResult == 0 ? -1 : rowposns[0]);
	PROTECT (colpatterns);
	nprotect++;
    }
    NcolPattern = length(colpatterns);
#ifdef DEBUG
    Rprintf ("  tsvGetData: received %d colpatterns\n", NcolPattern);
#endif

    colposns = (long *)R_alloc (NcolPattern, sizeof(long));
    if (colposns == NULL) {
	fclose (tsvp);
	error ("tsgGetData: ERROR: unable to allocate working memory for %d colposns\n", NcolPattern);
    }
    colpats = (const char **)R_alloc (NcolPattern, sizeof(char *));
    if (colpats == NULL) {
	fclose (tsvp);
	error ("tsgGetData: ERROR: unable to allocate working memory for %d colpats\n", NcolPattern);
    }
    for (ii = 0; ii < NcolPattern; ii++) {
	colpats[ii] = CHAR(STRING_ELT(colpatterns,ii));
    }

    /* Read TSV file header line. */
    tsvheaderlen = get_tsv_line_buffer (buffer, sizeof(buffer), tsvp, 0L);
    tsvheadercols = num_columns (buffer, tsvheaderlen);
#ifdef DEBUG
    buffer[tsvheaderlen] = '\0';
    warn ("Read tsv buffer %ld chars: %s", tsvheaderlen, buffer);
#endif

    res = find_col_indices (buffer, tsvheaderlen, LOGICAL(findany)[0], NcolPattern, colpats, colposns, warn);
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
	    get_tsv_fields (buffer, sizeof(buffer), tsvheadercols, results, NrowResult, rowid++, tsvp, rowposns[ii], maxColWanted, wantedFields);
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

