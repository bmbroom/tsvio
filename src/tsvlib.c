/* Copyright 2013 UT MD Anderson Cancer Center.
 *
 * Author : Bradley Broom
 */

/* This is the C language component of the R tsvio library.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#include <share.h>
#include <sys/stat.h>
#else
#include <unistd.h>  /* For unlink */
#endif
#include <math.h>
#include <sys/types.h>

#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <Rinternals.h>

#include "dht.h"
#include "tsvio.h"

/* Size of per line input buffer. */
#define LINEBUFFERSIZE	(10*1024*1024)

SEXP tsvGenIndex (SEXP dataFile, SEXP indexFile);
SEXP tsvGetLines (SEXP dataFile, SEXP indexFile, SEXP patterns, SEXP findany);
SEXP tsvGetData (SEXP dataFile, SEXP indexFile, SEXP rowpatterns, SEXP colpatterns, SEXP dtype, SEXP findany);

/* Allocate resources needed by the package.
 */
void
R_init_tsvio (DllInfo *info)
{
    static const R_CallMethodDef callMethods[] = {
	{ "tsvGenIndex", (DL_FUNC) &tsvGenIndex, 2 },
	{ "tsvGetLines", (DL_FUNC) &tsvGetLines, 4 },
	{ "tsvGetData",  (DL_FUNC) &tsvGetData,  6 },
	{ NULL, NULL, 0 }
    };
    R_registerRoutines (info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols (info, FALSE);
}

/* Release resources used by the package.
 */
void
R_unload_tsvio (DllInfo *info)
{
}

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

int
is_fatal_error (enum status res)
{
    return (res != OK) && (res != EMPTY_FILE) && (res != INCOMPLETE_LAST_LINE);
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
	    warning ("%s: last line of tsvfile '%s' is incomplete\n", name, CHAR(STRING_ELT(dataFile,0)));
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
    long ii;

    PROTECT (dataFile = AS_CHARACTER(dataFile));
    PROTECT (indexFile = AS_CHARACTER(indexFile));

    if (length(dataFile) == 0 || length(indexFile) == 0) {
        error ("parameter cannot be NULL");
    }

    if (length(dataFile) != length(indexFile)) {
        error ("parameters dataFile and indexFile must have the same length");
    }

    for (ii = 0; ii < length(dataFile); ii++) {
	tsvp = fopen (CHAR(STRING_ELT(dataFile,ii)), "rb");
	if (tsvp == NULL) {
	    error ("unable to open datafile '%s' for reading", CHAR(STRING_ELT(dataFile,ii)));
	}
	indexp = fopen (CHAR(STRING_ELT(indexFile,ii)), "wb");
	if (indexp == NULL) {
	    fclose (tsvp);
	    error ("unable to open indexfile '%s' for writing", CHAR(STRING_ELT(indexFile,ii)));
	}
	res = generate_index (tsvp, indexp);
	fclose (tsvp);
	fclose (indexp);
	report_genindex_errors (res, "tsvGenIndex", dataFile, indexFile);
    }
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
	warning ("get_tsv_line: line starting at %ld is prematurely terminated by EOF\n", posn);
    }
	
    buffer[len++] = '\n'; /* Check above ensures space for this. */
#ifdef DEBUG
    Rprintf ("< get_tsv_line_buffer (len=%d)\n", len);
#endif
    return len;
}

SEXP
get_tsv_line_buffer_SEXP (char *buffer, size_t bufsize, FILE *tsvp, long posn)
{
    int len;
    len = get_tsv_line_buffer (buffer, bufsize, tsvp, posn);
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
    long posn;
    long ii;
    enum status res;
    dynHashTab *dht;
    char *buffer;
    
#ifdef DEBUG
    Rprintf ("> tsvGetLines\n");
#endif

    /* Convert, if necessary, data into expected format. */
    PROTECT (dataFile = AS_CHARACTER(dataFile));
    PROTECT (indexFile = AS_CHARACTER(indexFile));
    PROTECT (patterns = AS_CHARACTER(patterns));
    PROTECT (findany = AS_LOGICAL(findany));
    nprotect += 4;

    if (length(dataFile) == 0 || length(indexFile) == 0 || length(patterns) == 0) {
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

    /* Create temporary hash table of labels we're looking for. */
    dht = newDynHashTab (1024, 0);
    for (ii = 0; ii < Npattern; ii++) {
	const char *str = CHAR(STRING_ELT(patterns,ii));
	insertStrVal (dht, str, strlen (str), -1L);
    }
    res = scan_index_file (indexp, dht, Npattern == 0);
    fclose (indexp);

    if (res != OK) {
	error ("I/O or format problem scanning index file");
    }

    /* Verify that we found the required number of labels. */
    Nresult = Npattern - countValues (dht, -1L);
#ifdef DEBUG
    Rprintf ("  tsvGetLines: found %d matches\n", Nresult);
#endif
    if ((Nresult == 0) || (!(LOGICAL(findany)[0]) && (Nresult != Npattern))) {
#ifdef DEBUG
	Rprintf ("  tsvGetLines: error finding matches\n");
#endif
	freeDynHashTab (dht);
	error ("tsvGetLines: match not found");
    }

    /* Return TSV header and selected lines. */

    PROTECT (results = allocVector(STRSXP, Nresult+1)); /* Includes header. */
    nprotect++;

    tsvp = fopen (CHAR(STRING_ELT(dataFile,0)), "rb");
    if (tsvp == NULL) {
	freeDynHashTab (dht);
	error ("tsvGetLines: unable to open datafile '%s' for reading\n", CHAR(STRING_ELT(dataFile,0)));
    }

    /* Allocate line buffer. */
    buffer = (char *)malloc(LINEBUFFERSIZE);
    if (buffer == NULL) error ("unable to allocate line buffer\n");
    SET_STRING_ELT (results, 0, get_tsv_line_buffer_SEXP (buffer, LINEBUFFERSIZE, tsvp, 0L));
    Nresult = 1;
    initIterator (dht, &ii);
    while (getNextStr (dht, &ii, NULL, NULL, NULL, &posn)) {
	SET_STRING_ELT (results, Nresult, get_tsv_line_buffer_SEXP (buffer, LINEBUFFERSIZE, tsvp, posn));
	Nresult++;
    }
    free (buffer);
    fclose (tsvp);
    freeDynHashTab (dht);

#ifdef DEBUG
    Rprintf ("< tsvGetLines\n");
#endif
    UNPROTECT (nprotect);
    return results;
}

static void set_result_str (SEXP result, long idx, char *s, long n)
{
    SET_STRING_ELT (result, idx, mkCharLen(s, n));
}

static void set_result_int (SEXP result, long idx, char *s, long n)
{
    long value;
    char *end;
    char *scopy = (char *)alloca(n+1);

    strncpy (scopy, s, n);
    scopy[n] = '\0';

    value = strtol (scopy, &end, 10);
    if (end == scopy) {
        if (scopy[0] == '\0' || strncmp (scopy, "NA", 2) == 0) {
	    value = NA_INTEGER;
	} else {
	    error ("Non-integer field '%.*s' encountered", n, scopy);
	}
    } else if (*end != '\t' && *end != '\n' && *end != '\r' && *end != '\0') {
	error ("unexpected non-numeric data following integer field: '%.*s'", n, scopy);
    }
    INTEGER(result)[idx] = value;
}

static void set_result_num (SEXP result, long idx, char *s, long n)
{
    double value;
    char *end;
    char *scopy = (char *)alloca(n+1);

    strncpy (scopy, s, n);
    scopy[n] = '\0';
    value = strtod (scopy, &end);
    if (end == scopy) {
        if (scopy[0] == '\0' || strncmp (scopy, "NA", 2) == 0) {
	    value = NA_REAL;
        } else if (strncmp (scopy, "-Inf", 4) == 0) {
	    value = R_NegInf;
        } else if (strncmp (scopy, "Inf", 3) == 0) {
	    value = R_PosInf;
	} else {
	    error ("Non-numeric field '%.*s' encountered", n, scopy);
	}
    } else if (*end != '\t' && *end != '\n' && *end != '\r' && *end != '\0') {
	error ("unexpected non-numeric data following numeric field: '%.*s'", n, scopy);
    }
    REAL(result)[idx] = value;
}

typedef void (*setterFunction) (SEXP, long, char *, long);

setterFunction
get_result_setter (SEXP dtype)
{
    if (IS_CHARACTER(dtype)) return set_result_str;
    if (IS_INTEGER(dtype)) return set_result_int;
    if (IS_NUMERIC(dtype)) return set_result_num;
    return NULL;
}

/* R matrix is laid out in column-major order.
 */
void
get_tsv_fields (SEXP result,	     /* Destination R 'matrix' */
		setterFunction setResult, /* For setting an element of result */
		long nrows,	     /* Number of rows in result. */
		long rowid,	     /* Row of result in which to save fields from this line. */
		FILE *tsvp,	     /* Open file from which to read data. */
		long rowposn,	     /* Offset in bytes from start of file to this row's data. */
		long maxColumnWanted,/* Largest column we need. */
		long *columnMap,     /* Col of result in which to save field, or -1L if not wanted. */
		char *buffer,	     /* Line buffer for (re-)use by this function. */
		long buffer_size)    /* Number of bytes in buffer. */
{
    long linelen;
    long indexp;
    long fstart;
    long inputColumn, outputColumn;

    /* Read line into buffer. */
    linelen = get_tsv_line_buffer (buffer, buffer_size, tsvp, rowposn);

    indexp = 0;
    /* Advance over first column (row header) and its terminator. */
    while ((indexp < linelen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
	indexp++;
    }
    if (indexp < linelen) indexp++; /* Advance over field-terminator, if any. */

    inputColumn = 0;
    /* Assert: inputColumn data columns in the buffer have been processed. */
    /* Assert: indexp is positioned at start of a field or immediately following buffer contents. */
    while ((inputColumn <= maxColumnWanted) && (indexp < linelen)) {

	/* Read field. */
	fstart = indexp;
	while ((indexp < linelen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
	    indexp++;
	}

	/* Insert inputColumn into output matrix if required. */
	if (inputColumn <= maxColumnWanted) {
	    outputColumn = columnMap[inputColumn];
	    if (outputColumn >= 0) {
		setResult (result, outputColumn*nrows+rowid, buffer+fstart, indexp-fstart);
	    }
	}

	if (indexp < linelen) indexp++; /* Advance over field-terminator, if any. */
	inputColumn++;
    }
}

enum status
scan_header_line (dynHashTab *dht, FILE *tsvp, int insertall, char *buffer, long buffersize)
{
    long rowlen, linelen, headercols, rowcols, numpats;
    long indexp;
    long fstart;
    char *s;

    /* Determine number of columns on first and second lines. Input header line. */
    fseek (tsvp, 0L, SEEK_SET);
    if (!fgets (buffer, buffersize, tsvp)) {
        error ("unable to read data file header line");
    }
    if (!fgets (buffer, buffersize, tsvp)) {
	/* File contains a header only? */
        return OK;
    }
    rowlen = strlen(buffer);
    rowcols = num_columns (buffer, rowlen);
    fseek (tsvp, 0L, SEEK_SET);
    if (!(s = fgets (buffer, buffersize, tsvp))) {
        error ("unable to re-read data file header line");
    }
    linelen = strlen (buffer);
    headercols = num_columns (buffer, linelen);

    #ifdef DEBUG
        Rprintf ("> scan_header_line: headercols=%ld, rowcols=%d, headerlen=%ld, rowlen=%ld, buffersize=%ld\n",
	         headercols, rowcols, linelen, rowlen, buffersize);
    #endif

    numpats = 0;
    indexp = 0;
    /* Assert: numpats fields have been inserted into the dht this call. */
    /* Assert: indexp is positioned at start of a field or immediately following buffer contents. */
    while (indexp < linelen) {

	/* Read field (aka pattern). */
	fstart = indexp;
	while ((indexp < linelen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
	    indexp++;
	}

	/* Insert field into dht if not first non-R-style column header. */
	if ((fstart > 0) || (rowcols != headercols)) {
	    if (insertall) {
		insertStrVal (dht, buffer+fstart, indexp-fstart, numpats);
	    } else {
		changeStrVal (dht, buffer+fstart, indexp-fstart, numpats);
	    }
	    numpats++;
	}

	if (indexp < linelen) indexp++; /* Advance over field-terminator, if any. */
    }
    if (numpats != (rowcols-1)) {
        error ("scan_header_line: program bug detected: number of patterns (%ld) differs from number of data columns (%ld)\n", numpats, rowcols-1);
    }
    return OK;
}

SEXP
autoRowPatterns (FILE *indexfile)
{
    char *buffer;
    long linelen, numpats, indexp;
    SEXP element, pats;

    buffer = (char *)malloc(LINEBUFFERSIZE);
    if (buffer == NULL) error ("unable to allocate line buffer\n");

    /* Count number of row index entries, allocate vector. */
    rewind (indexfile);
    numpats = 0;
    while (fgets (buffer, LINEBUFFERSIZE, indexfile)) {
        numpats++;
    }
    PROTECT (pats = allocVector(STRSXP, numpats));

    /* Read each row label and assign to list of patterns. */
    rewind (indexfile);
    numpats = 0;
    while (fgets (buffer, LINEBUFFERSIZE, indexfile)) {
	linelen = strlen (buffer);
	indexp = 0;
	while ((indexp < linelen) && buffer[indexp] != '\t' && buffer[indexp] != '\n') {
	    indexp++;
	}
	element = mkCharLen(buffer, indexp);
	SET_STRING_ELT (pats, numpats, element);
        numpats++;
    }

    free (buffer);

    UNPROTECT (1);
    return pats;
}

void
closeTsvFiles (long numFiles, FILE **tsvpp, FILE **indexpp)
{
    long ii;
    if (tsvpp) {
        for (ii = 0; ii < numFiles; ii++)
	    if (tsvpp[ii])
	        fclose (tsvpp[ii]);
	free (tsvpp);
    }
    if (indexpp) {
        for (ii = 0; ii < numFiles; ii++)
	    if (indexpp[ii])
	        fclose (indexpp[ii]);
	free (indexpp);
    }
}

SEXP
dhtToStringVec (const dynHashTab *dht)
{
    SEXP names;
    long ii;
    const char *str;
    long len;
    long order;

    PROTECT (names = allocVector(STRSXP, dhtNumStrings(dht)));
    initIterator (dht, &ii);
    while (getNextStr (dht, &ii, &str, &len, &order, NULL)) {
	SET_STRING_ELT (names, order, mkCharLen(str,len));
    }
    UNPROTECT (1);
    return names;
}

typedef struct {
    long rowPosn;	/* Byte offset of desired row in file. */
    long outputRow;	/* Row index of row in destination matrix. */
} rowInfo_t;

int
compare_rowInfo_t (const void *a, const void *b)
{
    const rowInfo_t *ap = (rowInfo_t *)a;
    const rowInfo_t *bp = (rowInfo_t *)b;

    if (ap->rowPosn < bp->rowPosn) return -1;
    if (ap->rowPosn > bp->rowPosn) return 1;
    return 0;
}

/* Read the contents of one data file and store the results in the destination matrix results.
 */
void
getDataFromFile (SEXP results,	    /* Destination matrix. */
		 setterFunction setResult, /* For setting an element of results */
		 long NrowResult,   /* Number of rows in destination matrix. */
		 FILE *indexp,	    /* File descriptor for index file. */
		 FILE *tsvp,	    /* File descriptor for data file. */
		 dynHashTab *rowdht,/* DHT containing desired row labels. */
		 dynHashTab *coldht,/* DHT containing desired column labels. */
		 char *buffer,	    /* Buffer for (re-)use by this function. */
		 long buffersize)   /* Number of bytes in buffer. */
{
    enum status res;
    long ii, inputColumn, outputColumn;
    long maxInputColumn, *columnMap;
    rowInfo_t *rowInfo;
    long rowsWanted, nrow;

    /* Determine desired rows in this file, and their byte offset in this file. */
    setAllValues (rowdht, -1L);
    res = scan_index_file (indexp, rowdht, 0);
    if (res != OK) {
	warn ("problem scanning index file, skipping\n");
	return;
    }
    rowsWanted = countNotValues (rowdht, -1L);
    if (rowsWanted == 0) {
	warn ("input file matches no desired row labels, skipping\n");
	return;
    }

    /* Determine desired columns in this file, and their column number in this file. */
    setAllValues (coldht, -1L);
    res = scan_header_line (coldht, tsvp, 0, buffer, buffersize);
    if (res != OK || countNotValues (coldht, -1L) == 0) {
	warn ("input file matches no desired column labels, skipping\n");
	return;
    }

    // That are three column name orders:
    // 1. Order of names in original request list (no longer available)
    // 2. Order of names in this tsv file (called inputColumns below)
    // 3. Order of names in output matrix (called outputColumns below)

    // We generate here a mapping from the order of columns in this tsv file (input columns)
    // to the order of columns in the output matrix:  outputColumn == columnMap[inputColumn].
    // columnMap[inputColumn] == -1L iff inputColumn is not contained in the output matrix.
    // We make columnMap long enough to contain the largest wanted input column.
    maxInputColumn = -1L;
    initIterator (coldht, &ii);
    while (getNextStr (coldht, &ii, NULL, NULL, NULL, &inputColumn)) {
	if (inputColumn > maxInputColumn) maxInputColumn = inputColumn;
    }
    columnMap = (long *)R_alloc (maxInputColumn+1, sizeof(long));
    for (ii = 0; ii <= maxInputColumn; ii++) {
	columnMap[ii] = -1;
    }
    initIterator (coldht, &ii);
    while (getNextStr (coldht, &ii, NULL, NULL, &outputColumn, &inputColumn)) {
	if (inputColumn >= 0) {
	    columnMap[inputColumn] = outputColumn;
	}
    }

    // Scan rows present in this tsv file.
    // First sort rows into ascending positions within the input file.
    rowInfo = (rowInfo_t *)R_alloc (rowsWanted, sizeof(rowInfo_t));
    nrow = 0;
    initIterator (rowdht, &ii);
    while (nrow < rowsWanted && getNextStr (rowdht, &ii, NULL, NULL, &rowInfo[nrow].outputRow, &rowInfo[nrow].rowPosn)) {
	if (rowInfo[nrow].rowPosn >= 0L) {
	    nrow++;
	}
    }
    qsort (rowInfo, rowsWanted, sizeof(rowInfo_t), compare_rowInfo_t);
    for (nrow = 0; nrow < rowsWanted; nrow++) {
	get_tsv_fields (results, setResult, NrowResult, rowInfo[nrow].outputRow, tsvp, rowInfo[nrow].rowPosn, maxInputColumn, columnMap, buffer, buffersize);
    }
}

SEXP
tsvGetData (SEXP dataFile, SEXP indexFile, SEXP rowpatterns, SEXP colpatterns, SEXP dtype, SEXP findany)
{
    /* Local variables that must have a defined value before jumping to the exit. */
    long nprotect = 0;
    long numFiles = 0;
    FILE **tsvpp = NULL, **indexpp = NULL;
    SEXP results = R_NilValue;
    setterFunction setResult;

    /* Other local variables (exit code will not clean up). */
    long NrowPattern, NrowResult;
    long NcolPattern, NcolResult;
    SEXP dimnames;
    long ii;
    enum status res;
    char *buffer;
#ifdef _WIN32
    char tmpname[] = "tmpXXXXXX";
    char tmpname2[10];
    int rez;
#else
    char tmpname[] = "/tmp/tsvindex-XXXXXX";
#endif
    int tmpfd;
    dynHashTab *rowdht, *coldht;
    
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

    setResult = get_result_setter (dtype);
    if (setResult == NULL) {
        error ("unable to directly load data matrices of type dtype");
    }

    numFiles = length(dataFile);
    if (numFiles == 0) {
        error ("parameter dataFile cannot be NULL\n");
    }

    if (length (dataFile) != length(indexFile)) {
        error ("parameters dataFile and indexFile must have the same length\n");
    }

    buffer = (char *)malloc(LINEBUFFERSIZE);
    if (buffer == NULL) error ("unable to allocate line buffer\n");
    tsvpp = (FILE **)malloc(sizeof(FILE *) * numFiles);
    if (tsvpp == NULL) error ("unable to allocate file handles for %ld tsv files\n", numFiles);
    for (ii = 0; ii < numFiles; ii++) tsvpp[ii] = NULL;
    indexpp = (FILE **)malloc(sizeof(FILE *) * numFiles);
    if (indexpp == NULL) error ("unable to allocate file handles for %ld index files\n", numFiles);
    for (ii = 0; ii < numFiles; ii++) indexpp[ii] = NULL;

    /* Open all data files. */
    for (ii = 0; ii < numFiles; ii++) {
	tsvpp[ii] = fopen (CHAR(STRING_ELT(dataFile,ii)), "rb");
	if (tsvpp[ii] == NULL) {
	    free (buffer);
	    closeTsvFiles (numFiles, tsvpp, indexpp);
	    error ("unable to open datafile '%s' for reading\n", CHAR(STRING_ELT(dataFile,ii)));
	}
    }

    /* Open all index files. */
    for (ii = 0; ii < numFiles; ii++) {
	indexpp[ii] = fopen (CHAR(STRING_ELT(indexFile,ii)), "rb");
	if (indexpp[ii] == NULL) {
	    warning ("unable to read index file '%s': attempting to create\n", CHAR(STRING_ELT(indexFile,ii)));
	    indexpp[ii] = fopen (CHAR(STRING_ELT(indexFile,ii)), "wb+");
	    if (indexpp[ii] == NULL) {
		warning ("unable to create indexfile '%s': try to create a temp file\n", CHAR(STRING_ELT(indexFile,ii)));
#ifdef _WIN32
                strcpy_s (tmpname2, sizeof(tmpname2), tmpname);
		rez = _mktemp_s (tmpname2, sizeof(tmpname2));
                if (rez == 0) {
                    _sopen_s (&tmpfd, tmpname2, _O_RDWR | _O_CREAT | _O_TEMPORARY | _O_SHORT_LIVED, _SH_DENYNO, _S_IREAD|_S_IWRITE);
                } else {
                    tmpfd = -1;
                }
#else
		tmpfd = mkstemp (tmpname);
#endif
		if (tmpfd < 0) {
		    free (buffer);
		    closeTsvFiles (numFiles, tsvpp, indexpp);
		    error ("tsvGetData: unable to create even a temporary indexfile\n");
		}
		indexpp[ii] = fdopen (tmpfd, "wb+");
#ifndef _WIN32
		unlink (tmpname);
#endif
	    }
	    res = generate_index (tsvpp[ii], indexpp[ii]);
	    if (is_fatal_error (res)) {
		free (buffer);
		closeTsvFiles (numFiles, tsvpp, indexpp);
	    }
	    report_genindex_errors (res, "tsvGetData", dataFile, indexFile);
	    rewind (tsvpp[ii]);
	    rewind (indexpp[ii]);
	}
    }

    /* Insert explicitly specified row patterns. */
    NrowPattern = length(rowpatterns);
    rowdht = newDynHashTab (1024, NrowPattern == 0 ? DHT_STRDUP : 0);
#ifdef DEBUG
    Rprintf ("  tsvGetData: received %d explicitly specified rowpatterns\n", NrowPattern);
#endif
    if (NrowPattern > 0) {
	for (ii = 0; ii < NrowPattern; ii++) {
	    const char *str = CHAR(STRING_ELT(rowpatterns,ii));
	    insertStrVal (rowdht, str, strlen (str), -1L);
	}
    }

    /* Scan all index files for matching row labels. */
    for (ii = 0; ii < numFiles; ii++) {
	res = scan_index_file (indexpp[ii], rowdht, NrowPattern == 0);
	if (res != OK) {
	    free (buffer);
	    closeTsvFiles (numFiles, tsvpp, indexpp);
	    freeDynHashTab (rowdht);
	    error ("i/o or syntax error %d processing indexfile %d\n", res, ii+1);
	}
    }

    NrowResult = countNotValues (rowdht, -1L);
#ifdef DEBUG
    Rprintf ("  tsvGetData: found %d row matches\n", NrowResult);
#endif
    if (NrowResult == 0) {
	free (buffer);
	closeTsvFiles (numFiles, tsvpp, indexpp);
	freeDynHashTab (rowdht);
        error ("no matching rows found\n");
    }
    if (NrowResult != NrowPattern && NrowPattern > 0 && !LOGICAL(findany)[0]) {
	free (buffer);
	closeTsvFiles (numFiles, tsvpp, indexpp);
	freeDynHashTab (rowdht);
        error ("not all required row patterns were matched\n");
    }

    if (NrowPattern > 0) {
	/* Create new hashtab containing only found row patterns */
        dynHashTab *tmpdht = newDynHashTab (NrowResult*2, 0);
	const char *str;
	long posn, len;
	for (ii = 0; ii < NrowPattern; ii++) {
	    str = CHAR(STRING_ELT(rowpatterns,ii));
	    len = strlen (str);
	    posn = getStringValue (rowdht, str, len);
	    if (posn >= 0) {
	        insertStrVal (tmpdht, str, len, posn);
	    }
	}
	free (rowdht);
	rowdht = tmpdht;
    }

    /* Scan all header lines for matching column labels. */

    NcolPattern = length(colpatterns);
#ifdef DEBUG
    Rprintf ("  tsvGetData: received %d explicitly specified column patterns\n", NcolPattern);
#endif
    coldht = newDynHashTab (1024, NcolPattern == 0 ? DHT_STRDUP : 0);
    for (ii = 0; ii < NcolPattern; ii++) {
	const char *str = CHAR(STRING_ELT(colpatterns,ii));
	insertStrVal (coldht, str, strlen (str), -1L);
    }
    for (ii = 0; ii < numFiles; ii++) {
	res = scan_header_line (coldht, tsvpp[ii], NcolPattern == 0, buffer, LINEBUFFERSIZE);
	if (res != OK) {
	    free (buffer);
	    closeTsvFiles (numFiles, tsvpp, indexpp);
	    freeDynHashTab (rowdht);
	    freeDynHashTab (coldht);
	    error ("i/o or syntax error scanning header of datafile %d\n", ii+1);
	}
    }

    NcolResult = countNotValues (coldht, -1L);
#ifdef DEBUG
    Rprintf ("  tsvGetData: found %d col matches\n", NcolResult);
#endif
    if (NcolResult == 0) {
	free (buffer);
	closeTsvFiles (numFiles, tsvpp, indexpp);
	freeDynHashTab (rowdht);
	freeDynHashTab (coldht);
        error ("no matching cols found\n");
    }
    if (NcolResult != NcolPattern && NcolPattern > 0 && !LOGICAL(findany)[0]) {
	free (buffer);
	closeTsvFiles (numFiles, tsvpp, indexpp);
	freeDynHashTab (rowdht);
	freeDynHashTab (coldht);
        error ("not all required col patterns were matched\n");
    }

    if (NcolPattern > 0) {
	/* Create new hashtab containing only found col patterns */
        dynHashTab *tmpdht = newDynHashTab (NcolResult*2, 0);
	const char *str;
	long posn, len;
	for (ii = 0; ii < NcolPattern; ii++) {
	    str = CHAR(STRING_ELT(colpatterns,ii));
	    len = strlen (str);
	    posn = getStringValue (coldht, str, len);
	    if (posn >= 0) {
	        insertStrVal (tmpdht, str, len, posn);
	    }
	}
	free (coldht);
	coldht = tmpdht;
    }


    /* Allocate space for result. */
    PROTECT (results = allocVector(TYPEOF(dtype), NrowResult*NcolResult)); nprotect++;
    for (ii = 0; ii < numFiles; ii++) {
	getDataFromFile (results, setResult, NrowResult,
	                 indexpp[ii], tsvpp[ii],
			 rowdht, coldht,
			 buffer, LINEBUFFERSIZE);
    }

    /* Add dimensions and row/column names to the results matrix. */
    PROTECT (results = add_dims (results, NrowResult, NcolResult));
    nprotect++;
    PROTECT (dimnames = allocVector (VECSXP, 2)); nprotect++;
    SET_VECTOR_ELT(dimnames, 0, dhtToStringVec (rowdht));
    SET_VECTOR_ELT(dimnames, 1, dhtToStringVec (coldht));
    setAttrib (results, R_DimNamesSymbol, dimnames);

#ifdef DEBUG
    Rprintf ("< tsvGetData\n");
#endif
    free (buffer);
    closeTsvFiles (numFiles, tsvpp, indexpp);
    freeDynHashTab (rowdht);
    freeDynHashTab (coldht);
    UNPROTECT (nprotect);
    return results;
}

