#' Simple Utilities for Tab Separated Value (TSV) Files
#'
#' Utilities for indexing and rapidly loading (subsets of) data from (large) tab separated value (TSV) files.
#' The TSV files are required to have a unique row label in the first column of each line and a unique
#' column label in the first line of the file.
#' Files may be formatted in either spreadsheet/Unix format (same number of fields on each line) or R format
#' (one less column on the first line only).
#' The data matrix in the files are expected to have the same data types in all entries.  (The row and column
#' labels are always expected to be strings.)
#'
#' @name tsvio-package
#' @title tsvio: Simple Utilities for Tab Separated Value (TSV) Files
#' @docType package
#' @useDynLib tsvio
#'
#' @seealso tsvGenIndex, tsvGetLines, tsvGetData
NULL

#' Produce a simple index of a tsv file.
#'
#' This function reads a TSV file and produces an index to the start of each row.
#' The TSV file is required to have a header line and at least one data line.  The header line may
#' contain either the same number or one fewer columns than the data lines, which must all contain
#' the same number of columns.  The first column of each data line will be indexed.
#'
#' @param filename The name (and path) of the file(s) containing the data to index.
#'
#' @param indexfile The name (and path) of the file(s) to which the index will be written.  There must
#' be exactly one index file for every filename.
#'
#' @export
#'
#' @examples
#' datafile = tempfile("data");
#' writeLines(c("C1\tC2", "R1\tFoo\tBar", "R2\tBoing\tBoing", "R3\tThe\tEnd"), file(datafile));
#' indexfile = tempfile("index");
#' tsvGenIndex (datafile, indexfile)
#'
#' @seealso tsvGetLines, tsvGetData
tsvGenIndex <- function (filename, indexfile) {
    return (.Call  ("tsvGenIndex", filename, indexfile));
}

#' Read matching lines from a tsv file, using a pre-computed index file.
#'
#' This function reads lines that match the given patterns from a TSV file with the assistance of
#' a pre-computed index file to the start of each row.
#'
#' The index file must have been created by tsvGenIndex and the data file must not have changed
#' since the index file was created.
#'
#' @param filename The name (and path) of the file containing the data to index.
#'
#' @param indexfile The name (and path) of the file to which the index will be written.
#'
#' @param patterns A vector of strings containing the string to match against the index entries.  Only
#' lines with keys that exactly match at least one pattern string are returned.
#'
#' @param findany If false, all patterns must be matched. If true (default) at least one pattern must match.
#'
#' @return A matrix containing one row for each matched line.  Columns are described by the header line.
#'
#' @export
#'
#' @examples
#' datafile = tempfile("data");
#' writeLines(c("C1\tC2", "R1\tFoo\tBar", "R2\tBoing\tBoing", "R3\tThe\tEnd"), file(datafile));
#' indexfile = tempfile("index");
#' tsvGenIndex (datafile, indexfile);
#' tsvGetLines (datafile, indexfile, c("R1", "R3"))
#'
#' @seealso tsvGenIndex, tscGetData
tsvGetLines <- function (filename, indexfile, patterns, findany=TRUE) {
    .Call("tsvGetLines", filename, indexfile, patterns, findany)
}

#' Read matching lines from a tsv file, using a pre-computed index file.
#'
#' This function reads lines that match the given patterns from a TSV file with the assistance of
#' a pre-computed index file to the start of each row.
#'
#' The index file must have been created by tsvGenIndex and the data file must not have changed
#' since the index file was created.
#'
#' @param filename The name (and path) of the file containing the data to index.
#'
#' @param indexfile The name (and path) of the file to which the index will be written.
#'
#' @param rowpatterns A vector of strings containing the string to match against the index entries.  Only
#' lines with keys that exactly match at least one pattern string are returned.
#' @param colpatterns A vector of strings to match against the column headers in the first row
#'
#' @param dtype A prototype element that specifies by example the type of matrix to return.  The
#' value of the parameter is ignored.  Accepted types are string (default), numeric (float), and integer.
#'
#' @param findany If false, all patterns must be matched. If true (default) at least one pattern must match.
#'
#' @return A matrix containing one row for each matched line and one column for each matched column.
#'
#' @export
#'
#' @examples
#' datafile = tempfile("data");
#' writeLines(c("x\tC1\tC2", "R1\tFoo\tBar", "R2\tBoing\tBoing", "R3\tThe\tEnd"), file(datafile));
#' indexfile = tempfile("index");
#' tsvGenIndex (datafile, indexfile);
#' tsvGetData (datafile, indexfile, c("R1", "R3"), c('C2'))
#'
#' @seealso tsvGenIndex, tsvGetLines
tsvGetData <- function (filename, indexfile, rowpatterns, colpatterns, dtype="", findany=TRUE) {
    .Call("tsvGetData", filename, indexfile, rowpatterns, colpatterns, dtype, findany)
}
