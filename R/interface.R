#' @name tsvio-package
#' @title tsvio: Simple utilities for Tab Separated Value (TSV) files
#' @docType package
#' @useDynLib tsvio
NULL

#' Produce a simple index of a tsv file.
#'
#' This function reads a TSV file and produces an index to the start of each row.
#' The TSV file is required to have a header line and at least one data line.  The header line may
#' contain either the same number or one fewer columns than the data lines, which must all contain
#' the same number of columns.  The first column of each data line will be indexed.
#'
#' @param filename The name (and path) of the file containing the data to index.
#'
#' @param indexfile The name (and path) of the file to which the index will be written.
#'
#' @export
#'
#' @examples
#' #tsvGenIndex ("data.tsv", "index.tsv")
#'
#' @seealso tsvGetLines
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
#' tab <- tsvGetLines ("data.tsv", "index.tsv", c("pattern1", "pattern2"))
#'
#' @seealso tsvGenIndex
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
#' @param findany If false, all patterns must be matched. If true (default) at least one pattern must match.
#'
#' @return A matrix containing one row for each matched line and one column for each matched column.
#'
#' @export
#'
#' @examples
#' tab <- tsvGetData ("data.tsv", "index.tsv", c("pattern1", "pattern2"), c('cpat1'))
#'
#' @seealso tsvGenIndex
tsvGetData <- function (filename, indexfile, rowpatterns, colpatterns, findany=TRUE) {
    .Call("tsvGetData", filename, indexfile, rowpatterns, colpatterns, findany)
}
