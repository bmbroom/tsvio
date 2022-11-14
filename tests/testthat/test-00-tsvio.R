library (tsvio)

gen_r_text_file <- function () {
    datafile = tempfile("data");
    con = file(datafile);
    writeLines(c("C1\tC2", "R1\tFoo\tBar", "R2\tBouncy\tBoing", "R3\tThe\tEnd"), con);
    close (con);
    indexfile = tempfile("index");
    expect_silent (tsvGenIndex (datafile, indexfile));
    return (list (datafile=datafile, indexfile=indexfile));
};

gen_r_numeric_file <- function () {
    datafile = tempfile("data");
    con = file(datafile);
    writeLines(c("C1\tC2", "R1\t1.1\t1.2", "R2\t2.1\t2.2", "R3\t3.1\t3.2"), con);
    close (con);
    indexfile = tempfile("index");
    expect_silent (tsvGenIndex (datafile, indexfile));
    return (list (datafile=datafile, indexfile=indexfile));
};

gen_r_integer_file <- function () {
    datafile = tempfile("data");
    con = file(datafile);
    writeLines(c("C1\tC2", "R1\t11\t12", "R2\t21\t22", "R3\t31\t32"), con);
    close (con);
    indexfile = tempfile("index");
    expect_silent (tsvGenIndex (datafile, indexfile));
    return (list (datafile=datafile, indexfile=indexfile));
};

gen_std_text_file <- function () {
    datafile = tempfile("data");
    con = file(datafile);
    writeLines(c("junk\tC1\tC2", "R1\tFoo\tBar", "R2\tBouncy\tBoing", "R3\tThe\tEnd"), con);
    close (con);
    indexfile = tempfile("index");
    expect_silent (tsvGenIndex (datafile, indexfile));
    return (list (datafile=datafile, indexfile=indexfile));
};

noeol <- function (s) {
    sub ("[\r\n]*$", "", s)
};

do_text_get_data <- function (tf) {
    expect_silent (mat <- tsvGetData (tf$datafile, tf$indexfile, NULL, NULL));
    expect_identical (dim(mat), as.integer(c(3, 2)));
    expect_silent (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R1","R2"), c("C2","C1")));
    expect_identical (dim(mat), as.integer(c(2, 2)));
    expect_identical (rownames(mat), c("R1", "R2"));
    expect_identical (colnames(mat), c("C2", "C1"));
    expect_identical (mat[1,1], "Bar");
    expect_identical (mat[1,2], "Foo");
    expect_identical (mat[2,1], "Boing");
    expect_identical (mat[2,2], "Bouncy");
    expect_silent (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R1","R2"), c("C2","C1","C1","FOO")));
    expect_identical (dim(mat), as.integer(c(2, 2)));
    expect_identical (rownames(mat), c("R1", "R2"));
    expect_identical (colnames(mat), c("C2", "C1"));
    expect_silent (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R2"), c("C1")));
    expect_identical (dim(mat), as.integer(c(1, 1)));
    expect_identical (rownames(mat), "R2");
    expect_identical (colnames(mat), "C1");
    expect_error (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R2"), c("FOO")));
    expect_error (mat <- tsvGetData (tf$datafile, tf$indexfile, c("FOO"), c("C1")));
    expect_error (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R2,FOO"), c("C1"), findany=FALSE));
    expect_error (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R2"), c("C1,FOO"), findany=FALSE));
};

do_numeric_get_data <- function (tf) {
    expect_silent (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R3","R2"), c("C2","C1","C1","FOO"), 1));
    expect_identical (dim(mat), as.integer(c(2, 2)));
    expect_identical (rownames(mat), c("R3", "R2"));
    expect_identical (colnames(mat), c("C2", "C1"));
    expect_equal (mat[1,1], 3.2);
    expect_equal (mat[1,2], 3.1);
    expect_equal (mat[2,1], 2.2);
    expect_equal (mat[2,2], 2.1);
};

do_integer_get_data <- function (tf) {
    expect_silent (mat <- tsvGetData (tf$datafile, tf$indexfile, c("R3","R2"), c("C2","C1","C1","FOO"), as.integer(1)));
    expect_identical (dim(mat), as.integer(c(2, 2)));
    expect_identical (rownames(mat), c("R3", "R2"));
    expect_identical (colnames(mat), c("C2", "C1"));
    expect_identical (mat[1,1], as.integer(32));
    expect_identical (mat[1,2], as.integer(31));
    expect_identical (mat[2,1], as.integer(22));
    expect_identical (mat[2,2], as.integer(21));
};

do_get_lines <- function (tf) {
    expect_silent (lines <- tsvGetLines (tf$datafile, tf$indexfile, c("R3","R2")));
    expect_identical (length(lines), as.integer(3));
    expect_identical (noeol(lines[1]), "C1\tC2");
    expect_identical (noeol(lines[2]), "R2\tBouncy\tBoing");
    expect_identical (noeol(lines[3]), "R3\tThe\tEnd");
};

test_that("tsvGetData from R text file", {
    tf <- gen_r_text_file();
    do_text_get_data (tf);
    file.remove (tf$datafile, tf$indexfile);
});

test_that("tsvGetData from non-R text file", {
    tf <- gen_std_text_file();
    do_text_get_data (tf);
    file.remove (tf$datafile, tf$indexfile);
});

test_that("tsvGetData from R numeric file", {
    tf <- gen_r_numeric_file();
    do_numeric_get_data (tf);
    file.remove (tf$datafile, tf$indexfile);
});


test_that("tsvGetData from R integer file", {
    tf <- gen_r_integer_file();
    do_integer_get_data (tf);
    file.remove (tf$datafile, tf$indexfile);
});


test_that("tsvGetLines from R text file", {
    tf <- gen_r_text_file();
    do_get_lines (tf);
    file.remove (tf$datafile, tf$indexfile);
});

