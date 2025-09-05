# tests/testthat/test-gff2matrix.R

test_that("gff2matrix correctly converts a data frame to a binary matrix", {
    # 1. Create a sample input data frame.
    sample_gff <- data.frame(
        seqnames = c("SeqA", "SeqA", "SeqB"),
        type = c("MOTIF", "MOTIF", "MOTIF"),
        start = c(10, 50, 25),
        end = c(20, 60, 35),
        Name = c("ZINC_FINGER", "EGF_DOMAIN", "ZINC_FINGER")
    )
    
    # 2. Run the function we are testing.
    result_matrix <- gff2matrix(sample_gff)
    
    # 3. Define the correct expected output.
    expected_matrix <- matrix(
        c(1, 0, 1, 0, 1, 0),
        nrow = 3,
        ncol = 2,
        dimnames = list(
            c("MOTIF:10-20", "MOTIF:25-35", "MOTIF:50-60"),
            c("SeqA", "SeqB")
        )
    )
    
    expected_matrix <-
        expected_matrix[order(rownames(expected_matrix)),]
    
    # 4. Use `expect_*` functions to check if the result is correct.
    expect_true(is.matrix(result_matrix))
    expect_equal(dim(result_matrix), dim(expected_matrix))
    expect_equal(result_matrix, expected_matrix)
    
})

test_that("gff2matrix handles empty input gracefully", {
    empty_gff <- data.frame(
        seqnames = character(0),
        type = character(0),
        start = integer(0),
        end = integer(0),
        Name = character(0)
    )
    
    result_matrix <- gff2matrix(empty_gff)
    
    expect_true(is.matrix(result_matrix))
    expect_equal(nrow(result_matrix), 0)
    expect_equal(ncol(result_matrix), 0)
})
