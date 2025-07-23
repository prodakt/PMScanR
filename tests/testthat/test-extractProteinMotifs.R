# tests/testthat/test-extractProteinMotifs.R

test_that("extractProteinMotifs correctly parses motifs from a PSA file", {
    # 1. Create a temporary PSA file with known content for our test.
    psa_content <- c(
        ">sp|P01942|HBA_ANTTR Hemoglobin subunit alpha - PS00223 Globin family signature.",
        "VLSPADKTNVKAAWGKVGAHAGEYGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHG",
        ">sp|P02025|HBA_MESAU Hemoglobin subunit alpha - PS00223 Globin family signature.",
        "MVLSAADKGNVKAAWGKVGGHAAEYGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHG",
        ">sp|P02025|HBA_MESAU Hemoglobin subunit alpha - PS00822 Hemoglobin alpha-chain specific region signature.",
        "DDMPNALSALSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPAEFTPAVHASLDKFLASV"
    )
    
    # Create a temporary file to write this content to
    temp_psa_file <- tempfile(fileext = ".txt")
    writeLines(psa_content, temp_psa_file)
    
    # 2. Run the function we are testing.
    result_list <- extractProteinMotifs(temp_psa_file)
    
    # 3. Define what the correct output should be.
    expected_list <- list(
        PS00223 = c(
            "VLSPADKTNVKAAWGKVGAHAGEYGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHG",
            "MVLSAADKGNVKAAWGKVGGHAAEYGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHG"
        ),
        PS00822 = c(
            "DDMPNALSALSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPAEFTPAVHASLDKFLASV"
        )
    )
    
    # 4. Use `expect_*` functions to check the result.
    
    # Check that the output is a list
    expect_true(is.list(result_list))
    
    # Check that the names of the list elements are correct
    expect_equal(sort(names(result_list)), sort(names(expected_list)))
    
    # Check that the content of the list is identical to our expectation
    expect_equal(result_list, expected_list)
    
    # Clean up the temporary file
    unlink(temp_psa_file)
})

test_that("extractProteinMotifs handles files with no motifs", {
    # Create a file with no valid motif headers
    no_motif_content <- c(">Some other header line",
                          "ACGTACGTACGT",
                          "# A comment line")
    
    temp_file <- tempfile(fileext = ".txt")
    writeLines(no_motif_content, temp_file)
    
    result_list <- extractProteinMotifs(temp_file)
    
    # Expect an empty list
    expect_true(is.list(result_list))
    expect_equal(length(result_list), 0)
    
    unlink(temp_file)
})
