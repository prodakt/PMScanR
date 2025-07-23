# tests/testthat/test-freqPie.R

test_that("freqPie calculates data correctly for plotting", {
    # 1. Create a sample input data frame.
    sample_data <- data.frame(Name = c(
        "ZINC_FINGER",
        "EGF_DOMAIN",
        "ZINC_FINGER",
        "ZINC_FINGER",
        "KINASE"
    ))
    
    # 2. Run the function to get the plot object.
    result_plot <- freqPie(sample_data)
    
    # 3. Use `expect_*` functions to check the plot object.
    
    expect_s3_class(result_plot, "ggplot")
    
    plot_data <- result_plot$data
    
    expected_data <- data.frame(
        Name = c("EGF_DOMAIN", "KINASE", "ZINC_FINGER"),
        count = c(1, 1, 3),
        percentage = c(20, 20, 60)
    )
    expected_data$Name_and_procent <- paste0(expected_data$Name,
                                             " (",
                                             round(expected_data$percentage, 1),
                                             "%)")
    expected_data <-
        expected_data[order(expected_data$Name, decreasing = TRUE),]
    rownames(expected_data) <- NULL
    
    # Check if the calculated data frame within the plot is correct
    expect_equal(plot_data$Name, expected_data$Name)
    expect_equal(plot_data$count, expected_data$count)
    expect_equal(plot_data$percentage, expected_data$percentage)
    
})

test_that("freqPie handles an empty data frame", {
    empty_data <- data.frame(Name = character(0))
    
    result_plot <- freqPie(empty_data)
    
    expect_s3_class(result_plot, "ggplot")
    
    # Check that the data inside the plot object is empty
    expect_equal(nrow(result_plot$data), 0)
    
})
