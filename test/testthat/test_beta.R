library(testthat)

test_that("Beta diversity works correctly", {
  source('./beta_diversity.R')
  
  load('./dataset.Rda')
  res_case <- beta_diversity(dataset_sorted)
  res_case[,3:4] <- round(res_case[,3:4], 2)
  
  load('./expected_results_beta.Rda')
  res[,3:4] <- round(res[,3:4], 2)
  
  expect_equal(res, res_case)
})