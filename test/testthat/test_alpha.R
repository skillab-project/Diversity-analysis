library(testthat)

test_that("Alpha diversity works correctly", {
  source('./alpha_diversity.R')
  
  load('./dataset.Rda')
  res_case <- alpha_diversity(dataset_sorted)
  
  load('./expected_results_alpha.Rda')
  
  expect_equal(res, res_case)
})