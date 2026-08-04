library(testthat)

test_that("ISA works correctly", {
  source('./isa_diversity.R')
  
  load('./dataset.Rda')
  res_case <- isa_function(dataset_sorted)
  
  load('./expected_results_ISA.Rda')
  
  expect_equal(res[1:10, ], res_case[1:10, ])
})