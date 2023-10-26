# test/test_main.R  
  
test_that("add_two_numbers adds two numbers correctly", {  
  expect_equal(add_two_numbers(1, 2), 3)  
  expect_equal(add_two_numbers(5, -3), 2)  
})