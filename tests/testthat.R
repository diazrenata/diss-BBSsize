library(testthat)
library(dissBBSsize)
test_dir("testthat", reporter = c("check", "progress"))

test_check("dissBBSsize")
