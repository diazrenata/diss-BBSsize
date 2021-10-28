library(testthat)
library(BBSsize)
test_dir("testthat", reporter = c("check", "progress"))

test_check("BBSsize")
