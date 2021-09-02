test_that("trivial", {

  expect_true(TRUE)


}
)


h <- hartland


test_that("single year works", {

  h_94 <- simulate_isd_ts(h, censusyears = 1994, isd_seed = 1997)

})

test_that("isds reproduce", {

h_isd <- simulate_isd_ts(h, isd_seed = 2021)

  # If isd_seed is held the same, you should always get the same ISD
  for(i in 1:10) {

    h_isd2 <- simulate_isd_ts(h, isd_seed = 2021)

    expect_true(dplyr::all_equal(h_isd$isd, h_isd2$isd))

  }

  # If the isd_seed changes, you should get differences.
  for(i in 1:10) {

    h_isd2 <- simulate_isd_ts(h, isd_seed = 2021 + i)

    expect_false(all(h_isd$isd$mass == h_isd2$isd$mass))
    expect_true(all(h_isd$isd$id == h_isd2$isd$id))
    expect_true(all(h_isd$isd$year == h_isd2$isd$year))

  }

})
