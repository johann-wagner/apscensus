test_that("Dataset has expected dimensions", {
  expect_equal(nrow(aps_employee_census_2024_individual), 140396)
  expect_equal(ncol(aps_employee_census_2024_individual), 214)
})

test_that("Key demographic columns are ordered factors with correct levels", {
  expect_s3_class(aps_employee_census_2024_individual$agency_size, "factor")
  expect_true(is.ordered(aps_employee_census_2024_individual$agency_size))

  expect_s3_class(aps_employee_census_2024_individual$gender, "factor")
  expect_true(is.ordered(aps_employee_census_2024_individual$gender))

  expect_s3_class(aps_employee_census_2024_individual$age_range, "factor")
  expect_true(is.ordered(aps_employee_census_2024_individual$age_range))

  expect_s3_class(aps_employee_census_2024_individual$classification, "factor")
  expect_true(is.ordered(aps_employee_census_2024_individual$classification))
})
