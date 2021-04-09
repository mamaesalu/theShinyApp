#context("test missing and duplicates")

test_that("N/A values are identified correctly", {
  df <- data.frame (col1  = c("1", "2", "3", "4"),
                    col2 = c("a", "b", NA , "c")
  )
  expect_equal(getMissing(df, "col1"), df %>% dplyr::filter(is.na(col1)))
  expect_equal(getMissing(df, "col2"), df %>% dplyr::filter(is.na(col2)))
})

test_that("N/A values are identified correctly2", {
  df <- data.frame (col1  = c("1", NA, "3", "4"),
                    col2 = c(NA, "b", NA , "c")
  )
  expect_equal(getMissing(df, "col1"), df %>% dplyr::filter(is.na(col1)))
  expect_equal(getMissing(df, "col2"), df %>% dplyr::filter(is.na(col2)))
})

test_that("N/A values are identified correctly2", {
  df <- data.frame (col1  = c("1", NA, "3", "4"),
                    col2 = c(NA, NA, NA , NA)
  )
  expect_equal(getMissing(df, "col1"), df %>% dplyr::filter(is.na(col1)))
  expect_equal(getMissing(df, "col2"), df %>% dplyr::filter(is.na(col2)))
})