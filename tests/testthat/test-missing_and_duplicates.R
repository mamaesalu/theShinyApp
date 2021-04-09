test_that("N/A values are identified correctly - no NAs", {
  df <- data.frame (col1  = c("1", "2", "3", "4"),
                    col2 = c("a", "b", "x" , "c")
  )
  expect_equal(getMissing(df, "col1"), df %>% dplyr::filter(is.na(col1)))
  expect_equal(getMissing(df, "col2"), df %>% dplyr::filter(is.na(col2)))
})

test_that("N/A values are identified correctly - some NAs", {
  df <- data.frame (col1  = c("1", NA, "3", "4"),
                    col2 = c(NA, "b", NA , "c")
                    )
  
  expect_equal(getMissing(df, "col1"), df %>% dplyr::filter(is.na(col1)))
  expect_equal(getMissing(df, "col2"), df %>% dplyr::filter(is.na(col2)))
})

test_that("N/A values are identified correctly - all NAs", {
  df <- data.frame (col1  = c("1", NA, "3", "4"),
                    col2 = c(NA, NA, NA , NA)
  )
  expect_equal(getMissing(df, "col1"), df %>% dplyr::filter(is.na(col1)))
  expect_equal(getMissing(df, "col2"), df %>% dplyr::filter(is.na(col2)))
})

test_that("Duplicates are identified correctly - some NAs", {
  df <- data.frame (col1 = c("1", NA, "1", "1"),
                    col2 = c("a", "a", NA , NA)
  )
  df2 <- data.frame (col1  = c("1", NA),
                    col2 = c("a", "a")
  )
  
  df3 <- data.frame (col1  = c("1", "1", "1"),
                     col2 = c("a", NA, NA)
  )
  
  expect_equal(getDuplicates(df, "col1"), df3)
  expect_equal(getDuplicates(df, "col2"), df2)
})