test_that("matching registry codes are identified correctly", {
  df <- data.frame (regcode  = c("1", "2", "3", "4"),
                    col2 = c("a", "b", "x" , "c")
  )
  
  df2 <- data.frame (ariregistri_kood  = c("1", "2", "3", "4"),
                     nimi = c("a", "b", "x" , "c")
  )
  
  df3 <- regcodesmatch(df, df2, "regcode")
  
  expect_equal(nrow(df3), 4)
  expect_equal(df3$result[[1]], TRUE)
  expect_equal(df3$result[[2]], TRUE)
  expect_equal(df3$result[[3]], TRUE)
  expect_equal(df3$result[[4]], TRUE)
})

test_that("non-matching registry codes are identified correctly", {
  df <- data.frame (regcode  = c("1", "2", "3", "4"),
                    col2 = c("a", "b", "x" , "c")
  )
  
  df2 <- data.frame (ariregistri_kood  = c("5", "6", "7", "8", "9", "10"),
                     nimi = c("a", "b", "x" , "c", "d", "e")
  )
  
  df3 <- regcodesmatch(df, df2, "regcode")
  
  expect_equal(nrow(df3), 4)
  expect_equal(df3$result[[1]], FALSE)
  expect_equal(df3$result[[2]], FALSE)
  expect_equal(df3$result[[3]], FALSE)
  expect_equal(df3$result[[4]], FALSE)
})

test_that("matching and non-matching registry codes are identified correctly", {
  df <- data.frame (regcode  = c("1", "2", "3", "4", "5"),
                    col2 = c("a", "b", "x" , "c", "d")
  )
  
  df2 <- data.frame (ariregistri_kood  = c("5", "1", "7", "2", "9", "10"),
                     nimi = c("a", "b", "x" , "c", "d", "e")
  )
  
  df3 <- regcodesmatch(df, df2, "regcode")
  
  expect_equal(nrow(df3), 5)
  expect_equal(df3$result[[1]], TRUE)
  expect_equal(df3$result[[2]], TRUE)
  expect_equal(df3$result[[3]], FALSE)
  expect_equal(df3$result[[4]], FALSE)
  expect_equal(df3$result[[5]], TRUE)
})

test_that("N/As are omitted in identifying matching/non-matching regcodes", {
  df <- data.frame (regcode  = c("1", NA, "3", "4", "5"),
                    col2 = c("a", "b", "x" , "c", "d")
  )
  
  df2 <- data.frame (ariregistri_kood  = c("5", "1", "7", "4", "9", "10"),
                     nimi = c("a", "b", "x" , "c", "d", "e")
  )
  
  df3 <- regcodesmatch(df, df2, "regcode")
  
  expect_equal(nrow(df3), 4)
  expect_equal(df3$result[[1]], TRUE)
  expect_equal(df3$result[[2]], FALSE)
  expect_equal(df3$result[[3]], TRUE)
  expect_equal(df3$result[[4]], TRUE)
})

test_that("matching/non-matching names are identified correctly for matching regcodes", {
  df <- data.frame (regcode  = c("1", "2", "3", "4", "5"),
                    col2 = c("a", "b", "x" , "c", "d")
  )
  
  df2 <- data.frame (ariregistri_kood  = c("5", "1", "7", "4", "9", "10"),
                     nimi = c("d", "b", "x" , "c", "d", "e")
  )
  
  df3 <- namesmismatchtotal(df, df2, "regcode", "col2")
  
  #FALSE if name matches (there is no mis-match)
  expect_equal(nrow(df3), 3)
  expect_equal(df3$result[[1]], TRUE)
  expect_equal(df3$result[[2]], FALSE)
  expect_equal(df3$result[[3]], FALSE)
  
})

test_that("matching/non-matching names are identified correctly for regcodes (with NAs)", {
  df <- data.frame (regcode  = c("1", NA, "3", "4", "5"),
                    col2 = c("a", "b", "x" , "c", "d")
  )
  
  df2 <- data.frame (ariregistri_kood  = c("5", "1", "7", "4", "9", "10"),
                     nimi = c("a", "b", "x" , "c", "d", "e")
  )
  
  df3 <- namesmismatchtotal(df, df2, "regcode", "col2")
  
  #FALSE if name matches (there is no mis-match)
  expect_equal(nrow(df3), 3)
  expect_equal(df3$result[[1]], TRUE)
  expect_equal(df3$result[[2]], FALSE)
  expect_equal(df3$result[[3]], TRUE)
})
