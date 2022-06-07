test_that("Column presence check returns null if all there", {
  df <- data.frame(
    longitude=c(1,2,3,4),
    latitude=c(1,2,3,4),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  required <- c("longitude", "latitude")
  
  expect_null(check_fields_(df, required))
})


test_that("Column presence check returns message with missing columns", {
  df <- data.frame(
    longitude=c(1,2,3,4),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  required <- c("longitude", "latitude")
  
  expected_pattern <- ".+ longitude and latitude.+ latitude"
  
  expect_true(grepl(expected_pattern, check_fields_(df, required)))
})


test_that("Column type check returns null if all okay", {
  df <- data.frame(
    longitude=c(1,2,3,4),
    latitude=c(1,2,3,4),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  numeric_fields <- c("longitude", "latitude")
  
  expect_null(check_numeric_(df, numeric_fields))
})


test_that("Column type check returns message with bad columns", {
  df <- data.frame(
    longitude=c(1,2,3,4),
    latitude=as.character(c(1,2,3,4)),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  numeric_fields <- c("longitude", "latitude")
  
  expected_pattern <- ".+ latitude .+"
  
  expect_true(grepl(expected_pattern, check_numeric_(df, numeric_fields)))
})

test_that("Column completeness check returns null if all okay", {
  df <- data.frame(
    longitude=c(1,2,3,4),
    latitude=c(1,2,3,4),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  complete_fields <- c("longitude", "latitude")
  
  expect_null(check_complete_(df, complete_fields))
})

test_that("Column completeness check returns message with incomplete columns", {
  df <- data.frame(
    longitude=c(1,2,3,4),
    latitude=c(1,2,3,NA_real_),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  complete_fields <- c("longitude", "latitude")
  
  expected_pattern <- ".+ latitude .+"
  
  expect_true(grepl(expected_pattern, check_complete_(df, complete_fields)))
})

test_that("Column range check returns null if all okay", {
  df <- data.frame(
    longitude=c(99,0,10,-179),
    latitude=c(-89,0,60,89),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  expect_null(check_range_(df, "latitude", min=-90, max=90))
})

test_that("Column range check returns message with values out of bounds columns", {
  df <- data.frame(
    longitude=c(9999,0,10,-181),
    latitude=c(-89,0,60,89),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  expected_pattern <- ".+ longitude .+"
  
  expect_true(grepl(expected_pattern, check_range_(df, "longitude", min=-180, max=180)))
})

test_that("Column rounding check returns null if all okay", {
  df <- data.frame(
    longitude=c(99.333333,0.43549584,10.202999,-179.0001),
    latitude=c(-89.30982383,0.01111,60.11111,89.010101),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  expect_null(check_rounded_(df, "latitude", digits=0, threshold=0.1))
})

test_that("Column rounding check returns message if rounded values above threshold", {
  df <- data.frame(
    longitude=c(9999,0,10,-181),
    latitude=c(-89,0,60,89),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  expected_pattern <- ".*100 %.*latitude.*"
  
  expect_true(grepl(expected_pattern, check_rounded_(df, "latitude", digits=0, threshold=0.1)))
})

test_that("Column zero count check returns null if all okay", {
  df <- data.frame(
    longitude=c(99.333333,0.43549584,10.202999,-179.0001),
    latitude=c(-89.30982383,0.01111,60.11111,89.010101),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  expect_null(check_zeros_(df, "latitude"))
})

test_that("Column zero count check returns message if zeros present", {
  df <- data.frame(
    longitude=c(9999,0,10,-181),
    latitude=c(-89,0,60,89),
    name=c("burt", "ernie", "elmo", "oscar")
  )
  
  expected_pattern <- ".*25 %.*latitude.*"
  
  expect_true(grepl(expected_pattern, check_zeros_(df, "latitude")))
})

test_that("CSV validation returns single message for missing fields", {
  csv <- data.frame(
    longitude=c(9999,0,10,-181, NA_real_),
    name=c("burt", "ernie", "elmo", "oscar", "big bird")
  )
  
  expected_pattern <- ".+ longitude and latitude.+ latitude"
  
  expect_true(grepl(expected_pattern, validate_csv(csv)))
})

test_that("CSV validation returns single message for non-numeric fields", {
  csv <- data.frame(
    longitude=c(9999,0,10,-181, NA_real_),
    latitude=c("burt", "ernie", "elmo", "oscar", "big bird")
  )
  
  expected_pattern <- ".+ latitude .+"
  
  expect_true(grepl(expected_pattern, validate_csv(csv)))
})

test_that("CSV validation returns a message for every warning check", {
  csv <- data.frame(
    longitude=c(9999,0,10,-181, NA_real_),
    latitude=c(0.111, 90.000, 10, -96,0),
    names=c("burt", "ernie", "elmo", "oscar", "big bird")
  )
  
  messages <- validate_csv(csv)
  
  expect_length(messages, 7)
})
