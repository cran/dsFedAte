library(testthat)

options(
  datashield.privacyControlLevel = "banana",
  nfilter.tab                    = 3,
  nfilter.subset                 = 3,
  nfilter.glm                    = 0.33,
  nfilter.string                 = 80,
  nfilter.stringShort            = 20,
  nfilter.kNN                    = 3,
  nfilter.levels.density         = 0.33,
  nfilter.levels.max             = 40,
  nfilter.noise                  = 0.25,
  nfilter.privacy.old            = 5
)

make_test_data <- function(n = 30) {
  set.seed(42)
  data.frame(X1 = rnorm(n), X2 = rnorm(n),
             W  = rbinom(n, 1, 0.5), Y = rnorm(n))
}

# addStudyIdDS uses eval(parse(), envir = parent.frame()) — the test data must
# be visible in the calling frame.
assign("test_df", make_test_data(), envir = .GlobalEnv)

# Happy path: column presence --------------------------------------------------

test_that("addStudyIdDS creates K-1 integer dummy columns", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "2", "3")
  expect_true("study_id2" %in% names(result))
  expect_true("study_id3" %in% names(result))
  expect_false("study_id1" %in% names(result))   # no column for the reference level
})

test_that("addStudyIdDS does not create a study_id factor column", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "2", "3")
  expect_false("study_id" %in% names(result))
})

test_that("addStudyIdDS dummy columns are integer class", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "2", "3")
  expect_type(result$study_id2, "integer")
  expect_type(result$study_id3, "integer")
})

# Happy path: dummy values -----------------------------------------------------

test_that("addStudyIdDS reference study has all-zero dummy columns", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "1", "3")
  expect_true(all(result$study_id2 == 0L))
  expect_true(all(result$study_id3 == 0L))
})

test_that("addStudyIdDS non-reference study has correct 0/1 pattern", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "2", "3")
  expect_true(all(result$study_id2 == 1L))
  expect_true(all(result$study_id3 == 0L))
})

test_that("addStudyIdDS last study has correct 0/1 pattern", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "3", "3")
  expect_true(all(result$study_id2 == 0L))
  expect_true(all(result$study_id3 == 1L))
})

test_that("addStudyIdDS dummy columns match model.matrix() treatment contrasts", {
  # Verify the explicit integers are numerically identical to the columns
  # model.matrix() would produce from an equivalent factor.
  test_df  <- make_test_data()
  result   <- addStudyIdDS("test_df", "2", "3")
  f        <- factor(rep(2L, nrow(result)), levels = 1:3)
  mm       <- model.matrix(~ f)[, -1L, drop = FALSE]
  expect_equal(result$study_id2, as.integer(mm[, "f2"]))
  expect_equal(result$study_id3, as.integer(mm[, "f3"]))
})

# Happy path: dimensions -------------------------------------------------------

test_that("addStudyIdDS nrow is unchanged, ncol increases by K-1", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "1", "3")
  expect_equal(nrow(result), nrow(test_df))
  # K=3: adds study_id2 and study_id3 = 2 new columns
  expect_equal(ncol(result), ncol(test_df) + 2L)
})

test_that("addStudyIdDS preserves all original columns and values", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "1", "2")
  expect_true(all(c("X1", "X2", "W", "Y") %in% names(result)))
  expect_equal(result$X1, test_df$X1)
  expect_equal(result$Y,  test_df$Y)
})

test_that("addStudyIdDS K=2 produces exactly one dummy column", {
  test_df <- make_test_data()
  result  <- addStudyIdDS("test_df", "2", "2")
  expect_true("study_id2" %in% names(result))
  expect_false("study_id3" %in% names(result))
  expect_true(all(result$study_id2 == 1L))
})

# Sad path: privacy control level ---------------------------------------------

test_that("addStudyIdDS is blocked under non-permissive privacy level", {
  options(datashield.privacyControlLevel = "non-permissive")
  assign("test_df", make_test_data(), envir = .GlobalEnv)
  expect_error(addStudyIdDS("test_df", "1", "2"), "BLOCKED")
  options(datashield.privacyControlLevel = "banana")
})

# Sad path: disclosure check --------------------------------------------------

test_that("addStudyIdDS errors when dataset is below nfilter.subset", {
  options(nfilter.subset = 10)
  assign("test_df", make_test_data(n = 5), envir = .GlobalEnv)
  expect_error(addStudyIdDS("test_df", "1", "2"), "too few rows")
  options(nfilter.subset = 3)
})

# Sad path: invalid study_index -----------------------------------------------

test_that("addStudyIdDS errors when study_index exceeds K_studies", {
  assign("test_df", make_test_data(), envir = .GlobalEnv)
  expect_error(addStudyIdDS("test_df", "5", "3"), "study_index")
})

test_that("addStudyIdDS errors when study_index is zero", {
  assign("test_df", make_test_data(), envir = .GlobalEnv)
  expect_error(addStudyIdDS("test_df", "0", "3"), "study_index")
})
