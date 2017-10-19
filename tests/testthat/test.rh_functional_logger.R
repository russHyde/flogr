###############################################################################
# 2017-06-01
# - unit tests for LoggingTuple class methods 'get_dataset' and 'get_logdata'
# - unit tests for instantiation of the LoggingTuple class were added
# - unit tests for flog.filter_df added
#
# 2017-02-27
# - testthat unit tests for the logging-step and logging-workflow classes
#
###############################################################################

context("Unit tests for function_logger.R: classes LoggingStep/LoggingTuple and
functions flog/flog.filter_df")

###############################################################################
# Helpers

DF <- function(
    ...
  ){
  data.frame(..., stringsAsFactors = FALSE)
  }


###############################################################################
# Check that
# - a new LoggingTuple is the correct class
# - a new LoggingTuple has the correct slots present
# - the user has provided all necessary arguments (ie, dataset)

test_that("Instantiation of LoggingTuple class", {
  expect_true(
    is(LoggingTuple(dataset = NA, log.data = list()), "LoggingTuple"),
    info = "A new LoggingTuple is of the correct class"
    )
  expect_true(
    isS4(LoggingTuple(dataset = NA, log.data = list())),
    info = "A new LoggingTuple is an S4 object"
    )
  expect_error(
    object = LoggingTuple(log.data = list(1, 2, 3)),
    info   = "'dataset' must be defined at creation of a LoggingTuple"
    )
  expect_true(
    "dataset" %in% slotNames(LoggingTuple(dataset = NA)),
    info = "A newly made LoggingTuple should have a 'dataset' slot"
    )
  expect_true(
    "log.data" %in% slotNames(LoggingTuple(dataset = NA)),
    info = "A newly made LoggingTuple should have a 'log.data' slot"
    )
  expect_equal(
    object   = LoggingTuple(dataset = 12345)@dataset,
    expected = 12345,
    info     = "dataset should match the input"
    )
  expect_equal(
    object   = LoggingTuple(dataset = 12345)@log.data,
    expected = list(),
    info     = "default log.data should be an empty list()"
    )
  expect_equal(
    object   = LoggingTuple(dataset = 12345,
                            log.data = list("some.log")
                            )@log.data,
    expected = list("some.log"),
    info     = "if passed in, log.data should match the input"
    )
  expect_error(
    object   = LoggingTuple(dataset = 12345,
                            log.data = "NOT A LIST"
                            ),
    info     = "passed in log.data should be a list"
    )
  })

###############################################################################

# Check methods on LoggingTuple
# - Existing methods: get_dataset, get_logdata
test_that("Methods that act on LoggingTuple class", {
  expect_true(
    existsMethod("get_dataset", signature = "LoggingTuple"),
    info = "Method get_dataset should be defined on LoggingTuple"
    )
  expect_equal(
    object   = get_dataset(LoggingTuple(dataset = 123)),
    expected = 123,
    info     = "get_dataset should return the dataset entry"
    )
  expect_equal(
    object   = get_dataset(LoggingTuple(dataset = NULL)),
    expected = NULL,
    info     = "get_dataset on corner cases: NULL dataset"
    )

  expect_true(
    existsMethod("get_logdata", signature = "LoggingTuple"),
    info = "Method get_logdata should be defined on LoggingTuple"
    )
  expect_equal(
    object   = get_logdata(LoggingTuple(dataset = 123, log.data = list("LOG"))),
    expected = list("LOG"),
    info     = "get_logdata should return the logdata entry"
    )
  expect_equal(
    object   = get_logdata(LoggingTuple(dataset = NA)),
    expected = list(),
    info     = "get_logdata with default log.data value should return empty
list"
    )
  })

###############################################################################
# Check that a new LoggingStep is the correct class

test_that("Instantiation of LoggingStep class", {
  expect_true(
    is(LoggingStep(), "LoggingStep"),
    info = "a new LoggingStep is of the correct class"
    )

  expect_true(
    isS4(LoggingStep()),
    info = "a new LoggingStep is an S4 class"
    )
  })

###############################################################################

# Pipelines based on LoggingStep() are ran using the `flog` function

test_that(
  paste("flog is the function that should be used for running pipelines",
        " of 'LoggingStep's"), {
  expect_true(
    is(flog, "function"),
    info = "flog is a function"
    )

  expect_equal(
    names(formals(flog)),
    c(".data", "modifiers", "loggers", "logsteps"),
    info = "Check the arg-names for `flog` match the expected interface"
    )
  })

###############################################################################

# LoggingTuple[A] is effectively list(dataset: A, log.data = list(ANY))

# LoggingStep abstract:
# LoggingStep[A, B]: LoggingTuple[A] => LoggingTuple[B]

# LoggingStep application:
# lstep <- LoggingStep(modifier: A => B, logger: (A, B) => Any)
# .run_step(lstep)(.tuple = LoggingTuple[A])
#   => LoggingTuple[B]
#

# LoggingWorkflow application:
# flog(
#   .data: A | LoggingTuple[A],
#   logsteps: (vector|list)[LoggingStep[A,A'], ..., LoggingStep[A'',B]]
#   )
# flog(
#   .data:     A | LoggingTuple[A],
#   modifiers: (vector|list)[A => A', ..., A'' => B],
#   loggers:   (vector|list)[(A,A') => Any, ..., (A'',B) => Any]
#   )
# flog(
#   .data:     A | LoggingTuple[A],
#   modifiers: (vector|list)[A => A]
#   loggers:   (A, A) => Any
#   )

###############################################################################

# .run_step(LoggingStep(x)) only works on LoggingTuples
# but flog() can take either raw data (A) or a LoggingTuple containing A
# because the raw data will automatically be converted to a LoggingTuple within
# flog

# This, allows you to pipe the .run_step from multiple LoggingSteps together
#  since, you can pipe the output from one .run_step into the .data of another
#

###############################################################################
# TODO: From here all unit tests should be rewritten to use either
# .data = LoggingTuple(raw_data) or .data = raw_data in input to flog

# Simple LoggingStep examples:

test_that("A NULL modifier and a NULL logger", {
  # SETUP:
  lstep <- LoggingStep(
    modifier = function(x) NULL,
    logger   = function(post, pre) NULL
    )

  runner <- .run_step(lstep)

  # TESTS:
  expect_true(
    is(runner, "function"),
    info = ".run_step(LoggingStep(...)) should be a function"
    )

  expect_equal(
    object   = names(formals(runner))[1],
    expected = ".tuple",
    info     = paste("check that .tuple is the first arg to",
                     ".run_step(LoggingStep(...))")
    )

  expect_equal(
    object = runner(.tuple = LoggingTuple(dataset  = 1:10,
                                          log.data = list())),
    expected = LoggingTuple(dataset  = NULL,
                            log.data = list(NULL)),
    info = paste("With a NULL modifier and a NULL logger",
                 "result should be list(NULL, NULL)",
                 sep = ", ")
    )

  expect_equal(
    object = runner(.tuple = LoggingTuple(1)),
    expected = LoggingTuple(dataset = NULL,
                    log.data = list(NULL)),
    info   = paste("Single entry in dataset")
    )

  expect_equal(
    object = runner(.tuple = LoggingTuple(
                                   dataset = 1:10,
                                   log.data = list("not an", "empty", "list"))
                   ),
    expected = LoggingTuple(
                    dataset = NULL,
                    log.data = list("not an", "empty", "list", NULL)),
    info = "Log for the current process should append to the existing log"
    )

  # TODO: add a test for invalid argnames in LoggingTuple instantiation tests

  })

###############################################################################

test_that(
  paste("A modifier that does not modify the input",
        "and a logger that returns NULL"), {

  lstep <- LoggingStep(
    modifier = identity,
    logger   = function(post, pre) NULL
    )

  runner <- .run_step(lstep)

  expect_equal(
    object = runner(.tuple = LoggingTuple(1:10)),
    expected = LoggingTuple(
                    dataset = 1:10,
                    log.data = list(NULL)),
    info = "Non-empty input, modifier has no effect, logger should return NULL"
    )
  })

###############################################################################

test_that(
  paste("A modifier that returns unique values",
        "and a logger that returns the reduction in size"), {

  lstep <- LoggingStep(
    modifier = unique,
    logger   = function(post, pre){
      length(post) - length(pre)
      }
    )

  runner <- .run_step(lstep)

  expect_equal(
    object = runner(.tuple = LoggingTuple(1:10)),
    expected = LoggingTuple(
                    dataset = 1:10,
                    log.data = list(0)),
    info = "Remove dups, log diff in size: Unique set, diff should be 0"
    )
  expect_equal(
    object = runner(.tuple = LoggingTuple(
                                   dataset = c(),
                                   log.data = list(c()))),
    expected = LoggingTuple(
                    dataset = c(),
                    log.data = list(c(), 0)),
    info = "Remove dups, log diff in size: Empty set, diff should be 0"
    )
  })

###############################################################################

test_that(
  paste("The logger function in a LoggingStep should take `post` and",
        "(optionally) `pre` as its first and second arguments"), {

  no_pre_data       <- function(post)               TRUE
  no_post_data      <- function(pre)                TRUE
  wrong_order       <- function(pre, post)          TRUE
  misspelled        <- function(Post, pRe)          TRUE
  pre_should_be_2nd <- function(post, not.pre, pre) TRUE

  expect_error(
    # expect_no_error => see regexp = NA
    object = LoggingStep(
      modifier = identity,
      logger   = no_pre_data
      ),
    regexp = NA,
    info = paste("checks that no error occurs when logger's only argument is",
                 "`post` in the input to LoggingStep::logger")
    )

  expect_error(
    object = LoggingStep(
      modifier = identity,
      logger   = no_post_data
      ),
    info = "`post` is not an argument in the input to LoggingStep::logger"
    )

  expect_error(
    object = LoggingStep(
      modifier = identity,
      logger   = wrong_order
      ),
    info = paste("`post`/`pre` are incorrectly ordered as args to",
                 "LoggingStep::logger")
    )

  expect_error(
    object = LoggingStep(
      modifier = identity,
      logger   = misspelled
      ),
    info = paste("`post`/pre` are incorrectly spelled as args to",
                 "LoggingStep::logger")
    )

  expect_error(
    object = LoggingStep(
      modifier = identity,
      logger   = pre_should_be_2nd
      ),
    info = paste("If `pre` is provided, it should come second in the args",
                 "list to LoggingStep::logger")
    )
  })

###############################################################################

# tests for `flog`
test_that("Example `flog` pipelines: invalid args", {
  # SETUP:
  mod1 <- unique
  log1 <- function(post, pre){
    length(setdiff(pre, post))
    }
  lstep1 <- LoggingStep(mod1, log1)

  # TESTS:
  # in.list or in.data must be provided
  expect_error(
    object = flog(logsteps = lstep1),
    info = "check user provides .data to flog"
    )

  # Either (modifiers & loggers) xor logsteps should be provided
  expect_error(
    object = flog(.data = LoggingTuple(dataset = 1:3, log.data = list())),
    info   = "user provides either (modifiers & loggers) or logsteps"
    )
  expect_error(
    object = flog(
      .data     = LoggingTuple(dataset = 1:3, log.data = list()),
      modifiers = mod1,
      loggers   = log1,
      logsteps  = lstep1
      ),
    info   = paste("check user provides logsteps or (modifiers & loggers),",
                   "but not both")
    )
  # If modifiers & loggers are provided
  # - each should be an iterable containing funtions
  # - their length should be nonzero (includes logger)
  # - their length should be comensurate
  #   - either both lengths are equal
  #   - or at least one of their lengths is 1 [this is checked later]
  expect_error(
    object = flog(.data     = LoggingTuple(1:3, list()),
                  modifiers = c(mod1, mod1)
                  ),
    info = "check that if modifiers are provided, then loggers should also be"
    )
  expect_error(
    object = flog(.data   = LoggingTuple(1:3, list()),
                  loggers = c(log1, log1)
                  ),
    info = "check that if loggers are provided, then modifiers should also be"
    )
  expect_error(
    object = flog(.data     = LoggingTuple(1:3, list()),
                  modifiers = c(mod1, mod1),
                  loggers   = list()
                  ),
    info = "check that if modifiers are provided, then length(loggers)>0"
    )
  expect_error(
    object = flog(.data     = LoggingTuple(1:3, list()),
                  modifiers = list(),
                  loggers   = c(log1, log1)
                  ),
    info = "check that if loggers are provided, then length(modifiers)>0"
    )
  expect_error(
    object = flog(.data     = LoggingTuple(1:3, list()),
                  modifiers = c(mod1, mod1, mod1),
                  loggers   = c(log1, log1)
                  ),
    info = paste("check that if modifiers/loggers are provided,",
                 "both with length>1, then their lengths are equal")
    )

  # If logsteps are provided
  # - it's length shoud be nonzero
  # - it should be an iterable of LoggingSteps
  expect_error(
    object = flog(.data    = LoggingTuple(1:3, list()),
                  logsteps = list()
                  ),
    info   = "check that if logsteps is provided, it has nonzero length"
    )
  expect_error(
    object = flog(
      .data    = LoggingTuple(1:3, list()),
      logsteps = list(lstep1, "not", "all", "LoggingSteps", lstep1)
      ),
    info = "check that if logsteps is provided, it is a list of LoggingSteps"
    )

  # TODO: Errors when providing non-functions as modifier / logger

  })

###############################################################################

test_that("Example `flog` pipelines: using bare functions", {
  # call `flog(.data = LoggingTuple, modifiers = XXX, loggers = YYY)
  # - or `flog(.data = raw.data, modifiers = XXX, loggers = YYY)``
  # - include example using constant logging function with multiple modifiers
  # - and one with constant modifier and multiple loggers
  mod1 <- unique
  log1 <- function(post, pre){
    length(pre) - length(post)
    }
  lstep1 <- LoggingStep(mod1, log1)

  mod2 <- tolower
  log_to_df <- function(post, pre){
    data.frame(before = length(pre), after = length(post))
    }
  lstep2 <- LoggingStep(mod2, log_to_df)

  data.a <- c(letters[1:10], LETTERS[1:10])
  tuple.a <- LoggingTuple(dataset = data.a, log.data = list())

  # as used in the examples
  data.b <- c("I'm", "spaRtacus")

  expect_equal(
    object   = flog(tuple.a, mod1, log1),
    expected = LoggingTuple(dataset = data.a, log.data = list(0)),
    info     = paste(
      "checks if dropping non-unique entries in a non-redundant vector has",
      "no effect; log the (non)changing length"
      )
    )

  expect_equal(
    object = flog(tuple.a, mod2, log_to_df),
    expected = LoggingTuple(
      dataset = rep(letters[1:10], times = 2),
      log.data = list(data.frame(before = 20, after = 20))
      ),
    info = "checks converting a-jA-J to lowercase, and 20 letters remain"
    )

  expect_equal(
    object   = flog(data.a, modifiers = mod1, loggers = log1),
    expected = LoggingTuple(dataset = data.a, log.data = list(0)),
    info     = paste(
      "checks raw-data input to .data instead of as LoggingTuple; single step
pipeline"
      )
    )

  expect_equal(
    object = flog(
      .data     = LoggingTuple(data.a, list("Pre-existing log")),
      modifiers = mod1,
      loggers   = log1
      ),
    expected = LoggingTuple(dataset = data.a,
                            log.data = list("Pre-existing log", 0)),
    info = "check that the flog pipelines append to an existing log"
    )

  # pipeline, length 2
  # - unique then tolower
  expect_equal(
    object = flog(tuple.a, c(mod1, mod2), c(log1, log_to_df)),
    expected = LoggingTuple(
      dataset = rep(letters[1:10], times = 2),
      log.data = list(0, data.frame(before = 20, after = 20))
      ),
    info = paste("flog pipeline of length 2: doesn't affect input,",
                 "2 different modifiers, 2 different loggers")
    )

  # pipeline, length 2
  # - tolower then unique - once in lower case, 10 of the letters are repeated
  # -
  expect_equal(
    object   = flog(tuple.a, c(mod2, mod1), c(log_to_df, log1)),
    expected = LoggingTuple(
      dataset  = letters[1:10],
      log.data = list(data.frame(before = 20, after = 20), 10)
      ),
    info     = paste("flog pipeline of length 2: does affect input,",
                     "2 different modifiers, 2 different loggers")
    )

  # Ensure that if a single logger is provided, for multiple modifiers,
  #   then the results are the same as if multiple copies of the same logger
  #   were provided
  expect_equal(
    object   = flog(tuple.a, c(mod2, mod1), log1),
    expected = flog(tuple.a, c(mod2, mod1), c(log1, log1)),
    info     = paste("flog pipeline of length 2: does affect input,",
                     "2 different modifiers, only 1 logger")
    )

  # Repeat, but with multiple loggers and a single modifier
  expect_equal(
    object   = flog(tuple.a, mod2,          c(log1, log_to_df)),
    expected = flog(tuple.a, c(mod2, mod2), c(log1, log_to_df)),
    info     = "flog pipeline of length 2: does affect input, 1 modifier, 2
loggers"
    )

  # Check Spartacus example - as used in the docs
  # - this failed on initial writing, because I didn't give explicit named
  #     arguments for modifiers and loggers; positionally, tolower/log_to_df
  #     were read in as in.list/modifiers, respectively
  expect_equal(
    object = flog(data.b, modifiers = tolower, loggers = log_to_df),
    expected = LoggingTuple(
      dataset = c("i'm", "spartacus"),
      log.data = list(data.frame(before = 2, after = 2))
      ),
    info = "spaRtacus example as used in the Docs"
    )
  })

###############################################################################

test_that("Example `flog` pipelines: using LoggingStep objects", {
  # call `flog(.data, logsteps = ZZZ)`
  # SETUP:
  mod1 <- unique
  log1 <- function(post, pre){
    length(pre) - length(post)
    }
  lstep1 <- LoggingStep(mod1, log1)

  mod2 <- tolower
  log2 <- function(post, pre){
    data.frame(before = length(pre), after = length(post))
    }
  lstep2 <- LoggingStep(mod2, log2)

  data.a <- c(letters[1:10], LETTERS[1:10])
  tuple.a <- LoggingTuple(dataset = data.a, log.data = list())

  # TESTS:
  expect_equal(
    object   = flog(tuple.a, logsteps = lstep1),
    expected = LoggingTuple(
      dataset  = data.a,
      log.data = list(0)
      ),
    info = "unique as a flog pipeline on non-unique letters: in.list as input"
    )

  expect_equal(
    object   = flog(.data = tolower(data.a), logsteps = lstep1),
    expected = LoggingTuple(
      dataset  = letters[1:10],
      log.data = list(10)
      ),
    info = "unique as a flog pipeline on non-unique letters: in.data as input"
    )

  expect_equal(
    object   = flog(tuple.a, logsteps = list(lstep1)),
    expected = LoggingTuple(
      dataset  = data.a,
      log.data = list(0)
      ),
    info = "unique pipeline: logsteps as a list"
    )

  expect_equal(
    object   = flog(tuple.a, logsteps = list(lstep1, lstep2)),
    expected = LoggingTuple(
      dataset  = rep(letters[1:10], times = 2),
      log.data = list(0, data.frame(before = 20, after = 20))
      ),
    info = "unique pipeline: logsteps as a list"
    )
  })

###############################################################################

test_that("`flog` using a named list of LoggingSteps, modifiers or loggers
  should append the names to the log.data list that is returned", {
  .data <- data.frame(a = 1:3, b = 4:6)
  nrow.logger <- function(
      post,
      pre
    ){
    nrow(post)
    }

  expect_equal(
    object = flog(.data,
                  modifiers = identity,
                  loggers = list(step1 = nrow.logger)),
    expected = LoggingTuple(
      dataset = .data,
      log.data = list(step1 = 3)
      ),
    info = "loggers in a named list: names should be passed to log.data"
    )

  expect_equal(
    object = flog(.data,
                  modifiers = list(step.a = identity),
                  loggers   = nrow.logger),
    expected = LoggingTuple(
      dataset = .data,
      log.data = list(step.a = 3)
      ),
    info = "modifierss in a named list: names should be passed to log.data"
    )
})

###############################################################################

test_that("Example of piping one flog command into another", {
  # call `optional.data.list %>% flog(some, args) %>% flog(some, other, args)`
  })

###############################################################################
test_that("flog.filter_df: invalid inputs", {
  # - Most of these tests are performed by flog() at present, so not entirely
  # useful as unit tests of flog.filter_df
  nrow.logger <- function(
      post,
      pre
    ){
    data.frame(n = nrow(post))
    }

  # Invalid inputs:
  # - Raw data should be a data.frame
  expect_error(
    flog.filter_df("Not a list or data.frame"),
    info = "input to flog.filter_df should be a data.frame or LoggingTuple
containing a data.frame"
    )

  expect_error(
    flog.filter_df(LoggingTuple("Not a data.frame")),
    info = "LoggingTuple input to flog.filter_df should have data.frame as
dataset"
    )

  expect_error(
    flog.filter_df(LoggingTuple(log.data = list(),
                                dataset = "Not a data.frame"),
                   filter.dots = "identity",
                   logger = nrow.logger
                   ),
    info = "LoggingTuple with named args should have data.frame as
'dataset' entry"
    )

  expect_error(
    flog.filter_df(.data = "Not a data.frame",
                   filter.dots = "identity",
                   logger = nrow.logger),
    info = "If raw data is provided instead of LoggingTuple, it should be a
data.frame"
    )

  # - No .data defined
  expect_error(
    flog.filter_df(filter.dots = "identity",
                   logger      = nrow.logger),
    info = "User should provide .data to flog.filter_df"
    )

  # - Both in.list and in.data are defined
  expect_error(
    flog.filter_df(in.list = list(data.frame()),
                   filter.dots = "identity",
                   logger = nrow.logger,
                   in.data = data.frame()),
    info = "User should provide only one of in.list or in.data"
    )
  # - filter.dots is not a vector of strings (OR [to implement] A list/vector
  # or functions)
  expect_error(
    flog.filter_df(in.list = list(data.frame()),
                   filter.dots = 1,
                   logger = nrow.logger),
    info = "filter.dots should be a vector of strings, a vector of functions,
or a list of strings and functions for use in filtering"
    )
  # TODO: - filter.dots is a string but does not refer to a column in the
  # data.frame (difficult to test for complex string-based filters)
  })

###############################################################################
test_that("flog.filter_df", {
  # SETUP:
  nrow.logger <- function(post, pre){
    data.frame(n = nrow(post))
    }

  # Valid inputs:
  # - identity mapping, count the rows
  valid1.df   <- data.frame(a = 1:3, b = letters[1:3])
  valid1.filt <- "identity"
  expected1   <- LoggingTuple(
    dataset  = valid1.df,
    log.data = list(
      data.frame(filter.name = "identity", n = 3L))
    )
  expect_equal(
    object   = flog.filter_df(LoggingTuple(valid1.df),
                              valid1.filt,
                              nrow.logger
                              ),
    expected = expected1,
    info     = "Identity mapping, and count the number of rows"
    )

  # - filter on a single column, count the rows
  # - TODO: ? How should the filtering steps behave for NA values
  valid2.df   <- DF(filter.me = c(TRUE, FALSE, TRUE),
                    a = 1:3,
                    b = letters[1:3]
                    )
  valid2.filt <- "filter.me"
  expected2   <- LoggingTuple(
    dataset  = DF(filter.me = rep(TRUE, 2),
                  a         = c(1, 3),
                  b         = c("a", "c")
                  ),
    log.data = list(
      data.frame(filter.name = "filter.me", n = 2L))
      )
  expect_equal(
    object   = flog.filter_df(LoggingTuple(valid2.df),
                              valid2.filt,
                              nrow.logger),
    expected = expected2,
    info     = "Keep TRUE values for single filter, and count the number of
rows"
    )

  # - Input already has a log.data defined in in.list
  valid3.tuple <- LoggingTuple(
    dataset  = DF(a = 1:3, b = letters[1:3]),
    log.data = list(preceding.step = "Interesting result?")
    )
  expect_equal(
    object   = flog.filter_df(
      .data       = valid3.tuple,
      filter.dots = "identity",
      logger      = nrow.logger
      ),
    expected = LoggingTuple(
      dataset  = get_dataset(valid3.tuple),
      log.data = list(
        preceding.step = "Interesting result?",
        data.frame(filter.name = "identity", n = 3L)
        )
      ),
    info = "Input that contains a pre-existing log.data entry, this should be
appended to"
    )

  # TODO:
  # - Input with a factor in some column - factor levels should be unchanged
  # - String-based complex filter "column1 > 1 & !drop.me & keep.me" ?
  # - Input provided as in.data rather than in.list
  # - filter.dots with a named vector: names should be used instead of inferred
  # from the string-values, when adding filter.name to $log.data data.frame
  })
