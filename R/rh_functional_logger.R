###############################################################################

# rh_functional_logger.R: version 1.0.0

###############################################################################
# 2017-06-01
# - Rewrite of functional_logger.R from my project `drug_markers`
# - Functions flog() and flog.filter_df() originally admitted data input from
# both in.list = list(dataset = data.set, log.data = list(...)) and from
# in.data = data.set. This wasn't very clean, and required i) that the log.data
# list was checked for presence, validity etc and ii) that dataset/log.data
# names were checked in both flog() and flog.filter_df(). It also may lead to
# some ambiguity when the intended in.list `dataset` is a list.
# - To relieve these issues, we introduce a lightweight class `LoggingTuple`
# that holds the `dataset` and `log.data` values that are passed through a
# flog-based pipeline. This class is unlikely to be used as the `dataset`
# passed into a pipeline, so should lead to less ambiguity, and we can add a
# quick data-to-LoggingTuple function call at the start of both flog() and
# flog.filter_df() (and future functions along this line).
#

###############################################################################
# 2017-02-27
# - Definition of a simple way to log the steps in an R workflow
# - Things that I'd like to log:
#   - Differences in size between an initial dataframe and a filtered dataframe
#   eg, where multiple filtering steps are applied to a dataframe
#   - Plots and other summary data of intermediate processing steps that should
#   not be printed out to the console or to files in a pure-functional
#   processing workflow
#
###############################################################################

# RH-style for names:
#
# ThisIsAClass
# this_is_a_function
# - no dots in function names
# - unless function is of a particular subtype of function in which
#     case the function type comes first: eg, filter.some_specific_name,
#     builder.some_class_of_function, plot.some_kind_of_plot)
# this.is.a.variable
# Non-exported functions prefixed with '.'
#
# Use 2-spaces, no-tabs

# TODO: change formatting
# Variables and column names should not have '.'-separated names
# Nor should function types have '.'-separated prefixes

###############################################################################

#' @name         flog-NULL-function
#' @title        flog-NULL-function
#' @description  Unnecessary description for NULL function
#'
#' @importFrom   methods       callNextMethod
#' @importFrom   methods       is
#' @importFrom   methods       new
#' @importFrom   methods       setValidity
#' @importFrom   methods       setClass
#' @importFrom   methods       setGeneric
#' @importFrom   methods       setMethod
#' @importFrom   methods       setReplaceMethod
NULL

###############################################################################

# Typical loggers:


#' null_logger: Logs no information about the changes induced by a modifier in
#'   a LoggingStep.
#'
#' @param        post          The dataset that exists after a modifier has
#'   been applied to some input dataset in a LoggingStep.
#'
#' @param        pre           A dataset before a modifier has been applied to
#'   it in a LoggingStep.
#'
#' @export
#'
null_logger <- function(post, pre){
  NULL
  }

#' nrow_logger: Logs the change in the number of rows in a dataset after
#'   applying the modifier in a LoggingStep.
#'
#' @inheritParams   null_logger
#'
#' @export
#'
nrow_logger <- function(post, pre){
  data.frame(
    dataset = c(pre, post),
    n.row   = c(nrow(pre), nrow(post))
    )
  }

###############################################################################

#' Constructor for LoggingTuple Class.
#'
#' Lightweight datastructure that is passed between different LoggingStep
#' functions. This holds the `current` dataset and a log of the various steps
#' that resulted in the generation of the current dataset.
#'
#' @param        dataset       May be any datatype, but must be defined.
#' @param        log.data      A list. If missing, this is set to the empty
#'   list.
#'
#' @name         LoggingTuple-class
#' @rdname       LoggingTuple-class
#'
#' @export       LoggingTuple
#'
LoggingTuple <- methods::setClass(
  "LoggingTuple",
  slots = list(
    dataset  = "ANY",
    log.data = "list"
    )
  )

#' Initializer for LoggingTuple
#'
#' @name         LoggingTuple
#' @rdname       LoggingTuple-class
#'

methods::setMethod(
  "initialize",
  signature = "LoggingTuple",
  definition = function(
      .Object,
      dataset,
      log.data = list()
    ){
    .Object          <- methods::callNextMethod()
    .Object@dataset  <- dataset
    .Object@log.data <- log.data
    .Object
    }
  )

# TODO: add roxygen2 docs for get_dataset / get_logdata
# getters for LoggingTuple class.
get_dataset <- function(object) NULL
get_logdata <- function(object) NULL

#' Get dataset entry using S4 method
#'
#' @docType      methods
#' @name         get_dataset
#' @rdname       get_dataset-methods
#' @export
methods::setGeneric("get_dataset",
  function(object){
    standardGeneric("get_dataset")
    }
  )

#' Get logdata entry using S4 method
#'
#' @docType      methods
#' @name         get_logdata
#' @rdname       get_logdata-methods
#' @export
methods::setGeneric("get_logdata", function(object){
  standardGeneric("get_logdata")
  })

#' @name         get_dataset
#' @rdname       get_dataset-methods
#' @aliases      get_dataset,LoggingTuple-method
#'
methods::setMethod(
  f          = "get_dataset",
  signature  = "LoggingTuple",
  definition = function(object){
    object@dataset
    }
  )

#' @name         get_logdata
#' @rdname       get_logdata-methods
#' @aliases      get_logdata,LoggingTuple-method
#'
methods::setMethod(
  f          = "get_logdata",
  signature  = "LoggingTuple",
  definition = function(object){
    object@log.data
    }
  )

###############################################################################

#' Constructor for LoggingStep Class.
#'
#' Encapsulates a single data-manipulation function (`modifier`) and a single
#'   logging function (`logger`) to compare the input/output of `modifier`.
#' The user should not use the (non-exported) `run_step` method of LoggingStep
#'   directly. They should always use `flog(in.list = some.list,
#'   logsteps = LS)` or `flog(in.data = some.input, logsteps = LS)`.
#'
#' @param        modifier      A function that modifies a given dataset. This
#'   must be able
#'   to take the dataset as it's first parameter and have no other missing-but
#'   necessary parameters
#' @param        logger        A function that can compare the outpur from
#'   `modifier`
#'   against the input to that function and return a (preferably small)
#'   summary of the difference between output and input. It has obligatory
#'   first parameter-name `post` (holds the dataset after the corresponding
#'   `modifier` has ran) and may have an optional second parameter called `pre`
#'   (which holds the dataset that was passed into the current `modifier`)
#'
#' @name         LoggingStep-class
#' @rdname       LoggingStep-class
#'
#' @importFrom   methods       setClass
#'
#' @exportClass  LoggingStep
#'
LoggingStep <- methods::setClass(
  "LoggingStep",
  slots = list(
    modifier = "function",
    logger   = "function"
    )
  )

#' Initialize method for LoggingStep
#'
#' @name         LoggingStep
#' @rdname       LoggingStep-class
#'

methods::setMethod(
  "initialize",
  signature = "LoggingStep",
  definition = function(
      .Object,
      modifier = function(in.data) NULL,
      logger   = function(post, pre) NULL
    ){
    .Object          <- methods::callNextMethod()
    .Object@modifier <- modifier
    .Object@logger   <- logger
    .Object
  })

# Set validity for the LoggingStep class
#
methods::setValidity(
  "LoggingStep",
  function(object){
    # It is not possible to constrain the functions that may be used as the
    #   `modifier` in LoggingStep, since these functions may act on any given
    #   dataset.
    #
    # However, we constrain the names of the arguments to `logger`:
    # A valid `logger` function must have first argument named 'post'
    #   and (optionally) a second argument named 'pre'; that is, if the
    #   function has more than one argument, the second should be called
    #   'pre'
    f_names <- names(formals(object@logger))
    correct_first_arg  <- length(f_names) >= 1 &&
                          f_names[1] == "post"

    correct_second_arg <- length(f_names) >= 2 &&
                          f_names[2] == "pre"

    correct_args <- correct_first_arg &&
                    (length(f_names) == 1 || correct_second_arg)

    correct_args
  })

###############################################################################

#' .run_step : returns a modifying/logging function-pair from a LoggingStep;
#'
#' Non-exported function: User must use flog() to apply a LoggingStep.
#'
#' Given an input dataset (as part of a list(dataset, log.data)), the functions
#'   returned by .run_step will apply a `modifier` function to  that dataset and
#'   append a log of the changes to log.data
#'
#' @param        object        A LoggingStep object, or a subclass thereof.
#'
.run_step <- function(object, ...){
  NULL
  }

#
methods::setGeneric(".run_step")

#' .run_step(LoggingStep): Returns a modifying/logging function-pair
#'
#' @param        object        A LoggingStep object, or a subclass thereof.
#' @name         .run_step
#'
#' @importFrom   magrittr      %>%
#'
methods::setMethod(
  ".run_step",
  methods::signature(object = "LoggingStep"),
  definition = function(
      object,
      step.name = NULL
    ){
    # TODO: Validity test on step.name

    function(.tuple){
      # Input to the function built by .run_step should always be a
      # LoggingTuple and therefore have a defined @dataset and @log.data entry,
      # which can be accessed using get_dataset and get_logdata
      stopifnot(methods::is(.tuple, "LoggingTuple"))

      # Apply the modifier function to the dataset, compute any logging
      # information based on the results of applying the modifier, and then
      # return the new dataset along with any logging data (add the latter to
      # the existing logging data if any was present)
      modifier    <- object@modifier
      logger      <- object@logger
      new_dataset <- modifier(get_dataset(.tuple))
      log_entry   <- logger(new_dataset, get_dataset(.tuple))
      log_entry_list <- list(log_entry) %>% setNames(step.name)

      LoggingTuple(
        dataset = new_dataset,
        log.data = append(
          get_logdata(.tuple),
          log_entry_list
          )
        )
      }
    }
  )

###############################################################################

# TODO: update all documentation / examples
# - use .data instead of in.list or in.data
# - refer to LoggingTuple rather than list(dataset, log.data)

# TODO: add vectorised logsteps to examples section, ie,
# -   piped.data %>% flog(
#       logsteps = c(lstep1, lstep2)
#       )
# TODO: Ensure names are passed from logsteps to log.data

#' flog: functional logging for R pipelines
#'
#' Runs a consecutive pipeline of functions (modifiers) on an input dataset
#'   and after each modifier is ran, computes some logging information related
#'   to having ran that step. The logging information is passed through the
#'   pipeline in a side-effect-free manner. The user must specify a list of
#'   modifier functions and a list of logging functions (either by setting both
#'   modifiers and loggers or by setting logsteps, which uses the LoggingStep
#'   class).  The user either provides lists of modifier and logger functions,
#'   or provides a list of LoggingStep objects that encapsulates these
#'   functions. Returns a LoggingTuple (you can extract the dataset or logging
#'   data using get_dataset or get_logdata, respectively). Separate flog()
#'   functions can be piped together, by separate calls to flog() since the
#'   output format is the same as the input format (since LoggingTuple is a
#'   valid input); see the examples - that's where the examples are.
#'
#' @param        .data         A LoggingTuple or a raw dataset. If the
#'   input is a raw dataset, this will be converted to a LoggingTuple prior to
#'   being passed into the pipeline of functions. flog() applies each of the
#'   modifier functions, sequentially, to the `dataset` entry of .data
#'   and the result of this pipeline is returned in the `dataset` entry of
#'   LoggingTuple output by `flog()`. After each modifier has been applied,
#'   the corresponding logger function is used to generate a summary of the
#'   differences to the dataset that were induced by the modifier function.
#'   The logging results are returned as the list `log.data` that is present
#'   in the returned LoggingTuple.
#'
#' @param        modifiers     A list of functions that sequentially
#'   modify the input dataset. If a single function is provided it need not be
#'   in a list and will be repeated to match the length of `loggers`. If
#'   provided, the user must also define the list `loggers` and must not
#'   provide a `logsteps` argument.
#'
#' @param        loggers       A list of functions that compare output
#'   to input at each stage of the dataset %>% modifier1 %>% ... %>% modifierN
#'   pipeline. If a single function is provided it need not be in a list and
#'   will be repeated to match the length of `modifiers` (ie, if 6 modifiers
#'   are provided and only one logger, the logger will be used after each of
#'   the modifiers has ran). If provided, the user must also define the
#'   argument `modifiers` and must not provide a `logsteps` argument.
#'
#' @param        logsteps      A list (or a singleton) of LoggingStep
#'   objects. Each of these specifies a modifier function and a logger
#'   function that modify and log the changes in a dataset at each step of a
#'   flog() pipeline. Providing a `logsteps` argument excludes the user from
#'   providing a pair of `modifiers` or `loggers` arguments to flog().
#'
#' @return       A LoggingTuple, where the dataset entry
#'   is the result of running .data@dataset \%>\% modifiers1 \%>\% modifiers2
#'   \%>\% ... \%>\% modifiersN and the log.data entry contains the logging
#'   information for each of the N processing steps (for the k'th step, this
#'   is given by comparing the input to the k'th modifier to the output from
#'   running the k'th modifier using the k'th logger function).
#'
#' @importFrom   magrittr      %>%
#'
#' @export
#'
#' @examples
#'   .tuple <- LoggingTuple(dataset = c(2, 1, 1), log.data = list())
#'   logF <- function(post, pre){
#'     data.frame(before = length(pre), after = length(post))
#'     }
#'
#'   flog(.tuple, modifiers = sum, loggers = logF)
#'
#'   # `.tuple` is the first arg so that flog() steps can be piped together
#'   # Hence, the next code is equivalent to
#'   # .tuple %>% flog(unique, logF) %>% flog(sort, logF) %>% flog(sum, logF)
#'   # and
#'   # .tuple %>% flog(c(unique, sort, sum), c(logF, logF, logF))
#'   # since, if either modifiers (loggers) is a single entry, it is recycled
#'   # to match the number of entries in loggers (modifiers, resp.)
#'
#'   flog(.tuple, c(unique, sort, sum), logF)
#'
#'   # You can use a raw dataset (ie, without log.data) as input as follows:
#'
#'   my.data <- c("I'm", "spaRtacus")
#'   # - Explicit set-up of LoggingTuple:
#'   #  flog(.data = LoggingTuple(dataset = my.data, log.data = list()),
#'   #       tolower, logF)
#'   # - Implicit set-up of `dataset` and `log.data` entries of in.list:
#'   #  flog(.data = LoggingTuple(my.data, list()), tolower, logF)
#'   #  flog(.data = LoggingTuple(my.data),         tolower, logF)
#'   # - Use bare dataset (and flog will convert it to a LoggingTuple)
#'   flog(my.data, modifiers = tolower, loggers = logF)
#'
flog <- function(
    .data,
    modifiers,
    loggers,
    logsteps
  ){
  if (missing(.data)) {
    stop(".data should be defined in flog()")
    }

  # Validity tests on .data
  # - If it isn't a LoggingTuple, it should be converted to one.
  if (methods::is(.data, "LoggingTuple")) {
    tuple <- .data
    } else {
    tuple <- LoggingTuple(.data)
    }

  # START of logsteps definition
  # TODO: move this wiry code into a separate function
  #
  # The user must provide logsteps or both modifiers and loggers, but must not
  #   provide both logsteps and either modifiers or loggers
  # - If logsteps (a singleton or list of LoggingStep objects) has not been
  #     provided, this is created using the modifiers and loggers lists
  # - Then we check that each entry of logsteps is of class LoggingStep
  #     so that we can use each of them in this pipeline of LoggingSteps
  if (missing(logsteps)) {
    if (missing(modifiers) || missing(loggers)) {
      stop("neither logsteps nor (modifiers & loggers) were defined in flog")
      }
    if (length(loggers) == 0) {
      stop("at least one logger is required if logsteps are not provided")
      }
    if (length(modifiers) == 0) {
      stop("at least one modifier is required if logsteps are not provided")
      }
    if (length(modifiers) > 1 &&
       length(loggers)   > 1 &&
       length(modifiers) != length(loggers)
       ) {
      stop(
        paste("if the modifiers/loggers have length > 1, then their lengths",
              "should be identical")
        )
      }

    # Define `logsteps` in terms of `modifiers` and `loggers`
    # - Note that if multiple modifiers are provided, they arrive as a list
    # - Whereas, a single modifier arrives as a fuction (and also for loggers)
    # - Therere, we have to convert singleton functions into a list before
    #     using Map())
    .uplist <- function(.x){
      if (is.list(.x)) {
        .x
        } else {
        list(.x)
        }
      }
    mod.list <- .uplist(modifiers)
    log.list <- .uplist(loggers)
    step.names <- if (length(names(log.list)) > length(names(mod.list))) {
      names(log.list)
      } else {
      names(mod.list)
      }

    logsteps <- Map(
      function(mod_fn, log_fn){
        LoggingStep(mod_fn, log_fn)
        },
      mod.list,
      log.list
      ) %>%
      setNames(step.names)
    } else {
    if (!missing(modifiers) || !missing(loggers)) {
      stop("logsteps or (modifiers & loggers) should be defined, but not both")
      }
    }
  # END of logsteps definition

  # `logsteps` should either be a single LoggingStep object or a list thereof
  stopifnot(
    length(logsteps) > 0 &&
    (
      methods::is(logsteps, "LoggingStep") ||
      all(sapply(logsteps, function(x) methods::is(x, "LoggingStep")))
      )
    )

  # Recursively run the LoggingSteps over the input data
  # This constructs: in.data %>% mod1 %>% mod2 %>% ... %>% modN
  #   as the first component of the output, where modX is the modifier function
  #   for LoggingStep `X`
  # And the second component of the output is a length-N list containing the
  #   logging notes re each of the N steps
  go <- function(in.tuple, lsteps){
    if (methods::is(lsteps, "LoggingStep")) {
      return(.run_step(lsteps)(in.tuple))
      }
    if (length(lsteps) == 0) {
      return(in.tuple)
      } else {
      step.name <- if (is.null(names(lsteps))) {
        NULL
        } else {
        names(lsteps)[1]
        }
      return(
        go(
          in.tuple = .run_step(object    = lsteps[[1]],
                               step.name = step.name
                               )(in.tuple),
          lsteps   = lsteps[-1]
          )
        )
      }
    }

  go(tuple, logsteps)
  }

##############################################################################

#' flog.filter_df
#'
#'
#' Applies a sequence of filtering steps to a data.frame and returns a
#' (dataset, log.data) tuple where the log.data list contains a single
#' data.frame generated by combining the results of each individual logging
#' function
#'
#' @param        .data         A LoggingTuple or a raw dataset. The dataset
#'   entry should be a data.frame. See drug.markers::flog.
#'
#' @param        filter.dots   A vector or list of strings that define the
#' filters that shouold be applied to a data.frame; each of these should be a
#' valid input to the .dots arg of dplyr::filter_. If any of the entries is
#' "identity", then the data.frame that is passed in is unaltered by that
#' specific filtering step (although may be altered by subsequent steps).
#'
#' @param        logger        A single logging function(post, pre) for
#' comparing a dataframe after a single filtering step (post) to the dataframe
#' that before that step (pre). See drug.markers::flog for more details. This
#' logging function is applied after each filtering step in filter.dots has
#' ran.
#'
#' @importFrom   dplyr         bind_rows   filter_   mutate_
#' @importFrom   magrittr      %>%
#' @importFrom   stats         setNames
#'
#' @export
#'
flog.filter_df <- function(
    .data,
    filter.dots,
    logger
  ){
  # TODO: Remove these validity tests, since they're copied in from flog()
  # ... or move the validity test and manipulation step to a separate function
  # ... that returns a LoggingTuple

  if (missing(.data)) {
    stop(".data should be defined in flog()")
    }

  # Validity tests on .data
  # - If it isn't a LoggingTuple, it should be converted to one.
  if (methods::is(.data, "LoggingTuple")) {
    tuple <- .data
    } else {
    tuple <- LoggingTuple(.data)
    }

  # TODO: pull this out as non-exported function .flog_filter_maker and unit
  #   test it
  # TODO: If .dots is a function, return that function
  # Converts a string into a filter-function for use in flog.filter_df
  filter_maker <- function(.dots){
    if (.dots == "identity") {
      return(identity)
      } else {
      return(
        function(DF){
          dplyr::filter_(DF, .dots = .dots)
          }
        )
      }
    }

  stopifnot(is.data.frame(get_dataset(tuple)))

  # TODO: Test to check that existing log.data is neither bind_row'ed nor
  # dropped nor causes errors (I think this is only possible if the logger
  # could return NULL for some input)
  # TODO: rewrite as an extension to the LoggingStep class
  # TODO: OR write a flog_and_logCollapse function and use this
  # TODO: named steps in the log.data in `result`
  # Assumes that the output from logger is a data.frame
  # Combines multiple, string-form filters, each to be applied sequentially,
  #   into a single flog-type function
  modifiers      <- Map(filter_maker, filter.dots)
  modifier_names <- names(modifiers)

  fd <- flog(.data     = get_dataset(tuple),
             modifiers = modifiers,
             loggers   = logger
             )

  new_log_value <- get_logdata(fd) %>%
    setNames(modifier_names) %>%
    # TODO: check for NULL-offset bug within rbind_all / bind_rows in dplyr-0.5
    dplyr::bind_rows(.id = "filter.name") %>%
    dplyr::mutate_(
      filter.name = ~ factor(filter.name, levels = modifier_names)
      ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  result <- LoggingTuple(
    dataset  = get_dataset(fd),
    log.data = append(get_logdata(tuple), list(new_log_value))
    )
  result
  }

##############################################################################
