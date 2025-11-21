#' Text Wrapping Helper Functions
#'
#' Collection of helper functions for text formatting and wrapping in plots and tables.
#'
#' @name helper_functions
#' @keywords internal
NULL

#' Wrap Text to Specified Width
#'
#' Wraps text to a specified character width, useful for plot titles and labels.
#'
#' @param text character string to wrap
#' @param width integer, maximum character width per line (default: 50)
#'
#' @return character string with line breaks inserted
#'
#' @export
#' @importFrom stringr str_wrap
wrapper <- function(text, width = 50) {
  if (is.null(text)) return(NULL)
  stringr::str_wrap(text, width = width)
}

#' Create Label Formatter for ggplot2
#'
#' Creates a function that wraps text labels to a specified width for use with
#' ggplot2 scale functions.
#'
#' @param width integer, maximum character width per line
#'
#' @return function that wraps text to specified width
#'
#' @export
#' @importFrom stringr str_wrap
wrap_format <- function(width) {
  function(x) {
    stringr::str_wrap(x, width = width)
  }
}

#' Trim Whitespace from Strings
#'
#' Removes leading and trailing whitespace from character strings.
#' This is a simple wrapper around base R trimws function.
#'
#' @param x character vector
#'
#' @return character vector with whitespace trimmed
#'
#' @export
trim <- function(x) {
  trimws(x)
}

#' Nest Data Frame by Groups (Internal Helper)
#'
#' Internal helper function to nest data frames by grouping variables.
#' This is a simplified version that works with the specific needs of the package.
#'
#' @param x grouped data frame
#'
#' @return nested data frame
#'
#' @keywords internal
#' @importFrom dplyr group_nest
.nest <- function(x) {
  # Use dplyr's group_nest if available, otherwise create manual nesting
  if (requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::group_nest(x, .key = "data")
  } else {
    stop("dplyr package required for data nesting")
  }
}

#' Create Data Frame (Internal Helper)
#'
#' Internal helper function to create data frames. This is a wrapper around
#' base data.frame with stringsAsFactors = FALSE as default.
#'
#' @param ... arguments passed to data.frame
#'
#' @return data frame
#'
#' @keywords internal
data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
} 