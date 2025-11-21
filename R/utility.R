#' Frequency Tables with Adjusted Rounding
#'
#' This function is ripped out of sjmisc with the rounding adjusted.
#' Creates frequency tables for variables with optional weighting and grouping.
#'
#' @param x data frame or vector to analyze
#' @param ... additional variables (for grouped data frames)
#' @param sort.frq string, sorting option: "none", "asc", "desc" (default: "none")
#' @param weights variable name for weighting (default: NULL)
#' @param auto.grp numeric, automatic grouping threshold (default: NULL)
#' @param show.strings bool, whether to show string variables (default: TRUE)
#' @param show.na bool, whether to show NA values (default: TRUE)
#' @param grp.strings numeric, precision for string grouping (default: NULL)
#' @param min.frq numeric, minimum frequency threshold (default: 0)
#' @param out string, output format: "txt", "viewer", "browser" (default: "txt")
#' @param title string, custom title (default: NULL)
#' @param encoding string, character encoding (default: "UTF-8")
#' @param file string, output file path (default: NULL)
#'
#' @return A list of data frames with frequency statistics and attributes
#'
#' @export
#' @importFrom sjlabelled get_label get_labels as_numeric copy_labels
#' @importFrom sjmisc is_empty var_type zap_inf replace_na empty_cols remove_empty_cols
#' @importFrom dplyr enquos bind_cols select_if n_distinct full_join bind_rows filter select group_vars group_keys
#' @importFrom stats na.omit weighted.mean sd xtabs qnorm
#' @importFrom rlang quo_name enquo
#' @importFrom purrr map_if
frq <- function(x,
                ...,
                sort.frq = c("none", "asc", "desc"),
                weights = NULL,
                auto.grp = NULL,
                show.strings = TRUE,
                show.na = TRUE,
                grp.strings = NULL,
                min.frq = 0,
                out = c("txt", "viewer", "browser"),
                title = NULL,
                encoding = "UTF-8",
                file = NULL) {
  
  out <- match.arg(out)
  
  if (out != "txt" && !requireNamespace("sjPlot", quietly = TRUE)) {
    message("Package `sjPlot` needs to be loaded to print HTML tables.")
    out <- "txt"
  }
  
  # check min.frq value
  if (!is.numeric(min.frq)) {
    message("min.frq value is not numeric. Returned output assumes default value 0.")
    min.frq <- 0
  }
  
  # get dot data
  xw <- sjlabelled:::.get_dot_data(x, dplyr::enquos(...))
  
  if (missing(weights)) {
    w <- NULL
    x <- xw
  } else {
    w <- try(rlang::quo_name(rlang::enquo(weights)), silent = TRUE)
    if (inherits(w, "try-error")) w <- NULL
    
    w.string <- try(eval(weights), silent = TRUE)
    if (!inherits(w.string, "try-error") && is.character(w.string)) w <- w.string
    
    if (!sjmisc::is_empty(w) && w != "NULL" && !.obj_has_name(xw, w) && .obj_has_name(x, w)) {
      x <- dplyr::bind_cols(xw, data.frame(x[[w]]))
      colnames(x)[ncol(x)] <- w
    } else if (!sjmisc::is_empty(.string_contains("$", w)) && length(w.string) > 1 && is.numeric(w.string)) {
      x <- cbind(xw, data.frame(w.string))
      w <- sub("(.*)\\$(.*)", "\\2", w)
      colnames(x)[ncol(x)] <- w
    } else {
      message(sprintf("Weights `%s` not found in data.", w))
      w <- NULL
      x <- xw
    }
  }
  
  
  if (!isTRUE(show.na)) {
    # remove empty columns
    
    rem.col <- empty_cols(x)
    
    if (!sjmisc::is_empty(rem.col)) {
      rem.vars <- colnames(x)[rem.col]
      x <- remove_empty_cols(x)
      
      message(sprintf("Following %i variables have only missing values and are not shown:", length(rem.vars)))
      cat(paste(sprintf("%s [%i]", rem.vars, rem.col), collapse = ", "))
      cat("\n")
    }
  }
  
  
  # match args
  sort.frq <- match.arg(sort.frq)
  
  # return values
  dataframes <- list()
  
  
  # remove strings from output, if requested
  # and check if there are any variables left to print
  
  if (!show.strings)
    x <- dplyr::select_if(x, .no_character)
  
  if ((all(sjmisc::is_empty(stats::na.omit(x), first.only = FALSE)) && show.na == FALSE) || all(suppressMessages(replace_na(sjmisc::is_empty(x, first.only = FALSE, all.na.empty = FALSE), value = FALSE))))
    return(NULL)
  
  
  # group strings
  
  if (!is.null(grp.strings)) {
    a <- attributes(x)
    
    if (!is.data.frame(x)) {
      was.df <- FALSE
      x <- data.frame(x, stringsAsFactors = FALSE)
    } else
      was.df <- TRUE
    
    
    x <- x %>%
      purrr::map_if(is.character, ~ group_str(
        strings = .x, precision = grp.strings, remove.empty = FALSE)
      ) %>%
      as.data.frame(stringsAsFactors = FALSE)
    
    if (was.df)
      attributes(x) <- a
    else
      attributes(x[[1]]) <- a
  }
  
  
  # do we have a grouped data frame?
  if (inherits(x, "grouped_df")) {
    
    grkey <- colnames(dplyr::group_keys(x))
    for (i in grkey) {
      if (is.character(x[[i]])) x[[i]] <- as.factor(x[[i]])
    }
    
    # get grouped data
    grps <- .get_grouped_data(x)
    
    # we may have more than two variables...
    for (j in seq_len(ncol(grps$data[[1]]))) {
      
      # now plot everything
      for (i in seq_len(nrow(grps))) {
        # copy back labels to grouped data frame
        tmp <- sjlabelled::copy_labels(grps$data[[i]][j], x)
        
        if (!is.null(w))
          wb <- grps$data[[i]][[w]]
        else
          wb <- NULL
        
        # user-defined title
        if (!is.null(title) && length(title) >= i)
          gr.title <- title[i]
        else
          gr.title <- NULL
        
        # iterate data frame, but don't select
        # weighting variable
        if (is.null(w) || colnames(tmp)[1] != w) {
          dummy <-
            .frq_helper(
              x = tmp[[1]],
              sort.frq = sort.frq,
              weight.by = wb,
              cn = colnames(tmp)[1],
              auto.grp = auto.grp,
              title = gr.title,
              show.na = show.na,
              min.frq = min.frq
            )
          
          attr(dummy, "group") <- .get_grouped_title(x, grps, i, sep = ", ", long = FALSE)
          
          # save data frame for return value
          dataframes[[length(dataframes) + 1]] <- dummy
        }
      }
    }
    
  } else {
    # if we don't have data frame, coerce
    if (!is.data.frame(x)) x <- data.frame(x, stringsAsFactors = FALSE)
    
    if (!is.null(w))
      wb <- x[[w]]
    else
      wb <- NULL
    
    for (i in seq_len(ncol(x))) {
      # iterate data frame, but don't select
      # weighting variable
      if (is.null(w) || colnames(x)[i] != w) {
        dummy <-
          .frq_helper(
            x = x[[i]],
            sort.frq = sort.frq,
            weight.by = wb,
            cn = colnames(x)[i],
            auto.grp = auto.grp,
            title = title,
            show.na = show.na,
            min.frq = min.frq
          )
        
        # save data frame for return value
        dataframes[[length(dataframes) + 1]] <- dummy
      }
    }
  }
  
  # add class-attr for print-method()
  if (out == "txt")
    class(dataframes) <- c("sjmisc_frq", "list")
  else
    class(dataframes) <- c("sjt_frq", "sjmisc_frq", "list")
  
  # save how to print output
  attr(dataframes, "print") <- out
  attr(dataframes, "encoding") <- encoding
  attr(dataframes, "file") <- file
  
  dataframes
}

#' Frequency Helper Function (Internal)
#'
#' Internal helper function for frequency table generation with proper
#' handling of weights, missing values, and various data types.
#'
#' @param x vector to analyze
#' @param sort.frq string, sorting method
#' @param weight.by numeric vector of weights
#' @param cn string, column name
#' @param auto.grp numeric, auto grouping threshold
#' @param title string, custom title
#' @param show.na bool, show NA values
#' @param min.frq numeric, minimum frequency threshold
#'
#' @return data frame with frequency statistics
#'
#' @keywords internal
.frq_helper <- function(x, sort.frq, weight.by, cn, auto.grp, title = NULL, show.na = TRUE, min.frq = 0) {
  # remember type
  vartype <- sjmisc::var_type(x)
  
  # convert NaN and Inf to missing
  x <- zap_inf(x)
  
  # variable with only missing?
  if (length(stats::na.omit(x)) == 0 && show.na == FALSE) {
    mydat <- data.frame(
      val = NA,
      label = NA,
      frq = NA,
      raw.prc = NA,
      valid.prc = NA,
      cum.perc = NA
    )
    return(structure(class = "sjmisc_frq", list(mydat = mydat)))
  }
  
  
  # save descriptive statistics
  
  xnum <- sjlabelled::as_numeric(x, keep.labels = FALSE)
  
  if (!is.null(weight.by)) {
    # make sure, vector and weights have same length, so remove missing from weights
    
    weight.by[is.na(xnum)] <- NA
    xnum[is.na(weight.by)] <- NA
    x[is.na(weight.by)] <- NA
    
    mean.value <- stats::weighted.mean(stats::na.omit(xnum), w = stats::na.omit(weight.by))
    
    if (requireNamespace("sjstats", quietly = TRUE))
      sd.value <- sjstats::wtd_sd(stats::na.omit(xnum), weights = stats::na.omit(weight.by))
    else
      sd.value <- NA
    
  } else {
    mean.value <- mean(xnum, na.rm = TRUE)
    sd.value <- stats::sd(xnum, na.rm = TRUE)
  }
  
  
  # get variable label (if any)
  varlab <- sjlabelled::get_label(x)
  
  
  # numeric variables with many distinct values may
  # be grouped for better overview
  
  if (!is.null(auto.grp) && dplyr::n_distinct(x, na.rm = TRUE) >= auto.grp) {
    gl <- group_labels(x, size = "auto", n = auto.grp)
    x <- group_var(x, size = "auto", n = auto.grp)
    gv <- sort(stats::na.omit(unique(x)))
    names(gv) <- gl
    attr(x, "labels") <- gv
  }
  
  
  # get value labels (if any)
  labels <-
    sjlabelled::get_labels(
      x,
      attr.only = TRUE,
      values = "n",
      non.labelled = TRUE
    )
  
  
  # if we don't have variable label, use column name
  if (sjmisc::is_empty(varlab) && !sjmisc::is_empty(cn))
    varlab <- cn
  else if (!sjmisc::is_empty(varlab) && !sjmisc::is_empty(cn))
    varlab <- sprintf("%s (%s)", varlab, cn)
  
  
  # do we have a labelled vector?
  if (!is.null(labels)) {
    # add rownames and values as columns
    dat <- data_frame(
      n = names(labels),
      v = as.character(labels)
    )
    
    colnames(dat) <- c("val", "label")
    
    # character vectors need to be converted with to_value
    # to avoid NAs, but only if character is non-numeric
    if (is.character(dat$val) && anyNA(suppressWarnings(as.numeric(dat$val))))
      dat$val <- sjlabelled::as_numeric(dat$val, keep.labels = FALSE)
    else
      dat$val <- as.numeric(dat$val)
    
    # weight data?
    if (!is.null(weight.by)) {
      dat2 <- data.frame((
        stats::xtabs(
          weights ~ x,
          data = data.frame(weights = stats::na.omit(weight.by), x = stats::na.omit(x)),
          na.action = stats::na.pass,
          exclude = NULL
        )
      ))
    } else {
      # create frequency table
      dat2 <- data.frame(table(x, useNA = "always"))
    }
    
    colnames(dat2) <- c("val", "frq")
    dat2$val <- sjlabelled::as_numeric(dat2$val, keep.labels = FALSE)
    
    # join frq table and label columns
    mydat <- suppressMessages(dplyr::full_join(dat, dat2))
    
    # replace NA with 0, for proper percentages, i.e.
    # missing values don't appear (zero counts)
    mydat$frq <- suppressMessages(sjmisc::replace_na(mydat$frq, value = 0))
  } else {
    # weight data?
    if (!is.null(weight.by)) {
      mydat <- data.frame((
        stats::xtabs(
          weights ~ x,
          data = data.frame(weights = stats::na.omit(weight.by), x = stats::na.omit(x)),
          na.action = stats::na.pass,
          exclude = NULL
        )
      ))
    } else {
      # if we have no labels, do simple frq table
      mydat <- data.frame(table(x, useNA = "always"))
    }
    
    colnames(mydat) <- c("val", "frq")
    
    if (!anyNA(suppressWarnings(as.numeric(attr(mydat$val, "levels"))))) {
      mydat$val <- sjlabelled::as_numeric(mydat$val, keep.labels = FALSE)
    }
    
    # add values as label
    mydat$label <- as.character("<none>")
    mydat <- mydat[c("val", "label", "frq")]
  }
  
  min.frq.string <- sprintf("n < %g", min.frq)
  
  if (any(mydat$frq[!is.na(mydat$val)] < min.frq)) {
    mydatS1 <- mydat[which(mydat$frq >= min.frq | is.na(mydat$val)), ]
    mydatS2 <- mydat[which(mydat$frq < min.frq & !is.na(mydat$val)), ]
    
    mydatS3 <- data_frame(
      val = min.frq.string,
      label = "<none>",
      frq = sum(mydatS2$frq)
    )
    
    if (mydatS3$frq == 0) {
      mydat <- mydatS1
    } else {
      mydat <- rbind(mydatS1, mydatS3)
      row.names(mydat) <- c(
        row.names(mydat)[-length(row.names(mydat))],
        as.character(as.integer(row.names(mydat)[length(row.names(mydat)) - 1]) + 1)
      )
    }
  }
  
  # need numeric
  if (is.factor(x) || is.character(x)) {
    x <- sjlabelled::as_numeric(x, keep.labels = FALSE)
  }
  
  # check if we have any NA-values - if not, add row for NA's
  if (!anyNA(mydat$val)) {
    mydat <- dplyr::bind_rows(
      mydat,
      data.frame(
        val = NA,
        label = NA,
        frq = 0
      )
    )
  }
  
  # valid values are one row less, because last row is NA row
  valid.vals <- nrow(mydat) - 1
  
  if (!all(is.na(mydat$val))) {
    
    extra.vals <- 1
    # Momentarily, in order to sort categories, we consider lower frequencies subtotal as a non valid value
    if (is.na(mydat$val[valid.vals]) & mydat$val[valid.vals + 1] == min.frq.string) {
      valid.vals <- valid.vals - 1
      extra.vals <- 2
    }
    
    
    # sort categories ascending or descending
    if (!is.null(sort.frq) && (sort.frq == "asc" || sort.frq == "desc")) {
      ord <- order(mydat$frq[seq_len(valid.vals)], decreasing = (sort.frq == "desc"))
    } else {
      ord <- seq_len(valid.vals)
    }
    mydat <- mydat[c(ord, (valid.vals + extra.vals):(valid.vals + 1)), ]
  }
  
  valid.vals <- nrow(mydat) - 1
  
  # raw percentages
  mydat$raw.prc <- mydat$frq / sum(mydat$frq)
  
  # compute valid and cumulative percentages
  mydat$valid.prc <- c(mydat$frq[seq_len(valid.vals)] / sum(mydat$frq[seq_len(valid.vals)]), NA)
  mydat$cum.prc <- c(cumsum(mydat$valid.prc[seq_len(valid.vals)]), NA)
  
  # proper rounding
  mydat$raw.prc <- 100 * round(mydat$raw.prc, 4)
  mydat$cum.prc <- 100 * round(mydat$cum.prc, 4)
  mydat$valid.prc <- 100 * round(mydat$valid.prc, 4)
  
  # "rename" labels for NA values
  if (!is.null(mydat$label)) mydat$label[is.na(mydat$val)] <- NA_character_
  
  if (!all(is.na(mydat$val))) {
    if (extra.vals == 1) {
      # save original order
      reihe <- sjlabelled::as_numeric(mydat$val, start.at = 1, keep.labels = FALSE)
      # sort
      if (sort.frq == "none") mydat <- mydat[order(reihe), ]
    } else if (extra.vals == 2) {
      # save original order
      reihe <- suppressWarnings(sjlabelled::as_numeric(mydat$val[-c(valid.vals, valid.vals + 1)], start.at = 1, keep.labels = FALSE))
      # sort
      if (sort.frq == "none") mydat <- mydat[c(order(reihe), valid.vals, valid.vals + 1), ]
    }
  }
  
  # remove NA, if requested
  has.na <- mydat$frq[nrow(mydat)] > 0
  if ((!is.logical(show.na) && show.na == "auto" && !has.na) || identical(show.na, FALSE)) {
    mydat <- mydat[-nrow(mydat), ]
  }
  
  # compute relative confidence intervals
  total_n <- sum(mydat$frq)
  rel_frq <- as.numeric(mydat$frq / total_n)
  ci <- stats::qnorm(.975) * suppressWarnings(sqrt(rel_frq * (1 - rel_frq) / total_n))
  total_ci <- data.frame(lower = total_n * (rel_frq - ci), upper = total_n * (rel_frq + ci))
  relative_ci <- data.frame(lower = rel_frq - ci, upper = rel_frq + ci)
  
  
  # add variable label and type as attribute, for print-method
  
  if (!is.null(title)) {
    attr(mydat, "label") <- title
    attr(mydat, "vartype") <- ""
    
  } else {
    attr(mydat, "label") <- varlab
    attr(mydat, "vartype") <- vartype
  }
  
  attr(mydat, "mean") <- mean.value
  attr(mydat, "sd") <- sd.value
  
  attr(mydat, "ci") <- total_ci
  attr(mydat, "relative.ci") <- relative_ci
  
  row.names(mydat) <- NULL
  
  mydat
}

#' Get Grouped Title (Internal Helper)
#'
#' Internal helper function to generate titles for grouped data.
#'
#' @param x data frame
#' @param grps grouped data
#' @param i integer, group index
#' @param sep string, separator
#' @param long bool, long format
#'
#' @return string, formatted title
#'
#' @keywords internal
.get_grouped_title <- function(x, grps, i, sep = ", ", long = FALSE) {
  # create title for first grouping level
  tp <- .get_title_part(x, grps, 1, i)
  
  if (long)
    title <- sprintf("%s: %s", tp[1], tp[2])
  else
    title <- sprintf("%s", tp[2])
  
  # do we have another groupng variable?
  if (length(dplyr::group_vars(x)) > 1) {
    tp <- .get_title_part(x, grps, 2, i)
    
    if (long)
      title <- sprintf("%s%s%s: %s", title, sep, tp[1], tp[2])
    else
      title <- sprintf("%s%s%s", title, sep, tp[2])
  }
  
  # return title
  title
}

#' Get Title Part (Internal Helper)
#'
#' Internal helper function to get title parts for grouped data.
#'
#' @param x data frame
#' @param grps grouped data
#' @param level integer, level index
#' @param i integer, group index
#'
#' @return character vector with title parts
#'
#' @keywords internal
.get_title_part <- function(x, grps, level, i) {
  # prepare title for group
  var.name <- colnames(grps)[level]
  
  # get values from value labels
  vals <- sjlabelled::get_values(x[[var.name]])
  t2 <- NULL
  
  # if we have no value labels, get values directly
  if (is.null(vals)) {
    vals <- grps[[var.name]]
    if (is.factor(grps[[var.name]])) vals <- as.character(vals)
    lab.pos <- i
  } else {
    # find position of value labels for current group
    lab.pos <- which(vals == grps[[var.name]][i])
    t2 <- sjlabelled::get_labels(x[[var.name]])[lab.pos]
  }
  
  # get variable and value labels
  t1 <- sjlabelled::get_label(x[[var.name]], def.value = var.name)
  
  # if we have no value label, use value instead
  if (sjmisc::is_empty(t2)) t2 <- vals[lab.pos]
  
  # generate title
  c(t1, t2)
}

#' Check Object Has Name (Internal Helper)
#'
#' Internal helper function to check if object has specific name.
#'
#' @param x object to check
#' @param name string, name to look for
#'
#' @return logical, TRUE if name exists
#'
#' @keywords internal
.obj_has_name <- function(x, name) {
  name %in% names(x)
}

#' String Contains Pattern (Internal Helper)
#'
#' Internal helper function to find pattern in strings.
#'
#' @param pattern string, pattern to search for
#' @param x character vector to search in
#'
#' @return integer vector of matching positions
#'
#' @keywords internal
.string_contains <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E")
  grep(pattern, x, perl = TRUE)
}

#' Get Grouped Data (Internal Helper)
#'
#' Internal helper function to process grouped data frames.
#'
#' @param x grouped data frame
#'
#' @return processed grouped data
#'
#' @keywords internal
.get_grouped_data <- function(x) {
  # nest data frame
  grps <- .nest(x)
  
  # remove NA category for grouped data
  cc <- grps %>%
    dplyr::select(-.data$data) %>%
    stats::complete.cases()
  
  # select only complete cases
  grps <- dplyr::filter(grps, !! cc)
  
  # arrange data
  
  if (length(dplyr::group_vars(x)) == 1)
    reihe <- order(grps[[1]])
  else
    reihe <- order(grps[[1]], grps[[2]])
  
  grps <- grps[reihe, , drop = FALSE]
  
  grps
}

#' Check Not Character (Internal Helper)
#'
#' Internal helper function to check if variable is not character type.
#'
#' @param x variable to check
#'
#' @return logical, TRUE if not character
#'
#' @keywords internal
.no_character <- function(x) !is.character(x)

#' Convert Frequency Object to Data Frame
#'
#' S3 method to convert frequency objects to data frames.
#'
#' @param x frequency object
#' @param row.names row names (unused)
#' @param optional logical (unused)
#' @param ... additional arguments
#'
#' @return data frame with frequency results
#'
#' @keywords internal
.as.data.frame.frq <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- lapply(x, function(i) {
    i$variable <- attr(i, "label")
    i$group <- attr(i, "group")
    cols <- c("variable", "group", "val", "label", "frq", "raw.prc", "vaid.prc", "cum.prc")
    i[, intersect(cols, colnames(i))]
  })
  do.call(rbind, x)
}

#' Get Frequency Table and Statistics
#'
#' This function organizes the output of frq() to provide frequency tables and basic statistics
#' for survey data analysis.
#'
#' @param data R data frame containing the survey data
#' @param varname string, variable name to analyze
#' @param get_cum bool, whether to get the cumulative percentages (default: FALSE)
#' @param weight bool, whether to weight the data (default: TRUE)
#' @param haven bool, whether the data was read by haven (TRUE) or sjlabelled (FALSE) (default: TRUE)
#'
#' @return A list containing:
#' \itemize{
#'   \item table - frequency table without a "total" row
#'   \item table_N - frequency table with a "total" row
#'   \item N - valid N count
#'   \item mean - mean value
#'   \item sd - standard deviation
#'   \item question - question label
#' }
#'
#' @export
#' @importFrom sjlabelled get_labels get_label
#' @importFrom dplyr filter select contains
get_a_lot <- function(data, varname,
                      get_cum = FALSE, weight=TRUE, haven = T) {
  
  # dt: R data frame
  # varname: string, variable name
  # get_cum: bool, whether get the cumulative percentages
  # weight: bool, whether to weight the data
  # haven: bool, whether the data was read by sjlabelled (F) or haven (T)
  # return a list
  
  # NOTE: Depending on different machine environments, 
  # <NA> in var may cause error in frq() when weight applies!
  
  # # set question levels in case freq doesn't pick up all levels of answer
  # "raw" frequency table

  if(nrow(data) != 0){
    if(haven) {
      if (weight){
        
        freq = frq(data[,varname], weights=data$Weight)
      } else {
        freq = frq(data[,varname],)
      }
    } else {
      if (weight){
        
        freq = frq(factor(data[,varname],levels=get_labels(data[,varname])),weights=data$Weight)
      } else {
        freq = frq(factor(data[,varname],levels=get_labels(data[,varname])))
      }
    }
    
    # keep only the relevant columns and rows
    freq_tbl = data.frame(freq[[1]])
    
    # remove NA values
    freq_tbl = freq_tbl %>%
      dplyr::filter(val != "<NA>" | label !="<NA>")
    
    
    if (nrow(freq_tbl)>0 | !is.null(freq)) {
      freq_tbl = freq_tbl %>%
        dplyr::select(-contains("label"), -raw.prc)
    }
    
    # this is a quick check for when table is totally empty (i.e. no valid responses)
    
    # keep cumulative column or not
    if(get_cum) {
      
      names(freq_tbl) <- c(varname, "N", "Percent","Cumulative")
      freq_tbl_N <- freq_tbl
      
    } else{
      
      if (ncol(freq_tbl)>3) {
        freq_tbl = freq_tbl %>%
          dplyr::select(-cum.prc)
      }
      names(freq_tbl) <- c(varname, "N", "Percent")
      
      # append a summary/"total" row to the dataframe
      freq_tbl_N <- freq_tbl
      freq_tbl_N[,1] <- as.character(freq_tbl_N[,1])
      freq_tbl_N[nrow(freq_tbl_N) + 1,] = list("Weighted Total",sum(freq_tbl[2]),100)
    }
    
    # output 
    result = list(table = freq_tbl, # frequency table without a "total" row
                  table_N = freq_tbl_N, # frequency table with a "total" row 
                  # (output with a column of cummulative percentage 
                  # won't get the "total" row)
                  N = sum(freq_tbl[2]), # valid N
                  mean = attributes(freq[[1]])$mean, # mean
                  sd = attributes(freq[[1]])$sd, # standard deviation
                  question = get_label(data[,varname])# question
    )
    
  } else {
    if(get_cum){
      stop("not implemented for a empty table")
    }
    levels = get_labels(data[,varname])
    freq_tbl = data.frame(var=factor(levels, levels=get_labels(data[,varname])), N=rep(0,length(levels)), Percent=rep(0,length(levels)))
    if (ncol(freq_tbl)>3) {
      freq_tbl = freq_tbl %>%
        dplyr::select(-cum.prc)
    }
    names(freq_tbl) <- c(varname, "N", "Percent")
    
    # append a summary/"total" row to the dataframe
    freq_tbl_N <- freq_tbl
    freq_tbl_N[,1] <- as.character(freq_tbl_N[,1])
    freq_tbl_N[nrow(freq_tbl_N) + 1,] = list("Weighted Total",sum(freq_tbl[2]),100)
    
    # output 
    result = list(table = freq_tbl, # frequency table without a "total" row
                  table_N = freq_tbl_N, # frequency table with a "total" row 
                  # (output with a column of cummulative percentage 
                  # won't get the "total" row)
                  N = 0, # valid N
                  mean = 0, # mean
                  sd = 0, # standard deviation
                  question = get_label(data[,varname])# question
    )
    
  }
  
  return(result)
}

#' Generate Interlocking Sequence
#'
#' Get an interlocking sequence with user defined interlocking value.
#' This sequence function returns a sequence to rearrange data frame column order.
#' For example, with 6+1 columns, transforms order from 1,2,3,4,5,6,7 to 1,2,5,3,6,4,7
#' if start_second is TRUE.
#'
#' @param seq_length integer, the number of columns in your sequence
#' @param interlock_value integer, the interlocking value (typically (ncol-1)/2)
#' @param start_second bool, whether to start the interlock on the second element (default: TRUE)
#'
#' @return A numeric vector containing the interlocked sequence
#'
#' @examples
#' # For 7 columns with interlock_value of 3
#' get_interlock_seq(7, 3, TRUE)
#' # Returns: 1, 2, 5, 3, 6, 4, 7
#'
#' @export
get_interlock_seq <- function(seq_length, interlock_value, start_second = T){
  sequence_length <- seq_length  # Change this to whatever length you desire

  # Generate the sequence
  sequence <- numeric(sequence_length)
  
  # if starting the interlock on the second element:
  if(start_second){
    for (i in 1:sequence_length) {
      if (i == 1) { # 1 is 1
        sequence[i] = i
      } else if (i %% 2 == 0) { # every even element is normal sequence value
        sequence[i] = i/2+1
      } else { # every odd element is normal sequence + interlock value
        sequence[i] = floor(i/2)+interlock_value+1
      }
    }
  } else {
    for (i in 1:sequence_length) {
      if (i == 1) { # 1 is 1
        sequence[i] = i 
      } else if (i %% 2 == 0) { # every even element is normal + interlock
        sequence[i] = i/2+interlock_value 
      } else { # every odd element is normal sequence
        sequence[i] = ceiling(i/2)
      }
    }
  }
  return(sequence)
}

#' Get Variables by String Pattern
#'
#' This function gets all the variables for a check-all/matrix question by their string prefix.
#' It automatically removes common write-in response variables.
#'
#' @param data R data frame containing the survey data
#' @param str string, string prefix to search for (e.g., "supported_")
#'
#' @return A character vector of variable names matching the pattern
#'
#' @examples
#' # Get all variables starting with "satisfaction_"
#' vars <- get_vars_by_str(survey_data, "satisfaction_")
#'
#' @export
get_vars_by_str <- function(data, str){
  col_name <- grepl(str,names(data))
  varlist <- names(data)[col_name] 
  
  # remove write-in responses (may need to copy and paste this line if there are other write-ins)
  varlist <- varlist[grep("OtherSpec$", varlist, invert=TRUE)] # remove otherSpec!
  varlist <- varlist[grep("Write$", varlist, invert=TRUE)] # remove otherSpec!
  varlist <- varlist[grep("WriteIn$", varlist, invert=TRUE)] # remove otherSpec!
  
  return(varlist)
}

#' Get Weighted Descriptive Statistics
#'
#' Homebrew summary function for weighted indices and descriptive statistics.
#' This function calculates weighted means, standard deviations, and other statistics
#' for survey data, with optional grouping capabilities.
#'
#' @param num data.frame, numeric dataframe (must not contain labelled variables)
#' @param lab data.frame, labelled dataframe
#' @param group character, group variable name for splitting analysis (default: NULL)
#' @param index character, index variable name to analyze (default: NULL)
#' @param formula formula object, formula for Cronbach's alpha calculation (default: NULL)
#' @param round bool, whether to round the results (default: TRUE)
#' @param force bool, forces rounding even when round=FALSE (default: FALSE)
#'
#' @return A data.frame containing descriptive statistics
#'
#' @examples
#' # Overall statistics for a variable
#' stats <- get_w_desc_stats(num_data, lab_data, index = "satisfaction")
#'
#' # Statistics by group
#' stats_by_group <- get_w_desc_stats(num_data, lab_data, 
#'                                   group = "gender", index = "satisfaction")
#'
#' @export
#' @importFrom survey svydesign svycralpha
#' @importFrom haven is.labelled
#' @importFrom rlang is_formula
#' @importFrom forcats fct_drop
#' @importFrom stats weighted.mean complete.cases
get_w_desc_stats = function(num, lab, group = NULL, index = NULL, formula = NULL, round = T, force = F) {
  
  # check valid numeric data
  if(any(sapply(num, haven::is.labelled))){
    stop("Error: Numeric data required")
  }
  
  # check valid formula
  if(!is.null(formula)){
    if(!rlang::is_formula(formula)){
      stop("Error: Formula must be in a formula object, see `help(formula)`")
    }
  }
  
  # if splitting by group
  if(!is.null(group)){
    
    num[, group] <- as.factor(num[, group])
    lab[, group] <- as.factor(lab[, group])
    num[, group] <- forcats::fct_drop(num[,group])
    lab[, group] <- forcats::fct_drop(lab[,group])
    
    #grp_names = sjlabelled::get_labels(lab[, group], drop.unused = T)
    grp_names = levels(lab[, group])
    
    # split data by group factor
    datalist <- split(num, lab[, group])

    # recursively get descriptive stats
    if(force){
      tablelist <- lapply(datalist, get_w_desc_stats, group = NULL, index = index, formula = formula, round = T)
    } else {
      tablelist <- lapply(datalist, get_w_desc_stats, group = NULL, index = index, formula = formula, round = F)
    }

    # left join on "statistics" column and assign names
    joined <- do.call(rbind, tablelist)
    
    # filter out invalid rows (0 mean, 0 var, 0 N)
    joined[rowSums(joined[])>0,]
    
    rownames(joined) <- grp_names

    return(joined)
    
  # overall
  } else {
    if(!is.null(index)){
      #avg = unname(svymean(dw$variable[,index], dw, na.rm = T)[1])
      #var = unname((svyvar(dw$variable[,index], dw, na.rm = T))[1])
      #n = ifelse(!is.na(dw$variables[, index]), 1, 0)
      #wt = sum(n * dw$variables[, "Weight"])
      avg = weighted.mean(num[, index], num[, "Weight"], na.rm = T)
      variance = .w_variance(num[, index], num[, "Weight"], avg)
      sd = sqrt(variance)
      n = num[complete.cases(num[, index]),]
      wt = sum(n[, "Weight"], na.rm = T)
    } else {
      #dw = svydesign(ids=~1, weights=nm[, "Weight"], data=num)
      n = num[complete.cases(num[, index]),]
      wt = sum(n[, "Weight"], na.rm = T)
    }
    
    # get cronbach alpha using survey package
    if(!is.null(formula)){
      dw = svydesign(ids=~1, weights=num[, "Weight"], data=num)
      cronbach = unname(svycralpha(formula, dw, na.rm = T))
      stat = data.frame(cbind(avg, sd, cronbach, variance, wt))
      names(stat) = c("Mean", "Standard Deviation", "Cronbach's Alpha", "Var", "N")
    } else if(is.null(index)) {
      stat = data.frame(wt)
      names(stat) = c("N")
    } else {
      stat = data.frame(cbind(avg, sd, variance, wt))
      names(stat) = c("Mean", "Standard Deviation", "Var", "N")
    }
    
    if(round){
      stat = round(stat, 2)
    }
    
    # return procedure
    return(stat)
  }
}

#' Calculate Weighted Variance (Internal Helper Function)
#'
#' Internal helper function to calculate weighted variance.
#'
#' @param x numeric vector of values
#' @param w numeric vector of weights
#' @param xbar numeric, weighted mean of x
#'
#' @return numeric, weighted variance
#'
#' @references https://en.wikipedia.org/wiki/Weighted_arithmetic_mean#Frequency_weights
#'
#' @keywords internal
.w_variance <- function(x, w, xbar){
  #ref: https://en.wikipedia.org/wiki/Weighted_arithmetic_mean#Frequency_weights
  valid <- complete.cases(x)
  x <- x[valid]
  w <- w[valid]
  sum(w * (x - xbar)^2)/(sum(w)-1)
}

#' Knit Expand for Dynamic Chunk Creation
#'
#' Function to create a chunk within chunk for dynamic figure generation.
#' This function is specialized to knit a figure with custom settings and alt-text
#' within for loops in R Markdown documents.
#'
#' @param chunkname string, unique identifier/chunk name (cannot be blank)
#' @param height numeric, figure height for the chunk
#' @param alttext string, alt-text description for accessibility
#'
#' @details
#' Note: All plots should be saved (named) as .p1 and then use this function (kexpand).
#' These chunks must have a UNIQUE identifier/chunk name and it cannot be left blank
#' (it conflicts with any default chunk name in the parent/global knitr environment).
#'
#' @references https://stackoverflow.com/questions/27233537/plot-several-pictures-with-different-size-in-a-for-loop
#'
#' @export
#' @importFrom knitr knit knit_expand
kexpand <- function(chunkname, height, alttext) {
  cat(knitr::knit(text = knitr::knit_expand(text = 
                                sprintf('```{r %s, fig.height=%s, fig.alt="%s"}\n .pl \n```', chunkname, height, alttext)),
           envir = parent.frame(), quiet = TRUE))
}

#' Read SPSS Files with Proper Data Handling
#'
#' Homebrew function to read SPSS files using haven, with proper handling of
#' labelled data and factors. Returns both labelled and numeric versions of the data.
#'
#' @param data_path string, file path to the SPSS file
#' @param na_vals character vector, user-defined NA value labels (e.g., "No answer", "I don't know") (default: NULL)
#' @param raw bool, whether to return data with haven labelled format (default: FALSE)
#'
#' @return If raw=FALSE, returns a list with:
#' \itemize{
#'   \item dataLab - data with proper factor conversion
#'   \item dataNum - data with labels stripped (numeric)
#' }
#' If raw=TRUE, returns the raw haven-read data.
#'
#' @details
#' This function uses haven to read SPSS files and properly converts labelled variables
#' to factors while handling user-defined missing values. The function returns a tibble,
#' which may not be fully compatible with all CITL functions.
#'
#' @examples
#' # Read SPSS file with default settings
#' survey_data <- read.spss("path/to/survey.sav")
#'
#' # Read with custom NA values
#' survey_data <- read.spss("path/to/survey.sav", 
#'                         na_vals = c("No answer", "Don't know"))
#'
#' @export
#' @importFrom dplyr %>% mutate_if
#' @importFrom sjlabelled to_label
#' @importFrom forcats fct_drop
#' @importFrom haven read_spss is.labelled zap_labels
read.spss <- function(data_path, na_vals = NULL, raw = F) {
  
  # must use haven to read, sjlabelled does not read-in correctly
  # haven auto-sets user-defined spss na VALUES to R NA (not na VALUE-LABELS)
  dataRaw = haven::read_spss(data_path)
  
  # if raw
  if(raw){
    return(dataRaw)
  }
  
  if(!is.vector(na_vals)){
    na_vals = c(na_vals)
  }
  
  # data manipulation
  dataLab = dataRaw %>%
    # must convert to factor using `sjlabelled::to_label`
    mutate_if(haven::is.labelled, sjlabelled::to_label) %>%
    # must use forcats::fct_drop as it is generic
    mutate_if(is.factor, forcats::fct_drop, only = na_vals)
  
  # zap_labels
  dataNum = dataRaw %>%
    mutate_if(haven::is.labelled, haven::zap_labels)
  
  return(list(dataLab = dataLab,
              dataNum = dataNum))
}

#' Run Inner Loop for Significance Testing Tables
#'
#' Run the loop to print statistically significant comparison tables by groups for reports.
#' This function conducts appropriate statistical tests (t-test or ANOVA) and prints
#' formatted tables only for statistically significant results.
#'
#' @param lab data.frame, labelled data
#' @param num data.frame, numeric data  
#' @param var string, variable name (single variable)
#' @param group character vector, grouping variable names
#' @param sort_by string, sorting option for tables (see gen_table for options) (default: "Likert")
#' @param add_total bool, whether to add total row (default: TRUE)
#' @param add_mean bool, whether to add mean row (default: TRUE)
#' @param caption string, table caption text
#' @param caption2 string, additional caption text (default: NULL)
#' @param question string, question text for matrix questions (default: NULL)
#' @param weight bool, whether to use weights (default: TRUE)
#' @param binary bool, whether the variable is yes/no type (default: FALSE)
#' @param debug bool, debug mode returns generated objects (default: FALSE)
#'
#' @details
#' This function:
#' \itemize{
#'   \item Performs appropriate statistical tests based on group size
#'   \item Only prints tables for statistically significant results
#'   \item Handles special formatting for disability groups (6th group)
#'   \item Includes redaction for insufficient sample sizes
#' }
#'
#' @export
run_inner_loop <- function(lab, num, var, group, sort_by = "Likert", add_total = T, add_mean = T, caption, caption2 = NULL, question = NULL, weight = T, binary = F, debug = F){
  for(i in 1:length(group)){
    if(!binary){
      sublab <- lab[, c(var, group, "Weight")]
      validlab = sublab[complete.cases(sublab), ]
      if(nrow(validlab) == 0) {
        next
      }
      if(length(unique(validlab[, group[i]])) > 2){
        sig = our_anova_v23_2(num, lab, var, group[i], ph = F, weight = weight)$aov$Sig
      } else if(length(unique(lab[, group[i]])) > 1) {
        sig = our_ttest_v23(num, lab, var, group[i], weight = weight)$ttest$Sig
      } else {
        cat('<div style="text-align: center;">', sep = "")
        if(!is.null(question)){
          if(!is.null(caption2)){
            cat(paste0("**", caption, " ", question, caption2, " <br> ** \n"))
          } else {
            cat(paste0("**", caption, " ", question, " <br> ** \n"))
          }
        } else {
          cat(paste0("**", caption, " <br> ** \n"))
        }
        #a = lab[complete.cases(lab[, var]), ]
        cat(paste0("**All respondents belongs to the group: ", unique(validlab[, group[i]]), ", See Overall Ratings table**"))
        cat('</div>', sep = "")
        cat("&nbsp;")
        cat("\n")
        sig = "Insig"
      }
    } else {
      sig = "Significant"
    }
    if(sig == "Significant"){
      gtable = gen_table_group_dym(lab, num, 
                                   var = var ,
                                   group = group[i], 
                                   newname = "Ratings", 
                                   weight = weight, add_total=add_total, 
                                   add_mean = add_mean, sort_by = sort_by)
      if(all(sapply(trim(gtable[, 2:ncol(gtable)]), stringi::stri_isempty))){
        cat('<div style="text-align: center;">', sep = "")
        cat(paste0("**Table redacted due to insufficient number of respondents** \n"))
        cat('</div>', sep = "")
        cat("&nbsp;")
        cat("\n")
      } else {
        if(!is.factor(lab[, group[i]])){
          lab[, group[i]] <- as.factor(lab[, group[i]])
        }
        dname = paste(levels(lab[, group[i]]), "(%)")
        colnames(gtable)[2:ncol(gtable)] = dname
        if(i == 6){
          if(!is.null(question)){
            if(!is.null(caption2)){
              print(print_com_kable(gtable, col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption, " ",
                                                     question, 
                                                     caption2, " <br> Rating by Self-Identified as Having a", group[i])))
            } else {
              print(print_com_kable(gtable,  col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption,
                                                     question, " <br> Rating by Self-Identified as Having a", group[i])))
            }
          } else {
            print(print_com_kable(gtable,  col1_width="4.5cm", 
                                  col_width="3cm", 
                                  caption = paste0(caption, " <br> Rating by Self-Identified as Having a", group[i])))
          }
          cat("\n")
          cat("&nbsp;")
        } else {
          if(!is.null(question)){
            if(!is.null(caption2)){
              print(print_com_kable(gtable,  col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption, " ",
                                                     question, 
                                                     caption2, " <br> Rating by ", group[i])))
            } else {
              print(print_com_kable(gtable,  col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption,
                                                     question, " <br> Rating by ", group[i])))
            }
          } else {
            print(print_com_kable(gtable,  col1_width="4.5cm", 
                                  col_width="3cm", 
                                  caption = paste0(caption, " <br> Rating by ", group[i])))
          }
          cat("\n")
          cat("&nbsp;")
        }
        
      }
      if(debug){
        print(gtable)
        print(dname)
        print(sig)
        print(group[i])
        print(var)
        if(!is.null(question)){
          print(question)
        }
      }
      rm(gtable)
      rm(dname)
    }
    rm(sig)
    rm(subLab)
    rm(validLab)
  }
}

#' Run Inner Loop with Graphics for Significance Testing
#'
#' Run the loop to print statistically significant comparison tables and charts by groups for reports.
#' This is a wrapper function that conducts statistical tests and generates both tables and
#' corresponding stacked bar charts for significant results.
#'
#' @param lab data.frame, labelled data
#' @param num data.frame, numeric data
#' @param var string, variable name (single variable)
#' @param group character vector, grouping variable names
#' @param sort_by string, sorting option for tables (see gen_table for options) (default: "Likert")
#' @param add_total bool, whether to add total row (default: TRUE)
#' @param add_mean bool, whether to add mean row (default: TRUE)
#' @param caption string, table caption text
#' @param caption2 string, additional caption text (default: NULL)
#' @param question string, question text for matrix questions (default: NULL)
#' @param weight bool, whether to use weights (default: TRUE)
#' @param binary bool, whether the variable is yes/no type (default: FALSE)
#' @param debug bool, debug mode returns generated objects (default: FALSE)
#'
#' @details
#' This function extends run_inner_loop by:
#' \itemize{
#'   \item Creating both tables and stacked bar charts
#'   \item Using dynamic chunk creation for figures
#'   \item Generating appropriate alt-text for accessibility
#'   \item Handling figure height adjustments based on group size
#' }
#'
#' @export
run_inner_loop_graph <- function(lab, num, var, group, sort_by = "Likert", add_total = T, add_mean = T, caption, caption2 = NULL, question = NULL, weight = T, binary = F, debug = F){
  for(i in 1:length(group)){
    if(!binary){
      # filter out NAs
      sublab <- lab[, c(var, group, "Weight")]
      validlab = sublab[complete.cases(sublab), ]
      if(nrow(validlab) == 0) {
        next
      }
      # If the group has more than 2 levels, anova, else T test. Store the significance indicator
      if(length(unique(validlab[, group[i]])) > 2){
        sig = our_anova_v23_2(num, lab, var, group[i], ph = F, weight = weight)$aov$Sig
      } else if(length(unique(lab[, group[i]])) > 1) {
        sig = our_ttest_v23(num, lab, var, group[i], weight = weight)$ttest$Sig
      } else {
        # Elaborate cat statement to say this question for this group is redacted
        cat('<div style="text-align: center;">', sep = "")
        if(!is.null(question)){
          if(!is.null(caption2)){
            cat(paste0("**", caption, " ", question, caption2, " <br> ** \n"))
          } else {
            cat(paste0("**", caption, " ", question, " <br> ** \n"))
          }
        } else {
          cat(paste0("**", caption, " <br> ** \n"))
        }
        #a = lab[complete.cases(lab[, var]), ]
        cat(paste0("**All respondents belongs to the group: ", unique(validlab[, group[i]]), ", See Overall Ratings table**"))
        cat('</div>', sep = "")
        cat("&nbsp;")
        cat("\n")
        sig = "Insig"
      }
    } else {
      # Binary questions, we print all the tables, for other question the print is controlled by whether or not if the table is significant
      sig = "Significant"
    }
    if(sig == "Significant"){
      # Use the gen_table family to print likert and yes/no questions
      gtable = gen_table_group_dym(lab, num, 
                                   var = var ,
                                   group = group[i], 
                                   newname = "Ratings", 
                                   weight = weight, add_total=add_total, 
                                   add_mean = add_mean, sort_by = sort_by)
      # If all non-header cells are empty (i.e. redacted) 
      # need to trim whitespace and must use stringi's stri_empty
      # Stringr and base-r seems to really struggle with finding blank cells
      if(all(sapply(trim(gtable[, 2:ncol(gtable)]), stringi::stri_isempty))){
        cat('<div style="text-align: center;">', sep = "")
        cat(paste0("**Table redacted due to insufficient number of respondents** \n"))
        cat('</div>', sep = "")
        cat("&nbsp;")
        cat("\n")
      } else {
        # force the group to be a factor so we can extract the levels in the correct order
        if(!is.factor(lab[, group[i]])){
          lab[, group[i]] <- as.factor(lab[, group[i]])
        }
        # format the column headers
        dname = paste(levels(lab[, group[i]]), "(%)")
        colnames(gtable)[2:ncol(gtable)] = dname
        # generate another table, this time with only the percentages to be used by the stack bar chart
        gtable2 = gen_table_group_dym(lab, num, 
                                      var = var ,
                                      group = group[i], 
                                      newname = "Ratings", 
                                      weight = weight, add_total=add_total, 
                                      add_mean = add_mean, sort_by = sort_by, 
                                      percent = T, NP = F)
        dname2 = levels(lab[, group[i]])
        colnames(gtable2)[2:ncol(gtable2)] = dname2
        figHeight = length(dname2)
        # chunk fig height settings
        if(figHeight == 2){
          figHeight = figHeight + 1
        } else if(figHeight == 3){
          figHeight = figHeight + 0.8
        } else if(figHeight > 10){
          figHeight = figHeight - 1
        } else if(figHeight == 4){
          figHeight = figHeight + 0.5
        } 
        # other chunk settings and text formatting
        figCName = paste0(var, group[i])
        replacementV = c("Table: " = "", ", " = "")
        questionAlt = gsub("\\/", " or ", question)
        captionAlt = gsub("<br>", " ", caption)
        if(!is.null(question)){
          figAlt = paste0("A group of stacked bar charts comparing the answers of different ", group[i] ," to the question: ",
                          stringr::str_replace_all(captionAlt, replacementV), " ", questionAlt)
          figTitle = paste0(gsub("Table: ", "", captionAlt), question, ", grouped by ", group[i])
        } else {
          figAlt = paste0("A group of stacked bar charts comparing the answers of different ", group[i] ," to the question: ",
                          stringr::str_replace_all(captionAlt, replacementV), " ", questionAlt)
          figTitle = paste0(gsub("Table: ", "", captionAlt), ", grouped by ", group[i])
        }
        # 6th group in the list is disability, needs a special text
        # other wise print the table depending on if it is a sub-matrix question or not
        # All the difference is just the text for the table header
        if(i == 6){
          if(!is.null(question)){
            if(!is.null(caption2)){
              print(print_com_kable(gtable, col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption, " ",
                                                     question, 
                                                     caption2, " <br> Rating by Self-Identified as Having a", group[i])))
            } else {
              print(print_com_kable(gtable,  col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption,
                                                     question, " <br> Rating by Self-Identified as Having a", group[i])))
            }
          } else {
            print(print_com_kable(gtable,  col1_width="4.5cm", 
                                  col_width="3cm", 
                                  caption = paste0(caption, " <br> Rating by Self-Identified as Having a", group[i])))
          }
          cat("\n")
          cat("&nbsp;")
          # this creates a chunk within chunk to print the stacked bar chart
          .pl <- print_stacked_bar_v23(gtable2, title = figTitle)
          kexpand(figCName, figHeight, figAlt)
          cat("&nbsp;")
        } else {
          if(!is.null(question)){
            if(!is.null(caption2)){
              print(print_com_kable(gtable,  col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption, " ",
                                                     question, 
                                                     caption2, " <br> Rating by ", group[i])))
            } else {
              print(print_com_kable(gtable,  col1_width="4.5cm", 
                                    col_width="3cm", 
                                    caption = paste0(caption,
                                                     question, " <br> Rating by ", group[i])))
            }
          } else {
            print(print_com_kable(gtable,  col1_width="4.5cm", 
                                  col_width="3cm", 
                                  caption = paste0(caption, " <br> Rating by ", group[i])))
          }
          cat("\n")
          cat("&nbsp;")
          .pl <- print_stacked_bar_v23(gtable2, title = figTitle)
          kexpand(figCName, figHeight, figAlt)
          cat("&nbsp;")
        }
        
      }
      if(debug){
        print(gtable)
        print(dname)
        print(sig)
        print(group[i])
        print(var)
        if(!is.null(question)){
          print(question)
        }
      }
      # clean up
      rm(gtable)
      rm(dname)
    }
    rm(sig)
    rm(subLab)
    rm(validLab)
  }
}