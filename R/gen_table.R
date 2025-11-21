#' Survey Table Generation Functions
#'
#' A comprehensive set of functions for generating frequency tables from survey data
#' with support for weighting, redaction, grouping, and various formatting options.
#'
#' @name gen_table
#' @docType package
NULL

#' Generate frequency table for individual variable
#'
#' Creates a frequency table for a single survey variable with options for sorting,
#' weighting, and adding summary statistics.
#'
#' @param data A labelled data frame containing survey data
#' @param var String, variable name of interest
#' @param newname String, new/shortened variable name for table ("Response" by default)
#' @param sort_by String, options for table sorting:
#'   \itemize{
#'     \item "Likert" (default) - sorts from highest (5) to lowest (1)
#'     \item "N" - sorts by descending N values (highest first)
#'     \item "A-Z" - sorts by alphabetical order
#'     \item "None" - keeps original order
#'   }
#' @param weight Boolean, whether to weight frequency table (default: FALSE)
#' @param add_total Boolean, whether to add total row to bottom of table (default: TRUE)
#' @param add_mean Boolean, whether to add mean row to bottom of table (default: TRUE)
#' @param redact Boolean, legacy argument (default: TRUE)
#' @param paren Boolean, whether to print percentages in parentheses (default: TRUE)
#'
#' @return A data frame containing the frequency table
#'
#' @examples
#' \dontrun{
#' # Basic frequency table
#' freq_table <- gen_table(survey_data, "satisfaction")
#' 
#' # With custom sorting and no mean
#' freq_table <- gen_table(survey_data, "satisfaction", 
#'                        sort_by = "N", add_mean = FALSE)
#' }
#'
#' @export
#' @importFrom dplyr %>% arrange desc sym filter select
#' @importFrom rlang sym
gen_table <- function(data, var, newname = "Response", sort_by = "Likert", 
                     weight = FALSE, add_total = TRUE, add_mean = TRUE, 
                     redact = TRUE, paren = TRUE) {
  
  # Get all results from get_a_lot
  all <- get_a_lot(data, var, weight = weight)
  
  # Generate frequency table with get_a_lot
  table <- all$table
  if (paren) {
    table$Percent <- paste0("(", paste0(sprintf('%.1f', round(table$Percent, digits = 1)), '%'), ")")
  } else {
    table$Percent <- paste0(sprintf('%.1f', round(table$Percent, digits = 1)), '%')
  }
  
  # Apply sorting
  if (sort_by == "None") {
    message("Not sorting table")
  } else if (sort_by == "N") {
    message("Sorting table by descending N")
    table <- table %>%
      dplyr::arrange(desc(N))
  } else if (sort_by == "A-Z") {
    message("Sorting table by alphabetical order")
    table[, 1] <- as.character(table[, 1])
    table <- table %>%
      dplyr::arrange(!!sym(var))
  } else if (sort_by == "Likert") {
    message("Sorting table by descending Likert scale (highest to lowest)")
    table <- table %>%
      dplyr::arrange(desc(!!sym(var)))
  }
  
  # Add total row if requested
  if (add_total) {
    if (is.factor(table[[1]])) {
      table[[1]] <- as.character(table[[1]])
    }
    
    table1_N <- all$table_N
    if (paren) {
      table1_N$Percent <- paste0("(", paste0(sprintf('%.1f', round(table1_N$Percent, digits = 1)), '%'), ")")
    } else {
      table1_N$Percent <- paste0(paste0(sprintf('%.1f', round(table1_N$Percent, digits = 1)), '%'))
    }
    
    table[nrow(table) + 1, ] <- table1_N[nrow(table1_N), ]
  }
  
  table$N <- round(table$N, 0)
  
  # Add mean row if requested
  if (add_mean) {
    if (is.factor(table[[1]])) {
      table[[1]] <- as.character(table[[1]])
    }
    
    table_mn <- all$mean
    table[nrow(table) + 1, 1] <- "Mean"
    table[nrow(table), 2] <- sprintf('%.2f', table_mn, digits = 2)
  }
  
  # Set custom question name
  colnames(table)[1] <- newname
  
  # Clean up NA values in final rows
  if (add_mean | add_total) {
    jtail <- table[nrow(table), ]
    jtail[is.na(jtail)] <- ""
    jtail <- sub("NA", "", jtail)
    table[nrow(table), ] <- jtail
  }
  
  return(table)
}

#' Generate grouped frequency tables
#'
#' Splits responses for a question into groups and reports each group level 
#' in a separate column.
#'
#' @param datalab A labelled data frame
#' @param datanum Numeric data frame (legacy parameter, leave as NULL)
#' @param var String, variable of interest
#' @param group String, grouping variable for splitting analyses
#' @param lvl String, legacy parameter (leave as NULL)
#' @param newname String, new/shortened variable name for table ("Response" by default)
#' @param sort_by String, options for table sorting (see \code{gen_table})
#' @param weight Boolean, whether to weight frequency table (default: FALSE)
#' @param add_total Boolean, whether to add total row (default: FALSE)
#' @param add_mean Boolean, whether to add mean row (default: FALSE)
#' @param redact Boolean, whether to redact low counts (default: TRUE)
#' @param cutoff Integer, cutoff value for redaction (default: 5)
#' @param percent Boolean, whether to return percent-only table (default: FALSE)
#' @param NP Boolean, whether to use N (%) format (default: TRUE)
#'
#' @return A data frame with grouped frequency tables
#'
#' @examples
#' \dontrun{
#' # Generate grouped table by gender
#' grouped_table <- gen_table_group_dym(survey_data, var = "satisfaction", 
#'                                     group = "gender")
#' }
#'
#' @export
#' @importFrom dplyr left_join select
#' @importFrom purrr reduce
gen_table_group_dym <- function(datalab, datanum = NULL, var, group, lvl = NULL, 
                               newname = "Response", sort_by = "Likert", weight = FALSE, 
                               add_total = FALSE, add_mean = FALSE, redact = TRUE, 
                               cutoff = 5, percent = FALSE, NP = TRUE) {
  
  # Get group levels
  if (is.factor(datalab[, group])) {
    grp_names <- levels(datalab[, group])
  } else {
    grp_names <- unique(datalab[, group])
  }
  
  if (is.null(lvl)) {
    # Split data by groups
    datalist <- split(datalab, datalab[, group])
  }
  
  # Generate tables for each group
  tablelist <- lapply(datalist, gen_table, var = var, sort_by = sort_by, 
                     weight = weight, add_total = add_total, add_mean = add_mean, 
                     paren = NP)
  
  if (length(tablelist) < 2) {
    stop("ERROR: Less than two group levels")
  }
  
  # Apply redaction logic
  if (redact) {
    if ((add_mean == TRUE) & (add_total == TRUE)) {
      Ns <- lapply(tablelist, "[", 1:(nrow(tablelist[[1]]) - 2), "N")
      totals <- lapply(tablelist, "[", (nrow(tablelist[[1]]) - 1), "N")
    } else if (((add_mean == FALSE) & (add_total == TRUE)) | ((add_mean == TRUE) & (add_total == FALSE))) {
      Ns <- lapply(tablelist, "[", 1:(nrow(tablelist[[1]]) - 1), "N")
      if (add_total) {
        totals <- lapply(tablelist, "[", (nrow(tablelist[[1]])), "N")
      }
    } else {
      Ns <- lapply(tablelist, "[", 1:(nrow(tablelist[[1]])), "N")
    }
    
    Ns <- as.data.frame(do.call(cbind, Ns), stringsAsFactors = FALSE)
    Ns <- as.data.frame(lapply(Ns, as.character), stringsAsFactors = FALSE)
    Ns <- as.data.frame(lapply(Ns, as.numeric), stringsAsFactors = FALSE)
    redactidx <- which(totals < cutoff | colSums(Ns) < cutoff)
    
    if (length(redactidx) > 0) {
      for (idx in redactidx) {
        tablelist[idx][[1]]$N <- ""
        tablelist[idx][[1]]$Percent <- ""
      }
    }
  }
  
  # Handle percent-only tables
  if (percent & NP) {
    for (i in 1:length(tablelist)) {
      tablelist[i][[1]]$N <- ""
    }
  } else if (percent & !NP) {
    for (i in 1:length(tablelist)) {
      tablelist[i][[1]] <- tablelist[i][[1]] %>%
        select(Response, Percent)
    }
  }
  
  # Format tables with N (%) format
  if (NP) {
    for (i in 1:length(tablelist)) {
      if (all(grepl("^$", tablelist[[i]]$Percent))) {
        tablelist[[i]]$NP <- tablelist[[i]]$N
      } else {
        tablelist[[i]]$NP <- paste(tablelist[[i]]$N, tablelist[[i]]$Percent)
      }
      tablelist[[i]] <- tablelist[[i]][, c("Response", "NP")]
    }
  }
  
  # Join all tables
  joined <- reduce(tablelist, left_join, by = "Response")
  names(joined) <- gsub('\\..*', '', colnames(joined))
  names(joined)[1] <- newname
  
  # Clean up final row
  jtail <- joined[nrow(joined), ]
  jtail[is.na(jtail)] <- ""
  jtail <- sub("NA", "", jtail)
  joined[nrow(joined), ] <- jtail
  
  return(joined)
}

#' Bind multiple variables as columns for stacked bar charts
#'
#' Creates a frequency table with multiple variables as columns, specifically
#' formatted for stacked bar chart visualization. This is a specialized version
#' of bind_col_v23 that prepares data for plotting.
#'
#' @param data A data frame containing survey data
#' @param varlist Character vector of variable names to include
#' @param redact Boolean, whether to redact low counts (default: TRUE)
#' @param percent Boolean, returns percent only (default: TRUE)
#' @param weight Boolean, whether to apply weights (default: FALSE)
#' @param cutoff Integer, redaction cutoff value (default: 5)
#'
#' @return A joined data frame optimized for stacked bar charts
#'
#' @examples
#' \dontrun{
#' # Prepare data for stacked bar chart
#' chart_data <- bind_col_vars(data, c("var1", "var2", "var3"))
#' }
#'
#' @export
bind_col_vars <- function(data, varlist, redact = TRUE, percent = TRUE, 
                         weight = FALSE, cutoff = 5) {
  
  # Get separate tables for each variable
  tbls <- list()
  
  for (v in varlist) {
    all <- get_a_lot(data, v, weight = weight)
    table <- all$table
    tablen <- all$table_N$N[nrow(all$table_N)]
    table$Percent <- paste0(sprintf('%.1f', round(table$Percent, digits = 1)), '%')
    table$N <- round(table$N, 0)
    colnames(table) <- c("Answer", "N", "Percent")
    
    # Apply redaction
    if (redact) {
      Ns <- sum(round(table$N))
      if (Ns < cutoff | round(tablen) < cutoff) {
        table$N <- rep(NA, length(table$N))
        table$Percent <- rep(NA, length(table$Percent))
      }
    }
    
    # Keep only percent if requested
    if (percent) {
      table <- table[-2]
    }
    
    tbls[[v]] <- table
  }
  
  # Join all tables
  joined <- reduce(tbls, left_join, by = "Answer")
  
  return(joined)
}

#' Bind multiple variables as rows with grouping
#'
#' Creates a frequency table with multiple variables as rows, split by a grouping
#' variable. This is typically used for check-all-that-apply questions analyzed
#' by demographic groups.
#'
#' @param data A data frame containing survey data
#' @param str String, prefix of variable set (NULL default)
#' @param varlist Character vector of variable names (NULL default)
#' @param group String, variable name for grouping (e.g., "gender")
#' @param newname String, new/shortened variable name ("Response" by default)
#' @param weight Boolean, whether to weight frequency table (default: FALSE)
#' @param sort_by String, table sorting options:
#'   \itemize{
#'     \item "none" - keeps original order (default)
#'     \item "N" - sorts by descending N values
#'     \item "A-Z" - sorts alphabetically
#'   }
#' @param add_total Boolean, whether to add total row (default: FALSE)
#' @param redact Boolean, whether to redact low counts (default: TRUE)
#' @param cutoff Integer, redaction cutoff value (default: 5)
#'
#' @return A joined data frame with variables as rows, split by groups
#'
#' @examples
#' \dontrun{
#' # Check-all questions grouped by gender
#' grouped_check <- bind_row_group_v23_2(data, str = "services_", 
#'                                      group = "gender")
#' 
#' # With explicit variable list
#' vars <- c("services_1", "services_2", "services_3")
#' grouped_check <- bind_row_group_v23_2(data, varlist = vars, 
#'                                      group = "age_group")
#' }
#'
#' @export
#' @importFrom sjlabelled get_label
#' @importFrom stringr str_remove_all
bind_row_group_v23_2 <- function(data, str = NULL, varlist = NULL, group, 
                                newname = "Response", weight = FALSE, 
                                sort_by = "none", add_total = FALSE, 
                                redact = TRUE, cutoff = 5) {
  
  # Get levels and names of grouping variable
  if (is.factor(data[, group])) {
    grp_levels <- length(levels(data[, group]))
    grp_names <- levels(data[, group])
  } else {
    grp_levels <- length(unique(data[, group]))
    grp_names <- unique(data[, group])
  }
  
  # Split the dataframe into group dataframes
  datalist <- split(data, data[, group])
  
  # Apply bind_row to each sub-dataframe
  tablelist <- lapply(datalist, bind_row, str = str, varlist = varlist, 
                     newname = "Response", weight = weight, sort_by = sort_by, 
                     add_total = add_total, redact = redact, cutoff = cutoff)
  
  # Join all the tables
  joined <- reduce(tablelist, left_join, by = "Response")
  
  # Replace question variable names with actual question text
  if (add_total) {
    for (i in 1:(nrow(joined) - 1)) {
      string <- joined[i, 1]
      q <- get_label(data[, string])
      q <- gsub(".*: ", "", q)
      q <- gsub(".*\\? ", "", q)
      joined[i, 1] <- q
    }
  } else {
    for (i in 1:(nrow(joined))) {
      string <- joined[i, 1]
      q <- get_label(data[, string])
      q <- gsub(".*: ", "", q)
      q <- gsub(".*\\? ", "", q)
      joined[i, 1] <- q
    }
  }
  
  # Clean up column names (remove suffixes from joins)
  names(joined) <- str_remove_all(names(joined), "\\.[a-z]")
  names(joined)[1] <- newname
  
  # Ensure grouping variable is factor
  if (!is.factor(data[, group])) {
    data[, group] <- as.factor(data[, group])
  }
  
  # Format column names with group labels
  dname <- paste(levels(data[, group]), "(%)")
  colnames(joined)[2:ncol(joined)] <- dname
  
  return(joined)
}

#' Bind multiple variables as columns in frequency table
#'
#' Creates a frequency table with multiple variables as columns, typically used
#' for matrix questions where each variable represents a sub-question.
#'
#' @param data A data frame containing survey data
#' @param varlist Character vector of variable names to include
#' @param weight Boolean, whether to apply weights (default: TRUE)
#' @param newname String, text for top-left cell ("Response" by default)
#' @param add_mean Boolean, add mean row at bottom (default: TRUE)
#' @param add_total Boolean, add total row at bottom (default: TRUE)
#' @param sort_by String, sorting method:
#'   \itemize{
#'     \item "None" - data source order (default)
#'     \item "N" - sort by N values
#'     \item "A-Z" - sort by choice text
#'     \item "Likert" - 5-1 Likert scale order
#'   }
#' @param redact Boolean, redact low response counts (default: TRUE)
#' @param cutoff Integer, redaction cutoff value (default: 5)
#' @param percent Boolean, show percentages only (default: FALSE)
#' @param qs Character vector, full question text for column headers
#'
#' @return A joined data frame with variables as columns
#'
#' @examples
#' \dontrun{
#' # Matrix question with multiple items
#' matrix_vars <- c("satisfaction_1", "satisfaction_2", "satisfaction_3")
#' questions <- c("Overall quality", "Staff helpfulness", "Timeliness")
#' 
#' matrix_table <- bind_col_v23(data, matrix_vars, qs = questions)
#' }
#'
#' @export
bind_col_v23 <- function(data, varlist, weight = TRUE, newname = "Response", 
                        add_mean = TRUE, add_total = TRUE, sort_by = "None", 
                        redact = TRUE, cutoff = 5, percent = FALSE, qs) {
  
  # Get separate tables for each variable
  tbls <- list()
  totalsvec <- vector()
  
  for (v in varlist) {
    all <- get_a_lot(data, v, weight = weight)
    table <- all$table
    table$Percent <- paste0("(", paste0(sprintf('%.1f', round(table$Percent, digits = 1)), '%'), ")")
    
    # Apply sorting
    if (sort_by == "None") {
      message("Not sorting table")
    } else if (sort_by == "N") {
      message("Sorting table by descending N")
      table <- table %>%
        dplyr::arrange(desc(N))
    } else if (sort_by == "A-Z") {
      message("Sorting table by alphabetical order")
      table[, 1] <- as.character(table[, 1])
      table <- table %>%
        dplyr::arrange(!!sym(v))
    } else if (sort_by == "Likert") {
      message("Sorting table by descending Likert scale (highest to lowest)")
      table <- table %>%
        dplyr::arrange(desc(!!sym(v)))
    }
    
    # Add total row if requested
    if (add_total) {
      if (is.factor(table[[1]])) {
        table[[1]] <- as.character(table[[1]])
      }
      
      table1_N <- all$table_N
      table1_N$Percent <- paste0("(", paste0(sprintf('%.1f', round(table1_N$Percent, digits = 1)), '%'), ")")
      table[nrow(table) + 1, ] <- table1_N[nrow(table1_N), ]
      totalsvec <- c(totalsvec, round(table1_N$N[nrow(table1_N)]))
    }
    
    table$N <- round(table$N, 0)
    
    # Add mean row if requested
    if (add_mean) {
      if (is.factor(table[[1]])) {
        table[[1]] <- as.character(table[[1]])
      }
      
      table_mn <- all$mean
      table[nrow(table) + 1, 1] <- "Mean"
      table[nrow(table), 2] <- sprintf('%.2f', table_mn, digits = 2)
      table[nrow(table), 3] <- ""
    }
    
    colnames(table) <- c("Answer", "N", "Percent")
    tbls[[v]] <- table
  }
  
  # Apply redaction
  if (redact) {
    if ((add_mean) & (add_total)) {
      Ns <- lapply(tbls, "[", 1:(nrow(tbls[[1]]) - 2), "N")
    } else if ((!add_mean) & (!add_total)) {
      Ns <- lapply(tbls, "[", 1:(nrow(tbls[[1]])), "N")
    } else {
      Ns <- lapply(tbls, "[", 1:(nrow(tbls[[1]]) - 1), "N")
    }
    
    Ns <- as.data.frame(do.call(cbind, Ns), stringsAsFactors = FALSE)
    Ns <- as.data.frame(lapply(Ns, as.character), stringsAsFactors = FALSE)
    Ns <- as.data.frame(lapply(Ns, as.numeric), stringsAsFactors = FALSE)
    redactidx <- which(totalsvec < cutoff | colSums(Ns) < cutoff)
    
    if (length(redactidx) > 0) {
      for (idx in redactidx) {
        tbls[idx][[1]]$N <- ""
        tbls[idx][[1]]$Percent <- ""
      }
    }
  }
  
  # Handle percent-only option
  if (percent) {
    for (i in 1:length(tbls)) {
      tbls[i][[1]]$N <- ""
    }
  }
  
  # Combine N and Percent columns
  for (i in 1:length(tbls)) {
    tbls[[i]]$NP <- paste(tbls[[i]]$N, tbls[[i]]$Percent)
    tbls[[i]] <- tbls[[i]][, c("Answer", "NP")]
  }
  
  # Join all tables
  joined <- reduce(tbls, left_join, by = "Answer")
  joined <- joined %>%
    replace(is.na(.), "")
  
  # Set column names
  names(joined) <- gsub('\\..*', '', colnames(joined))
  names(joined)[1] <- newname
  names(joined)[2:ncol(joined)] <- qs[1:length(qs)]
  
  return(joined)
}

#' Bind multiple variables as rows in frequency table
#'
#' Creates a frequency table with multiple variables as rows, typically used
#' for check-all-that-apply questions.
#'
#' @param data A data frame containing survey data
#' @param str String, prefix of variable set (NULL default)
#' @param varlist Character vector of variable names (NULL default)
#' @param newname String, new/shortened variable name ("Response" by default)
#' @param weight Boolean, whether to weight frequency table (default: FALSE)
#' @param sort_by String, table sorting options:
#'   \itemize{
#'     \item "none" - keeps original order (default)
#'     \item "N" - sorts by descending N values
#'     \item "A-Z" - sorts alphabetically
#'   }
#' @param add_total Boolean, whether to add total row (default: FALSE)
#' @param redact Boolean, whether to redact low counts (default: TRUE)
#' @param cutoff Integer, redaction cutoff value (default: 5)
#'
#' @return A joined data frame with variables as rows
#'
#' @examples
#' \dontrun{
#' # Check-all questions with string prefix
#' check_table <- bind_row(data, str = "services_used_")
#' 
#' # Or with explicit variable list
#' vars <- c("services_used_1", "services_used_2", "services_used_3")
#' check_table <- bind_row(data, varlist = vars)
#' }
#'
#' @export
#' @importFrom tibble rownames_to_column
bind_row <- function(data, str = NULL, varlist = NULL, newname = "Response", 
                    weight = FALSE, sort_by = "none", add_total = FALSE, 
                    redact = TRUE, cutoff = 5) {
  
  # Extract variable list from string prefix if not provided
  if (is.null(varlist)) {
    col_name <- grepl(str, names(data))
    varlist <- names(data)[col_name]
    
    # Remove write-in responses
    varlist <- varlist[grep("OtherSpec$", varlist, invert = TRUE)]
    varlist <- varlist[grep("Write$", varlist, invert = TRUE)]
    varlist <- varlist[grep("WriteIn$", varlist, invert = TRUE)]
  }
  
  tbls <- list()
  tbl_N <- vector()
  
  # Process each variable
  for (v in varlist) {
    # Get frequency table for "checked" responses only
    tbl <- get_a_lot(data, v, weight = weight)$table[2, ]
    tbl_N[v] <- get_a_lot(data, v, weight = weight)$N
    
    # Handle cases with no checked responses
    if (is.na(tbl$N)) {
      tbl$N <- 0
      tbl$Percent <- 0
    }
    
    tbl$N <- round(tbl$N, 0)
    tbl$Percent <- round(tbl$Percent, 1)
    colnames(tbl) <- c("Response", "N", "Percent")
    
    # Get question label
    q <- attributes(data[[v]])$label
    q <- gsub(".*: ", "", q)
    q <- gsub(".*\\? ", "", q)
    
    # Add to table list
    if (length(q) > 0) {
      rownames(tbl) <- q
      tbls[[q]] <- tbl
    } else {
      rownames(tbl) <- v
      tbls[[v]] <- tbl
    }
  }
  
  # Combine all tables
  joined <- do.call("rbind", tbls)[, -1]
  joined <- tibble::rownames_to_column(joined, "Response")
  
  # Apply sorting
  if (sort_by == "N") {
    message("Sorting table by descending N")
    joined <- joined %>%
      dplyr::arrange(desc(N))
  } else if (sort_by == "A-Z") {
    message("Sorting table by alphabetical order")
    joined <- joined %>%
      dplyr::arrange(Response)
  } else {
    message("NOT sorting table")
  }
  
  # Apply redaction
  if (redact) {
    redactRow <- which(joined$N > 0 & joined$N < cutoff & as.numeric(joined$Percent) > 0)
    if (length(redactRow) > 0) {
      joined$N <- round(joined$N, 0)
      joined$Percent <- paste0("(", paste0(sprintf('%.1f', joined$Percent, digits = 1), "%"), ")")
      joined[redactRow, "N"] <- ""
      joined[redactRow, "Percent"] <- ""
    }
  } else {
    joined$N <- round(joined$N, 0)
    joined$Percent <- paste0("(", paste0(sprintf('%.1f', joined$Percent, digits = 1), "%"), ")")
  }
  
  # Format output
  if (!is.character(joined$N) & !is.character(joined$Percent)) {
    joined$N <- round(joined$N, 0)
    if (sum(joined$N) > 0) {
      joined$Percent <- paste0("(", paste0(sprintf('%.1f', joined$Percent, digits = 1), "%"), ")")
    } else {
      joined$Percent <- rep(NA, length(joined$Percent))
    }
  }
  
  # Add total row if requested
  if (add_total) {
    total <- sum(joined[, 2])
    if (is.factor(joined[, 1])) {
      joined[, 1] <- as.character(joined[, 1])
    }
    joined[nrow(joined) + 1, ] <- c("Total N", total, "")
    joined$N <- as.numeric(joined$N)
  }
  
  # Clean up NaN values
  joined[joined == "NaN%"] <- "(0.0%)"
  joined[joined == "NaN"] <- "(0.0%)"
  
  # Create combined N and Percent column
  if (all(is.na(joined$Percent))) {
    joined$NP <- joined$N
  } else {
    joined$NP <- paste(joined$N, joined$Percent)
  }
  
  joined <- joined[, c("Response", "NP")]
  colnames(joined)[1] <- newname
  colnames(joined)[2] <- "Checked"
  
  return(joined)
}