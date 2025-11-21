#' Print Combined Kable Table
#'
#' Prints combined kable table (i.e. combined course and college data) with
#' customizable formatting for both HTML and LaTeX output.
#'
#' @param tab Data frame, table to print (should be combined)
#' @param colnames Named vector, column names for header
#' @param newname String, new name for first column of table
#' @param caption String, caption to add to kable
#' @param col1_width String, width of first column in kable table (I found 3cm works better than 1in)
#' @param col_width String, width of all other columns in kable table (4.5cm is better)
#' @param add_footnote String, footnote to add below table (SHOULD BE AVOIDED, BREAKS ACCESSIBILITY)
#' @param add_mean Logical, whether or not to add mean row
#' @param add_total Logical, whether or not to add total row
#' @param lt Logical, whether to use longtable format for LaTeX
#'
#' @return A formatted kable table
#'
#' @export
#' @importFrom kableExtra kable_styling add_header_above column_spec row_spec scroll_box footnote
#' @importFrom knitr kable is_html_output is_latex_output
#'
#' @examples
#' \dontrun{
#' print_com_kable(my_table, caption = "Survey Results")
#' }
print_com_kable = function(tab,colnames=NULL,newname=NULL,caption=NULL,
                           col1_width="1in",col_width="2in",
                           add_footnote=NULL, add_mean=FALSE, add_total = F,
                           lt = T) {
  
  # tab: data frame, table to print (should be combined)
  # colnames: named vector, column names for header
  # newname: string, new name for first column of table
  # caption: string, caption to add to kable
  # col1_width: string, width of first column in kable table (I found 3cm works better than 1in)
  # col_width: string, width of all other columns in kable table (4.5cm is better)
  # add_footnote: string, footnote to add below table (SHOULD BE AVOIDED, BREAKS ACCESSIBILITY)
  
  
  # FIRST IF STATEMENT!
  if (add_mean==TRUE){
  # get table name to use as table reference label
    tab_label = deparse(substitute(tab))
  
    if (!is.null(newname)){
      # assign new name to first column
      names(tab)[1] = newname
      
    }
    
    # determine how many columns are in table (minus response/question column)
    ncols = ncol(tab)-1
    meanRow= nrow(tab)-1
    TotalRow=nrow(tab)-2
    if(add_total){
      tab[TotalRow, 1] = "Weighted Total"
    }
    
    rownames(tab) = NULL
    
    if(knitr::is_html_output()==TRUE){
      # if there are more than 8 question columns, we use a scroll-box
      if(ncols >= 8) {
        if(is.null(add_footnote)) {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
            add_header_above(header=colnames, bold=T) %>%
            column_spec(1, width = "1in")%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            row_spec(TotalRow, hline_after = T) %>%
            row_spec(meanRow, hline_after = T) %>%
            scroll_box(width = "100%", fixed_thead = T)
        } else {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
            add_header_above(header=colnames, bold=T)%>%
            footnote(general=add_footnote)%>%
            column_spec(1, width = col1_width)%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            row_spec(TotalRow, hline_after = T) %>%
            row_spec(meanRow, hline_after = T) %>%
            scroll_box(width = "100%", fixed_thead = T)
        }
      } else {
        if (is.null(add_footnote)) {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped","hover"), full_width = F) %>%
            add_header_above(header=colnames, bold=T) %>%
            column_spec(1, width = col1_width)%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            row_spec(TotalRow, hline_after = T) %>%
            row_spec(meanRow, hline_after = T)
          
        } else {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped","hover"), full_width = F) %>%
            add_header_above(header=colnames, bold=T)%>%
            column_spec(1, width = col1_width)%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            footnote(general=add_footnote)%>%
            row_spec(TotalRow, hline_after = T) %>%
            row_spec(meanRow, hline_after = T)
        }
      }
    # this is for pdf output
    } else if (knitr::is_latex_output()==TRUE){
      if (nrow(tab) > 6) {
        # using print here requires chunk option to be results='asis'
        # print ensures that table reference can be rendered in latex!!
        print(
        kable(tab, format="latex",digits=1, booktabs=T,longtable=T, align=c('l',rep('r',ncols)),linesep="",
              caption=ifelse(!is.null(caption),caption,NA),
              label=tab_label) %>%
          kable_styling(latex_options=c("HOLD_position","repeat_header"),position="center") %>%
          column_spec(1, width=col1_width) %>%
          column_spec(2:ncols,width=col_width) %>%
          add_header_above(header=colnames, bold=T) %>%
          footnote(general=add_footnote, threeparttable=TRUE)%>%
          row_spec(TotalRow, hline_after = T) %>%
          row_spec(meanRow, hline_after = T)
       
        )
     
      } else {
        # using print here requires chunk option to be results='asis'
        # print ensures that table reference can be rendered in latex!!
        print(
        kable(tab, format="latex",digits=1, booktabs=T,align=c('l',rep('r',ncols)),linesep="",
              caption=ifelse(!is.null(caption),caption,NA),
              label=tab_label) %>%
          kable_styling(latex_options="HOLD_position",position="center") %>%
          column_spec(1, width=col1_width) %>%
          column_spec(2:ncols,width=col_width) %>%
          add_header_above(header=colnames, bold=T) %>%
          footnote(general=add_footnote, threeparttable=TRUE)%>%
          row_spec(TotalRow, hline_after = T) %>%
          row_spec(meanRow, hline_after = T)
        
        )
  
      }
    }
    
    
    ##### ALL OF THE FREAKING ABOVE IS UNDER THE ADD MEAN IF-STATEMENT INDENT YOUR FREAKING CODES!!!!!!!!!!!!! #####
    ###### >:( #####
    
    # IF addmean = F
  } else {
    # get table name to use as table reference label
    tab_label = deparse(substitute(tab))

    if (!is.null(newname)){
      # assign new name to first column
      names(tab)[1] = newname

    }

    # determine how many columns are in table (minus response/question column)
    ncols = ncol(tab)-1
    TotalRow=nrow(tab)-1
    if(add_total){
      tab[TotalRow, 1] = "Weighted Total"
    }
    
    rownames(tab) = NULL

    if(knitr::is_html_output()==TRUE){
      # if the number of columns is greater than 10, print as horizontally scrollable table
      if(ncols >= 8){
        if (is.null(add_footnote)) {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped","hover"), full_width = F, fixed_thead = T) %>%
            add_header_above(header=colnames, bold=T) %>%
            column_spec(1, width = col1_width)%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            row_spec(TotalRow, hline_after = T) %>%
            scroll_box(width = "100%", fixed_thead = F)
          
        } else {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped","hover"), full_width = F, fixed_thead = T) %>%
            add_header_above(header=colnames, bold=T)%>%
            column_spec(1, width = col1_width)%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            footnote(general=add_footnote)%>%
            row_spec(TotalRow, hline_after = T) %>%
            scroll_box(width = "100%", fixed_thead = F)
        }
      } else { # standard print
        if (is.null(add_footnote)) {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped","hover"), full_width = F) %>%
            add_header_above(header=colnames, bold=T) %>%
            column_spec(1, width = col1_width)%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            row_spec(TotalRow, hline_after = T)
          
        } else {
          kable(tab, digits=1, escape=FALSE, align=c('l',rep('r',ncols)),"html",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(bootstrap_options = c("striped","hover"), full_width = F) %>%
            add_header_above(header=colnames, bold=T)%>%
            footnote(general=add_footnote)%>%
            column_spec(1, width = col1_width)%>%
            column_spec(2:ncol(tab), width = col_width) %>%
            row_spec(TotalRow, hline_after = T) 
        }
      }
    } else if (knitr::is_latex_output()==TRUE){

      if (nrow(tab) > 6) {
        # using print here requires chunk option to be results='asis'
        # print ensures that table reference can be rendered in latex!!
        print(
          kable(tab, format="latex",digits=1, booktabs=T,longtable=lt, align=c('l',rep('r',ncols)),linesep="",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(latex_options=c("HOLD_position","repeat_header","scale_down"),position="center") %>%
            column_spec(1, width=col1_width) %>%
            column_spec(2:ncols,width=col_width) %>%
            add_header_above(header=colnames, bold=T) %>%
            footnote(general=add_footnote, threeparttable=TRUE)%>%
            row_spec(TotalRow, hline_after = T)
        )

      } else {
        # using print here requires chunk option to be results='asis'
        # print ensures that table reference can be rendered in latex!!
        print(
          kable(tab, format="latex",digits=1, booktabs=T,align=c('l',rep('r',ncols)),linesep="",
                caption=ifelse(!is.null(caption),caption,NA),
                label=tab_label) %>%
            kable_styling(latex_options=c("HOLD_position","scale_down"),position="center") %>%
            column_spec(1, width=col1_width) %>%
            column_spec(2:ncols,width=col_width) %>%
            add_header_above(header=colnames, bold=T) %>%
            footnote(general=add_footnote, threeparttable=TRUE)%>%
            row_spec(TotalRow, hline_after = T)
        )
      }
    }
  }
}

#' Print Descriptive Statistics Table
#'
#' Function to print the result of get_w_desc_stats or something similar.
#' It is very basic formatting for descriptive statistics tables.
#'
#' @param tab Data frame containing descriptive statistics
#' @param caption String, caption for the table (default: NULL)
#' @param footnote String, footnote text to add below table (default: NULL)
#'
#' @return A formatted kable table with descriptive statistics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' print_desc_stats(stats_table, caption = "Sample Statistics")
#' }
print_desc_stats <- function(tab, caption = NULL, footnote = NULL) {
  ncols = ncol(tab)
  if(knitr::is_html_output()==TRUE){
    kb = kable(tab, escape=FALSE, align=c('l',rep('r',ncol(tab))),"html", 
               caption = ifelse(!is.null(caption),caption,NA)) %>%
      kable_styling(bootstrap_options = c("striped","hover"), full_width = F) %>%
      column_spec(2:ncol(tab), width = "1in") %>%
      footnote(general=footnote)
  } else {
    kb = print_kable = kable(tab, caption=ifelse(!is.null(caption),caption,NA),
                             format="latex",booktabs=T,
                             longtable = F, align=c("l",rep('r',ncol(tab))),linesep = "") %>% 
      kable_styling(latex_options=c("HOLD_position"),position="center",full_width = F) %>%
      row_spec(0, bold = T)
  }
  print(kb)
}

#' Print Simple Kable Table
#'
#' Prints a simple frequency table with customizable formatting for both
#' HTML and LaTeX output. Handles mean and total rows appropriately.
#'
#' @param tab Data frame, simple frequency table
#' @param newname String, new name for first column of table (default: NULL)
#' @param caption String, caption to include with table (default: NULL)
#' @param col1_width String, column 1 width for kable table (default: "7cm")
#' @param col_width String, column width for other columns (default: "2cm")
#' @param latex_label String, name to use for latex table reference (default: NULL).
#'   To be used if tab name is generic and custom latex references are required
#' @param add_footnote String, prints footnote if not NULL (default: NULL)
#'
#' @return A formatted kable table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' print_simple_kable(freq_table, newname = "Response", caption = "Frequency Table")
#' }
print_simple_kable = function(tab,newname=NULL,caption=NULL,col1_width="7cm",col_width="2cm",
                              latex_label=NULL,add_footnote=NULL) {
  
  # table: R data frame, simple frequency table
  # newname: string, new name for first column of table
  # caption: string, caption to include with table
  # col1_width: string, column 1 width for kable table
  # col_width: string, column width for other columns
  # latex_label: string, name to use for latex table reference (to be used if tab name is generic and custom latex references are required)
  # add_footnote: string, prints footnote if not NULL
  
  
  if (is.null(latex_label)) {
    # get table name to use as table reference label
    tab_label = deparse(substitute(tab))
    if (grepl("[[i]]",tab_label)) {
      # fix tab_label to be variable name if in for loop
      name = gsub(".*: ", "", caption)
      tab_label = gsub("[[:space:]]","",name)
      tab_label = strtrim(tolower(gsub("[[:punct:]]","",tab_label)),30)
    }
  } else {
    tab_label = latex_label
  }
  
  
  # determine if mean or total rows exist
  total_row = ifelse(any(grepl("Total",tab[,1],ignore.case=T)),TRUE,FALSE)
  if(total_row){
    tab[which(grepl("Total",tab[,1])),1] = "Weighted Total"
  }
  mean_row = ifelse(any(grepl("Mean",tab[,1],ignore.case=T)),TRUE,FALSE)
  
  if (!is.null(newname)){
    # assign new name to first column
    names(tab)[1] = newname
  }
  
  if(knitr::is_html_output()==TRUE){
      print_kable = kable(tab, align=c('l','r','r'),format="html",caption=ifelse(!is.null(caption),caption,NA),
            label=tab_label) %>%
        kable_styling(bootstrap_options = c("striped","hover"), full_width = F) %>%
        row_spec(0, bold = T) %>%
        column_spec(1, width = col1_width) %>%
        column_spec(2:3, width = col_width) %>%
        footnote(general=add_footnote)
      
      # add line before and italicize
      if (total_row | mean_row) {
        print_kable = print_kable %>%
          row_spec(nrow(tab)-1, hline_after=T) %>%
          row_spec(nrow(tab), italic=F)
      } else if (total_row & mean_row) {
        print_kable = print_kable %>%
          row_spec(nrow(tab)-2, hline_after=T) %>%
          row_spec((nrow(tab)-1):nrow(tab), italic=F)
      }
      
      (print_kable)

  } else if (knitr::is_latex_output()==TRUE){
    if (nrow(tab) > 6) {
      # using print here requires chunk option to be results='asis'
      # print ensures that table reference can be rendered in latex!!
      print_kable = kable(tab, caption=ifelse(!is.null(caption),caption,NA),
            label=tab_label,format="latex",booktabs=T,
            longtable = T, align=c("l","r","r"),linesep = "") %>% 
        kable_styling(latex_options=c("HOLD_position","repeat_header"),position="center",full_width = F) %>%
        row_spec(0, bold = T) %>%
        column_spec(1, width = col1_width) %>%
        column_spec(2:3, width = col_width) %>%
        footnote(general=add_footnote,threeparttable=TRUE)

    } else {
      # using print here requires chunk option to be results='asis'
      # print ensures that table reference can be rendered in latex!!

      # don't use longtable
      print_kable = kable(tab, caption=ifelse(!is.null(caption),caption,NA),
            label=tab_label,format="latex",booktabs=T,
            longtable = F, align=c("l","r","r"),linesep = "") %>% 
        kable_styling(latex_options=c("HOLD_position"),position="center",full_width = F) %>%
        row_spec(0, bold = T) %>%
        column_spec(1, width = col1_width) %>%
        column_spec(2:3, width = col_width) %>%
        footnote(general=add_footnote,threeparttable=TRUE)
  
    }
    
    # add line before and italicize
    if (total_row | mean_row) {
      print_kable = print_kable %>%
        row_spec(nrow(tab)-1, extra_latex_after="\\midrule") %>%
        row_spec(nrow(tab), italic=F)
    } else if (total_row & mean_row) {
      print_kable = print_kable %>%
        row_spec(nrow(tab)-2, extra_latex_after="\\midrule") %>%
        row_spec((nrow(tab)-1):nrow(tab), italic=F)
    }
    
    (print_kable)
  }
  
}