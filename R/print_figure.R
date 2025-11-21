#' Print Grouped Bar Chart
#'
#' Creates a grouped bar chart from frequency table data for check-all questions,
#' displaying responses across different groups with customizable color schemes.
#'
#' @param gtab Data frame containing frequency table for check-all questions
#' @param group String specifying the group name for labeling
#' @param title String specifying chart title (optional, default NULL)
#' @param scaled Boolean indicating whether to use scaled coloring scheme (default FALSE)
#'
#' @return A ggplot2 object containing the grouped bar chart
#'
#' @details The function creates a horizontal grouped bar chart with:
#' - Automatic color palette selection based on number of groups (2-6 groups supported)
#' - Two color schemes: scaled (gradient) or standard (distinct colors)
#' - Text labels showing percentages on bars
#' - Wrapped axis labels for readability
#'
#' @examples
#' \dontrun{
#' # Create grouped bar chart with standard colors
#' chart1 <- grouped_bar_chart_v23(frequency_table, "Gender", "Survey Responses")
#' 
#' # Create grouped bar chart with scaled colors
#' chart2 <- grouped_bar_chart_v23(frequency_table, "Age Group", scaled = TRUE)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal coord_flip ylab theme
#' @importFrom ggplot2 element_blank element_text position_dodge2 guides guide_legend
#' @importFrom ggplot2 scale_fill_manual scale_x_discrete scale_y_continuous geom_text ggtitle
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove str_extract
#' @importFrom reshape2 melt
#' @importFrom forcats fct_rev fct_relevel
#' @export
grouped_bar_chart_v23 = function(gtab, group, title=NULL, scaled = F) {
  
  gtab2 = gtab
  
  gtab2[2:ncol(gtab)] <- sapply(gtab[2:ncol(gtab)], .helper)
  
  group_name = group
  
  labelwidth=30 #Ash changed temporarily so that label texts don't overflow
  
  names(gtab2) = c("Answer", str_remove(names(gtab2[2:ncol(gtab2)]), "\\s*\\(.*?\\)"))
  # print(gtab2)
  gtab2[gtab2 == 0] <- NA
  data = reshape2::melt(gtab2, id.vars = 1, na.rm = T)
  data$Answer = fct_rev(fct_relevel(data$Answer, gtab[, 1]))
  rownames(data) <- NULL
  # print(data)
  # set color palette for figures (if scaled or not)
  grp_levels = length(unique(data$variable))
  
  if(scaled){
    if (grp_levels == 2){
      palette = c("#11294B","#E74B26")
      numrows=1
    } else if (grp_levels == 3){
      palette = c("#11294B","#A6ACAF","#E74B26")
      numrows=1
    } else if (grp_levels == 4){
      palette = c("#11294B","#565F75","#CC8266","#E74B26")
      numrows=1
    } else if (grp_levels == 5) {
      palette = c("#11294B","#565F75","#A6ACAF","#CC8266","#E74B26")
      numrows=2
    } else if (grp_levels == 6) {
      palette = c("#11294B","#565F75","#A6ACAF","#BB9788","#CC8266","#E74B26")
      numrows=2
    }
  } else {
    if (grp_levels == 2){
      palette = c("#11294B","#E74B26")
      numrows=1
    } else if (grp_levels == 3){
      palette = c("#11294B","#1D58A7","#E74B26")
      numrows=1
    } else if (grp_levels == 4){
      palette = c("#11294B","#1D58A7","#FCB316","#E74B26")
      numrows=1
    } else if (grp_levels == 5) {
      palette = c("#11294B","#1D58A7","#006230","#FCB316","#E74B26")
      numrows=2
    } else if (grp_levels == 6) {
      palette = c("#11294B","#1D58A7","#006230","#5C0E41","#FCB316","#E74B26")
      numrows=2
    }
  }
  
  p1 = ggplot(data, aes(x=Answer, y=value, fill=fct_rev(droplevels(variable)))) + 
    geom_bar(stat="identity", position=position_dodge2(preserve = "single")) +
    theme_minimal() +
    coord_flip() +
    ylab(paste0("Percent Responses by ",group_name)) +
    theme(legend.position="top",panel.grid.major.y=element_blank(),
          legend.text=element_text(size=8),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=8),
          #legend.margin=margin(t=-0.5,unit="cm"),
          plot.title=element_text(size=12,
                                  hjust=0.5#,
                                  #vjust=0.5,
                                  #family="Times",
                                  #face=c("bold")
          ),
          legend.title=element_blank(),
          legend.spacing.x=unit(0.2,"cm")
          #text=element_text(family="serif")
    ) +
    guides(fill=guide_legend(nrow=numrows,reverse=T)) +
    scale_fill_manual(values=rev(palette),drop=FALSE)+
    xlab("") +
    ggtitle(wrapper(title,width=65)) +
    scale_y_continuous(limits=c(0,107),expand=c(0,0))+
    geom_text(aes(y = value+5, 
                  label = paste0(format(value,nsmall=1),"%")), 
              size=3,
              #family="serif", #Ash added on 04/30/2019
              fontface="bold",#Ash added on 04/30/2019
              colour="#2c3e50",
              vjust = 0.5,
              #hjust=ifelse(angle==0,0,-0.25),
              #hjust=0.95,
              position = position_dodge2(preserve = "single", width = 0.9),
              check_overlap = F) +
    scale_x_discrete(labels=wrap_format(labelwidth),drop=FALSE)
  # scale_y_continuous(expand=c(0,0))+
  # geom_text(aes(y = table$Percent, label = ifelse(table$Percent >=8,paste0(round(table$Percent,1),"%"),"")),
  #           position=position_stack(reverse=TRUE,vjust=0.5),colour="white",fontface="bold",show.legend=FALSE)
  
  return(p1)
  
  }

#' Helper Function for Grouped Bar Chart
#'
#' Internal helper function to extract numeric percentage values from formatted strings.
#'
#' @param x Character vector containing percentage strings
#' @return Numeric vector of percentage values
#' @keywords internal
.helper <- function(x){
  as.numeric(str_extract(x, "\\d+\\.\\d+(?=%)"))
}

#' Print Simple Bar Chart
#'
#' Creates a horizontal bar chart from frequency table data for individual questions,
#' displaying response percentages with customizable styling.
#'
#' @param tab Data frame containing frequency table for individual question responses
#' @param title String specifying chart title (optional, default NULL)
#' @param alt_text String specifying alternative text for accessibility (optional, default NULL)
#'
#' @return A ggplot2 object containing the horizontal bar chart
#'
#' @details The function creates a horizontal bar chart with:
#' - Automatic filtering of total and mean rows
#' - Blue color scheme (#1D58A7)
#' - Percentage labels on bars
#' - Wrapped axis labels for long text
#' - Y-axis scaled from 0-115% with breaks at 25% intervals
#'
#' @examples
#' \dontrun{
#' # Create simple bar chart
#' chart <- print_bar_v23(frequency_table, "Question Responses")
#' 
#' # Create bar chart with alt text for accessibility
#' chart_accessible <- print_bar_v23(frequency_table, "Ratings", "Bar chart showing survey ratings")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal coord_flip ylab theme
#' @importFrom ggplot2 element_blank element_text scale_fill_manual scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous xlab labs geom_text ggtitle
#' @importFrom dplyr %>% filter mutate
#' @importFrom stringr str_extract
#' @export
print_bar_v23 = function(tab, title=NULL, alt_text=NULL) {
  
  # tab: data frame, frequency table for individual question
  
  # keep only N and Percent columns, change column 1 name to Answer
  colnames(tab)[1] = "Answer"
  
  # remove percent symbol
  tab$Percent = as.numeric(stringr::str_extract(tab$Checked, "\\d+\\.\\d+(?=%)"))
  
  ## if total Or mean row, remove
  tab = tab %>%
    filter(Answer != "Total") %>%
    filter(Answer != "Total N") %>%
    filter(Answer != "Mean") %>%
    # fix factor levels in order of current table 
    # THIS ASSUMES the table is in descending Likert order!!
    mutate(Answer = factor(Answer, levels=rev(.$Answer)))
  
  n_reps = (nrow(tab) + nrow(tab) %% 2)
  
  usesAlt_text = grepl("a", alt_text)
  
  fig = ggplot(tab, aes(x=Answer, y=Percent, fill=Answer)) +
    # create bar chart of percentages
    geom_bar(stat="identity", position=position_stack(reverse=TRUE)) + 
    theme_minimal() +
    ylab("Percent Responses") +
    theme(panel.grid.major.y=element_blank(),
          legend.text=element_text(size=8),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=8),
          plot.title=element_text(size=10,
                                  hjust=0.5),
          legend.position="none"
    ) +
    ggtitle(wrapper(title,width=65)) +
    # flip coordinates so bar chart is horizontal
    coord_flip() +
    # define custom color palette
    scale_fill_manual(values=rep("#1D58A7", n_reps),drop=FALSE)+
    # word wrap long labels
    scale_x_discrete(labels=wrap_format(35),drop=FALSE)+
    # add percent values to labels and add extra space to right of graph so labels don't get cut off
    #scale_y_continuous(limits=c(0,max(tab$Percent)+8),labels=function(x) {paste0(x,"%")}) +
    scale_y_continuous(limits=c(0,115), breaks=c(0, 25, 50, 75, 100),labels=function(x) {paste0(x,"%")}) +
    # remove x and y labels
    ylab("") + xlab("") + 
    labs(alt = alt_text) +
    geom_text(aes(y = Percent,size=1.2, label = ifelse(Percent > 0, paste0(sprintf('%.1f',Percent),"%"),""), hjust = -0.15,
                  size=3,
                  vjust=0.5,
                  check_overlap = TRUE,
                  angle=0,show.legend=FALSE))
    
  
  # #set theme
  # theme_pubr() +
  # #theme_classic() +
  # # increase axis text size
  # theme(axis.text.y=element_text(size=12)) + #,axis.text.x=element_text(size=12)) +
  # # remove legend
  # theme(legend.position="none") +
  # add text labels to each value on plot
  
  # return plot
  return(fig)
}

#' Print Index Boxplots
#'
#' Creates a horizontal boxplot visualization for multiple indices/scales,
#' useful for comparing distributions across different measurement constructs.
#'
#' @param datanum Numeric data frame containing the index variables and weights
#' @param indices Character vector listing the variable names of indices to plot
#' @param indices_name Character vector containing the display names for the indices
#' @param title String specifying optional chart title (default NULL)
#'
#' @return A ggplot2 object containing the horizontal boxplot
#'
#' @details The function creates horizontal boxplots with:
#' - Weighted boxplots using the 'Weight' variable from datanum
#' - Custom color palette (up to 9 colors supported)
#' - Legend positioned at bottom with wrapped text
#' - Y-axis scale from 1-5 (typical for Likert scales)
#' - Text wrapping for long index names (25 character width)
#'
#' @examples
#' \dontrun{
#' # Create boxplot for satisfaction indices
#' indices_vars <- c("satisfaction_overall", "satisfaction_quality", "satisfaction_service")
#' indices_labels <- c("Overall Satisfaction", "Quality Satisfaction", "Service Satisfaction")
#' boxplot <- print_index_box(data_numeric, indices_vars, indices_labels, "Satisfaction Indices")
#' }
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_minimal theme element_blank
#' @importFrom ggplot2 element_text guides guide_legend scale_x_discrete scale_fill_manual
#' @importFrom ggplot2 ylab ylim coord_flip ggtitle
#' @importFrom forcats fct_rev
#' @importFrom stringr str_wrap
#' @export
print_index_box <- function(datanum, indices, indices_name, title = NULL){
  n = length(indices)
  colorList = c("#FF5F05", "#849BC1", "#F5821E", "#5783BC", "#ADA7A5", "#009FD4", "#D2D2D2", "#BFE7F7", "#F8FAFC")
  colorList = colorList[1:n]
  colorList = rev(colorList)
  palette = colorList[1:n]
  temp = datanum[, c(indices, "Weight")]
  temp = melt(temp, id.vars = c("Weight"))
  temp$variable <- as.factor(temp$variable)
  p1 = ggplot(data = temp, aes(x=forcats::fct_rev(variable),y=value, weight = Weight)) +
    geom_boxplot(aes(fill=forcats::fct_rev(variable))) +
    theme_minimal() +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          legend.text=element_text(size=8),
          axis.title.y=element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title=element_text(size=10, color="#2c3e50"))+
    guides(fill=guide_legend(ncol = 3, reverse = TRUE, byrow = T)) +
    # comment out the following two lines to test if label match with bar
    scale_x_discrete(labels=function(x) str_wrap(rev(indices_name), width = 25), drop=FALSE) +
    scale_fill_manual(labels=function(x) str_wrap(rev(indices_name), width = 25) ,values=palette, drop=FALSE)+
    ylab("Rating")+
    ylim(1, 5)+
    coord_flip()+
    ggtitle(paste(strwrap(title,width=55), collapse = "\n"))
  
  return(p1)
}

#' Print Stacked Bar Chart
#'
#' Creates a horizontal stacked bar chart from frequency table data,
#' typically used for displaying Likert scale responses across multiple questions.
#'
#' @param tab Data frame containing frequency table with percentages (created from bind_col functions)
#' @param title String specifying chart title (optional, default NULL)
#' @param sort Boolean indicating whether to sort questions by decreasing percentages (default FALSE)
#' @param trunc_str Boolean indicating whether to truncate question strings longer than 100 characters (default TRUE)
#' @param ylab String specifying y-axis label description (default "Percent Responses by Respondents")
#' @param legend_pos String specifying legend position (default "top", see ggplot2 documentation for valid positions)
#'
#' @return A ggplot2 object containing the horizontal stacked bar chart
#'
#' @details The function creates horizontal stacked bar charts with:
#' - Automatic color palette selection for 2-6 response categories
#' - Percentage labels on segments (only shown if segment >= 8%)
#' - Automatic legend row adjustment based on answer length
#' - Text wrapping for long question names (35 character width)
#' - Support for standard Likert scale color schemes
#'
#' @examples
#' \dontrun{
#' # Create basic stacked bar chart
#' chart1 <- print_stacked_bar_v23(likert_table, "Survey Results")
#' 
#' # Create sorted chart with custom y-label
#' chart2 <- print_stacked_bar_v23(likert_table, "Satisfaction Ratings", 
#'                                sort = TRUE, ylab = "Percentage of Responses")
#' 
#' # Create chart with bottom legend
#' chart3 <- print_stacked_bar_v23(likert_table, legend_pos = "bottom")
#' }
#'
#' @importFrom dplyr %>% filter mutate arrange select
#' @importFrom tidyr gather
#' @importFrom stringr str_trunc word
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal theme element_blank element_text
#' @importFrom ggplot2 guides guide_legend scale_fill_manual xlab scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous geom_label coord_flip ggtitle position_stack
#' @export
print_stacked_bar_v23 = function(tab, title=NULL, sort=FALSE, trunc_str=TRUE,
                                 ylab = "Percent Responses by Respondents",
                                 legend_pos = "top") {
  
  # tab: R data frame, table with percentages created from *bind_col*
  # title: string, title to print on ggplot (NULL by default)
  # sort: bool, whether to sort questions by decreasing percentages
  # trunc_str: bool, whether to truncate question strings if longer than 100 chars
  # ylab = ylab description (horizontal axis description), String
  # legend_pos: string, legend position, to see valid positions please reference
  #             ggplot2 documentation
  
  # keep only N and Percent columns, change column 1 name to Answer
  colnames(tab)[1] = "Answer"
  
  # Make unique names so R can manipulate the df/tbl
  # names(tab) = make.unique(names(tab))
  
  ## if total Or mean row, remove
  tab = tab %>%
    filter(!grepl("Total|Mean", Answer)) %>%
    # fix factor levels in order of current table 
    # THIS ASSUMES the table is in descending Likert order!!
    mutate(Answer = factor(Answer, levels=rev(.$Answer)))
  
  # convert table to long format
  table = tab %>%
    gather(Question, Percent, -Answer) %>%
    # remove percent symbol
    mutate(Percent = as.numeric(gsub("%","",.$Percent)),
           # convert question into factor in order of original table
           Question = factor(Question, levels=rev(unique(.$Question))))
  
  # truncate answer if >100 char
  if (trunc_str) {
    if (any(nchar(as.character(table$Question)) > 100)) {
      # truncate words
      table$Question = as.character(table$Question)
      table$Question[nchar(table$Question) > 100] = 
        paste0(word(table$Question[nchar(table$Question) > 100],start=1,end=12,sep=boundary("word")),"...") 
      #str_trunc(table$Question[nchar(table$Question) > 100], 100)
      table$Question = factor(table$Question, levels=rev(unique(table$Question)))
    }
  }
  
  
  if (sort) {
    table = table %>%
      mutate(Question = factor(Question, levels=rev(unique(Question[order(Answer,Percent,decreasing=TRUE)]))
                               ,ordered=TRUE))
  }
  
  # set color palette for figures and number of rows for color legends
  if (length(levels(table$Answer)) == 4){
    palette = c("#11294B","#565F75","#CC8266","#E74B26")
    numrows=2
  } else if (length(levels(table$Answer)) == 5) {
    palette = c("#11294B","#565F75","#A6ACAF","#CC8266","#E74B26")
    numrows=3
  } else if (length(levels(table$Answer)) == 6) {
    palette = c("#11294B","#565F75","#A6ACAF","#BB9788","#CC8266","#E74B26")
    numrows=3
  } else if(length(levels(table$Answer)) == 2) {
    palette = c("#11294B","#E74B26")
    numrows=2
  }
  
  palette = rev(palette)
  
  # if the choices are too long, make the legend 3 rows, default if 2
  legendrows = ifelse((any(sapply(as.character(tab$Answer), Vectorize(nchar)) >= 50)|
                         median(sapply(as.character(tab$Answer), Vectorize(nchar)) >= 25)), 3, 2)
  legendrows = ifelse(length(levels(table$Answer)) < 5, 1, legendrows)
  
  # ref: https://stackoverflow.com/questions/41646433/center-text-layer-in-ggplot-barchart
  # why geom label and not text: accessibility

  # create plot from percentages
  p1 = ggplot(table, aes(x=Question, y=Percent)) +
    # reverse order of values high Likert scale to right
    geom_bar(aes(fill=Answer), stat="identity", position=position_stack(reverse=TRUE)) + 
    # remove most theme elements and cusotmize title size
    theme_minimal() +
    theme(legend.position=legend_pos,
          legend.title=element_blank(),
          legend.text=element_text(size=8, color="#2c3e50"),
          axis.title.x=element_text(size=8, color="#2c3e50"),
          panel.grid=element_blank(),
          axis.text.x=element_blank(),
          plot.title=element_text(size=12, color="#2c3e50"),
          axis.text.y = element_text(size=10, color="#2c3e50")) +
    # adjust number of rows in legend
    guides(fill=guide_legend(nrow = legendrows, byrow = T)) +
    # specify color palette
    scale_fill_manual(values=palette,drop=FALSE)+
    # specify axis labels
    #ylab(ylab) +
    xlab("") +
    # wrap x axis labels so they aren't too long
    scale_x_discrete(labels=wrap_format(35),drop=FALSE) +
    # get rid of extra space around y axis
    scale_y_continuous(expand=c(0,0))+
    # add text labels containing percent values
    geom_label(data = table,
               aes(y = Percent, label = ifelse(Percent >=8, paste0(round(Percent,1),"%"),NA),
                   group = Answer),
               # aes(y = Percent, label = paste0(round(Percent,1),"%"), 
               #     group = Answer, alpha = ifelse(Percent >= 8, 0, 1)),
               position=position_stack(reverse=T, vjust=0.5),
               colour="#2c3e50",label.size=NA,fontface="bold",show.legend=FALSE) +
    # make horizontal bar chart
    coord_flip()
  
  if (!is.null(title)) {
    
    # add title if not NULL
    p1 = p1 +
      ggtitle(wrapper(title,width=55))
    
  }
  
  return(p1)

  
}