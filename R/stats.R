#' Games-Howell Post Hoc Test for ANOVA
#'
#' Performs Games-Howell post hoc test for ANOVA when equal variances cannot be assumed.
#' This function handles both weighted and unweighted data.
#'
#' @param num R data frame containing numeric data (not labelled)
#' @param lab R data frame containing labelled data (factor/string)
#' @param var String, variable in question (dependent variable)
#' @param group String, group name to split by (independent variable)
#' @param weight Boolean, whether or not to weight frequency table (default: TRUE)
#'
#' @return A data frame with Games-Howell test results including mean differences,
#'   standard errors, test statistics, p-values, and confidence intervals
#'
#' @details The 2023 updates correctly get the weighted N, mean and variance.
#'   However the unweighted stats are not tested recently and could be broken.
#'
#' @references https://rpubs.com/aaronsc32/games-howell-test
#'
#' @examples
#' \dontrun{
#' # Assuming you have numeric and labelled data frames
#' result <- games_howell_v23(numeric_data, labelled_data, "score", "treatment")
#' }
#'
#' @export
#' @importFrom stats aggregate ptukey qtukey
games_howell_v23 <- function(num, lab, var, group, weight=T) {
  
  #Reference: https://rpubs.com/aaronsc32/games-howell-test
  
  if((!is.factor(num[, group]))|(!is.factor(lab[, group]))){
    message("Coercing grouping variables to factors")
    num[, group] <- as.factor(num[, group])
    lab[, group] <- as.factor(lab[, group])
  }
  
  # get groups
  valid <- complete.cases(lab[,var], lab[,group])
  validGroups <- lab[valid, ]
  grp = sort(unique(validGroups[,group]), decreasing = F)
  groups <- length(grp) 
  levels = grp
  
  #Create combinations
  combs <- combn(grp, 2)
  
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # variance = variance of each group sample
  # The 2023 updates correctly gets the weighted N, mean and variance
  # However the unweighted stats are not tested recently and it could be broken
  
  if (weight) {
    stats <- get_w_desc_stats(num, lab, group, var, round = F)
    n <- stats$N
    Mean <- stats$Mean
    variance <- stats$Var
  } else {
    n <- aggregate(num[,var], list(num[, group]), FUN = length)[,2]
    Mean <- aggregate(num[,var], list(num[, group]), FUN = mean, na.rm = T)[,2]
    variance <- aggregate(num[,var], list(num[, group]), FUN = stats::var, na.rm = T)[,2]
  }
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[1,x]] - Mean[combs[2,x]]
    
    #t-values
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((variance[combs[1,x]] / n[combs[1,x]]) + (variance[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (variance[combs[1,x]] / n[combs[1,x]] + variance[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((variance[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (variance[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (variance[combs[1,x]] / n[combs[1,x]] + variance[combs[2,x]] / n[combs[2,x]]))
    seSpss <- sqrt((variance[combs[1,x]] / n[combs[1,x]] + variance[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      -1*(-1*mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se)
    })[[1]]
    
    # Group Combinations
    # group.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(as.character(combs[1,x]), as.character(combs[2,x]), mean.diff, seSpss, t, p, df,round(lower.conf, 4), round(upper.conf, 4))
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {unlist(x)})
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE), stringsAsFactors = F)
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(3, 4:ncol(results))] <- round(as.numeric(as.matrix(results[c(3, 4:ncol(results))])), digits = 5)
  
  # Rename data frame columns
  colnames(results) <- c("Selected", "Comparison(s)", 'Mean Difference', 'Standard Error', 'GH T Statistics','p-value','Df','LCI','UCI')
  
  return(results)
}

#' ANOVA with Homogeneity of Variance Testing
#'
#' Computes ANOVA (checking for equality of variances first) on weighted or unweighted data.
#' Automatically selects appropriate test based on variance equality and provides post hoc testing.
#'
#' @param num R data frame (numeric, not labelled)
#' @param lab R data frame, labelled data
#' @param var String, variable for which to calculate ANOVA
#' @param group String, group to test for differences
#' @param ph Boolean, whether or not to run post hoc tests (default: TRUE)
#' @param weight Boolean, whether or not to weight data (default: TRUE)
#' @param details Boolean, whether or not to return ANOVA details (default: FALSE)
#'
#' @return A list containing:
#'   \item{aov}{ANOVA summary with Levene's test p-value, significance indicator, and ANOVA p-value}
#'   \item{post_hoc}{Post hoc test results (if ph=TRUE and significant)}
#'   \item{details}{Detailed ANOVA results (if details=TRUE)}
#'
#' @details Uses Levene's test to check homogeneity of variances. If variances are unequal,
#'   uses Welch's F test. If equal, uses standard ANOVA. Post hoc tests use Games-Howell
#'   for unequal variances or Tukey's HSD for equal variances.
#'
#' @examples
#' \dontrun{
#' # Basic ANOVA
#' result <- our_anova_v23_2(numeric_data, labelled_data, "score", "treatment")
#' 
#' # ANOVA without post hoc tests
#' result <- our_anova_v23_2(numeric_data, labelled_data, "score", "treatment", ph = FALSE)
#' }
#'
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom forcats fct_drop
#' @importFrom stats lm anova oneway.test
our_anova_v23_2 = function(num, lab, var, group, ph = T, weight=T, details = F) {
  
  # converting it to factor
  num[, group] <- as.factor(num[, group])
  lab[, group] <- as.factor(lab[, group])
  subnum <- num[, c(var, group, "Weight")]
  sublab <- lab[, c(var, group, "Weight")]
  # get valid data for the var and group combo used to correct df
  validnum = subnum[complete.cases(subnum), ]
  validlab = sublab[complete.cases(sublab), ]
  validnum[, group] <- forcats::fct_drop(validnum[, group])
  validlab[, group] <- forcats::fct_drop(validlab[, group])
  
  #print(levels(validlab[, group]))
  
  # initialize variables
  post_hoc = NULL
  Sig = "Insignificant"
  var_p = NULL
  pval = NULL
  #insigs = NULL
  
  # get weighted Ns and Ns
  wNs = as.numeric(get_w_desc_stats(validnum, validlab, group, var)$N)
  Ns = as.data.frame(table(validlab[, group]))$Freq
  
  # technically anova runs when group n > 2 mathematically 
  # (ignoring model assumption and effects and all that)
  # we will use the redact cutoff of n < 5 
  if(any(wNs < 5) | any(Ns < 5)){
    Sig = "Insufficent sample size"
    #insigs = which((colSums(table(validnum[, c(var, group)])) < 5))
  } else if(length(levels(validnum[, group])) < 2){
    Sig = "Insufficent sample size"
    #insigs = group[which(length(levels(validnum[, group])) < 2)]
  } else {
    
    # check for equality of variances (switch to a weighted median (todo) since it is more robust)
    # using weighted mean
    var_equal = our_levene_v23(var, group, validnum, center=mean, weight)
    var_p = var_equal[1,]$`Pr(>F)`
    
    # if heteroskedastic
    if(var_p < 0.05){
      
      # update 2023: now uses Welch's F test that adjusts for weights
      if(weight){
        aov_res = welch_f(validlab, validnum, var, group)
      } else {
        aov_res = oneway.test(validnum[, var] ~ validnum[, group])
      }
      
      pval = aov_res$p.value[1]
      
      # if significant
      if (pval < 0.05) {
        Sig = "Significant"
        if(ph){
          post_hoc = games_howell_v23(validnum, validlab, var, group, weight)
        }
      } 
      # if homoskedastic
    } else {
      
      # Need to correct the df of the lm object
      # see: https://stackoverflow.com/questions/65458098/correcting-dfs-when-using-sample-weights-with-lm
      # for one-way anova, `stats::anova` equals `car::Anova` (both returns type II by default)
      
      if(weight){
        lm_res <- lm(validnum[,var] ~ validnum[,group],weights = validnum$Weight)
        # correct df
        lm_res$df.residual <- sum(validnum$Weight) - length(coef(lm_res))
      } else {
        lm_res <- lm(validnum[,var] ~ validnum[,group])
      }
      aov_res = anova(lm_res)
      pval = aov_res$`Pr(>F)`[1]
      
      if (pval < 0.05) {
        Sig = "Significant"
        if(ph){
          post_hoc <- our_tukey(validnum, validlab, var, group, aov_res, weight)
        }
      }
    }
  }

  aovs <- suppressMessages(dplyr::bind_cols(ifelse(is.null(var_p), NA, round(var_p, 5)),
                           Sig,
                           ifelse(is.null(pval), NA, round(pval, 5))))
  names(aovs) <- c("Levene", "Sig", "p-value")
  
  if(details){
    result = list(aov = aovs,
                  details = aov_res,
                  post_hoc = post_hoc)
  } else {
    result = list(aov = aovs,
                  post_hoc = post_hoc)
  }
  
  
  return(result)
}

#' Levene's Test for Homogeneity of Variance (Weighted Version)
#'
#' Modified version of the original leveneTest function to handle weighted data
#' using weighted group means instead of raw means.
#'
#' @param y String, dependent variable name
#' @param group String, grouping variable name
#' @param data Data frame containing the variables
#' @param center Function to use for centering (default: mean)
#' @param weight Boolean, whether to use weighted analysis (default: TRUE)
#'
#' @return ANOVA table with F-statistic and p-value for Levene's test
#'
#' @details This function has identical parameters to `car::leveneTest` except for
#'   the weight boolean toggle and is fixed to use mean centering. When weight=FALSE,
#'   it calls the original car::leveneTest function.
#'
#' @note TODO: implement weighted median centering option
#'
#' @examples
#' \dontrun{
#' # Test for equal variances with weighting
#' levene_result <- our_levene_v23("score", "group", mydata, weight = TRUE)
#' }
#'
#' @export
#' @importFrom survey svydesign svyby svymean
#' @importFrom car leveneTest
#' @importFrom stats lm anova complete.cases
our_levene_v23 <- function (y, group, data, center=mean, weight=TRUE) { # original levene.test
  
  valid <- complete.cases(data[,y], data[,group])
  
  if (weight) {
    
    data <- data[valid,]
    dw = data
    dw <- svydesign(ids=~1, weights=~Weight, data=dw)
    
    yvar = dw$variables[,y]
    groupvar = dw$variables[,group]
    
    # This has so far proven to be correct when we check levene's F statistics in SPSS
    # But probably should avoid survey package (its not as consistently correct compared to get_w_desc_stats)
    meds <- svyby(~eval(as.name(y)), ~eval(as.name(group)), dw, svymean, na.rm=T)
    meds = meds[,2]
    
    resp <- abs(yvar - meds[groupvar])
    
    data$resp <- resp
    data$groupvar = as.factor(groupvar)
    
    dw2 <- svydesign(ids=~1, weights=~Weight, data=data)
    
    # 2023 changes: original implementation incorrect
    # R's lm does not correct for replication/frequency weights when calculating degrees of freedom and residuals
    # see: https://stackoverflow.com/questions/65458098/correcting-dfs-when-using-sample-weights-with-lm
    
    mod <- lm(resp ~ groupvar, data = data, weight = data$Weight)
    mod$df.residual <- sum(data$Weight) - length(coef(mod))
    table <- anova(mod)[, c(1, 4, 5)]
    rownames(table) <- c(group, "")
    dots <- deparse(substitute(...))
    attr(table, "heading") <- paste("Levene's Test for Homogeneity of Variance (center = ", 
                                    deparse(substitute(center)), if(!(dots == "NULL")) paste(":", dots),  ")", sep="")
    
  } else {
    # just call car's levene, it is proven to be correct when no weights are used

    table <- car::leveneTest(data[, y], data[, group], center=mean)
    rownames(table) <- c(group, "")
  }
  
  return(table)
  
}

#' Two-Sample T-Test with Variance Equality Testing
#'
#' Computes t-test (checking for equality of variances first) on weighted or unweighted data.
#' Automatically determines whether to use equal or unequal variance t-test.
#'
#' @param num Numerical data frame
#' @param lab Labelled data frame
#' @param var String, variable for which to calculate t-test
#' @param group String, group to test for differences
#' @param weight Boolean, whether or not to weight data (default: TRUE)
#'
#' @return A list containing:
#'   \item{ttest}{Summary t-test results with Levene's test p-value, significance, and t-test p-value}
#'   \item{t_result}{Detailed t-test results including mean differences, test statistics, and confidence intervals}
#'
#' @details First performs Levene's test for equality of variances, then conducts
#'   appropriate t-test. Uses minimum sample size of 5 per group.
#'
#' @examples
#' \dontrun{
#' # Basic t-test
#' result <- our_ttest_v23(numeric_data, labelled_data, "score", "treatment")
#' 
#' # Unweighted t-test
#' result <- our_ttest_v23(numeric_data, labelled_data, "score", "treatment", weight = FALSE)
#' }
#'
#' @export
our_ttest_v23 = function(num, lab, var, group, weight=T) {
  
  # converting it to factor
  subnum <- num[, c(var, group, "Weight")]
  sublab <- lab[, c(var, group, "Weight")]
  # get valid data for the var and group combo used to correct df
  validnum = subnum[complete.cases(subnum), ]
  validlab = sublab[complete.cases(sublab), ]
  validnum[, group] <- as.factor(validnum[, group])
  validlab[, group] <- as.factor(validlab[, group])
  validnum[, group] <- forcats::fct_drop(validnum[, group])
  validlab[, group] <- forcats::fct_drop(validlab[, group])
  
  Sig = "Insignificant"
  tresult = NULL
  var_p = NULL
  
  # band-aid fix to avoid report code breaking, unsure of the root cause
  # seems to be ok as the tables that fail this is below the redact cutoff
  wNs = tryCatch({
    as.numeric(get_w_desc_stats(validnum, validlab, group, var)$N)
  }, error = function(e) {
    return(0)
  })
  
  Ns = as.data.frame(table(validlab[, group]))$Freq
  
  # We are not running test for any group size that have less than 5 people
  # mathmatically t-test should for group n > 2 like anova
  if(any(wNs < 5)|any(Ns < 5)){
    Sig = "Insufficent sample size"
  } else if(length(levels(validnum[, group])) < 2){
    Sig = "Insufficent sample size"
  } else {
    # check for equal variances
    var_equal = our_levene_v23(var, group, validnum, center=mean, weight)
    var_p = var_equal[1,]$`Pr(>F)`
    
    if(var_p < 0.05){
      ve = F
    } else {
      ve = T
    }
    
    tresult = two_sample_ttest(validnum, validlab, var, group, weight, ve)
    
    if(tresult$`p-value`[1] < 0.05){
      Sig = "Significant"
    }
  }
  
  ttest = dplyr::bind_cols(ifelse(is.null(var_p), NA, round(var_p, 5)), 
                           Sig,
                           ifelse(is.null(tresult), NA, tresult$`p-value`[1]))
  colnames(ttest) = c("Levene", "Sig", "p-value")
  
  result <- list(ttest = ttest,
                 t_result = tresult)
  return(result)
}

#' Tukey's HSD Post Hoc Test
#'
#' Tukey's HSD post hoc test that supports both weighted and unweighted analyses.
#' Used for pairwise comparisons following significant ANOVA with equal variances.
#'
#' @param num Numerical data frame
#' @param lab Labelled data frame
#' @param var String, variable for which to calculate comparisons
#' @param group String, group to test for differences
#' @param aov_res Data frame, aov, or lm class object with detailed ANOVA results
#' @param weight Boolean, whether or not to weight data (default: TRUE)
#'
#' @return Data frame with pairwise comparison results including mean differences,
#'   standard errors, test statistics, p-values, and confidence intervals
#'
#' @details Implements both standard Tukey HSD for equal group sizes and 
#'   Tukey-Kramer method for unequal group sizes. Requires prior ANOVA results
#'   for MSE and degrees of freedom calculations.
#'
#' @references 
#'   \url{https://en.wikipedia.org/wiki/Tukey's_range_test}
#'   \url{https://real-statistics.com/one-way-analysis-of-variance-anova/unplanned-comparisons/tukey-kramer-test/}
#'
#' @examples
#' \dontrun{
#' # Assuming you have ANOVA results
#' anova_result <- anova(lm(score ~ treatment, data = mydata))
#' tukey_result <- our_tukey(numeric_data, labelled_data, "score", "treatment", anova_result)
#' }
#'
#' @export
our_tukey <- function(num, lab, var, group, aov_res, weight=T) {
  
  require(survey)
  
  # References: 
  # https://en.wikipedia.org/wiki/Tukey%27s_range_test
  # https://real-statistics.com/one-way-analysis-of-variance-anova/unplanned-comparisons/tukey-kramer-test/
  
  if(is.null(aov_res)){
    stop("Missing Anova Results")
  }
  
  if((!is.factor(num[, group]))|(!is.factor(lab[, group]))){
    message("Coercing grouping variables to factors")
    num[, group] <- as.factor(num[, group])
    lab[, group] <- as.factor(lab[, group])
  }

  # get groups
  valid <- complete.cases(lab[,var], lab[,group])
  validGroups <- lab[valid, ]
  grp = sort(unique(validGroups[,group]), decreasing = F)
  groups <- length(grp) 
  levels = grp
  
  #Create combinations
  combs <- combn(grp, 2)
  
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample

  MSE = aov_res$`Mean Sq`[2]
  DF = aov_res$Df[2]
  
  if (weight) {
    stats <- get_w_desc_stats(num, lab, group, var, round = F)
    n <- stats$N
    Mean <- stats$Mean
    
  } else {
    n <- tapply(num[,var], num[,group], length)
    Mean <- tapply(num[,var], num[,group], mean, na.rm=T)
  }
  
  # check for unequal group sizes
  if(length(unique(n)) == 1){
    eqN = TRUE
  } else {
    eqN = FALSE
  }
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[1,x]] - Mean[combs[2,x]]
    
    # q-values
    # equal group sizes -> studentized range
    if(eqN){
      se <- sqrt(MSE/n[combs[2,x]])
      q <- abs(mean.diff) / se
    } else { # Tukey-Kramer
      se <- sqrt((MSE/2)*(1/n[combs[1,x]]+1/n[combs[2,x]]))
      q <- abs(mean.diff) / se
    }
    
    #p-values
    p <- ptukey(q, groups, DF, lower.tail = FALSE)
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = DF) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      -1*(-1*mean.diff + qtukey(p = 0.95, nmeans = groups, df = DF) * se)
    })[[1]]
    
    # Group Combinations
    group.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(as.character(combs[1,x]), as.character(combs[2,x]), mean.diff, se, q, p, round(lower.conf, 4), round(upper.conf, 4))
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {unlist(x)})
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE), stringsAsFactors = F)
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(3, 4:ncol(results))] <- round(as.numeric(as.matrix(results[c(3, 4:ncol(results))])), digits = 5)
  
  # Rename data frame columns
  colnames(results) <- c("Selected", "Comparison(s)", 'Mean Difference', 'Standard Error', 'Tukey Q Statistics', 'p-value', 'LCI', 'UCI')
  
  return(results)
}

#' Independent Two-Sample T-Test
#'
#' Performs independent two-sample t-test with support for both equal and unequal variances
#' and weighted data analysis.
#'
#' @param num Numeric data frame
#' @param lab Labelled data frame
#' @param var String, variable name for dependent variable
#' @param group String, grouping variable name
#' @param weight Boolean, whether to use weighted test (default: TRUE)
#' @param var_equal Boolean, whether group population variances are assumed equal
#'   (default: TRUE, but should be determined by Levene's test)
#'
#' @return Data frame with t-test results including group comparisons, mean differences,
#'   variance equality assumption, test statistics, degrees of freedom, and p-values
#'
#' @details Implements both Student's t-test (equal variances) and Welch's t-test
#'   (unequal variances). The var_equal parameter should typically be determined
#'   by prior Levene's test results.
#'
#' @references \url{https://en.wikipedia.org/wiki/Student's_t-test}
#'
#' @examples
#' \dontrun{
#' # Equal variances assumed
#' result <- two_sample_ttest(numeric_data, labelled_data, "score", "treatment", 
#'                           weight = TRUE, var_equal = TRUE)
#' 
#' # Unequal variances
#' result <- two_sample_ttest(numeric_data, labelled_data, "score", "treatment", 
#'                           weight = TRUE, var_equal = FALSE)
#' }
#'
#' @export
#' @importFrom stats complete.cases pt var
two_sample_ttest <- function(num, lab, var, group, weight, var_equal = T){
  
  # Reference: https://en.wikipedia.org/wiki/Student%27s_t-test
  
  valid <- complete.cases(num[,var], num[,group])
  num <- num[valid,]
  lab <- lab[valid,]
  sig = "Insignificant difference in means"
  
  if(is.factor(lab[, group])){
    groups = levels(lab[, group])
  } else {
    groups = unique(lab[, group])
  }
  #lvl = group.comb <- paste(groups[1], ':', groups[2])
  
  if (weight) {
    stats <- get_w_desc_stats(num, lab, group, var, round = F)
    n <- stats$N
    Mean <- stats$Mean
    variance <- stats$Var
    
  } else {
    # n <- tapply(num[,var], num[,group], length)
    # Mean <- tapply(num[,var], num[,group], mean, na.rm=T)
    # variance <- tapply(num[,var], num[,group], var)
    n <- aggregate(num[,var], list(num[, group]), FUN = length)[,2]
    Mean <- aggregate(num[,var], list(num[, group]), FUN = mean, na.rm = T)[,2]
    variance <- aggregate(num[,var], list(num[, group]), FUN = stats::var, na.rm = T)[,2]
  }
  
  mean.diff = Mean[1]-Mean[2]
  
  if(var_equal){
    ve = "Equal Variance"
    s = sqrt(((n[1]-1)*variance[1]+(n[2]-1)*variance[2])/(n[1]+n[2]-2))
    df = (n[1]+n[2]-2)
    t = mean.diff/(s*sqrt(1/n[1]+1/n[2]))
  } else {
    ve = "Unequal Variance"
    df = (variance[1]/n[1]+variance[2]/n[2])^2/((variance[1]/n[1])^2/(n[1]-1)+(variance[2]/n[2])^2/(n[2]-1))
    t = mean.diff/sqrt(variance[1]/n[1]+variance[2]/n[2])
  }
  
  pval = round(ifelse(mean.diff < 0, (pt(t, df) * 2), (pt(t, df, lower.tail = F) * 2)), 5)
  
  results <- as.data.frame(cbind.data.frame(groups[1], groups[2], mean.diff, ve, t, df, pval), stringsAsFactors = F)
  results[, 1:6] <- sapply(results[, 1:6], as.character)
  results[, c(3, 5, 6, 7)] <- sapply(results[, c(3, 5, 6, 7)], as.numeric)
  results[, c(3, 5, 6, 7)] <- sapply(results[, c(3, 5, 6, 7)], round, 5)
  
  names(results) <- c("Selected", "Comparison(s)", "Mean Difference", "Variance Equality", "T Statistics", "Df", "p-value")
  return(results)
}

#' Welch's F-Test for Unequal Variances
#'
#' Modified version of `stats::oneway.test` made compatible with weighted data
#' and fixed to unequal variances assumption.
#'
#' @param dlab Labelled data frame
#' @param dnum Numeric data frame  
#' @param var String, dependent variable name
#' @param group String, grouping variable name
#'
#' @return An htest object containing:
#'   \item{statistic}{F-statistic value}
#'   \item{parameter}{Numerator and denominator degrees of freedom}
#'   \item{p.value}{P-value for the test}
#'   \item{method}{Description of the test method}
#'   \item{data.name}{Names of the variables tested}
#'
#' @details This function implements Welch's one-way ANOVA that does not assume
#'   equal variances and properly handles weighted data. It's used when Levene's
#'   test indicates heteroscedasticity.
#'
#' @references 
#'   See `get_w_desc_stats.R` and `stats::oneway.test` for more details
#'
#' @examples
#' \dontrun{
#' # Test for group differences with unequal variances
#' welch_result <- welch_f(labelled_data, numeric_data, "score", "treatment")
#' }
#'
#' @export
welch_f <- function(dlab, dnum, var, group) {
  stats = get_w_desc_stats(dnum, dlab, group = group, index = var, round = F)
  
  # drop group lvls with no ppl in it
  stats = stats %>%
    filter(N != 0)
  
  METHOD <- "One-way analysis of means by Welch's F"
  DNAME <- paste(var, "and", group)
  
  datalab = dlab
  if(is.factor(datalab[,group])){
    k = length(levels(datalab[,group]))
  } else {
    k = length(unique(datalab[,group]))
  }
  
  n.i = stats$N
  m.i = stats$Mean
  v.i = stats$Var
  w.i = n.i/v.i
  sum.w.i <- sum(w.i)

  tmp <- sum((1 - w.i / sum.w.i)^2 / (n.i - 1)) / (k^2 - 1)
  #tmp <- sum((1/(n.i-1))*(1-w.i/sum.w.i)^2)
  m <- sum(w.i * m.i) / sum.w.i
  STATISTIC <- sum(w.i * (m.i - m)^2) / ((k - 1) * (1 + 2 * (k - 2) * tmp))
  #STATISTIC <- ((1/(k-1))*sum(w.i*(m.i-m)^2)) / (1+(2*(k-2))/(k^2-1)*tmp)
  #df = (k^2-1)/(3 * temp)
  PARAMETER <- c(k - 1, 1 / (3 * tmp))
  PVAL <- pf(STATISTIC, k - 1, 1 / (3 * tmp), lower.tail = FALSE)
  METHOD <- paste(METHOD, "(not assuming equal variances)")
  names(STATISTIC) <- "F"
  names(PARAMETER) <- c("num df", "denom df")
  RVAL <- list(statistic = STATISTIC,
               parameter = PARAMETER,
               p.value = PVAL,
               method = METHOD,
               data.name = DNAME)
  class(RVAL) <- "htest"
  RVAL
}