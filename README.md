# citlR

## Survey Data Tools

A collection of functions for analyzing survey data at the Center for Innovation in Teaching & Learning (CITL) at the University of Illinois Urbana-Champaign.

## Installation

You can install the development version of citlR from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("your-username/citlR")
```

Or install from source:

```r
# If you have the package files locally
install.packages("path/to/citlR", repos = NULL, type = "source")
```

## Overview

citlR provides a comprehensive set of functions for survey data analysis, including:

- **Frequency table generation** with support for weighting, redaction, and grouping
- **Statistical testing** (ANOVA, t-tests) with automatic variance equality checking  
- **Data visualization** with customizable charts and plots
- **Table formatting** for both HTML and LaTeX output
- **Utility functions** for data processing and manipulation

## Key Features

### Table Generation
- `gen_table()` - Generate frequency tables for individual variables
- `gen_table_group_dym()` - Create grouped frequency tables
- `bind_col_v23()` - Bind multiple variables as columns
- `bind_row()` - Bind multiple variables as rows

### Statistical Analysis
- `our_anova_v23_2()` - ANOVA with homogeneity of variance testing
- `our_ttest_v23()` - Two-sample t-tests with variance equality testing
- `games_howell_v23()` - Games-Howell post hoc test
- `get_w_desc_stats()` - Weighted descriptive statistics

### Visualization
- `print_stacked_bar_v23()` - Horizontal stacked bar charts
- `print_bar_v23()` - Simple bar charts
- `grouped_bar_chart_v23()` - Grouped bar charts
- `print_index_box()` - Index boxplots

### Table Formatting
- `print_simple_kable()` - Simple frequency tables
- `print_com_kable()` - Combined tables with custom formatting
- `print_desc_stats()` - Descriptive statistics tables

## Basic Usage

```r
library(citlR)

# Load your survey data
data <- read.spss("path/to/survey.sav")

# Generate a frequency table
freq_table <- gen_table(data$dataLab, "satisfaction_rating")

# Create a grouped table
grouped_table <- gen_table_group_dym(data$dataLab, 
                                    var = "satisfaction_rating", 
                                    group = "gender")

# Print formatted table
print_simple_kable(freq_table, 
                  caption = "Satisfaction Ratings",
                  newname = "Response")

# Generate visualization  
chart <- print_stacked_bar_v23(grouped_table, 
                              title = "Satisfaction by Gender")
```

## Dependencies

This package requires R >= 4.0.0 and depends on several packages including:
- tidyverse, dplyr, tidyr
- ggplot2, ggpubr  
- knitr, kableExtra
- sjlabelled, sjmisc
- survey, car
- And others (see DESCRIPTION file)

## License

MIT License - see LICENSE file for details.

## Development

This package uses roxygen2 for documentation. To regenerate documentation:

```r
devtools::document()
```

To build and check the package:

```r
devtools::check()
devtools::build()
``` 