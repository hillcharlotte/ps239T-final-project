## Short Description

This project attempts to answer how the occupations of campaign donors shifted after implementation of Seattle’s voucher-based campaign finance system. 

1. Are certain occupational groups (e.g. certain business interests, labor, etc.) more or less represented among the donor pool now versus in previous comparable elections? 

2. Looking at the 2017 election specifically, were the occupations of voucher donors systematically different from those who contributed using their own "real" money?

I answer these questions by sorting donors into occupational categories, running a series of statistical tests, and creating relevant visualizations.

## Dependencies

My code depends on the following software: 

1. R 3.4.0, GUI 1.70, El Capitan build

## Files

List all other files contained in the repo, along with a brief description of each one, like so:

### Data

1. 2017_contributors.csv: A dataset of all contributors to Seattle's 2017 election, available here: http://web6.seattle.gov/ethics/elections/lists.aspx. 
2. 2015_contributors.csv: A dataset of all contributors to Seattle's 2015 election, available here: http://web6.seattle.gov/ethics/elections/lists.aspx. 

### Code

1. 01_categorize_data.R: Loads, cleans, merges, and categorizes the data for analysis.
2. 02_final_analysis.R: Conducts descriptive analysis of the data, producing the tables and visualizations found in the Results directory. 

### Select Results

This is a select group of visualizations and tables created in the final_analysis.R code. 

1. coverage-over-time.jpeg: Graphs the number of articles about each region over time.
2. regression-table.txt: Summarizes the results of OLS regression, modelling *nyt* on a number of covariates.

### More Information

For more information, contact Charlotte Hill at charlottehill@berkeley.edu
