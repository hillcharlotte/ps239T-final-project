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

This is a group of visualizations and tables created with the final_analysis.R code. Additional visualizations and tables can be found within the final_analysis.R file.

1. crosstabs-occupation-voucher.png: Crosstabs showing the relationship between occupation types and contribution types in the 2017 election. Each occupation type is broken down further into nonvoucher contributions (0) and voucher contributions (1).
2. regression-results: Summarizes the results of an OLS regression, with voucher usage as the dependent variable and occupation dummies as the independent variables. The reference category is "lawyers," the most common occupational category during the 2015 election (pre-voucher implementation). The regression shows that certain occupations are statistically more likely than lawyers to give vouchers (as compared to non-voucher contributions).
3. PS239T Presentation.pdf: A longer version of the presentation I gave on my results in PS239T.
4. 2015/2015-nonvoucher-contributions-by-occupation-percent.jpeg: Plot of percent of 2015 (nonvoucher) contributions by occupation.
5. 2015/2015-nonvoucher-contributions-by-occupation.jpeg: Plot of number of 2015 (nonvoucher) contributions by occupation.
6. 2015/2015-nonvoucher-dollars-by-occupation.jpeg: Plot of number of 2015 (nonvoucher) dollars contributed by occupation.
7. 2017/2017-voucher-nonvoucher-contributions-by-occupation-percent.jpeg: Plot of percent of 2017 voucher vs. nonvoucher contributions by occupation.
8. 2017/2017-voucher-nonvoucher-contributions-by-occupation.jpeg: Plot of number of 2017 voucher vs. nonvoucher contributions by occupation.
9. 2017/2017-voucher-nonvoucher-dollars-by-occupation.jpeg: Plot of number of 2017 voucher vs. nonvoucher dollars contributed by occupation.
10. 2015_2017_Comparison/contributor_occupations_by_cycle.jpeg: Chart of contributor occupations in 2015 vs. 2017.
11. 2015_2017_Comparison/voucher_nonvoucher_contributions_by_occupation_cycle.jpeg: Plot of voucher vs. nonvoucher contributions by occupation in 2015 vs. 2017.
12. 2015_2017_Comparison/voucher_nonvoucher_dollars_by_occupation_cycle.jpeg: Plot of voucher vs. nonvoucher dollars contributed by occupation in 2015 vs. 2017.

### More Information

For more information, contact Charlotte Hill at charlottehill@berkeley.edu
