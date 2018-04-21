# This code takes the dataset created in 01_categorize_data.R and uses it for analysis and visualization. 
# We find below that there are meaningful differences between the occupations of 2015 and 2017 Seattle donors
# and additional meaingful differences between 2017 voucher and nonvoucher donors.


############################################### 1. SET UP DATA ###############################################
# Clear workspace
rm(list = ls())

# Load packages
library(pacman)
p_load(dplyr, lubridate, taRifx, ggplot2, gmodels, readr)

# Set working directory
setwd("/Users/charlottehill/Documents/PhD/Spring 2018/Seattle voucher research")

# Read in final contributor data for analysis
seattle_occ <- read_csv("final_data.csv")
# View(seattle_occ)

############################################### 2. SUBSET DATA ###############################################

# Create subset of 2017 donors with identified occupation types:
seattle_occ_2017 <- subset(seattle_occ, is.na(OccupationType)==FALSE & Cycle==2017)

# Turn Occupation Type and Voucher into factors
seattle_occ_2017$OccupationType <- as.factor(seattle_occ_2017$OccupationType)
seattle_occ_2017$Voucher <- as.factor(seattle_occ_2017$Voucher)

# Reorder Occupation Type by total amount given
seattle_occ_2017$OccupationType <- reorder(seattle_occ_2017$OccupationType, seattle_occ_2017$Amount, sum)

# Do the same things for the 2015 data subset:
seattle_occ_2015 <- subset(seattle_occ, is.na(OccupationType)==FALSE & Cycle==2015)
seattle_occ_2015$OccupationType <- as.factor(seattle_occ_2015$OccupationType)
seattle_occ_2015$Voucher <- as.factor(seattle_occ_2015$Voucher)
seattle_occ_2015$OccupationType <- reorder(seattle_occ_2015$OccupationType, seattle_occ_2015$Amount, sum)

############################################### 3. CREATE OCCUPATION DUMMIES ###############################################

# Create a series of dummies for each occupation type
# First, create the dummy and set it to zero.
seattle_occ_2017$OtherPrivSector <- 0 

# Use the information in the OccupationType column to determine whether the voucher should be a 0 or 1.
for (i in 1:length(seattle_occ_2017$OtherPrivSector)) {
  if (seattle_occ_2017$OccupationType[i]=="Other Private-Sector") { # If the occupation type is "Other Private-Sector"...
    seattle_occ_2017$OtherPrivSector[i] <- 1 # ...then replace the 0 in this column with a 1.
  }
}

# Do the same for the other occupation types. First, create the dummy and set it to zero. 
# Then, use the information in the OccupationType column to determine whether the voucher should be a 0 or a 1.
seattle_occ_2017$Retired <- 0
for (i in 1:length(seattle_occ_2017$Retired)) {
  if (seattle_occ_2017$OccupationType[i]=="Retired") {
    seattle_occ_2017$Retired[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Service <- 0
for (i in 1:length(seattle_occ_2017$Service)) {
  if (seattle_occ_2017$OccupationType[i]=="Service & Nonprofit") {
    seattle_occ_2017$Service[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Lawyers <- 0
for (i in 1:length(seattle_occ_2017$Lawyers)) {
  if (seattle_occ_2017$OccupationType[i]=="Lawyers") {
    seattle_occ_2017$Lawyers[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Government <- 0
for (i in 1:length(seattle_occ_2017$Government)) {
  if (seattle_occ_2017$OccupationType[i]=="Government") {
    seattle_occ_2017$Government[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Labor <- 0
for (i in 1:length(seattle_occ_2017$Labor)) {
  if (seattle_occ_2017$OccupationType[i]=="Blue-Collar & Labor") {
    seattle_occ_2017$Labor[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Technology <- 0
for (i in 1:length(seattle_occ_2017$Technology)) {
  if (seattle_occ_2017$OccupationType[i]=="Technology") {
    seattle_occ_2017$Technology[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Health <- 0
for (i in 1:length(seattle_occ_2017$Health)) {
  if (seattle_occ_2017$OccupationType[i]=="Health") {
    seattle_occ_2017$Health[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$RealEstate <- 0
for (i in 1:length(seattle_occ_2017$RealEstate)) {
  if (seattle_occ_2017$OccupationType[i]=="Real Estate") {
    seattle_occ_2017$RealEstate[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Unemployed <- 0
for (i in 1:length(seattle_occ_2017$Unemployed)) {
  if (seattle_occ_2017$OccupationType[i]=="Unemployed") {
    seattle_occ_2017$Unemployed[i] <- 1
  }
}

# Create the dummy and set it to zero. Then use the information in OccupationType to determine whether Voucher should be 0 or 1.
seattle_occ_2017$Finance <- 0
for (i in 1:length(seattle_occ_2017$Finance)) {
  if (seattle_occ_2017$OccupationType[i]=="Finance & Insurance") {
    seattle_occ_2017$Finance[i] <- 1
  }
}

# We now have dummy variables for every occupation!

############################################### 4. RUN CALCULATIONS & STATISTICAL TESTS ###############################################

# Identify average amounts given in 2015 and 2017:
summary(seattle_occ_2015$Amount)
summary(seattle_occ_2017$Amount)

# Create simple crosstab of occupation type and voucher dummy:
table(seattle_occ_2017$OccupationType, seattle_occ_2017$Voucher)

# Create more informative crosstab:
CrossTable(seattle_occ_2017$OccupationType, seattle_occ_2017$Voucher, prop.t = FALSE, prop.chisq = FALSE)

# Run regression model, with voucher usage as dependent variable and occupation dummies as independent variables:  
seattle_occ_2017$Voucher <- as.numeric(seattle_occ_2017$Voucher)
reg_dummies <- lm(Voucher~OtherPrivSector + Retired + Labor + Service + Government + Technology + Health + RealEstate + Unemployed + Finance, seattle_occ_2017)

#View the results of the regression model
summary(reg_dummies)

############################################### 5. PLOT 2015 DATA ###############################################

# Plot 2015 donation dollars by occupation type:

    # First, reorder Occupation Type by total amount given
    seattle_occ_2015$OccupationType <- reorder(seattle_occ_2015$OccupationType, seattle_occ_2015$Amount, sum)

    # Using dataset "seattle_occ_2015", set x to OccupationType and y to Amount, fill by Voucher level, and label the levels.
    plot_amount_by_occupation_2015 <- ggplot(seattle_occ_2015, aes(x = OccupationType, y = Amount, fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) +
      geom_bar(stat = "identity") +
      scale_fill_discrete(name = "Contribution Type") + # label the legend
      labs(title = "2015 Non-Voucher Dollars Contributed by Occupation Type", # label the axes
           x = "Occupation Type", y = "Total Dollars Contributed") +
      scale_y_continuous(labels = scales::comma, limits = c(0, 200000)) + # Create a common scale so we can compare this plot to a similar one for 2017
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

    # View the plot:
    plot_amount_by_occupation_2015

# Plot 2015 number of donations by occupation type:
    # Reorder Occupation Type by # of contributions given, using Cycle because it's a standard number for each observation:
    seattle_occ_2015$OccupationType <- reorder(seattle_occ_2015$OccupationType, seattle_occ_2015$Cycle, sum)

    # Using dataset "seattle_occ_2015", set x to OccupationType, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2015, aes(OccupationType, fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) + 
      geom_bar() +
      scale_fill_discrete(name = "Contribution Type") + # label the legend
      labs(title = "2015 Voucher vs. Non-Voucher Contributions by Occupation Type", 
           x = "Occupation Type", y = "Number of Contributions") + #label the axes
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

# Plot percentage of donations by occupation type in 2015:
    # Reorder data (using Cycle because it's the same value for each observation)
    seattle_occ_2015$OccupationType <- reorder(seattle_occ_2015$OccupationType, as.numeric(seattle_occ_2015$Cycle), sum)

    # Using dataset "seattle_occ_2015", set x to OccupationType, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2015, aes(OccupationType, fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "stack") + # change from count to proportion
      scale_y_continuous(labels = scales::percent) + # convert from proportion to percent
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "2015 Voucher vs. Non-Voucher Contributions by Occupation Type", # label axes
           x = "Occupation Type", y = "Percentage of Contributions") +
      theme_minimal() + #assign theme
      coord_flip() # flip coordinates

############################################### 6. PLOT 2017 DATA ###############################################

# Re-create 2017 dataset with voucher as a factor: 
seattle_occ_2017 <- subset(seattle_occ, is.na(OccupationType)==FALSE & Cycle==2017)
seattle_occ_2017$Voucher <- as.factor(seattle_occ_2017$Voucher)

# Plot 2017 donation dollars by occupation type:
    # Reorder Occupation Type by total amount given
    seattle_occ_2017$OccupationType <- reorder(seattle_occ_2017$OccupationType, seattle_occ_2017$Amount, sum)
    
    # Using dataset "seattle_occ_2018", set x to OccupationType and y to Amount, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2017, aes(x = OccupationType, y = Amount, fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) +  
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "2017 Voucher vs. Non-Voucher Dollars Contributed by Occupation Type", # label axes
           x = "Occupation Type", y = "Total Dollars Contributed") +
      scale_y_continuous(labels = scales::comma, limits = c(0, 200000)) + # create common scale so we can compare to 2015 plot
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

# Plot 2017 number of donations by occupation type:
    # Reorder Occupation Type by number of donations given (using Cycle because it's the same number for each observation):
    seattle_occ_2017$OccupationType <- reorder(seattle_occ_2017$OccupationType, seattle_occ_2017$Cycle, sum)

    # Using dataset "seattle_occ_2017", set x to OccupationType, fill by Voucher level, and label the levels.
    plot_donations_by_occupation_2017 <- ggplot(seattle_occ_2017, aes(OccupationType, fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) + 
      geom_bar() +
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "2017 Voucher vs. Non-Voucher Contributions by Occupation Type", # label axes
           x = "Occupation Type", y = "Number of Contributions") +
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates
    plot_donations_by_occupation_2017

# Plot percentage of donations by occupation type in 2017:
    # Reorder data (using Cycle because it's the same value for each observation)
    seattle_occ_2017$OccupationType <- reorder(seattle_occ_2017$OccupationType, as.numeric(seattle_occ_2017$Cycle), sum)

    # Using dataset "seattle_occ_2017", set x to OccupationType, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2017, aes(OccupationType, fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "stack") + # change from count to proportion
      scale_y_continuous(labels = scales::percent) + # convert from proportion to percent
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "2017 Voucher vs. Non-Voucher Contributions by Occupation Type", # label axes
           x = "Occupation Type", y = "Percentage of Contributions") +
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

# Create subcategories for voucher and non-voucher contributions in 2017
seattle_occ_2017_vouch <- subset(seattle_occ_2017, Voucher==1)
seattle_occ_2017_cash <- subset(seattle_occ_2017, Voucher==0)

# Plot voucher dollars by occupation type:

    # Reorder OccupationType by total dollars donated
    seattle_occ_2017_vouch$OccupationType <- reorder(seattle_occ_2017_vouch$OccupationType, seattle_occ_2017_vouch$Amount, sum) 

    # Using dataset "seattle_occ_2017_vouch", set x to OccupationType and y to Amount, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2017_vouch, aes(OccupationType, Amount, fill = factor(Voucher, levels = c(1), labels = c("Voucher")))) +
      geom_bar(stat = "identity") +
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "Voucher Dollars Contributed by Occupation Type", # label axes
           x = "Occupation Type", y = "Total Dollars Contributed") +
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

# Plot non-voucher dollars by occupation type: 

    # Reorder OccupationType by total dollars donated
    seattle_occ_2017_cash$OccupationType <- reorder(seattle_occ_2017_cash$OccupationType, seattle_occ_2017_cash$Amount, sum)
    
    # Using dataset "seattle_occ_2017_cash", set x to OccupationType and y to Amount, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2017_cash, aes(OccupationType, Amount, fill = factor(Voucher, levels = c(0), labels = c("Non-Voucher")))) +
      geom_bar(stat = "identity") +
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "Non-Voucher Dollars Contributed by Occupation Type", # label axes
           x = "Occupation Type", y = "Total Dollars Contributed") +
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

# Plot number of voucher contributions by occupation type
    # Create column of 1s so we can sum up total number of contributions 
    seattle_occ_2017_vouch$val <- 1

    # Reorder Occupation Type by total number of contributions
    seattle_occ_2017_vouch$OccupationType <- reorder(seattle_occ_2017_vouch$OccupationType, seattle_occ_2017_vouch$val, sum)

    # Using dataset "seattle_occ_2017_vouch", set x to OccupationType, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2017_vouch, aes(x = OccupationType, fill = factor(Voucher, levels = c(1), labels = c("Voucher")))) +
      geom_histogram(stat = "count") +
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "Voucher Contributions by Occupation Type", # label axes
           x = "Occupation Type", y = "Number of Contributions") +
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

# Plot number of non-voucher contributions by occupation type
    # Create column of 1s so we can sum up total number of contributions 
    seattle_occ_2017_cash$val <- 1
    
    # Reorder Occupation Type by total number of contributions
    seattle_occ_2017_cash$OccupationType <- reorder(seattle_occ_2017_cash$OccupationType, seattle_occ_2017_cash$val, sum)
 
    # Using dataset "seattle_occ_2017_cash", set x to OccupationType, fill by Voucher level, and label the levels.
    ggplot(seattle_occ_2017_cash, aes(x = OccupationType, fill = factor(Voucher, levels = c(0), labels = c("Non-Voucher")))) +
      geom_histogram(stat = "count") +
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "Non-Voucher Contributions by Occupation Type", # label axes
           x = "Occupation Type", y = "Number of Contributions") +
      theme_minimal() + # assign theme
      coord_flip() # flip coordinates

############################################### 7. PLOT COMPARISONS OF 2015 & 2017 ###############################################

# Set up data by turning Occupation Type, Cycle, and Voucher into factors
seattle_occ$OccupationType <- as.factor(seattle_occ$OccupationType)
seattle_occ$Cycle <- as.factor(seattle_occ$Cycle)
seattle_occ$Voucher <- as.factor(seattle_occ$Voucher)

# Plot contributor occupations by election cycle

    # Using dataset "seattle_occ", set x to OccupationType and fill by Cycle
    ggplot(data = seattle_occ, aes(x = OccupationType, fill = Cycle)) + 
      geom_bar(position = "dodge") + # stack bars next to each other
      scale_fill_discrete(name = "Cycle") + # label legend
      labs(title = "Contributor Occupations By Election Cycle", subtitle = "2015 vs. 2017",
           x = "Occupation", y = "Number of Contributions") + # label axes and title
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) + # impose character breaks for the x axis
      theme_minimal() # apply theme

# Plot number of voucher vs. non-voucher contributions by occupation type and cycle

    # Using dataset "seattle_occ," set x to Cycle, fill by Voucher level, and label Voucher levels
    ggplot(data = seattle_occ, aes(x = Cycle, fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) + 
      geom_bar(stat = "count", position = "stack") + # stack bars on top of each other
      facet_wrap( ~ OccupationType) + # facet plot
      scale_fill_discrete(name = "Contribution Type") + #label legend
      labs(title = "Number of Voucher vs. Non-Voucher Contributions by Occupation Type", subtitle = "2015 vs. 2017",
           x = "Election Cycle", y = "Number of Contributions") + # label axes and title
      theme_minimal() # apply theme

# Plot total voucher vs. non-voucher dollars contributed by occupation type

    # using dataset "seattle_occ," set x to Cycle and y to Amount, fill by Voucher level, and label Voucher levels
    ggplot(data = seattle_occ, aes(x = Cycle, y = Amount, 
                                   fill = factor(Voucher, levels = c(1, 0), labels = c("Voucher", "Non-Voucher")))) + 
      geom_bar(stat = "identity", position = "stack") + # stack bars on top of each other
      facet_wrap( ~ OccupationType) + # facet plot
      scale_fill_discrete(name = "Contribution Type") + # label legend
      labs(title = "Total Voucher vs. Non-Voucher Dollars Contributed by Occupation Type", subtitle = "2015 vs. 2017",
           x = "Election Cycle", y = "Total Dollars Contributed") + # label axes and title
      theme_minimal() # apply theme
