########################################
## Install packages and Load Libraries #
########################################
#install.packages("corrplot")
#install.packages("aplpack")
#install.packages("modes")
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("waffle")
#install.packages("ggdist")
#install.packages("tidyquant")
#install.packages("tidyverse")
#install.packages("tidyr")

library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(tidyr)
library(reshape2)

###########################################################
## Load the Countries Expense data set from the CSV file ##
###########################################################
govdata <- read_csv("../GovExpensePerCapita.csv") 

# Display the first few lines of the data
writeLines("\nCountries Expense data samples:")
print(head(govdata))

# View(govdata)

# Setup a tabular view of the data set
#gvt <- gvisTable(my.govdata)
# Plot (display) the tabular view
#plot(gvt)


##############
## Bar plots #
##############

my_colors <- c("#35945F", "#133358", "#B36161")
names <- c('Countries' ,'Year')
CountriesExpense_factored = govdata
CountriesExpense_factored[,names] <- lapply(CountriesExpense_factored[,names] , factor)
CountriesExpense_factored %>%
  ggplot(aes(x=Year, y=Expense, fill = Countries)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = my_colors) +
  ggtitle("Government Expense on Healthcare (per capita)")

###################
## Personal Data ##
###################
personaldata <- read_csv("../Countries Expense in USD/Recent Files/personalpercapita.csv") 

# Display the first few lines of the data
writeLines("\nCountries Expense data samples:")
print(head(personaldata))

# View(personaldata)


##############
## Bar plots #
##############

names <- c('Countries' ,'Year')
CountriesExpense_factored = personaldata
CountriesExpense_factored[,names] <- lapply(CountriesExpense_factored[,names] , factor)
CountriesExpense_factored %>%
  ggplot(aes(x=Year, y=Expense, fill = Countries)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = my_colors) +
  ggtitle("Personal Expense on Healthcare (per capita)")


# US Cost Breakdown
costs <- read_csv("../Comparisons/USBreakDown.csv")

# Display the first few lines of the data
writeLines("\nCountries Expense data samples:")
print(head(costs))

# Convert Year column to factor
costs$Year <- as.factor(costs$Year)

# Melt the data frame for easier plotting
costs_melted <- melt(costs, id.vars = "Year")

# Plot stacked bar chart
ggplot(costs_melted, aes(x = Year, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Cost", fill = "Expense Category") +
  theme_minimal() +
  ggtitle("U.S. Healthcare Cost Breakdown (per capita)")



# adding the Netherlands total cost
netherlands_costs <- read_csv("../Comparisons/TotalNetherlandsExpenseperCapita.csv")
netherlands_costs$Year <- as.factor(netherlands_costs$Year)

# Rename the columns to match the US data
names(netherlands_costs) <- c("Year", "value")

# Combine US and Netherlands data
combined_data <- bind_rows(
  mutate(costs_melted, Country = "US"),
  mutate(netherlands_costs, Country = "Netherlands")
)

countries <- c("US" = "#4273CA", "Netherlands" = "blue")
# Plotting with overlapping bars
ggplot(combined_data, aes(x = Year, y = value, fill = Country)) +
  geom_bar(data = filter(combined_data, Country == "US"), stat = "identity", alpha = 0.7) +  # US bars with transparency
  geom_bar(data = filter(combined_data, Country == "Netherlands"), stat = "identity", color = "blue", alpha = 0.4) +  # Netherlands bars as outline
  scale_fill_manual(values = countries, labels = c("Netherlands", "US")) +  # Color for US and Netherlands bars
  labs(x = "Year", y = "Cost (per capita)", fill = "Country") +
  ggtitle("Total Healthcare Costs: US vs Netherlands") +
  theme_minimal()




# Specifically Drug Prices

# US
rxUS <- costs_melted[33:40,]
rxUS <- rxUS %>% rename(U.S. = value)

# Netherlands
rxN <- read_csv("../Comparisons/NetherlandsDrugPrices.csv")
rxN$Year <- as.factor(rxN$Year)

data <- data.frame(rxUS, rxN, "Year")
data <- select(data, Year, U.S., Netherlands)
data2 <- melt(data, id.vars="Year")

my_colors <- c("U.S." = "#B36161", "Netherlands" = "#35945F")

ggplot(data2, aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Costs", fill = "Country") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  ggtitle("Pharmaceutical Costs (per capita)")
