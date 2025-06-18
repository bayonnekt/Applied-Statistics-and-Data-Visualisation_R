# 3.Preparation and Exploration of Data Set 
# 3.1.Load required libraries 
install.packages("mosaic")
install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("caret")
install.packages("TTR")
install.packages("forecast")
install.packages("stats")
install.packages("e1071")
install.packages("digest")
install.packages("dplyr")
install.packages("zoo")
install.packages("xts")

library(mosaic)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(ggplot2)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(caret)
library(stats)
library(TTR)
library(forecast)
library(e1071)
library(digest)
library(dplyr)
library(zoo)
library(xts)

# 3.2.Load & renaming the data set
data <- read.csv("Inflation.csv") %>%
  rename(CO2Emissions = 'CO2.emissions..kt...EN.ATM.CO2E.KT.',
         CPI = 'Consumer.price.index..2010...100...FP.CPI.TOTL.',
         InflationConsumerPrices = 'Inflation..consumer.prices..annual.....FP.CPI.TOTL.ZG.',
         LendingInterestRates = 'Lending.interest.rate......FR.INR.LEND.',
         UnemploymentTotal = 'Unemployment..total....of.total.labor.force...national.estimate...SL.UEM.TOTL.NE.ZS.',
         Expense = 'Expense....of.GDP...GC.XPN.TOTL.GD.ZS.',
         Interest = 'Interest.payments....of.expense...GC.XPN.INTP.ZS.',
         GDPGrowth = 'GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.',
         GNIGrowth = 'GNI.growth..annual.....NY.GNP.MKTP.KD.ZG.',
         Exports = 'Exports.of.goods.and.services....of.GDP...NE.EXP.GNFS.ZS.',
         Imports = 'Imports.of.goods.and.services....of.GDP...NE.IMP.GNFS.ZS.',
         InterestPayments = 'Interest.payments....of.expense...GC.XPN.INTP.ZS.',
         AirTransportPassengerCarried = 'Air.transport..passengers.carried..IS.AIR.PSGR.',
         Country = 'Country.Name')

# 3.2.Explore the data 
# Display the dataset structure 
str(data)
# Summary Statistics 
summary(data)
# Check the dataset first few rows
head(data)
# Check the dataset last few rows
tail(data)

# 3.3.Remove duplicate(s) 
data_unique <- unique(data)

# 3.4. Check missing values 
any(is.na(data))

# 3.5.Summarise missing values  
colSums(is.na(data))

# 3.6.Replace missing values & '..' with NA  
data[data == '..'] <- NA

# 3.7.Convert columns to numerics
data$CO2Emissions <- as.numeric(data$CO2Emissions)
data$CPI <- as.numeric(data$CPI)
data$InflationConsumerPrices <- as.numeric(data$InflationConsumerPrices)
data$LendingInterestRates <- as.numeric(data$LendingInterestRates)
data$UnemploymentTotal <- as.numeric(data$UnemploymentTotal)
data$Expense <- as.numeric(data$Expense)
data$InterestPayments <- as.numeric(data$InterestPayments)
data$GDPGrowth <- as.numeric(data$GDPGrowth)
data$GNIGrowth <- as.numeric(data$GNIGrowth)
data$Exports <- as.numeric(data$Exports)
data$Imports <- as.numeric(data$Imports)
data$AirTransportPassengerCarried <- as.numeric(data$AirTransportPassengerCarried)

# 3.8.Replace missing values with the mean
data$CO2Emissions = ifelse(is.na(data$CO2Emissions), ave(data$CO2Emissions, FUN = function(x) mean(x, na.rm = TRUE)),data$CO2Emissions)
data$CPI = ifelse(is.na(data$CPI), ave(data$CPI, FUN = function(x) mean(x, na.rm = TRUE)),data$CPI)
data$InflationConsumerPrices = ifelse(is.na(data$InflationConsumerPrices), ave(data$InflationConsumerPrices, FUN = function(x) mean(x, na.rm = TRUE)),data$InflationConsumerPrices)
data$LendingInterestRates = ifelse(is.na(data$LendingInterestRates), ave(data$LendingInterestRates, FUN = function(x) mean(x, na.rm = TRUE)),data$LendingInterestRates)
data$UnemploymentTotal = ifelse(is.na(data$UnemploymentTotal), ave(data$UnemploymentTotal, FUN = function(x) mean(x, na.rm = TRUE)),data$UnemploymentTotal)
data$Expense = ifelse(is.na(data$Expense), ave(data$Expense, FUN = function(x) mean(x, na.rm = TRUE)),data$Expense)
data$InterestPayments = ifelse(is.na(data$InterestPayments), ave(data$InterestPayments, FUN = function(x) mean(x, na.rm = TRUE)),data$InterestPayments)
data$GDPGrowth = ifelse(is.na(data$GDPGrowth), ave(data$GDPGrowth, FUN = function(x) mean(x, na.rm = TRUE)),data$GDPGrowth)
data$GNIGrowth = ifelse(is.na(data$GNIGrowth), ave(data$GNIGrowth, FUN = function(x) mean(x, na.rm = TRUE)),data$GNIGrowth)
data$Exports = ifelse(is.na(data$Exports), ave(data$Exports, FUN = function(x) mean(x, na.rm = TRUE)),data$Exports)
data$Imports = ifelse(is.na(data$Imports), ave(data$Imports, FUN = function(x) mean(x, na.rm = TRUE)),data$Imports)
data$AirTransportPassengerCarried = ifelse(is.na(data$AirTransportPassengerCarried), ave(data$AirTransportPassengerCarried, FUN = function(x) mean(x, na.rm = TRUE)),data$AirTransportPassengerCarried)

# 3.9.Delete columns & rows
data <- data[-215,]
data <- data[-214,]
data <- data[-213,]
data <- data[-212,]
data <- data[-211,]
data <- data[,-4]
data <- data[,-2]

# 3.10.Standardise the data set
data$standardized_variable <- scale(data$UnemploymentTotal)
data$standardized_variable <- scale(data$CO2Emissions)
data$standardized_variable <- scale(data$Expense)
data$standardized_variable <- scale(data$Exports)
data$standardized_variable <- scale(data$Imports)
data$standardized_variable <- scale(data$InterestPayments)
data$standardized_variable <- scale(data$InflationConsumerPrices)
data$standardized_variable <- scale(data$AirTransportPassengerCarried)
data$standardized_variable <- scale(data$GNIGrowth)
data$standardized_variable <- scale(data$GDPGrowth)
data$standardized_variable <- scale(data$CPI)
data$standardized_variable <- scale(data$LendingInterestRates)

# 4.1.Comprehensive Descriptive Statistical Analysis 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

result <- data %>%
  group_by(Country) %>%
  summarise(Mode_CPI = Mode(CPI), Mode_Unemployment = Mode(UnemploymentTotal), 
            Mode_Expense = Mode(Expense), Mode_InterestPayment = Mode(InterestPayments), 
            Mode_AirTransportPassengerCarried = Mode(AirTransportPassengerCarried), 
            Mode_LendingInterestRates = Mode(LendingInterestRates), 
            Mode_InflationConsumerPrices = Mode(InflationConsumerPrices),
            Mode_CO2Emissions = Mode(CO2Emissions), Mode_Imports = Mode(Imports),
            Mode_Exports = Mode(Exports), Mode_GNIGrowth = Mode(GNIGrowth), Mode_GDPGrowth = Mode(GDPGrowth))

mode_values <- sapply(data, Mode)


result <- data %>%
  group_by(Country) %>%
  summarise(Skewness_CPI = skewness(CPI), Skewness_Unemployment = skewness(UnemploymentTotal), 
            Skewness_Expense = skewness(Expense), Skewness_InterestPayment = skewness(InterestPayments),
            Skewness_AirTransportPassengerCarried = skewness(AirTransportPassengerCarried), 
            Skewness_LendingInterestRates = skewness(LendingInterestRates),
            Skewness_InflationConsumerPrices = skewness(InflationConsumerPrices), 
            Skewness_CO2Emissions = skewness(CO2Emissions),
            Skewness_Exports = skewness(Exports), Skewness_Imports = skewness(Imports),
            Skewness_GNIGrowth = skewness(GNIGrowth), Skewness_GDPGrowth = skewness(GDPGrowth))

 grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_CPI = mean(CPI),
    Median_CPI = median(CPI),
    Mode_CPI = Mode(CPI),
    SD_CPI = sd(CPI),
    Skewness_CPI = skewness(CPI),
    Kurtosis_CPI = kurtosis(CPI)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_Expense = mean(Expense),
    Median_Expense = median(Expense),
    Mode_Expense = Mode(Expense),
    SD_Expense = sd(Expense),
    Skewness_Expense = skewness(Expense),
    Kurtosis_Expense = kurtosis(Expense)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_InterestPayment = mean(InterestPayments),
    Median_InterestPayment = median(InterestPayments),
    Mode_InterestPayment = Mode(InterestPayments),
    SD_InterestPayments = sd(InterestPayments),
    Skewness_InterestPayments = skewness(InterestPayments),
    Kurtosis_InterestPayments = kurtosis(InterestPayments)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_UnemploymentTotal = mean(UnemploymentTotal),
    Median_UnemploymentTotal = median(UnemploymentTotal),
    Mode_UnemploymentTotal = Mode(UnemploymentTotal),
    SD_UnemploymentTotal = sd(UnemploymentTotal),
    Skewness_UnemploymentTotal = skewness(UnemploymentTotal),
    Kurtosis_UnemploymentTotal = kurtosis(UnemploymentTotal)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_Exports = mean(Exports),
    Median_Exports = median(Exports),
    Mode_Exports = Mode(Exports),
    SD_Exports = sd(Exports),
    Skewness_Exports = skewness(Exports),
    Kurtosis_Exports = kurtosis(Exports)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_Imports = mean(Imports),
    Median_Imports = median(Imports),
    Mode_Imports = Mode(Imports),
    SD_Imports = sd(Imports),
    Skewness_Imports = skewness(Imports),
    Kurtosis_Imports = kurtosis(Imports)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_GNIGrowth = mean(GNIGrowth),
    Median_GNIGrowth = median(GNIGrowth),
    Mode_GNIGrowth = Mode(GNIGrowth),
    SD_GNIGrowth = sd(GNIGrowth),
    Skewness_GNIGrowth = skewness(GNIGrowth),
    Kurtosis_GNIGrowth = kurtosis(GNIGrowth)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_GDPGrowth = mean(GDPGrowth),
    Median_GDPGrowth = median(GDPGrowth),
    Mode_GDPGrowth = Mode(GDPGrowth),
    SD_GDPGrowth = sd(GDPGrowth),
    Skewness_GDPGrowth = skewness(GDPGrowth),
    Kurtosis_GDPGrowth = kurtosis(GDPGrowth)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_AirTransportPassengerCarried = mean(AirTransportPassengerCarried),
    Median_AirTransportPassengerCarried = median(AirTransportPassengerCarried),
    Mode_AirTransportPassengerCarried = Mode(AirTransportPassengerCarried),
    SD_AirTransportPassengerCarried = sd(AirTransportPassengerCarried),
    Skewness_AirTransportPassengerCarried = skewness(AirTransportPassengerCarried),
    Kurtosis_AirTransportPassengerCarried = kurtosis(AirTransportPassengerCarried)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_CO2Emissions = mean(CO2Emissions),
    Median_CO2Emissions = median(CO2Emissions),
    Mode_CO2Emissions = Mode(CO2Emissions),
    SD_CO2Emissions = sd(CO2Emissions),
    Skewness_CO2Emissions = skewness(CO2Emissions),
    Kurtosis_CO2Emissions = kurtosis(CO2Emissions)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_LendingInterestRates = mean(LendingInterestRates),
    Median_LendingInterestRates = median(LendingInterestRates),
    Mode_LendingInterestRates = Mode(LendingInterestRates),
    SD_LendingInterestRates = sd(LendingInterestRates),
    Skewness_LendingInterestRates = skewness(LendingInterestRates),
    Kurtosis_LendingInterestRates = kurtosis(LendingInterestRates)
  )
print(grouped_stats)

grouped_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_InflationConsumerPrices = mean(InflationConsumerPrices),
    Median_InflationConsumerPrices = median(InflationConsumerPrices),
    Mode_InflationConsumerPrices = Mode(InflationConsumerPrices),
    SD_InflationConsumerPrices = sd(InflationConsumerPrices),
    Skewness_InflationConsumerPrices = skewness(InflationConsumerPrices),
    Kurtosis_InflationConsumerPrices = kurtosis(InflationConsumerPrices)
  )
print(grouped_stats)


# 4.2.Correlation Analysis for the Indicators and Evaluate the Result
set.seed(123)
cleaned_data <- data.frame(
  InterestPayments = rnorm(210),
  CPI = rnorm(210),
  InflationConsumerPrices = rnorm(210),
  LendingInterestRates = rnorm(210),
  GDPGrowth = rnorm(210),
  GNIGrowth = rnorm(210),
  CO2Emissions = rnorm(210),
  UnemploymentTotal = rnorm(210),
  Imports = rnorm(210),
  Exports = rnorm(210),
  AirTransportPassengerCarried = rnorm(210),
  Expense = rnorm(210)
)

# Calculate correlation matrix
cor_matrix <- cor(data)

# Extract correlations with the dependent variable ("Expense")
cor_with_expense <- cor_matrix[, "Expense"]
# Print the correlation values
print(cor_with_expense)

cor_with_CPI <- cor_matrix[, "CPI"]
# Print the correlation values
print(cor_with_CPI)

cor_with_CO2Emissions <- cor_matrix[, "CO2Emissions"]
# Print the correlation values
print(cor_with_CO2Emissions)

cor_with_InterestPayments <- cor_matrix[, "InterestPayments"]
# Print the correlation values
print(cor_with_InterestPayments)

cor_with_InflationConsumerPrices <- cor_matrix[, "InflationConsumerPrices"]
# Print the correlation values
print(cor_with_InflationConsumerPrices)

cor_with_UnemploymentTotal <- cor_matrix[, "UnemploymentTotal"]
# Print the correlation values
print(cor_with_UnemploymentTotal)

cor_with_Exports <- cor_matrix[, "Exports"]
# Print the correlation values
print(cor_with_Exports)

cor_with_Imports <- cor_matrix[, "Imports"]
# Print the correlation values
print(cor_with_Imports)

cor_with_GNIGrowth <- cor_matrix[, "GNIGrowth"]
# Print the correlation values
print(cor_with_GNIGrowth)

cor_with_GDPGrowth <- cor_matrix[, "GDPGrowth"]
# Print the correlation values
print(cor_with_GDPGrowth)

cor_with_LendingInterestRates <- cor_matrix[, "LendingInterestRates"]
# Print the correlation values
print(cor_with_LendingInterestRates)

cor_with_AirTransportPassengerCarried <- cor_matrix[, "AirTransportPassengerCarried"]
# Print the correlation values
print(cor_with_AirTransportPassengerCarried)

cols_to_exclude <- c("Country", "Time")
data_subset <- data[, !(names(data) %in% cols_to_exclude)]
cor_matrix <- cor(data_subset)
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 40)

# 4.3.Two Hypotheses Testing Related to the Objectives and Test Them
# Hypothesis 
# Null Hypothesis (HO): There is no significant relationship the dependend and the combination of independent variables
# Alternative Hypothesis (H1): There is a significant relationship between the depend
hist(InterestPayments, main="Histogram of InterestPayments")
hist(CPI, main="Histogram of CPI")
hist(InflationConsumerPrices, main="Histogram of InflationConsumerPrices")
hist(UnemploymentTotal, main="Histogram of Unemployment")

# The histogram has a pretty much normal distribution shape, then the MANOVA method
#Multivariate Analysis of Variance (MANOVA)
model_mano = manova(cbind(InterestPayments, InflationConsumerPrices, UnemploymentTotal, CPI) ~ Expense 
                    + AirTransportPassengerCarried + CO2Emissions + Exports + Imports + GNIGrowth + GDPGrowth + LendingInterestRates, data = data)
summary(model_mano)

# 4.4.Regression Analysis. 
model <- lm(cbind(InterestPayments, InflationConsumerPrices, UnemploymentTotal, CPI) ~ Expense 
            + AirTransportPassengerCarried + CO2Emissions + Exports + Imports + GNIGrowth + GDPGrowth + LendingInterestRates, data = data)
summary(model)

plot()

# 4.5.Time Series Analysis
countries <- c("United Kingdom", "United States", "Malta", "Morocco", "Venezuela, RB", "Kenya", "Singapore", "Cambodia", "France", "Russian Federation")

data_filtered <- data %>%
  filter(Country %in% countries, Time >= 1980 & Time <= 2022)

ggplot(data_filtered, aes(x = Time, y = InterestPayments, color = Country)) +
  geom_line() +
  labs(title = "Multivariable Time Series of InterestPayments by Country",
       x = "Time",
       y = "InterestPayments",
       color = "Country") +
  scale_x_continuous(breaks = c(1980, 1981, 1982, 1985, 1988, 1989, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2009, 2012, 2015, 2017, 2019, 2020, 2022)) +
  theme_minimal()


ggplot(data_filtered, aes(x = Time, y = CPI, color = Country)) +
  geom_line() +
  labs(title = "Multivariable Time Series of CPI by Country",
       x = "Time",
       y = "CPI",
       color = "Country") +
  scale_x_continuous(breaks = c(1980, 1981, 1982, 1985, 1988, 1989, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2009, 2012, 2015, 2017, 2019, 2020, 2022)) +
  theme_minimal()

ggplot(data_filtered, aes(x = Time, y = UnemploymentTotal, color = Country)) +
  geom_line() +
  labs(title = "Multivariable Time Series of UnemploymentTotal by Country",
       x = "Time",
       y = "UnemploymentTotal",
       color = "Country") +
  scale_x_continuous(breaks = c(1980, 1981, 1982, 1985, 1988, 1989, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2009, 2012, 2015, 2017, 2019, 2020, 2022)) +
  theme_minimal()

ggplot(data_filtered, aes(x = Time, y = InflationConsumerPrices, color = Country)) +
  geom_line() +
  labs(title = "Multivariable Time Series of InflationConsumerPrices by Country",
       x = "Time",
       y = "InflationConsumerPrices",
       color = "Country") +
  scale_x_continuous(breaks = c(1980, 1981, 1982, 1985, 1988, 1989, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2009, 2012, 2015, 2017, 2019, 2020, 2022)) +
  theme_minimal()


ggplot(data_filtered, aes(x = Time, y = InterestPayments, color = Country)) +
  geom_line() +
  geom_text(aes(label = Country), hjust = -0.1, vjust = -0.5, size = 3, color = "black") +
  labs(title = "Multivariable Time Series of InterestPayments by Country",
       x = "Time",
       y = "InterestPayments",
       color = "Country") +
  scale_x_continuous(breaks = c(1980, 1981, 1982, 1985, 1988, 1989, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2009, 2012, 2015, 2017, 2019, 2020, 2022)) +
  theme_minimal()

# Read the data from the CSV file
data <- read.csv("cleaned_data.csv")
