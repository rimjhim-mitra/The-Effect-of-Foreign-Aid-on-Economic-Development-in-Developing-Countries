#clean environment
rm(list=ls())

#libraries
library(readr)
library(reshape2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plm)
library(car)
library(caret)
library(glmnet)

#read + format data
data<- read_csv("~/Downloads/data3.csv")
melted_data <- melt(data, id.vars = c("Country Name", "Series Name"), variable.name = "Year", value.name = "Value")

# aggregate duplicate rows
melted_data_aggregated <- melted_data %>%
  group_by(`Country Name`, `Series Name`, Year) %>%
  summarize(Value = mean(Value, na.rm = TRUE))

# wide data
data_wide <- spread(melted_data_aggregated, key = "Series Name", value = Value)
data_wide$Country_Name <- as.character(data_wide$`Country Name`)

#view column names
names(data_wide)

#list of sub-saharan african countries
sub_saharan_africa <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
                        "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", 
                        "Democratic Republic of the Congo", "Republic of the Congo", "Djibouti", 
                        "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", 
                        "The Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", 
                        "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", 
                        "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", 
                        "Nigeria", "Rwanda", "São Tomé and Príncipe", "Senegal", "Seychelles", 
                        "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", 
                        "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")

#assign 1 to sub-saharan africa coutrnies + 0 to rest
data_wide <- data_wide %>%
  mutate(sub_saharan = ifelse(Country_Name %in% sub_saharan_africa, 1, 0))

#subset data to include 50 other developing countries
all_developing <- c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                        "Argentina", "Armenia", "Azerbaijan", "Bangladesh", "Barbados", 
                        "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", 
                        "Botswana", "Brazil", "Bulgaria", "Burkina Faso", "Burundi", 
                        "Cambodia", "Cameroon", "Cape Verde", "Central African Republic", 
                        "Chad", "China", "Colombia", "Comoros", "Congo", 
                        "Costa Rica", "Croatia", "Cuba", "Cyprus", "Democratic Republic of the Congo", 
                        "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", 
                        "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", 
                        "Fiji", "Gabon", "Gambia", "Georgia", "Ghana", 
                        "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", 
                        "Haiti", "Honduras", "India", "Indonesia", "Iran", 
                        "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kenya", 
                        "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", 
                        "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", 
                        "Malawi", "Malaysia", "Maldives", "Mali", "Marshall Islands", 
                        "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", 
                        "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", 
                        "Namibia", "Nauru", "Nepal", "Nicaragua", "Niger", 
                        "Nigeria", "North Korea", "North Macedonia", "Oman", "Pakistan", 
                        "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", 
                        "Peru", "Philippines", "Qatar", "Romania", "Russia", 
                        "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                        "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", 
                        "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa", 
                        "South Korea", "South Sudan", "Sri Lanka", "Sudan", "Suriname", 
                        "Syria", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", 
                        "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", 
                        "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", 
                        "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", 
                        "Yemen", "Zambia", "Zimbabwe")


sub_data <- subset(data_wide, Country_Name %in% all_developing)

#time lag
sub_data <- sub_data %>%
  arrange(Country_Name, Year) %>%
  group_by(Country_Name) %>%
  mutate(foreign_aid_lag1 = lag(`Net official development assistance and official aid received (current US$)`, 1),
         foreign_aid_lag2 = lag(`Net official development assistance and official aid received (current US$)`, 2)) %>%
  ungroup()  

# Perform regression analysis with all available variables as controls
model1 <- lm(`GDP per capita growth (annual %)` ~ `Net official development assistance and official aid received (current US$)` + 
               `Central government debt, total (% of GDP)` + 
               `Control of Corruption: Estimate` + 
               `Ease of doing business score (0 = lowest performance to 100 = best performance)` + 
               `Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)` + 
               `General government final consumption expenditure (current US$)` + 
               `Gini index` + 
               `Political Stability and Absence of Violence/Terrorism: Estimate` + 
               `Population growth (annual %)`,
             data = sub_data)

# View summary of regression results
summary(model1)


model2 <- lm(`Mortality rate, infant (per 1,000 live births)` ~ `Net official development assistance and official aid received (current US$)` + 
               `Central government debt, total (% of GDP)` + 
               `Control of Corruption: Estimate` + 
               `Ease of doing business score (0 = lowest performance to 100 = best performance)` + 
               `Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)` + 
               `General government final consumption expenditure (current US$)` + 
               `Gini index` + 
               `Political Stability and Absence of Violence/Terrorism: Estimate` + 
               `Population growth (annual %)` + 
               `Poverty gap at $2.15 a day (2017 PPP) (%)`,
             data = sub_data)

# View summary of regression results
summary(model2)

model3 <- lm(`Poverty gap at $2.15 a day (2017 PPP) (%)` ~ `Net official development assistance and official aid received (current US$)` + 
               `Central government debt, total (% of GDP)` + 
               `Control of Corruption: Estimate` + 
               `Ease of doing business score (0 = lowest performance to 100 = best performance)` + 
               `Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)` + 
               `General government final consumption expenditure (current US$)` + 
               `Gini index` + 
               `Political Stability and Absence of Violence/Terrorism: Estimate` + 
               `Population growth (annual %)`,
             data = sub_data)

# View summary of regression results
summary(model3)

names(sub_data)
