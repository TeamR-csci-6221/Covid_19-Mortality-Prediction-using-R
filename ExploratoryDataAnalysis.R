read.csv("C:/Users/ASUS/OneDrive/Desktop/OUR WORLD COVID 19 DATASET.csv", header=FALSE)
dw <- read.csv("C:/Users/ASUS/OneDrive/Desktop/OUR WORLD COVID 19 DATASET.csv")
dim(dw)                                                                  
sapply(dw, class)
names(dw)

# Looking at the different locations
table(dw$location)

# Checking if columns have null values
null_col <- sapply(dw, function(x) any(is.na(x)))
print(null_col)

# Getting the sum of null values across each column
sum_null_col <- sapply(dw, function(x) sum(is.na(x)))
print(sum_null_col)

#Getting the cases of India
india_cases <- subset(dw, location == "India")
head(india_cases)
tail(india_cases)

#Getting the cases of United States
united_cases <- subset(dw, location == "United States")
head(united_cases)
tail(united_cases)

#Getting the cases of China
china_cases <- subset(dw, location == "China")
head(china_cases)
tail(china_cases)



#creating a line plot for total cases per day using the ggplot2 package. 
library(ggplot2)
# Convert 'date' column to Date format
dw$date <- as.Date(dw$date)

# Total cases per day line plot for the all location dw
ggplot(dw, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "Total Covid-19 Cases per day",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

# Total cases per day line plot for united_cases
ggplot(united_cases, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "Total Covid-19 Cases per Day in united states",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

# Total cases per day line plot for India_cases
ggplot(india_cases, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "Total Covid-19 Cases per Day in india",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

# Total cases per day line plot for china_cases
ggplot(china_cases, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "Total Covid-19 Cases per Day in China",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()



# Making a data frame for the last 30 days cases in India
last_30_days_india <- tail(india_cases,30)
print(last_30_days_india)
# Convert 'date' column to Date format
last_30_days_india$date <- as.Date(last_30_days_india$date)
# Total cases in the last 5 days line plot in India
ggplot(last_30_days_india, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "INDIA: Total Cases in Last 30 Days",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()


# Making a data frame for the last 30 day cases in United States
last_30_days_united <- tail(united_cases,30)
# Convert 'date' column to Date format
last_30_days_united$date <- as.Date(last_30_days_united$date)
# Total cases in the last 5 days line plot
ggplot(last_30_days_united, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "United States: Total Cases in Last 30 Days",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

# Making a data frame for the last 30 days cases in China
last_30_days_China <- tail(china_cases,30)
# Convert 'date' column to Date format
last_30_days_China$date <- as.Date(last_30_days_China$date)
# Total cases in the last 5 days line plot
ggplot(last_30_days_China, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "China: Total Cases in Last 30 Days",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()



# Total tests per day line plot to know how the government is taking care of this situation
ggplot(dw, aes(x = date, y = total_tests)) +
  geom_line() +
  labs(title = "Total Tests per Day",
       x = "Date",
       y = "Total Tests") +
  theme_minimal()


# Total tests in the last 5 days line plot in united States
last_5_days_united <- tail(united_cases,5)
ggplot(last_5_days_united, aes(x = date, y = total_tests)) +
  geom_line() +
  labs(title = "Total Tests in United - Last 5 Days",
       x = "Date",
       y = "Total Tests") +
  theme_minimal()

# Total tests in the last 5 days line plot in India
last_5_days_india <- tail(india_cases,5)
# Convert 'date' column to Date format
last_5_days_india$date <- as.Date(last_5_days_india$date)
ggplot(last_5_days_india, aes(x = date, y = total_tests)) +
  geom_line() + 
  labs(title = "Total Tests in india - Last 5 Days",
       x = "Date",
       y = "Total Tests") +
  theme_minimal()

# Total tests in the last 5days line Plot in China
last_5_days_China <- tail(china_cases,5)
last_5_days_China$date <- as.Date(last_5_days_China$date)
ggplot(last_5_days_China, aes(x = date, y = total_tests)) +
  geom_line() +
  labs(title = "Total Tests in china - Last 5 Days",
       x = "Date",
       y = "Total Tests") +
  theme_minimal()



# Making a data frame for Germany 
germany_cases <- subset(dw, location =="Germany")
head(germany_cases)
tail(germany_cases)

#creating a line plot for total cases per day using the ggplot2 package. 
library(ggplot2)
# Convert 'date' column to Date format
germany_cases$date <- as.Date(germany_cases$date)
# Total cases per day line plot for Germany
ggplot(germany_cases, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "Germany : Total Covid-19 Cases per day",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

# Making a data frame for the last 30 days cases in Germany
last_30_days_germany <- tail(germany_cases,30)
print(last_30_days_germany)
# Convert 'date' column to Date format
last_30_days_germany$date <- as.Date(last_30_days_germany$date)
# Total cases in the last 5 days line plot in Germany
ggplot(last_30_days_germany, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "Germany: Total Cases in Last 30 Days",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

# Total tests in the last 5 days line plot in Germany
last_5_days_germany <- tail(germany_cases,5)
print(last_5_days_germany)
ggplot(last_5_days_germany, aes(x = date, y = total_tests)) +
  geom_line() +
  labs(title = "Total Tests in Germany - Last 5 Days",
       x = "Date",
       y = "Total Tests") +
  theme_minimal()


# Understanding cases of India, China,United States , Japan and UK
ijcuu<- subset(dw, location %in% c("India", "China", "United States","Japan","United Kingdom"))

# Plotting total cases across China, India, US, Japan , UK
options(scipen = 10)
ggplot(ijcuu, aes(x = location, y = total_cases, fill = date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Cases in India, China, United States, Japan , United Kingdom",
       x = "Location",
       y = "Total Cases") +
  theme_minimal()


# Understanding cases of Germany and India
inge <- subset(dw, location %in% c("India", "Germany"))
# Plotting growth of cases across Germany and India
ggplot(inge, aes(x = location, y = total_cases, fill = date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Cases in Germany, India",
       x = "Location",
       y = "Total Cases") +
  theme_minimal()

# Understanding cases of United States and India
inus <- subset(dw, location %in% c("India", "United States"))
# Plotting growth of cases across United States and India
ggplot(inus, aes(x = location, y = total_cases, fill = date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Growth of Cases in United States and India",
       x = "Location",
       y = "Total Cases") +
  theme_minimal()


# Understanding cases of China and India
inch <- subset(dw, location %in% c("India", "China"))
# Plotting growth of cases across China and India
ggplot(inch, aes(x = location, y = total_cases, fill = date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Growth of Cases in China and India",
       x = "Location",
       y = "Total Cases") +
  theme_minimal()


# Understanding cases of Japan and India
injp <- subset(dw, location %in% c("India", "Japan"))
# Plotting growth of cases across Japan and India
ggplot(injp, aes(x = location, y = total_cases, fill = date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Growth of Cases in Japan and India",
       x = "Location",
       y = "Total Cases") +
  theme_minimal()


# Getting the latest data for "2022-02-01" 
last_day_cases <- dw[dw$date == "2022-02-01", ]
# Displaying the latest data
print(last_day_cases) 

# Sorting data with respect to 'total_cases'
#The - sign before last_day_cases$total_cases indicates that the sorting should be done in descending order
#So, countries with higher total cases will appear first after sorting.
country_max_cases <- last_day_cases[order(-last_day_cases$total_cases), ]
# Displaying the sorted data
print(country_max_cases)
# Top 5 countries with maximum cases
top_countries <- country_max_cases[1:6, ]
# Displaying the top 5 countries
print(top_countries)
ggplot(top_countries, aes(x = `location`, y = total_cases, fill = `location`)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Countries with Maximum Covid-19 Cases",
       x = "Country",
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Sorting data with respect to 'total_cases' ascending order, lower case will appear first
country_min_cases <- last_day_cases[order(last_day_cases$total_cases), ]
# Displaying the sorted data
print(country_min_cases)
# Top 5 countries with minimum cases
top_countries <- country_min_cases[1:6, ]
# Displaying the top 5 countries
print(top_countries)
ggplot(top_countries, aes(x = `location`, y = total_cases, fill = `location`)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Countries with Minimum Covid-19 Cases",
       x = "Country",
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))






