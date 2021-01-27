library(tidyr)
library(readxl)
library(dplyr)
library(ggmap)
library(devtools)
library(lubridate)
library(stringr)
library(caret)
library(tidyverse)
library(OptimalCutpoints)

# Read in data
impounds <- read_excel("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/DLLS impound repeats.xlsx")

# change data to data frame
impounds <- as.data.frame(impounds)

# change the "In Date" variable to be called "In_Date"
colnames(impounds)[which(colnames(impounds) == "In Date")] <- "In_Date"
dim(impounds) #132,403 observations, 14 columns

# Create separate variables for year, month, and day of "In_Date"
impounds <- impounds %>% mutate(year = lubridate::year(In_Date), month = lubridate::month(In_Date), day = lubridate::day(In_Date))


# 2020 data with geographic data and fixed zipcodes
impounds_2020_zipcodes <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_total_zipcodes.csv")
# Drop X2, X1, and X (extra ID variables)
impounds_2020_zipcodes <- impounds_2020_zipcodes[, -c(1, 2, 3)]
names(impounds_2020_zipcodes)

# 2019 data with geographic data and fixed zipcodes
impounds_2019_zipcodes <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_total_zipcodes.csv")
# Drop X1 and X (extra ID variables)
impounds_2019_zipcodes <- impounds_2019_zipcodes[, -c(1,2)]
names(impounds_2019_zipcodes)

# 2018 data with geographic data and fixed zipcodes
impounds_2018_zipcodes <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_total_zipcodes.csv")
# Drop X2, X1, and X (extra ID variables)
impounds_2018_zipcodes <- impounds_2018_zipcodes[, -c(1, 2)]
names(impounds_2018_zipcodes)

# 2017 data with geographic data and fixed zipcodes
impounds_2017_zipcodes <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_total_zipcodes.csv")
# Drop X2, X1, and X (extra ID variables)
impounds_2017_zipcodes <- impounds_2017_zipcodes[, -c(1, 2)]
names(impounds_2017_zipcodes)

# 2016 data with geographic data and fixed zipcodes
impounds_2016_zipcodes <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2016_total_zipcodes.csv")
# Drop X2, X1, and X (extra ID variables)
impounds_2016_zipcodes <- impounds_2016_zipcodes[, -c(1)]
names(impounds_2016_zipcodes)


# combine the two datasets
impounds_all_years <- rbind(impounds_2016_zipcodes, impounds_2017_zipcodes, impounds_2018_zipcodes, impounds_2019_zipcodes, impounds_2020_zipcodes)
names(impounds_all_years)
# Drop state column
impounds_all_years <- impounds_all_years[,-c(21)]
names(impounds_all_years)

# When gathering geographic data, missing values were removed. So these observations need to go back into the data set
# So I obtain all the observations that DID have missing values from 2020 and 2019 and merge them into one df
missing_crossing_2020 <- impounds %>% filter(year == "2020") %>% filter(is.na(crossing))
missing_crossing_2019 <- impounds %>% filter(year == "2019") %>% filter(is.na(crossing))
missing_crossing_2018 <- impounds %>% filter(year == "2018") %>% filter(is.na(crossing))
missing_crossing_2017 <- impounds %>% filter(year == "2017") %>% filter(is.na(crossing))
missing_crossing_2016 <- impounds %>% filter(year == "2016") %>% filter(is.na(crossing))

# merge
missing_crossing_all_years <- rbind(missing_crossing_2016, missing_crossing_2017, missing_crossing_2018, missing_crossing_2019, missing_crossing_2020)
# In order to merge this data with the geographic data set, they need to have the same column names
# So create geographic variables and fill with NA
missing_crossing_all_years$lon <- NA
missing_crossing_all_years$lat <- NA
missing_crossing_all_years$geoAddress <- NA

# combine geographic data with the previously missing data
# this is now the complete data for 2020 and 2019 -- will need to merge 2016-2018 back in
merged_all_years <- rbind(impounds_all_years, missing_crossing_all_years)
dim(merged_all_years)

final_impounds <- merged_all_years

# new zipcode variable that extracts the zipcode from the geoAddress column
final_impounds$zipcode_new <- as.numeric(str_sub(final_impounds$geoAddress, -10, -6))

# if the observation doesn't have a geoAddress column, just use the zipcode previously recorded
for(i in 1:length(final_impounds$zipcode_new)){
  if(is.na(final_impounds$zipcode_new[i])){
    final_impounds$zipcode_new[i] <- final_impounds$ZipCode[i]
  }
}
head(final_impounds)




# Extract the number and letter for age. For example 1Y 3M would become 1Y (for the years variable)
# Then replace the "Y" with nothing, leaving just the number
# Then change the string to numeric format
final_impounds$age_year <- as.numeric(str_replace(str_extract(final_impounds$age_now, "\\d{1,2}[Y]"), "Y", ""))
final_impounds$age_month <- as.numeric(str_replace(str_extract(final_impounds$age_now, "\\d{1,2}[M]"), "M", ""))
final_impounds$age_week <- as.numeric(str_replace(str_extract(final_impounds$age_now, "\\d{1,2}[W]"), "W", ""))
final_impounds$age_day <- as.numeric(str_replace(str_extract(final_impounds$age_now, "\\d{1,2}[D]"), "D", ""))
final_impounds[1:200, c("age_now", "age_year", "age_month", "age_week")]

# 1Y 3M would become 1 (year) 3(months) NA (weeks) NA (days)
# So change the missing values to be 0
final_impounds$age_year <- replace_na(final_impounds$age_year, 0)
final_impounds$age_month <- replace_na(final_impounds$age_month, 0)
final_impounds$age_week <- replace_na(final_impounds$age_week, 0)
final_impounds$age_day <- replace_na(final_impounds$age_day, 0)

# However, if the age was originally missing, we don't want the years, months, etc to be 0
# We want to keep those as missing values
for(i in 1:length(final_impounds$age_now)){
  if(is.na(final_impounds$age_now[i])){
    final_impounds$age_year[i] <- NA
    final_impounds$age_month[i] <- NA
    final_impounds$age_week[i] <- NA
    final_impounds$age_day[i] <- NA
  }
}

# Create a variable that calculates the age is days
# Use this variable to compare with Blake's number of days to make sure the function is working properly
final_impounds$age_num_days <- final_impounds$age_year*365 + final_impounds$age_month*30 + final_impounds$age_week*7 + final_impounds$age_day

# Create an age variable in terms of years
final_impounds$age_num_years <- final_impounds$age_num_days/365

#write.csv(final_impounds, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/final_impounds.csv")
#final_impounds <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/final_impounds.csv")


blake <- read_xlsx("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/Number of Days.xlsx")
blake <- as.data.frame(blake)
blake <- blake[, c(1,7)]
colnames(blake) <- c("Impound", "Days_Blake")

##### WE GOT THE SAME RESULT FOR DAYS!!
age_test <- merge(final_impounds, blake, by = "Impound", all.x = TRUE)
which(age_test$age_num_days != age_test$Days_Blake)


goodzips <- as.list(str_extract(final_impounds$zipcode_new, "\\d{5}"))
goodzip_obs <- which(final_impounds$zipcode_new %in% goodzips)
badzips <- final_impounds[-goodzip_obs,]
table(badzips$year)
dim(final_impounds %>% filter(year == 2018))
rownames(badzips)
final_impounds[rownames(badzips), "zipcode_new"] <- NA

# write.csv(final_impounds, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/final_impounds_v2.csv")

final_impounds_v2 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/final_impounds_v2.csv")
names(final_impounds_v2)


final_impounds_v2$sex[which(final_impounds_v2$sex == "L")] <- "U"
table(final_impounds_v2$sex)

impounds_v3 <- final_impounds_v2 %>% filter(animal_type == "DOG" | animal_type == "CAT") %>% select(-c("X"))
impounds_v3$Repeat <- ifelse(is.na(impounds_v3$Prev_InDate), 0, 1)
names(impounds_v3)

#impounds_v3 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/final_impounds_v3_blake.csv")
#impounds_v3 <- impounds_v3 %>% select(-c("X.1", "X"))
names(impounds_v3)

# Format original date variable
original_date <- format(impounds_v3$In_Date, format = "%Y-%m-%d")
# Create a new variable with a formatted date (used for graphing)
impounds_v3$In_Date_Formatted <- as.Date(as.POSIXct(original_date, format = "%Y-%m-%d"))

# Create a variable for day of the week
impounds_v3$Week_day <- lubridate::wday(impounds_v3$In_Date_Formatted, label = TRUE)
levels(impounds_v3$Week_day)


# find all the dates that follow the format YYYY-MM-DD
good_dates <- str_extract_all(impounds_v3$In_Date, "\\d{4}-\\d{1,}-\\d{1,}")
# determine the observations that have well formattted dates
good_dates_obs <- which(impounds_v3$In_Date %in% good_dates)
length(good_dates_obs) # 125553 observations

# 1757 NAs for the formatted date -- so we need to find the dates that aren't good
sum(is.na(impounds_v3$In_Date_Formatted))

# Get rows of data that aren't in the good dates list
bad_dates <- impounds_v3[-good_dates_obs, ]
dim(bad_dates) # 1757 bad dates
length(str_extract_all(bad_dates$In_Date, "\\d{10}")) # 1757 -- so all bad dates are 10 digit numbers

#write.csv(impounds_v3, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/final_impounds_v3.csv")
impounds_v3 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/final_impounds_v3.csv")
names(impounds_v3)

impounds_v3 %>% filter(!is.na(Prev_Intake)) %>% group_by(Prev_InType) %>% summarise(n = n())

# Total Impounds over Time
impounds_v3 %>% group_by(In_Date_Formatted) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = In_Date_Formatted, y= n)) +
  geom_line()+
  stat_smooth(geom='line', alpha=0.7, se=FALSE, color = "red")+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Date", y = "Number of Impounds", title = "Impounds over Time")

# Total Impounds over Time by animal type
impounds_v3 %>% group_by(In_Date_Formatted) %>% 
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  gather(key = "animal", value = "count", -In_Date_Formatted) %>%
  ggplot(aes(x = In_Date_Formatted, y= count, color = animal)) +
  geom_line()+
  stat_smooth(aes(fill = animal),color = "black", geom='line', alpha=0.3, se=FALSE)+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Date", y = "Number of Impounds", title = "Impounds over Time by Animal Type")+
  ylim(0, 160)

# Total Impounds over Time by impound type
impounds_v3 %>% group_by(In_Date_Formatted) %>% 
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 
  gather(key = "type", value = "count", -In_Date_Formatted) %>%
  ggplot(aes(x = In_Date_Formatted, y= count, color = type)) +
  geom_line()+
  stat_smooth(aes(fill = type),color = "black", geom='line', alpha=0.3, se=FALSE)+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "Date", y = "Number of Impounds", title = "Impounds over Time")


# Total impounds by Animal Type
impounds_v3 %>% 
  group_by(animal_type) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = animal_type, y = n, fill = animal_type))+
  geom_col() + 
  theme_bw() + 
  scale_x_discrete(labels = c("Cat", "Dog")) +
  theme(legend.position = 0)+
  labs(x = "Animal Type", y = "Impounds", title = "Total Impounds by Animal Type")

# Total impounds by Impound Type
impounds_v3 %>%
  group_by(Type) %>% 
  summarize(n = n()) %>%
  mutate(Type_order = fct_reorder(Type, desc(n))) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = Type_order, y = n, fill = Type_order))+
  geom_col() + 
  theme_bw() + 
  theme(legend.position = 0)+
  labs(x = "Impound Type", y = "Impounds", title = "Total Impounds by Impound Type")


# Total Impounds by Year
impounds_v3 %>% group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n, fill = year)) +
  geom_col() +
  theme_bw() + 
  theme(legend.position = 0) + 
  labs(x = "Year", y = "Impounds", title = "Total Impounds by Year")

# Total Impounds by Year and by Animal Type
impounds_v3 %>% group_by(year) %>%
  summarise(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>%
  gather(key = "animal", value = "count", -year) %>%
  ggplot(aes(x = year, y = count, fill = animal)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  labs(x = "Year", y = "Impounds", title = "Total Impounds by Year and Animal Type")

# Total Impounds by Year and by Impound Type
impounds_v3 %>% group_by(year) %>%
  summarise(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>%
  gather(key = "type", value = "count", -year) %>%
  ggplot(aes(x = year, y = count, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  labs(x = "Year", y = "Impounds", title = "Total Impounds by Year and Impound Type")+
  ylim(0, 28000)

# Total Impounds by Month
impounds_v3 %>% mutate(month_factor = as.factor(month)) %>%
  group_by(month_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(month_factor)) %>%
  ggplot(aes(x = month_factor, y= n, fill = month_factor)) +
  geom_col() +
  theme_bw() + 
  scale_x_discrete(label = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  theme(legend.position = 0) +
  labs(x = "Month", y = "Impounds", title = "Total Impounds by Month")

# Total Impounds by Day
impounds_v3 %>% mutate(day_factor = as.factor(day)) %>%
  group_by(day_factor) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = day_factor, y= n,)) +
  geom_col() +
  theme_bw() + 
  theme(legend.position = 0) +
  labs(x = "Day", y = "Impounds", title = "Total Impounds by Day")


# Total Impounds by Zip Code (Top 20)
impounds_v3 %>% mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  select(!zip_factor) %>%
  slice(1:20) %>%
  ggplot(aes(x = zip_factor_order, y= n)) +
  geom_col(position = "dodge", fill = "steelblue") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zip Code", y = "Impounds", title = "Total Impounds by Zip Code (Top 20)")

# Total Impounds by Zip Code and Animal Type (Top 10)
impounds_v3 %>% 
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(cats + dogs))) %>%
  arrange(desc(cats + dogs)) %>%
  select(!zip_factor) %>%
  slice(1:10) %>%
  gather(key = "animal", value = "count", -zip_factor_order) %>%
  ggplot(aes(x = zip_factor_order, y= count, fill = animal)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  labs(x = "Zip Code", y = "Impounds", title = "Total Impounds by Zip Code and Animal Type (Top 10)")

# Total Impounds by Zip Code and Impound Type (Top 10)
impounds_v3 %>% mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(Confiscate+Dispos_Req+Keepsafe+Owner_Sur+Stray+Transfer))) %>%
  arrange(desc(Confiscate+Dispos_Req+Keepsafe+Owner_Sur+Stray+Transfer)) %>%
  select(!zip_factor) %>%
  slice(1:10) %>%
  gather(key = "type", value = "count", -zip_factor_order) %>%
  ggplot(aes(x = zip_factor_order, y= count, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  labs(x = "Zip Code", y = "Impounds", title = "Total Impounds by Zip Code and Impound Type (Top 10)")

# Repeat Impounds over Time
impounds_v3 %>% group_by(In_Date_Formatted) %>% 
  summarize(n = sum(Repeat)) %>% 
  ggplot(aes(x = In_Date_Formatted, y= n)) +
  geom_line()+
  stat_smooth(geom='line', alpha=0.7, se=FALSE, color = "red")+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Date", y = "Number of Repeated Impounds", title = "Repeated Impounds over Time")


# Repeat Impounds over Time by Animal Type
impounds_v3 %>%
  filter(!is.na(In_Date_Formatted)) %>%
  group_by(In_Date_Formatted) %>% 
  filter(Repeat == 1) %>%
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  gather(key = "animal", value = "count", -In_Date_Formatted) %>%
  ggplot(aes(x = In_Date_Formatted, y= count, color = animal)) +
  geom_line()+
  stat_smooth(aes(fill = animal), geom='line', alpha=0.4, se=FALSE, color = "black")+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(limits = c(0, 40)) +
  labs(x = "Date", y = "Repeat Impounds", title = "Repeated Impounds over Time by Animal Type")

# Repeat Impounds over Time by Animal Type
impounds_v3 %>% group_by(In_Date_Formatted) %>% 
  filter(Repeat == 1) %>%
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 
  gather(key = "type", value = "count", -In_Date_Formatted) %>%
  ggplot(aes(x = In_Date_Formatted, y= count, color = type)) +
  geom_line()+
  stat_smooth(aes(fill = type), geom='line', alpha=0.4, se=FALSE, color = "black")+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Date", y = "Repeat Impounds", title = "Repeated Impounds over Time by Impound Type")

# Repeat Impounds by Year
impounds_v3 %>% group_by(year) %>%
  summarize(n = sum(Repeat)) %>%
  ggplot(aes(x = year, y = n, fill = year))+
  geom_col() + 
  theme_bw() + 
  theme(legend.position = 0) +
  labs(x = "Year", "Repeat Impounds", title = "Repeat Impounds by Year")


# Repeat Impounds by Year and by animal type
impounds_v3 %>% 
  filter(Repeat == 1) %>%
  group_by(year) %>%
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>%
  gather(key = "animal", value = "count", -year) %>%
  ggplot(aes(x = year, y = count, fill = animal))+
  geom_col(position = "dodge") + 
  theme_bw() + 
  labs(x = "Year", y = "Repeat Impounds", title = "Repeat Impounds by Year and Animal Type")


# Repeat Impounds by Year and by impound type
impounds_v3 %>% 
  filter(Repeat == 1) %>%
  group_by(year) %>%
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>%
  gather(key = "type", value = "count", -year) %>%
  ggplot(aes(x = year, y = count, fill = type))+
  geom_col(position = "dodge") + 
  theme_bw() + 
  labs(x = "Year", y = "Repeat Impounds", title = "Repeat Impounds by Year and Impound Type")+
  ylim(0, 28000)


# Total Impounds by Day of Week
impounds_v3 %>% group_by(Week_day) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(Week_day)) %>%
  ggplot(aes(x = Week_day, y= n, fill = Week_day)) +
  geom_col() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of Week", y = "Number of Impounds", title = "Impounds per Week Day") +
  theme(legend.position = 0)

# Total Impounds by Day of Week and by Animal Type
impounds_v3 %>% group_by(Week_day) %>% 
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  filter(!is.na(Week_day)) %>%
  gather(key = "animal", value = "count", -Week_day) %>%
  ggplot(aes(x = Week_day, y= count, fill = animal)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of Week", y = "Number of Impounds", title = "Impounds per Week Day")

# Total Impounds by Day of Week and by Impound Type
impounds_v3 %>% group_by(Week_day) %>% 
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 
  filter(!is.na(Week_day)) %>%
  gather(key = "type", value = "count", -Week_day) %>%
  ggplot(aes(x = Week_day, y= count, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of Week", y = "Number of Impounds", title = "Impounds per Week Day by Impound Type")

# Repeat impounds by day of week
impounds_v3 %>% group_by(Week_day) %>% 
  summarize(n = sum(Repeat)) %>% 
  filter(!is.na(Week_day)) %>%
  ggplot(aes(x = Week_day, y= n, fill = Week_day)) +
  geom_col() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of Week", y = "Number of Repeat Impounds", title = "Repeat Impounds per Week Day") +
  theme(legend.position = 0)


# Repeat Impounds by Day of Week and by Animal Type
impounds_v3 %>% group_by(Week_day) %>% 
  filter(Repeat == 1) %>%
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  filter(!is.na(Week_day)) %>%
  gather(key = "animal", value = "count", -Week_day) %>%
  ggplot(aes(x = Week_day, y= count, fill = animal)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of Week", y = "Repeat Impounds", title = "Repeat Impounds by Week Day")

# Total impounds by month and by animal type
impounds_v3 %>% mutate(month_factor = as.factor(month)) %>%
  group_by(month_factor) %>% 
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  filter(!is.na(month_factor)) %>%
  gather(key = "month", value = "count", -month_factor) %>%
  ggplot(aes(x = month_factor, y= count, fill = month)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  scale_x_discrete(label = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  labs(x = "Month", y = "Impounds", title = "Total Impounds by Month and Animal Type")


# Total impounds by month and by impound type
impounds_v3 %>% mutate(month_factor = as.factor(month)) %>%
  group_by(month_factor) %>% 
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 
  filter(!is.na(month_factor)) %>%
  gather(key = "type", value = "count", -month_factor) %>%
  ggplot(aes(x = month_factor, y= count, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  scale_x_discrete(label = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  labs(x = "Month", y = "Impounds", title = "Total Impounds by Month and Impound Type")


# Repeat impounds by month and by animal type
impounds_v3 %>% mutate(month_factor = as.factor(month)) %>%
  filter(Repeat == 1) %>%
  group_by(month_factor) %>% 
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  filter(!is.na(month_factor)) %>%
  gather(key = "month", value = "count", -month_factor) %>%
  ggplot(aes(x = month_factor, y= count, fill = month)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  scale_x_discrete(label = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  labs(x = "Month", y = "Impounds", title = "Repeat Impounds by Month and Animal Type")


# Repeat impounds by month and by impound type
impounds_v3 %>% mutate(month_factor = as.factor(month)) %>%
  filter(Repeat == 1) %>%
  group_by(month_factor) %>% 
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 
  filter(!is.na(month_factor)) %>%
  gather(key = "type", value = "count", -month_factor) %>%
  ggplot(aes(x = month_factor, y= count, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  scale_x_discrete(label = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))+
  labs(x = "Month", y = "Impounds", title = "Repeat Impounds by Month and Impound Type")

# Repeat Impounds by Day
impounds_v3 %>% filter(Repeat == 1) %>%
  mutate(day_factor = as.factor(day)) %>%
  group_by(day_factor) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = day_factor, y= n,)) +
  geom_col() +
  theme_bw() + 
  theme(legend.position = 0) +
  labs(x = "Day", y = "Impounds", title = "Repeat Impounds by Day")

# Repeat Impounds by Day of Week and by Impound Type
impounds_v3 %>% 
  filter(!is.na(Week_day)) %>%
  mutate(Week_day_factor = factor(Week_day, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
  group_by(Week_day_factor) %>% 
  filter(Repeat == 1) %>%
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 

  gather(key = "type", value = "count", -Week_day_factor) %>%
  ggplot(aes(x = Week_day_factor, y= count, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Day of Week", y = "Repeat Impounds", title = "Repeat Impounds by Week Day and Impound Type")


# Repeat Impounds by Zip Code (Top 20)
impounds_v3 %>% 
  filter(Repeat == 1) %>% 
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  select(!zip_factor) %>%
  slice(1:20) %>%
  ggplot(aes(x = zip_factor_order, y= n)) +
  geom_col(position = "dodge", fill = "steelblue") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zip Code", y = "Impounds", title = "Repeat Impounds by Zip Code (Top 20)")

# Repeat Impounds by Zip Code and Animal Type (Top 10)
impounds_v3 %>% 
  filter(Repeat == 1) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(cats = sum(animal_type == "CAT"), dogs = sum(animal_type == "DOG")) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(cats + dogs))) %>%
  arrange(desc(cats + dogs)) %>%
  select(!zip_factor) %>%
  slice(1:10) %>%
  gather(key = "animal", value = "count", -zip_factor_order) %>%
  ggplot(aes(x = zip_factor_order, y= count, fill = animal)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  labs(x = "Zip Code", y = "Impounds", title = "Repeat Impounds by Zip Code and Animal Type (Top 10)")

# Total Impounds by Zip Code and Impound Type (Top 10)
impounds_v3 %>% mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(Confiscate = sum(Type == "CONFISCATE"), Dispos_Req = sum(Type == "DISPOS REQ"),
            Keepsafe = sum(Type == "KEEPSAFE"), Owner_Sur = sum(Type == "OWNER SUR"),
            Stray = sum(Type == "STRAY"), Transfer = sum(Type == "TRANSFER")) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(Confiscate+Dispos_Req+Keepsafe+Owner_Sur+Stray+Transfer))) %>%
  arrange(desc(Confiscate+Dispos_Req+Keepsafe+Owner_Sur+Stray+Transfer)) %>%
  select(!zip_factor) %>%
  slice(1:10) %>%
  gather(key = "type", value = "count", -zip_factor_order) %>%
  ggplot(aes(x = zip_factor_order, y= count, fill = type)) +
  geom_col(position = "dodge") +
  theme_bw() + 
  labs(x = "Zip Code", y = "Impounds", title = "Total Impounds by Zip Code and Impound Type (Top 10)")

# Total Impounds, Top 5 Zip Codes, 2020
impounds_v3 %>% 
  filter(year == 2020) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  select(!zip_factor) %>%
  slice(1:5) %>%
  ggplot(aes(x = zip_factor_order, y= n)) +
  geom_col(position = "dodge", fill = "steelblue") +
  ylim(0, 5000)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zip Code", y = "Impounds", title = "Most Impounds by Zip Code, 2020")

# Total Impounds, Top 5 Zip Codes, 2019
impounds_v3 %>% 
  filter(year == 2019) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  select(!zip_factor) %>%
  slice(1:5) %>%
  ggplot(aes(x = zip_factor_order, y= n)) +
  geom_col(position = "dodge", fill = "steelblue") +
  ylim(0, 5000)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zip Code", y = "Impounds", title = "Most Impounds by Zip Code, 2019")


# Total Impounds, Top 5 Zip Codes, 2018
impounds_v3 %>% 
  filter(year == 2018) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  select(!zip_factor) %>%
  slice(1:5) %>%
  ggplot(aes(x = zip_factor_order, y= n)) +
  geom_col(position = "dodge", fill = "steelblue") +
  ylim(0, 5000)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zip Code", y = "Impounds", title = "Most Impounds by Zip Code, 2018")

# Total Impounds, Top 5 Zip Codes, 2017
impounds_v3 %>% 
  filter(year == 2017) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  select(!zip_factor) %>%
  slice(1:5) %>%
  ggplot(aes(x = zip_factor_order, y= n)) +
  geom_col(position = "dodge", fill = "steelblue") +
  ylim(0, 5000)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zip Code", y = "Impounds", title = "Most Impounds by Zip Code, 2017")

# Total Impounds, Top 5 Zip Codes, 2016
impounds_v3 %>% 
  filter(year == 2016) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  group_by(zip_factor) %>% 
  summarize(n = n()) %>% 
  filter(!is.na(zip_factor)) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  select(!zip_factor) %>%
  slice(1:5) %>%
  ggplot(aes(x = zip_factor_order, y= n)) +
  geom_col(position = "dodge", fill = "steelblue") +
  ylim(0, 5000)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zip Code", y = "Impounds", title = "Most Impounds by Zip Code, 2016")

# Repeat impounds by Animal Type
impounds_v3 %>%
  filter(Repeat == 1) %>%
  group_by(animal_type) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = animal_type, y = n, fill = animal_type))+
  geom_col() + 
  theme_bw() + 
  scale_x_discrete(labels = c("Cat", "Dog")) +
  theme(legend.position = 0)+
  labs(x = "Animal Type", y = "Impounds", title = "Repeat Impounds by Animal Type")

# Repeat impounds by Impound Type
impounds_v3 %>%
  filter(Repeat == 1) %>%
  group_by(Type) %>% 
  summarize(n = n()) %>%
  mutate(Type_order = fct_reorder(Type, desc(n))) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = Type_order, y = n, fill = Type_order))+
  geom_col() + 
  theme_bw() + 
  theme(legend.position = 0)+
  labs(x = "Impound Type", y = "Impounds", title = "Repeat Impounds by Impound Type")


# Repeat impounds by impound type and by animal type
impounds_v3 %>%
  filter(Repeat == 1) %>%
  group_by(Type, animal_type) %>% 
  summarize(n = n()) %>%
  mutate(Type_order = factor(Type, levels = c("OWNER SUR", "STRAY", "TRANSFER", "CONFISCATE", "KEEPSAFE", "DISPOS REQ"))) %>%
  ggplot(aes(x = Type_order, y = n, fill = animal_type))+
  geom_col(position = "dodge") + 
  theme_bw() + 
  labs(x = "Impound Type", y = "Impounds", title = "Repeat Impounds by Impound Type and Animal Type")

# Total impounds by impound type and by animal type
impounds_v3 %>%
  group_by(Type, animal_type) %>% 
  summarize(n = n()) %>%
  mutate(Type_order = factor(Type, levels = c("STRAY", "OWNER SUR", "CONFISCATE", "TRANSFER", "KEEPSAFE", "DISPOS REQ"))) %>%
  ggplot(aes(x = Type_order, y = n, fill = animal_type))+
  geom_col(position = "dodge") + 
  theme_bw() + 
  labs(x = "Impound Type", y = "Impounds", title = "Total Impounds by Impound Type and Animal Type")

# Box Plot of Age by Impound Status
impounds_v3 %>%
  ggplot(aes(x = as.factor(Repeat), y = age_num_years, fill = as.factor(Repeat)))+
  geom_boxplot() +
  theme_bw() + 
  scale_x_discrete(labels = c("Impounds", "Repeat Impounds"))+
  theme(axis.title.x = element_blank(), legend.position = 0)+
  labs(y = "Age (years)", title = "Box Plot of Age")

# Box Plot of Age by Impound Type
impounds_v3 %>%
  ggplot(aes(x = Type, y = age_num_years, fill = Type))+
  geom_boxplot() +
  theme_bw() + 
    theme(axis.title.x = element_blank(), legend.position = 0)+
  labs(y = "Age (years)", title = "Box Plot of Age by Impound Type")

# Box Plot of Age by Animal Type
impounds_v3 %>%
  ggplot(aes(x = animal_type, y = age_num_years, fill = animal_type))+
  geom_boxplot() +
  theme_bw() + 
  theme(axis.title.x = element_blank(), legend.position = 0)+
  labs(y = "Age (years)", title = "Box Plot of Age by Animal Type")

# Box plot of age by top 20 zip codes
impounds_v3 %>% 
  filter(zipcode_new %in% c(75217, 75212, 75216, 75211, 75227, 75241, 75228, 
                            75253, 75224, 75232, 75208, 75215, 75203, 75220, 
                            75243, 75223, 75233, 75235, 75210, 75236)) %>%
  ggplot(aes(x = as.factor(zipcode_new), y = age_num_years, fill = as.factor(zipcode_new))) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = 0, axis.text.x = element_text(angle = 90)) +
  labs(x = "Zip Code", y = "Age", title = "Age Box Plot by Top 20 Zip Codes")

# total impounds by sex
impounds_v3 %>%
  group_by(sex) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = sex, y = n, fill = sex))+
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("Female", "Male", "Neutered", "Spayed", "Unknown"))+
  theme_bw()+
  theme(legend.position = 0) +
  labs(x = "Sex", y = "Count", title = "Total Impounds by Sex")

# total impounds by sex and animal type
impounds_v3 %>%
  group_by(sex, animal_type) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = sex, y = n, fill = animal_type))+
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("Female", "Male", "Neutered", "Spayed", "Unknown"))+
  theme_bw()+
  labs(x = "Sex", y = "Impounds", title = "Total Impounds by Sex and Animal Type")+
  ylim(0, 37000)

# repeat impounds by sex
impounds_v3 %>%
  filter(Repeat == 1) %>%
  group_by(sex) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = sex, y = n, fill = sex))+
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("Female", "Male", "Neutered", "Spayed", "Unknown"))+
  theme_bw()+
  theme(legend.position = 0) +
  labs(x = "Sex", y = "Count", title = "Repeat Impounds by Sex")

# repeat impounds by sex and animal type
impounds_v3 %>%
  filter(Repeat == 1) %>%
  group_by(sex, animal_type) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = sex, y = n, fill = animal_type))+
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("Female", "Male", "Neutered", "Spayed", "Unknown"))+
  theme_bw()+
  labs(x = "Sex", y = "Repeat Impounds", title = "Repeat Impounds by Sex and Animal Type")+
  ylim(0, 37000)

# percent of total impounds by zip code
impounds_v3 %>%
  count(zipcode_new) %>%
  filter(!is.na(zipcode_new)) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  mutate(perc_zip = n/nrow(impounds_v3) * 100) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  slice(1:20) %>%
  ggplot(aes(x = zip_factor_order, y = perc_zip))+
  geom_col()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Zip Code", y = "Percentage", title = "Percentage of Total Impounds by Zip Code")

impounds_v3 %>%
  filter(Repeat == 1) %>%
  count(zipcode_new) %>%
  filter(!is.na(zipcode_new)) %>%
  mutate(zip_factor = as.factor(zipcode_new)) %>%
  mutate(perc_zip = n/nrow(impounds_v3) * 100) %>%
  mutate(zip_factor_order = fct_reorder(zip_factor, desc(n))) %>%
  arrange(desc(n)) %>%
  slice(1:20) %>%
  ggplot(aes(x = zip_factor_order, y = perc_zip))+
  geom_col()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Zip Code", y = "Percentage", title = "Percentage of Repeat Impounds by Zip Code")

# Top Addresses?
impounds_v3 %>% 
  group_by(geoAddress) %>% 
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:20) 

impounds_v3 %>% filter(geoAddress == "2850 s beltline rd, dallas, tx 75253, usa") %>%
  select(Type, animal_type, year) %>%
  group_by(year) %>% summarize(n = n())

impounds_v3 %>% ggplot(aes(x = factor(Repeat))) + geom_bar() + theme_bw() + labs(x = "Repeat", y = "Count", title = "All Impounds") +
  scale_x_discrete(labels = c("No Repeat", "Repeat"))




train_preds <- c("Type", "sex", "animal_type", "year", "month", "day", "zipcode_new", "age_num_years")
impounds_v3$Type <- as.factor(impounds_v3$Type)
impounds_v3$sex <- as.factor(impounds_v3$sex)
impounds_v3$animal_type <- as.factor(impounds_v3$animal_type)
table(impounds_v3$Prev_InType)
table(impounds_v3$Type)
unique(impounds_v3$Type)

# only missing values in zipcode_new and age_num_years
impounds_v3_complete <- impounds_v3 %>% filter(!is.na(zipcode_new), !is.na(age_num_years))
impounds_v3_complete$Repreat <- as.factor(impounds_v3_complete$Repreat)

# split data into training and test sets
indxTrain <- createDataPartition(y = impounds_v3_complete$Repreat,p = 0.70,list = FALSE)
train_log <- impounds_v3_complete[indxTrain,]
test_log <- impounds_v3_complete[-indxTrain,]

# logistic regression model
log_model <- glm(Repreat ~ Type + sex + animal_type + year + month + day + zipcode_new + age_num_years,
                data = train_log, family = "binomial")
log_pred <- predict(log_model, newdata = test_log, type = "response")
log_label <- as.factor(ifelse(log_pred > 0.5, 1, 0))
# confusion matrix
confusionMatrix(log_label, test_log$Repreat)



#now use train function from caret package
log_reg_caret <- train(form = Repreat ~ Type + sex + animal_type + year + month + day + zipcode_new + age_num_years,
                       data = impounds_v3_complete, 
                       trControl = trainControl(method = "repeatedcv", repeats = 5),
                       method = "glm",
                       family = "binomial")

log_reg_caret$results
log_reg_caret$finalModel




