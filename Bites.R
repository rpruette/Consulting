
# bite data 

library(readxl)
library(lubridate)
library(dplyr)
library(ggmap)


# Read in data from xls format
bite18 <- read_xls("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/DLLS BiteStats FY-18.xls", col_names = TRUE)
bite19 <- read_xls("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/DLLS BiteStats FY-19.xls", col_names = TRUE)
bite20 <- read_xls("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/DLLS BiteStats FY-20.xls", col_names = TRUE)

# change to a data frame
bite18 <- as.data.frame(bite18)
bite19 <- as.data.frame(bite19)
bite20 <- as.data.frame(bite20)

# Remove blank spaces in column names
names(bite18) <- str_replace_all(names(bite18), " ", "_")
names(bite19) <- str_replace_all(names(bite19), " ", "_")
names(bite20) <- str_replace_all(names(bite20), " ", "_")

# Remove "\n" strings in column names
names(bite18) <- str_replace_all(names(bite18), "\n", "")
names(bite19) <- str_replace_all(names(bite19), "\n", "")
names(bite20) <- str_replace_all(names(bite20), "\n", "")

names(bite18) == names(bite19)
names(bite18) == names(bite20)

bites_all <- rbind(bite18, bite19, bite20)


# Create variables for incident year, month, and day
bites_all <- bites_all %>% mutate(incid_year = lubridate::year(Date_Incident_Reported), 
                            incid_month = lubridate::month(Date_Incident_Reported),
                            incid_day = lubridate::day(Date_Incident_Reported))

# Date_of_Bite and Time_of_Bite contain the exact same information so remove Time_of_Bite
which(bites_all$Date_of_Bite != bites_all$Time_of_Bite)
bites_all <- bites_all %>% select(-c(Time_of_Bite))
names(bites_all)
dim(bites_all)

# Create a variable that determines the time between when the bite incident occured and 
# when the incident was reported (units in hours)
bites_all <- bites_all %>% 
  mutate(time_diff_bite_reported_hrs = round(as.numeric(difftime(Date_Incident_Reported, Date_of_Bite), units = "hours"), digits = 4))

# Filter out obs that have missing address
bites_all_address <- bites_all %>% filter(!is.na(Address))
dim(bites_all_address)
dim(bites_all)
#ggmap::register_google(key = "####") #mp

# get longitude, latitude, and address for all obs
for(i in 4263:nrow(bites_all_address)){
  result <- geocode(bites_all_address$Address[i], output = "latlona", source = "google")
  bites_all_address$lon[i] <- as.numeric(result[1])
  bites_all_address$lat[i] <- as.numeric(result[2])
  bites_all_address$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematic(221, 255, 385, 662, 1518, 3035, 3059, 3148, 3588, 3598, 3801, 4149, 4261, 4262)

# observations that need to be fixed manually
bites_all_address$Address[221] <- "4026 CHINA ELM DR, dalls tx"
bites_all_address$Address[255] <- NA
bites_all_address$Address[385] <- "1714 CASCADE AVE, DALLS TX"
bites_all_address$Address[662] <- "4725 Samuell Blvd, Mesquite, TX"
bites_all_address$Address[1518] <- "5940 Arapaho Rd Dallas, TX 75248"
bites_all_address$Address[3035] <- NA
bites_all_address$Address[3059] <- "2300 Barnes Bridge Rd, Dallas, TX 75228"
bites_all_address$Address[3148] <- "4207 Alta Vista Ln Dallas, TX 75229"
bites_all_address$Address[3588] <- "3800 SOUTH TYLER ST dallas tx"
bites_all_address$Address[3598] <- NA
bites_all_address$Address[3801] <- "2300 Barnes Bridge Rd, Dallas, TX 75228"
bites_all_address$Address[4149] <- "2835 KELLER SPRINGS RD CARROLLTON, TX 75006"
bites_all_address$Address[4261] <- NA
bites_all_address$Address[4262] <- NA

# replace geo information with NA if address is NA
for(i in c(255, 3035, 3598, 4261, 4262)){
  bites_all_address$lon[i] <- NA
  bites_all_address$lat[i] <- NA
  bites_all_address$geoAddress[i] <- NA
  print(i)
}

# fix remaining obs that were adjusted manually
for(i in c(221, 385, 662, 1518, 3059, 3148, 3588, 3801, 4149)){
  result <- geocode(bites_all_address$Address[i], output = "latlona", source = "google")
  bites_all_address$lon[i] <- as.numeric(result[1])
  bites_all_address$lat[i] <- as.numeric(result[2])
  bites_all_address$geoAddress[i] <- as.character(result[3])
  print(i)
}

# join df that had missing address with df that did not have missing address
bites_missing_address <- bites_all %>% filter(is.na(Address)) %>% mutate(lon = NA, lat = NA, geoAddress = NA)
bites <- rbind(bites_all_address, bites_missing_address)
dim(bites)

#write.csv(bites, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/bites.csv")
#bites <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/bites.csv")

# create zipcode and state variables
bites <- bites %>% mutate(new_zipcode = as.numeric(str_sub(bites$geoAddress, -10, -6)), state = str_sub(bites$geoAddress, -13, -12))
# several obs that need to be manually adjusted (geocoded incorrectly)
which(bites$state != "tx")

bites$Address[355] <- "3855 MT WASHINGTON st, dallas tx!"
bites$Address[365] <- "2863 Fordham Rd, Dallas, TX 75216"
bites$Address[415] <- NA
bites$Address[425] <- NA
bites$Address[482] <- NA
bites$Address[490] <- NA
bites$Address[602] <- "2014 MOUNTAIN LAKE Rd, dallas tx"
bites$Address[603] <- "2014 MOUNTAIN LAKE Rd, dallas tx"
bites$Address[982] <- "6434 Skillman St, Dallas, TX 75231"
bites$Address[1781] <- NA
bites$Address[1858] <- "315 WOODMONT DR, dallas tx"
bites$Address[1881] <- NA
bites$Address[1882] <- NA
bites$Address[2001] <- NA
bites$Address[2033] <- NA
bites$Address[2104] <- "4833 Royal Ln, dallas tx"
bites$Address[2135] <- "2903 Grand Ave, Dallas, TX 75215"
bites$Address[2179] <- NA
bites$Address[2353] <- "2200 S Marsalis Ave, Dallas, TX 75216"
bites$Address[2354] <- NA
bites$Address[2383] <- NA
bites$Address[2938] <- NA
bites$Address[2986] <- NA
bites$Address[3005] <- "1022 N Walton Walker Service Rd E Irving, TX 75061"
bites$Address[3142] <- NA
bites$Address[3196] <- NA
bites$Address[3379] <- "Bernal Street, dallas tx"
bites$Address[3663] <- "5019 ANN ARBOR ave, dallas tx"
bites$Address[4009] <- NA
bites$Address[4117] <- "1931 Alaska Ave, dallas tx"
bites$Address[4222] <- NA
bites$Address[4280] <- NA
bites$Address[4481] <- "509 STORY ST, dallas tx"
bites$Address[4659] <- NA
bites$Address[4782] <- "1905 Atlas Dr, Dallas, TX 75216"
bites$Address[4835] <- NA
bites$Address[4837] <- NA
bites$Address[5065] <- "3204 Atlanta St, dallas tx"
bites$Address[5372] <- NA


# if Address is NA, change geo information to NA
for(i in 1:length(bites$Address)){
  if(is.na(bites$Address[i])){
    bites$lat[i] <- NA
    bites$lon[i] <- NA
    bites$geoAddress[i] <- NA
  }
}

# get geo information for others that were adjusted manually
for(i in c(355, 365, 602, 603, 982, 1858, 2104, 2135, 2353, 3005, 3379, 3663, 4117, 4481, 4782, 5065)){
  result <- geocode(bites$Address[i], output = "latlona", source = "google")
  bites$lon[i] <- as.numeric(result[1])
  bites$lat[i] <- as.numeric(result[2])
  bites$geoAddress[i] <- as.character(result[3])
  print(i)
}

bites <- bites %>% mutate(new_zipcode = as.numeric(str_sub(bites$geoAddress, -10, -6)), state = str_sub(bites$geoAddress, -13, -12))
# all observations have correct state now
unique(bites$state)

# write.csv(bites, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/bites_v2.csv")
bites <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/bites_v2.csv")

# District variable
table(bites$District)
# change "05 to 5, 5E to 5, and 8S to 8
bites$District[which(bites$District == "05")] <- 5
bites$District[which(bites$District == "5E")] <- 5
bites$District[which(bites$District == "8S")] <- 8
table(bites$District)


names(bites)

# Type of Break looks ok
table(bites$Type_of__Break)

# Severity of bite has SEVERAL levels and many appear to overlap
# Grouping these together as best as I can
table(bites$Severity_of_Bite)

for(i in 1:length(bites$Severity_of_Bite)){
  if(bites$Severity_of_Bite[i] %in% c("3 PUNCT", "PUNC", "PUNC SCRAT", "PUNC/SCRAT", "PUNCH", "PUNCT", "PUNK", "BROKEN", "SKIN BROKE")){
    bites$Severity_of_Bite[i] <- "PUNCTURE"
  }
  if(bites$Severity_of_Bite[i] %in% c("UKN", "UNK", "UKNOWN", "UNKNOWN", "N/A")){
    bites$Severity_of_Bite[i] <- "UNKNOWN"
  }
  if(bites$Severity_of_Bite[i] %in% c("MAJOR", "SEVERE", "STITCHES")){
    bites$Severity_of_Bite[i] <- "MAJOR"
  }
  if(bites$Severity_of_Bite[i] %in% c("NO SKIN", "NO SKIN BR", "NONE", "OK", "MINOR", "SMALL", "Minor")){
    bites$Severity_of_Bite[i] <- "MINOR"
  }
  if(bites$Severity_of_Bite[i] %in% c("SCR & BI", "SCRA TOOTH", "SCRAT NAIL")){
    bites$Severity_of_Bite[i] <- "SCRATCH"
  }
}
# A little more reasonable
table(bites$Severity_of_Bite)

# Day of week looks good
table(bites$Day_of_Week)

# Minor variable looks good
table(bites$Minor)

names(bites) <- str_replace_all(names(bites), "\\?", "")

# owned animal looks ok
table(bites$Owned_Animal)

# loose animal looks ok
table(bites$Loose_Animal)

# owner victim looks good
table(bites$Owner_Victim)

# At large variable has so many levels, many only having one observation
table(bites$At_Large)
for(i in 1:length(bites$At_Large)){
  if(bites$At_Large[i] %in% c("AT LARGE", "DDA - Y", "DDA--YES", "DDA-Y", "DDA-YES", "DPD-Y", "DPD-YES", "YES")){
    bites$At_Large[i] <- "YES"
  }
  else if(bites$At_Large[i] %in% c("CONFINED", "DDA - NO", "DDA N", "DDA NO", "DDA-N", "DDA-N (CAT)", 
                                   "DDA-N0", "DDA-NO", "DDA-NO DECEASED", "DDA/N", "DDY-N", "DPD-N", "N", "NO", "NO DDA", "No")){
    bites$At_Large[i] <- "NO"
  }else{
    bites$At_Large[i] <- "UNKNOWN"
  }
}
table(bites$At_Large)

# Dog or Cat looks good
table(bites$Dog_or_Cat)

# So many levels here -- not sure if this is worth dealing with?
unique(bites$Breed_of_Animal)


# number of obs with non-missing weight
sum(!is.na(bites$Weight_of_Animal))
# number of obs with missing weight -- a lot of missing data here
sum(is.na(bites$Weight_of_Animal))

# Sex variable looks good
table(bites$Sexof_Animal)

# spayed neutered looks good
table(bites$Spayed.Neutered)

# missing 562 obs for age
sum(is.na(bites$Age_.YRS.))

# Previous Bites is a mess
table(bites$Previous_Bite)

for(i in 1:length(bites$Previous_Bite)){
  if(bites$Previous_Bite[i] %in% c("0", "N", "N0", "NO", "NONE")){
    bites$Previous_Bite[i] <- "NO"
  }
  else if(bites$Previous_Bite[i] %in% c("N/A", "U", "UKN", "UKNW", "UNK", "UNK/A", "UNKN", "UNKNOWN")){
    bites$Previous_Bite[i] <- "UNKNOWN"
  }
  else if(bites$Previous_Bite[i] %in% c("Y", "YES")){
    bites$Previous_Bite[i] <- "YES"
  }
}
# now Previous Bite only has 3 levels
table(bites$Previous_Bite)

# Adopted from DAS looks good
table(bites$Adopted_From_DAS)

########################################
# I don't know what this variable means
table(bites$DD_That_Has_Bit)

#write.csv(bites, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/bites_v3.csv")
bites <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/bites_v3.csv")
names(bites)
dim(bites)
