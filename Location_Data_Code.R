library(readxl)
library(dplyr)
library(ggmap)
library(devtools)
library(lubridate)

# Read in data
impounds <- read_excel("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/DLLS impound repeats.xlsx")
#impounds <- read_excel(file.choose())

# change data to data frame
impounds <- as.data.frame(impounds)

# change the "In Date" variable to be called "In_Date"
colnames(impounds)[which(colnames(impounds) == "In Date")] <- "In_Date"
dim(impounds)


impounds <- impounds %>% mutate(year = lubridate::year(In_Date), month = lubridate::month(In_Date), day = lubridate::day(In_Date))
str(impounds)
table(impounds$month)

impounds_2020_Q1 <- impounds %>% filter(year == 2020) %>% filter(month == 1 | month == 2 | month == 3) %>% filter(!is.na(crossing))
dim(impounds_2020_Q1) #6991 observations (834 missing values for crossing)

#ggmap::register_google(key = "") 

for(i in 1:nrow(impounds_2020_Q1)){
  result <- geocode(impounds_2020_Q1$crossing[i], output = "latlona", source = "google")
  impounds_2020_Q1$lon[i] <- as.numeric(result[1])
  impounds_2020_Q1$lat[i] <- as.numeric(result[2])
  impounds_2020_Q1$geoAddress[i] <- as.character(result[3])
  print(i)
}
#problematic observations c(6698)
impounds_2020_Q1$crossing[6698] <- "620 cedarcliff dr, dallas, tx 75217"
impounds_2020_Q1$lon[6698] <- as.numeric(geocode(impounds_2020_Q1$crossing[6698], output = "latlona", source = "google")[1])
impounds_2020_Q1$lat[6698] <- as.numeric(geocode(impounds_2020_Q1$crossing[6698], output = "latlona", source = "google")[2])
impounds_2020_Q1$geoAddress[6698] <- as.character(geocode(impounds_2020_Q1$crossing[6698], output = "latlona", source = "google")[3])
impounds_2020_Q1[6698, ]

#save impounds_2020_Q1 to a csv file with new columns for latitude and longitude
#write.csv(impounds_2020_Q1, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_Q1.csv")



impounds_2020_Q2 <- impounds %>% filter(year == 2020) %>% filter(month == 4 | month == 5 | month == 6) %>% filter(!is.na(crossing))
dim(impounds_2020_Q2) #3529 observations (345 missing values for crossing)

for(i in 1:nrow(impounds_2020_Q2)){
  result <- geocode(impounds_2020_Q2$crossing[i], output = "latlona", source = "google")
  impounds_2020_Q2$lon[i] <- as.numeric(result[1])
  impounds_2020_Q2$lat[i] <- as.numeric(result[2])
  impounds_2020_Q2$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematic observations c(36, 826, 851)

impounds_2020_Q2$crossing[36] <- "14392 Wrangler Way, dallas tx"
impounds_2020_Q2$lon[36] <- as.numeric(geocode(impounds_2020_Q2$crossing[36], output = "latlona", source = "google")[1])
impounds_2020_Q2$lat[36] <- as.numeric(geocode(impounds_2020_Q2$crossing[36], output = "latlona", source = "google")[2])
impounds_2020_Q2$geoAddress[36] <- as.character(geocode(impounds_2020_Q2$crossing[36], output = "latlona", source = "google")[3])
impounds_2020_Q2[36, ]

impounds_2020_Q2$crossing[826] <- "14235 riata lane, dallas tx"
impounds_2020_Q2$lon[826] <- as.numeric(geocode(impounds_2020_Q2$crossing[826], output = "latlona", source = "google")[1])
impounds_2020_Q2$lat[826] <- as.numeric(geocode(impounds_2020_Q2$crossing[826], output = "latlona", source = "google")[2])
impounds_2020_Q2$geoAddress[826] <- as.character(geocode(impounds_2020_Q2$crossing[826], output = "latlona", source = "google")[3])
impounds_2020_Q2[826, ]

impounds_2020_Q2$crossing[851] <- "14235 riata lane, dallas tx"
impounds_2020_Q2$lon[851] <- as.numeric(geocode(impounds_2020_Q2$crossing[851], output = "latlona", source = "google")[1])
impounds_2020_Q2$lat[851] <- as.numeric(geocode(impounds_2020_Q2$crossing[851], output = "latlona", source = "google")[2])
impounds_2020_Q2$geoAddress[851] <- as.character(geocode(impounds_2020_Q2$crossing[851], output = "latlona", source = "google")[3])
impounds_2020_Q2[851, ]

#save impounds_2020_Q2 to a csv file with new columns for latitude and longitude
#write.csv(impounds_2020_Q2, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_Q2.csv")



impounds_2020_Q3 <- impounds %>% filter(year == 2020) %>% filter(month == 7 | month == 8 | month == 9) %>% filter(!is.na(crossing))
dim(impounds_2020_Q3) #3451 observations (590 missing values for crossing)

for(i in 1935:nrow(impounds_2020_Q3)){
  result <- geocode(impounds_2020_Q3$crossing[i], output = "latlona", source = "google")
  impounds_2020_Q3$lon[i] <- as.numeric(result[1])
  impounds_2020_Q3$lat[i] <- as.numeric(result[2])
  impounds_2020_Q3$geoAddress[i] <- as.character(result[3])
  print(i)
}

#save impounds_2020_Q3 to a csv file with new columns for latitude and longitude
#write.csv(impounds_2020_Q3, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_Q3.csv")

impounds_2020_Q1 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_Q1.csv")
impounds_2020_Q2 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_Q2.csv")
impounds_2020_Q3 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_Q3.csv")
impounds_2020_total <- rbind(impounds_2020_Q1, impounds_2020_Q2, impounds_2020_Q3)
#write.csv(impounds_2020_total, file ="/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_total.csv")
dim(impounds_2020_total)


big_d <- ggmap::get_googlemap("dallas tx", zoom = 11, maptype = "terrain")
ggmap(big_d)
ggmap(big_d)+
  geom_point(aes(x = lon, y = lat), color = "midnightblue", data = final_impounds, alpha = 0.1, size = 0.5) +
  theme(legend.position = 0)

ggmap(big_d) +
  stat_density2d(aes(x = lon, y = lat, fill = ..level..), size = 0.5, bins = 30, data = final_impounds_v2 %>% filter(Type == "STRAY"), geom = "polygon", alpha = 0.1) +
  scale_fill_continuous(name = "Count") + 
  labs(x = "Longitude", y = "Latitude", title = "Impounds 2020")



impounds_2019_Q1 <- impounds %>% filter(year == 2019) %>% filter(month == 1 | month == 2 | month == 3) %>% filter(!is.na(crossing))
dim(impounds_2019_Q1) #9704 observations (3 missing values for crossing)

for(i in 1:nrow(impounds_2019_Q1)){
  result <- geocode(impounds_2019_Q1$crossing[i], output = "latlona", source = "google")
  impounds_2019_Q1$lon[i] <- as.numeric(result[1])
  impounds_2019_Q1$lat[i] <- as.numeric(result[2])
  impounds_2019_Q1$geoAddress[i] <- as.character(result[3])
  print(i)
}
#problematic observations c()
head(impounds_2019_Q1)

#save impounds_2020_Q1 to a csv file with new columns for latitude and longitude
#write.csv(impounds_2019_Q1, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q1.csv")


impounds_2019_Q2 <- impounds %>% filter(year == 2019) %>% filter(month == 4 | month == 5 | month == 6) %>% filter(!is.na(crossing))
dim(impounds_2019_Q2) #11366 observations (1 missing value for crossing)

for(i in 8203:nrow(impounds_2019_Q2)){
  result <- geocode(impounds_2019_Q2$crossing[i], output = "latlona", source = "google")
  impounds_2019_Q2$lon[i] <- as.numeric(result[1])
  impounds_2019_Q2$lat[i] <- as.numeric(result[2])
  impounds_2019_Q2$geoAddress[i] <- as.character(result[3])
  print(i)
}
#problematic observations c(977, 979, 997, 7541, 8202)

impounds_2019_Q2$crossing[977] <- "2850 S Beltline Rd, Dallas Tx"
impounds_2019_Q2$lon[977] <- as.numeric(geocode(impounds_2019_Q2$crossing[977], output = "latlona", source = "google")[1])
impounds_2019_Q2$lat[977] <- as.numeric(geocode(impounds_2019_Q2$crossing[977], output = "latlona", source = "google")[2])
impounds_2019_Q2$geoAddress[977] <- as.character(geocode(impounds_2019_Q2$crossing[977], output = "latlona", source = "google")[3])
impounds_2019_Q2[977, ]

impounds_2019_Q2$crossing[979] <- "2850 S Beltline Rd, Dallas Tx"
impounds_2019_Q2$lon[979] <- as.numeric(geocode(impounds_2019_Q2$crossing[979], output = "latlona", source = "google")[1])
impounds_2019_Q2$lat[979] <- as.numeric(geocode(impounds_2019_Q2$crossing[979], output = "latlona", source = "google")[2])
impounds_2019_Q2$geoAddress[979] <- as.character(geocode(impounds_2019_Q2$crossing[979], output = "latlona", source = "google")[3])
impounds_2019_Q2[979, ]

impounds_2019_Q2$crossing[997]<- "2850 S Beltline Rd, Dallas Tx"
impounds_2019_Q2$lon[997] <- as.numeric(geocode(impounds_2019_Q2$crossing[997], output = "latlona", source = "google")[1])
impounds_2019_Q2$lat[997] <- as.numeric(geocode(impounds_2019_Q2$crossing[997], output = "latlona", source = "google")[2])
impounds_2019_Q2$geoAddress[997] <- as.character(geocode(impounds_2019_Q2$crossing[997], output = "latlona", source = "google")[3])
impounds_2019_Q2[997, ]

impounds_2019_Q2$crossing[7541] <- "7600 lyndon b johnson fwy, dallas tx"
impounds_2019_Q2$lon[7541] <- as.numeric(geocode(impounds_2019_Q2$crossing[7541], output = "latlona", source = "google")[1])
impounds_2019_Q2$lat[7541] <- as.numeric(geocode(impounds_2019_Q2$crossing[7541], output = "latlona", source = "google")[2])
impounds_2019_Q2$geoAddress[7541] <- as.character(geocode(impounds_2019_Q2$crossing[7541], output = "latlona", source = "google")[3])
impounds_2019_Q2[7541, ]

# Address cannot be understood. Remove this observation
impounds_2019_Q2 <- impounds_2019_Q2[-8202, ]

#save impounds_2019_Q2 to a csv file with new columns for latitude and longitude
#write.csv(impounds_2019_Q2, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q2.csv")




impounds_2019_Q3 <- impounds %>% filter(year == 2019) %>% filter(month == 7 | month == 8 | month == 9) %>% filter(!is.na(crossing))
dim(impounds_2019_Q3) #10489 observations (0 missing values for crossing)

for(i in 10369:nrow(impounds_2019_Q3)){
  result <- geocode(impounds_2019_Q3$crossing[i], output = "latlona", source = "google")
  impounds_2019_Q3$lon[i] <- as.numeric(result[1])
  impounds_2019_Q3$lat[i] <- as.numeric(result[2])
  impounds_2019_Q3$geoAddress[i] <- as.character(result[3])
  print(i)
}

# problematic observations c(50, 8433, 10368)
# stopped at 6460
impounds_2019_Q3$crossing[50] <- "9540 gonzales dr, dallas tx"
impounds_2019_Q3$lon[50] <- as.numeric(geocode(impounds_2019_Q3$crossing[50], output = "latlona", source = "google")[1])
impounds_2019_Q3$lat[50] <- as.numeric(geocode(impounds_2019_Q3$crossing[50], output = "latlona", source = "google")[2])
impounds_2019_Q3$geoAddress[50] <- as.character(geocode(impounds_2019_Q3$crossing[50], output = "latlona", source = "google")[3])
impounds_2019_Q3[50, ]

impounds_2019_Q3$crossing[8433] <- "7700 colebrook dr, dallas tx"
impounds_2019_Q3$lon[8433] <- as.numeric(geocode(impounds_2019_Q3$crossing[8433], output = "latlona", source = "google")[1])
impounds_2019_Q3$lat[8433] <- as.numeric(geocode(impounds_2019_Q3$crossing[8433], output = "latlona", source = "google")[2])
impounds_2019_Q3$geoAddress[8433] <- as.character(geocode(impounds_2019_Q3$crossing[8433], output = "latlona", source = "google")[3])
impounds_2019_Q3[8433, ]

impounds_2019_Q3$crossing[10368] <- "9667 wharf rd, dallas tx"
impounds_2019_Q3$lon[10368] <- as.numeric(geocode(impounds_2019_Q3$crossing[10368], output = "latlona", source = "google")[1])
impounds_2019_Q3$lat[10368] <- as.numeric(geocode(impounds_2019_Q3$crossing[10368], output = "latlona", source = "google")[2])
impounds_2019_Q3$geoAddress[10368] <- as.character(geocode(impounds_2019_Q3$crossing[10368], output = "latlona", source = "google")[3])
impounds_2019_Q3[10368, ]


#save impounds_2019_Q3 to a csv file with new columns for latitude and longitude
write.csv(impounds_2019_Q3, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q3.csv")



impounds_2019_Q4 <- impounds %>% filter(year == 2019) %>% filter(month == 10 | month == 11 | month == 12) %>% filter(!is.na(crossing))
dim(impounds_2019_Q4) #8590 observations (2 missing values for crossing)

for(i in 1:nrow(impounds_2019_Q4)){
  result <- geocode(impounds_2019_Q4$crossing[i], output = "latlona", source = "google")
  impounds_2019_Q4$lon[i] <- as.numeric(result[1])
  impounds_2019_Q4$lat[i] <- as.numeric(result[2])
  impounds_2019_Q4$geoAddress[i] <- as.character(result[3])
  print(i)
}

#save impounds_2019_Q4 to a csv file with new columns for latitude and longitude
write.csv(impounds_2019_Q4, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q4.csv")


impounds_2020 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_total.csv")
dim(impounds_2020)

impounds_2020$state <- str_sub(impounds_2020$geoAddress, -13, -12)
impounds_2020[which(impounds_2020$state != "tx"),]

#fix observations 2450, 3155, 4758, 5862, 6344
impounds_2020$crossing[2450] <- "1818 westmooreland, dallas tx"
impounds_2020$lon[2450] <- as.numeric(geocode(impounds_2020$crossing[2450], output = "latlona", source = "google")[1])
impounds_2020$lat[2450] <- as.numeric(geocode(impounds_2020$crossing[2450], output = "latlona", source = "google")[2])
impounds_2020$geoAddress[2450] <- as.character(geocode(impounds_2020$crossing[2450], output = "latlona", source = "google")[3])
impounds_2020[2450, ]


impounds_2020$crossing[3155] <- "BRIAIRCLIFF RD, dallas tx"
impounds_2020$lon[3155] <- as.numeric(geocode(impounds_2020$crossing[3155], output = "latlona", source = "google")[1])
impounds_2020$lat[3155] <- as.numeric(geocode(impounds_2020$crossing[3155], output = "latlona", source = "google")[2])
impounds_2020$geoAddress[3155] <- as.character(geocode(impounds_2020$crossing[3155], output = "latlona", source = "google")[3])
impounds_2020[3155, ]


impounds_2020$crossing[4758] <- "509 S STORY, dallas tx"
impounds_2020$lon[4758] <- as.numeric(geocode(impounds_2020$crossing[4758], output = "latlona", source = "google")[1])
impounds_2020$lat[4758] <- as.numeric(geocode(impounds_2020$crossing[4758], output = "latlona", source = "google")[2])
impounds_2020$geoAddress[4758] <- as.character(geocode(impounds_2020$crossing[4758], output = "latlona", source = "google")[3])
impounds_2020[4758, ]


impounds_2020$crossing[5862] <- "970 W TUTTLE, dallas tx"
impounds_2020$lon[5862] <- as.numeric(geocode(impounds_2020$crossing[5862], output = "latlona", source = "google")[1])
impounds_2020$lat[5862] <- as.numeric(geocode(impounds_2020$crossing[5862], output = "latlona", source = "google")[2])
impounds_2020$geoAddress[5862] <- as.character(geocode(impounds_2020$crossing[5862], output = "latlona", source = "google")[3])
impounds_2020[5862, ]



impounds_2020$crossing[6344] <- "BEAUCHAMP, dallas tx"
impounds_2020$lon[6344] <- as.numeric(geocode(impounds_2020$crossing[6344], output = "latlona", source = "google")[1])
impounds_2020$lat[6344] <- as.numeric(geocode(impounds_2020$crossing[6344], output = "latlona", source = "google")[2])
impounds_2020$geoAddress[6344] <- as.character(geocode(impounds_2020$crossing[6344], output = "latlona", source = "google")[3])
impounds_2020[6344, ]

# all 2020 observations have correct state now
impounds_2020$state <- str_sub(impounds_2020$geoAddress, -13, -12)
impounds_2020[which(impounds_2020$state != "tx"),]
write.csv(impounds_2020, "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2020_total_zipcodes.csv")




impounds_2019_Q1 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q1.csv")
impounds_2019_Q2 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q2.csv")
impounds_2019_Q3 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q3.csv")
impounds_2019_Q4 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_Q4.csv")
impounds_2019 <- rbind(impounds_2019_Q1, impounds_2019_Q2, impounds_2019_Q3, impounds_2019_Q4)

head(impounds_2019)

impounds_2019$state <- str_sub(impounds_2019$geoAddress, -13, -12)
impounds_2019[which(impounds_2019$state != "tx" & impounds_2019$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

# fix observations 6021, 11088, 11093, 11094, 11097, 11098, 11175, 11176, 19643, 19669, 20760, 20825, 23979, 29767

impounds_2019$crossing[6021] <- "marsh ln, dallas tx"
impounds_2019$lon[6021] <- as.numeric(geocode(impounds_2019$crossing[6021], output = "latlona", source = "google")[1])
impounds_2019$lat[6021] <- as.numeric(geocode(impounds_2019$crossing[6021], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[6021] <- as.character(geocode(impounds_2019$crossing[6021], output = "latlona", source = "google")[3])
impounds_2019[6021, ]

impounds_2019$crossing[11088] <- "PO BOX 52, dallas tx"
impounds_2019$lon[11088] <- as.numeric(geocode(impounds_2019$crossing[11088], output = "latlona", source = "google")[1])
impounds_2019$lat[11088] <- as.numeric(geocode(impounds_2019$crossing[11088], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[11088] <- as.character(geocode(impounds_2019$crossing[11088], output = "latlona", source = "google")[3])
impounds_2019[11088, ]

impounds_2019$crossing[11093] <- "PO BOX 52, dallas tx"
impounds_2019$lon[11093] <- as.numeric(geocode(impounds_2019$crossing[11093], output = "latlona", source = "google")[1])
impounds_2019$lat[11093] <- as.numeric(geocode(impounds_2019$crossing[11093], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[11093] <- as.character(geocode(impounds_2019$crossing[11093], output = "latlona", source = "google")[3])
impounds_2019[11093, ]


impounds_2019$crossing[11094] <- "PO BOX 52, dallas tx"
impounds_2019$lon[11094] <- as.numeric(geocode(impounds_2019$crossing[11094], output = "latlona", source = "google")[1])
impounds_2019$lat[11094] <- as.numeric(geocode(impounds_2019$crossing[11094], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[11094] <- as.character(geocode(impounds_2019$crossing[11094], output = "latlona", source = "google")[3])
impounds_2019[11094, ]


impounds_2019$crossing[11097] <- "PO BOX 52, dallas tx"
impounds_2019$lon[11097] <- as.numeric(geocode(impounds_2019$crossing[11097], output = "latlona", source = "google")[1])
impounds_2019$lat[11097] <- as.numeric(geocode(impounds_2019$crossing[11097], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[11097] <- as.character(geocode(impounds_2019$crossing[11097], output = "latlona", source = "google")[3])
impounds_2019[11097, ]

impounds_2019$crossing[11098] <- "PO BOX 52, dallas tx"
impounds_2019$lon[11098] <- as.numeric(geocode(impounds_2019$crossing[11098], output = "latlona", source = "google")[1])
impounds_2019$lat[11098] <- as.numeric(geocode(impounds_2019$crossing[11098], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[11098] <- as.character(geocode(impounds_2019$crossing[11098], output = "latlona", source = "google")[3])
impounds_2019[11098, ]


impounds_2019$crossing[11175] <- "PO BOX 52, dallas tx"
impounds_2019$lon[11175] <- as.numeric(geocode(impounds_2019$crossing[11175], output = "latlona", source = "google")[1])
impounds_2019$lat[11175] <- as.numeric(geocode(impounds_2019$crossing[11175], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[11175] <- as.character(geocode(impounds_2019$crossing[11175], output = "latlona", source = "google")[3])
impounds_2019[11175, ]

impounds_2019$crossing[11176] <- "PO BOX 52, dallas tx"
impounds_2019$lon[11176] <- as.numeric(geocode(impounds_2019$crossing[11176], output = "latlona", source = "google")[1])
impounds_2019$lat[11176] <- as.numeric(geocode(impounds_2019$crossing[11176], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[11176] <- as.character(geocode(impounds_2019$crossing[11176], output = "latlona", source = "google")[3])
impounds_2019[11176, ]


impounds_2019$crossing[19643] <- "7906 GROTON, dallas tx"
impounds_2019$lon[19643] <- as.numeric(geocode(impounds_2019$crossing[19643], output = "latlona", source = "google")[1])
impounds_2019$lat[19643] <- as.numeric(geocode(impounds_2019$crossing[19643], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[19643] <- as.character(geocode(impounds_2019$crossing[19643], output = "latlona", source = "google")[3])
impounds_2019[19643, ]


impounds_2019$crossing[19669] <- "900 IRVING HIEGHTS, dallas tx"
impounds_2019$lon[19669] <- as.numeric(geocode(impounds_2019$crossing[19669], output = "latlona", source = "google")[1])
impounds_2019$lat[19669] <- as.numeric(geocode(impounds_2019$crossing[19669], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[19669] <- as.character(geocode(impounds_2019$crossing[19669], output = "latlona", source = "google")[3])
impounds_2019[19669, ]


impounds_2019$crossing[20760] <- "4136 NEEDLE LEAF, dallas tx"
impounds_2019$lon[20760] <- as.numeric(geocode(impounds_2019$crossing[20760], output = "latlona", source = "google")[1])
impounds_2019$lat[20760] <- as.numeric(geocode(impounds_2019$crossing[20760], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[20760] <- as.character(geocode(impounds_2019$crossing[20760], output = "latlona", source = "google")[3])
impounds_2019[20760, ]


impounds_2019$crossing[20825] <- "1100 KING HIGHWAY, dallas tx"
impounds_2019$lon[20825] <- as.numeric(geocode(impounds_2019$crossing[20825], output = "latlona", source = "google")[1])
impounds_2019$lat[20825] <- as.numeric(geocode(impounds_2019$crossing[20825], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[20825] <- as.character(geocode(impounds_2019$crossing[20825], output = "latlona", source = "google")[3])
impounds_2019[20825, ]


impounds_2019$crossing[23979] <- "e. interstate i-30, DALLAS TX 75088"
impounds_2019$lon[23979] <- as.numeric(geocode(impounds_2019$crossing[23979], output = "latlona", source = "google")[1])
impounds_2019$lat[23979] <- as.numeric(geocode(impounds_2019$crossing[23979], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[23979] <- as.character(geocode(impounds_2019$crossing[23979], output = "latlona", source = "google")[3])
impounds_2019[23979, ]


impounds_2019$crossing[29767] <- "6000 NORTHWEST HWY, dallas tx"
impounds_2019$lon[29767] <- as.numeric(geocode(impounds_2019$crossing[29767], output = "latlona", source = "google")[1])
impounds_2019$lat[29767] <- as.numeric(geocode(impounds_2019$crossing[29767], output = "latlona", source = "google")[2])
impounds_2019$geoAddress[29767] <- as.character(geocode(impounds_2019$crossing[29767], output = "latlona", source = "google")[3])
impounds_2019[29767, ]

# all 2019 observations have correct state now
impounds_2019$state <- str_sub(impounds_2019$geoAddress, -13, -12)
impounds_2019[which(impounds_2019$state != "tx" & impounds_2019$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

#write.csv(impounds_2019, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2019_total_zipcodes.csv")




impounds_2018_Q1 <- impounds %>% filter(year == 2018) %>% filter(month == 1 | month == 2 | month == 3) %>% filter(!is.na(crossing))
dim(impounds_2018_Q1) #7936 observations (0 missing values for crossing)

for(i in 1349:nrow(impounds_2018_Q1)){
  result <- geocode(impounds_2018_Q1$crossing[i], output = "latlona", source = "google")
  impounds_2018_Q1$lon[i] <- as.numeric(result[1])
  impounds_2018_Q1$lat[i] <- as.numeric(result[2])
  impounds_2018_Q1$geoAddress[i] <- as.character(result[3])
  print(i)
}
#problematic observations c(1176, 1256, 1296, 1304, 1347, 1348)

impounds_2018_Q1$crossing[1176] <- "13122 C F Hawn Fwy 47 Dallas, TX 75253"
impounds_2018_Q1$lon[1176] <- as.numeric(geocode(impounds_2018_Q1$crossing[1176], output = "latlona", source = "google")[1])
impounds_2018_Q1$lat[1176] <- as.numeric(geocode(impounds_2018_Q1$crossing[1176], output = "latlona", source = "google")[2])
impounds_2018_Q1$geoAddress[1176] <- as.character(geocode(impounds_2018_Q1$crossing[1176], output = "latlona", source = "google")[3])
impounds_2018_Q1[1176, ]

impounds_2018_Q1$crossing[1256] <-  "9633 SEAGOVILLE RD, DALLAS TX"
impounds_2018_Q1$lon[1256] <- as.numeric(geocode(impounds_2018_Q1$crossing[1256], output = "latlona", source = "google")[1])
impounds_2018_Q1$lat[1256] <- as.numeric(geocode(impounds_2018_Q1$crossing[1256], output = "latlona", source = "google")[2])
impounds_2018_Q1$geoAddress[1256] <- as.character(geocode(impounds_2018_Q1$crossing[1256], output = "latlona", source = "google")[3])
impounds_2018_Q1[1256, ]

impounds_2018_Q1$crossing[1296] <- "6200 Elam Rd, dallas tx"
impounds_2018_Q1$lon[1296] <- as.numeric(geocode(impounds_2018_Q1$crossing[1296], output = "latlona", source = "google")[1])
impounds_2018_Q1$lat[1296] <- as.numeric(geocode(impounds_2018_Q1$crossing[1296], output = "latlona", source = "google")[2])
impounds_2018_Q1$geoAddress[1296] <- as.character(geocode(impounds_2018_Q1$crossing[1296], output = "latlona", source = "google")[3])
impounds_2018_Q1[1296, ]

impounds_2018_Q1$crossing[1304] <- "7800 HILLARD, dallas tx"
impounds_2018_Q1$lon[1304] <- as.numeric(geocode(impounds_2018_Q1$crossing[1304], output = "latlona", source = "google")[1])
impounds_2018_Q1$lat[1304] <- as.numeric(geocode(impounds_2018_Q1$crossing[1304], output = "latlona", source = "google")[2])
impounds_2018_Q1$geoAddress[1304] <- as.character(geocode(impounds_2018_Q1$crossing[1304], output = "latlona", source = "google")[3])
impounds_2018_Q1[1304, ]

impounds_2018_Q1$crossing[1347] <- "7700 HILLARD, dallas tx"
impounds_2018_Q1$lon[1347] <- as.numeric(geocode(impounds_2018_Q1$crossing[1347], output = "latlona", source = "google")[1])
impounds_2018_Q1$lat[1347] <- as.numeric(geocode(impounds_2018_Q1$crossing[1347], output = "latlona", source = "google")[2])
impounds_2018_Q1$geoAddress[1347] <- as.character(geocode(impounds_2018_Q1$crossing[1347], output = "latlona", source = "google")[3])
impounds_2018_Q1[1347, ]

impounds_2018_Q1$crossing[1348] <- "6200 ELAM rd, dallas tx"
impounds_2018_Q1$lon[1348] <- as.numeric(geocode(impounds_2018_Q1$crossing[1348], output = "latlona", source = "google")[1])
impounds_2018_Q1$lat[1348] <- as.numeric(geocode(impounds_2018_Q1$crossing[1348], output = "latlona", source = "google")[2])
impounds_2018_Q1$geoAddress[1348] <- as.character(geocode(impounds_2018_Q1$crossing[1348], output = "latlona", source = "google")[3])
impounds_2018_Q1[1348, ]

#write.csv(impounds_2018_Q1, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q1.csv")



impounds_2018_Q2 <- impounds %>% filter(year == 2018) %>% filter(month == 4 | month == 5 | month == 6) %>% filter(!is.na(crossing))
dim(impounds_2018_Q2) #9543 observations (1 missing value for crossing)

for(i in 1:nrow(impounds_2018_Q2)){
  result <- geocode(impounds_2018_Q2$crossing[i], output = "latlona", source = "google")
  impounds_2018_Q2$lon[i] <- as.numeric(result[1])
  impounds_2018_Q2$lat[i] <- as.numeric(result[2])
  impounds_2018_Q2$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematic observations (4596, 7345)

impounds_2018_Q2$crossing[4596] <- "2914 E ATOLL, dallas tx"
impounds_2018_Q2$lon[4596] <- as.numeric(geocode(impounds_2018_Q2$crossing[4596], output = "latlona", source = "google")[1])
impounds_2018_Q2$lat[4596] <- as.numeric(geocode(impounds_2018_Q2$crossing[4596], output = "latlona", source = "google")[2])
impounds_2018_Q2$geoAddress[4596] <- as.character(geocode(impounds_2018_Q2$crossing[4596], output = "latlona", source = "google")[3])
impounds_2018_Q2[4596, ]

impounds_2018_Q2$crossing[7345] <- NA
impounds_2018_Q2$lon[7345] <- NA
impounds_2018_Q2$lat[7345] <- NA
impounds_2018_Q2$geoAddress[7345] <- NA
impounds_2018_Q2[7345, ]

write.csv(impounds_2018_Q2, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q2.csv")



impounds_2018_Q3 <- impounds %>% filter(year == 2018) %>% filter(month == 7 | month == 8 | month == 9) %>% filter(!is.na(crossing))
dim(impounds_2018_Q3) #9795 observations (0 missing values for crossing)

for(i in 6946:nrow(impounds_2018_Q3)){
  result <- geocode(impounds_2018_Q3$crossing[i], output = "latlona", source = "google")
  impounds_2018_Q3$lon[i] <- as.numeric(result[1])
  impounds_2018_Q3$lat[i] <- as.numeric(result[2])
  impounds_2018_Q3$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematic observations c(4865, 6945)

impounds_2018_Q3$crossing[4865] <- "421 W LAWSON RD, DALLAS TX"
impounds_2018_Q3$lon[4865] <- as.numeric(geocode(impounds_2018_Q3$crossing[4865], output = "latlona", source = "google")[1])
impounds_2018_Q3$lat[4865] <- as.numeric(geocode(impounds_2018_Q3$crossing[4865], output = "latlona", source = "google")[2])
impounds_2018_Q3$geoAddress[4865] <- as.character(geocode(impounds_2018_Q3$crossing[4865], output = "latlona", source = "google")[3])
impounds_2018_Q3[4865, ]

impounds_2018_Q3$crossing[6945] <- "4863 FALLON PL, dallas tx"
impounds_2018_Q3$lon[6945] <- as.numeric(geocode(impounds_2018_Q3$crossing[6945], output = "latlona", source = "google")[1])
impounds_2018_Q3$lat[6945] <- as.numeric(geocode(impounds_2018_Q3$crossing[6945], output = "latlona", source = "google")[2])
impounds_2018_Q3$geoAddress[6945] <- as.character(geocode(impounds_2018_Q3$crossing[6945], output = "latlona", source = "google")[3])
impounds_2018_Q3[6945, ]

#write.csv(impounds_2018_Q3, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q3.csv")


impounds_2018_Q4 <- impounds %>% filter(year == 2018) %>% filter(month == 10 | month == 11 | month == 12) %>% filter(!is.na(crossing))
dim(impounds_2018_Q4) #8993 observations (0 missing values for crossing)

for(i in 3741:nrow(impounds_2018_Q4)){
  result <- geocode(impounds_2018_Q4$crossing[i], output = "latlona", source = "google")
  impounds_2018_Q4$lon[i] <- as.numeric(result[1])
  impounds_2018_Q4$lat[i] <- as.numeric(result[2])
  impounds_2018_Q4$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematici observations (3740)


impounds_2018_Q4$crossing[3740] <- "807 W SANNER, dallas tx"
impounds_2018_Q4$lon[3740] <- as.numeric(geocode(impounds_2018_Q4$crossing[3740], output = "latlona", source = "google")[1])
impounds_2018_Q4$lat[3740] <- as.numeric(geocode(impounds_2018_Q4$crossing[3740], output = "latlona", source = "google")[2])
impounds_2018_Q4$geoAddress[3740] <- as.character(geocode(impounds_2018_Q4$crossing[3740], output = "latlona", source = "google")[3])
impounds_2018_Q4[3740, ]

#write.csv(impounds_2018_Q4, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q4.csv")


impounds_2018_Q1 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q1.csv")
impounds_2018_Q2 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q2.csv")
impounds_2018_Q3 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q3.csv")
impounds_2018_Q4 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_Q4.csv")
impounds_2018 <- rbind(impounds_2018_Q1, impounds_2018_Q2, impounds_2018_Q3, impounds_2018_Q4)



impounds_2018$state <- str_sub(impounds_2018$geoAddress, -13, -12)
impounds_2018[which(impounds_2018$state != "tx" & impounds_2018$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

# fix observations 1305, 1312, 2435, 19216, 24290, 24828, 26984, 27631, 27650, 28672, 30611, 30658, 32129, 33864

impounds_2018$crossing[1305] <- "2300 KINGSWOOD dr, dallas tx"
impounds_2018$lon[1305] <- as.numeric(geocode(impounds_2018$crossing[1305], output = "latlona", source = "google")[1])
impounds_2018$lat[1305] <- as.numeric(geocode(impounds_2018$crossing[1305], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[1305] <- as.character(geocode(impounds_2018$crossing[1305], output = "latlona", source = "google")[3])
impounds_2018[1305, ]

impounds_2018$crossing[1312] <- "2300 KINGSWOOD dr, dallas tx"
impounds_2018$lon[1312] <- as.numeric(geocode(impounds_2018$crossing[1312], output = "latlona", source = "google")[1])
impounds_2018$lat[1312] <- as.numeric(geocode(impounds_2018$crossing[1312], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[1312] <- as.character(geocode(impounds_2018$crossing[1312], output = "latlona", source = "google")[3])
impounds_2018[1312, ]

impounds_2018$crossing[2435] <- "7311 maplecrest dr, dallas tx"
impounds_2018$lon[2435] <- as.numeric(geocode(impounds_2018$crossing[2435], output = "latlona", source = "google")[1])
impounds_2018$lat[2435] <- as.numeric(geocode(impounds_2018$crossing[2435], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[2435] <- as.character(geocode(impounds_2018$crossing[2435], output = "latlona", source = "google")[3])
impounds_2018[2435, ]

impounds_2018$crossing[19216] <- "1200 KINGS HIGHWAY, dallas tx"
impounds_2018$lon[19216] <- as.numeric(geocode(impounds_2018$crossing[19216], output = "latlona", source = "google")[1])
impounds_2018$lat[19216] <- as.numeric(geocode(impounds_2018$crossing[19216], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[19216] <- as.character(geocode(impounds_2018$crossing[19216], output = "latlona", source = "google")[3])
impounds_2018[19216, ]

impounds_2018$crossing[24290] <- "1200 KINGS HIGHWAY, dallas tx"
impounds_2018$lon[24290] <- as.numeric(geocode(impounds_2018$crossing[24290], output = "latlona", source = "google")[1])
impounds_2018$lat[24290] <- as.numeric(geocode(impounds_2018$crossing[24290], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[24290] <- as.character(geocode(impounds_2018$crossing[24290], output = "latlona", source = "google")[3])
impounds_2018[24290, ]

impounds_2018$crossing[24828] <- "3103 CLYDEDALE, dallas tx"
impounds_2018$lon[24828] <- as.numeric(geocode(impounds_2018$crossing[24828], output = "latlona", source = "google")[1])
impounds_2018$lat[24828] <- as.numeric(geocode(impounds_2018$crossing[24828], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[24828] <- as.character(geocode(impounds_2018$crossing[24828], output = "latlona", source = "google")[3])
impounds_2018[24828, ]

impounds_2018$crossing[26984] <- NA
impounds_2018$lon[26984] <- NA
impounds_2018$lat[26984] <- NA
impounds_2018$geoAddress[26984] <- NA
impounds_2018[26984, ]

impounds_2018$crossing[27631] <- "3600 GLENHAVEN, dallas tx"
impounds_2018$lon[27631] <- as.numeric(geocode(impounds_2018$crossing[27631], output = "latlona", source = "google")[1])
impounds_2018$lat[27631] <- as.numeric(geocode(impounds_2018$crossing[27631], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[27631] <- as.character(geocode(impounds_2018$crossing[27631], output = "latlona", source = "google")[3])
impounds_2018[27631, ]

impounds_2018$crossing[27650] <- "3600 GLENHAVEN, dallas tx"
impounds_2018$lon[27650] <- as.numeric(geocode(impounds_2018$crossing[27650], output = "latlona", source = "google")[1])
impounds_2018$lat[27650] <- as.numeric(geocode(impounds_2018$crossing[27650], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[27650] <- as.character(geocode(impounds_2018$crossing[27650], output = "latlona", source = "google")[3])
impounds_2018[27650, ]

impounds_2018$crossing[28672] <- "1506 S Buckner Blvd, Dallas, TX 75217"
impounds_2018$lon[28672] <- as.numeric(geocode(impounds_2018$crossing[28672], output = "latlona", source = "google")[1])
impounds_2018$lat[28672] <- as.numeric(geocode(impounds_2018$crossing[28672], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[28672] <- as.character(geocode(impounds_2018$crossing[28672], output = "latlona", source = "google")[3])
impounds_2018[28672, ]

impounds_2018$crossing[30611] <- "1000 GLEN OAKS, dallas tx"
impounds_2018$lon[30611] <- as.numeric(geocode(impounds_2018$crossing[30611], output = "latlona", source = "google")[1])
impounds_2018$lat[30611] <- as.numeric(geocode(impounds_2018$crossing[30611], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[30611] <- as.character(geocode(impounds_2018$crossing[30611], output = "latlona", source = "google")[3])
impounds_2018[30611, ]

impounds_2018$crossing[30658] <- "1000 GLEN OAKS, dallas tx"
impounds_2018$lon[30658] <- as.numeric(geocode(impounds_2018$crossing[30658], output = "latlona", source = "google")[1])
impounds_2018$lat[30658] <- as.numeric(geocode(impounds_2018$crossing[30658], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[30658] <- as.character(geocode(impounds_2018$crossing[30658], output = "latlona", source = "google")[3])
impounds_2018[30658, ]

impounds_2018$crossing[32129] <- "24 S MARRIFIELD, dallas tx"
impounds_2018$lon[32129] <- as.numeric(geocode(impounds_2018$crossing[32129], output = "latlona", source = "google")[1])
impounds_2018$lat[32129] <- as.numeric(geocode(impounds_2018$crossing[32129], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[32129] <- as.character(geocode(impounds_2018$crossing[32129], output = "latlona", source = "google")[3])
impounds_2018[32129, ]

impounds_2018$crossing[33864] <- "647 W Main St, Lewisville, TX 75057"
impounds_2018$lon[33864] <- as.numeric(geocode(impounds_2018$crossing[33864], output = "latlona", source = "google")[1])
impounds_2018$lat[33864] <- as.numeric(geocode(impounds_2018$crossing[33864], output = "latlona", source = "google")[2])
impounds_2018$geoAddress[33864] <- as.character(geocode(impounds_2018$crossing[33864], output = "latlona", source = "google")[3])
impounds_2018[33864, ]


# all 2018 observations have correct state now
impounds_2018$state <- str_sub(impounds_2018$geoAddress, -13, -12)
impounds_2018[which(impounds_2018$state != "tx" & impounds_2018$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

#write.csv(impounds_2018, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2018_total_zipcodes.csv")


########################## 2017 #############################

impounds_2017_Q1 <- impounds %>% filter(year == 2017) %>% filter(month == 1 | month == 2 | month == 3) %>% filter(!is.na(crossing))
dim(impounds_2017_Q1) #6794 observations (2 missing values for crossing)

for(i in 5454:nrow(impounds_2017_Q1)){
  result <- geocode(impounds_2017_Q1$crossing[i], output = "latlona", source = "google")
  impounds_2017_Q1$lon[i] <- as.numeric(result[1])
  impounds_2017_Q1$lat[i] <- as.numeric(result[2])
  impounds_2017_Q1$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematic observations c(5453)

impounds_2017_Q1$crossing[5453] <- "7700 BELTLINE, dallas tx"
impounds_2017_Q1$lon[5453] <- as.numeric(geocode(impounds_2017_Q1$crossing[5453], output = "latlona", source = "google")[1])
impounds_2017_Q1$lat[5453] <- as.numeric(geocode(impounds_2017_Q1$crossing[5453], output = "latlona", source = "google")[2])
impounds_2017_Q1$geoAddress[5453] <- as.character(geocode(impounds_2017_Q1$crossing[5453], output = "latlona", source = "google")[3])
impounds_2017_Q1[5453, ]

#write.csv(impounds_2017_Q1, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q1.csv")



impounds_2017_Q2 <- impounds %>% filter(year == 2017) %>% filter(month == 4 | month == 5 | month == 6) %>% filter(!is.na(crossing))
dim(impounds_2017_Q2) #9535 observations (0 missing values for crossing)

for(i in 1:nrow(impounds_2017_Q2)){
  result <- geocode(impounds_2017_Q2$crossing[i], output = "latlona", source = "google")
  impounds_2017_Q2$lon[i] <- as.numeric(result[1])
  impounds_2017_Q2$lat[i] <- as.numeric(result[2])
  impounds_2017_Q2$geoAddress[i] <- as.character(result[3])
  print(i)
}

#write.csv(impounds_2017_Q2, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q2.csv")



impounds_2017_Q3 <- impounds %>% filter(year == 2017) %>% filter(month == 7 | month == 8 | month == 9) %>% filter(!is.na(crossing))
dim(impounds_2017_Q3) #9124 observations (0 missing values for crossing)

for(i in 1:nrow(impounds_2017_Q3)){
  result <- geocode(impounds_2017_Q3$crossing[i], output = "latlona", source = "google")
  impounds_2017_Q3$lon[i] <- as.numeric(result[1])
  impounds_2017_Q3$lat[i] <- as.numeric(result[2])
  impounds_2017_Q3$geoAddress[i] <- as.character(result[3])
  print(i)
}

#write.csv(impounds_2017_Q3, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q3.csv")


impounds_2017_Q4 <- impounds %>% filter(year == 2017) %>% filter(month == 10 | month == 11 | month == 12) %>% filter(!is.na(crossing))
dim(impounds_2017_Q4) #7943 observations (0 missing values for crossing)

for(i in 2810:nrow(impounds_2017_Q4)){
  result <- geocode(impounds_2017_Q4$crossing[i], output = "latlona", source = "google")
  impounds_2017_Q4$lon[i] <- as.numeric(result[1])
  impounds_2017_Q4$lat[i] <- as.numeric(result[2])
  impounds_2017_Q4$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematic observations c(2792, 2809)

impounds_2017_Q4$crossing[2792] <- "9475 FOREST SPRING, dallas tx"
impounds_2017_Q4$lon[2792] <- as.numeric(geocode(impounds_2017_Q4$crossing[2792], output = "latlona", source = "google")[1])
impounds_2017_Q4$lat[2792] <- as.numeric(geocode(impounds_2017_Q4$crossing[2792], output = "latlona", source = "google")[2])
impounds_2017_Q4$geoAddress[2792] <- as.character(geocode(impounds_2017_Q4$crossing[2792], output = "latlona", source = "google")[3])
impounds_2017_Q4[2792, ]

impounds_2017_Q4$crossing[2809] <- "9475 FOREST SPRING, dallas tx"
impounds_2017_Q4$lon[2809] <- as.numeric(geocode(impounds_2017_Q4$crossing[2809], output = "latlona", source = "google")[1])
impounds_2017_Q4$lat[2809] <- as.numeric(geocode(impounds_2017_Q4$crossing[2809], output = "latlona", source = "google")[2])
impounds_2017_Q4$geoAddress[2809] <- as.character(geocode(impounds_2017_Q4$crossing[2809], output = "latlona", source = "google")[3])
impounds_2017_Q4[2809, ]

#write.csv(impounds_2017_Q4, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q4.csv")


impounds_2017_Q1 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q1.csv")
impounds_2017_Q2 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q2.csv")
impounds_2017_Q3 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q3.csv")
impounds_2017_Q4 <- read.csv("/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_Q4.csv")
impounds_2017 <- rbind(impounds_2017_Q1, impounds_2017_Q2, impounds_2017_Q3, impounds_2017_Q4)



impounds_2017$state <- str_sub(impounds_2017$geoAddress, -13, -12)
impounds_2017[which(impounds_2017$state != "tx" & impounds_2017$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

# fix observations 338, 2747, 3067, 3608, 3639, 5344, 8690, 26812, 29372


impounds_2017$crossing[338] <- "9600 FOREST LN ,DALLAS TX 75243"
impounds_2017$lon[338] <- as.numeric(geocode(impounds_2017$crossing[338], output = "latlona", source = "google")[1])
impounds_2017$lat[338] <- as.numeric(geocode(impounds_2017$crossing[338], output = "latlona", source = "google")[2])
impounds_2017$geoAddress[338] <- as.character(geocode(impounds_2017$crossing[338], output = "latlona", source = "google")[3])
impounds_2017[338, ]

impounds_2017$crossing[2747] <- "3223 LOCUST, dallas tx"
impounds_2017$lon[2747] <- as.numeric(geocode(impounds_2017$crossing[2747], output = "latlona", source = "google")[1])
impounds_2017$lat[2747] <- as.numeric(geocode(impounds_2017$crossing[2747], output = "latlona", source = "google")[2])
impounds_2017$geoAddress[2747] <- as.character(geocode(impounds_2017$crossing[2747], output = "latlona", source = "google")[3])
impounds_2017[2747, ]

impounds_2017$crossing[3067] <- NA
impounds_2017$lon[3067] <- NA
impounds_2017$lat[3067] <- NA
impounds_2017$geoAddress[3067] <- NA
impounds_2017[3067, ]

impounds_2017$crossing[3608] <- NA
impounds_2017$lon[3608] <- NA
impounds_2017$lat[3608] <- NA
impounds_2017$geoAddress[3608] <- NA
impounds_2017[3608, ]

impounds_2017$crossing[3639] <- NA
impounds_2017$lon[3639] <- NA
impounds_2017$lat[3639] <- NA
impounds_2017$geoAddress[3639] <- NA
impounds_2017[3639, ]

impounds_2017$crossing[5344] <- "3500 DEAR MEADOW, DALLAS TX"
impounds_2017$lon[5344] <- as.numeric(geocode(impounds_2017$crossing[5344], output = "latlona", source = "google")[1])
impounds_2017$lat[5344] <- as.numeric(geocode(impounds_2017$crossing[5344], output = "latlona", source = "google")[2])
impounds_2017$geoAddress[5344] <- as.character(geocode(impounds_2017$crossing[5344], output = "latlona", source = "google")[3])
impounds_2017[5344, ]


impounds_2017$crossing[8690] <- NA
impounds_2017$lon[8690] <- NA
impounds_2017$lat[8690] <- NA
impounds_2017$geoAddress[8690] <- NA
impounds_2017[8690, ]

impounds_2017$crossing[26812] <- "9800 ESTATE LN, DALLAS TX"
impounds_2017$lon[26812] <- as.numeric(geocode(impounds_2017$crossing[26812], output = "latlona", source = "google")[1])
impounds_2017$lat[26812] <- as.numeric(geocode(impounds_2017$crossing[26812], output = "latlona", source = "google")[2])
impounds_2017$geoAddress[26812] <- as.character(geocode(impounds_2017$crossing[26812], output = "latlona", source = "google")[3])
impounds_2017[26812, ]

impounds_2017$crossing[29372] <- "3000 OXFORDSHIRE, dallas tx"
impounds_2017$lon[29372] <- as.numeric(geocode(impounds_2017$crossing[29372], output = "latlona", source = "google")[1])
impounds_2017$lat[29372] <- as.numeric(geocode(impounds_2017$crossing[29372], output = "latlona", source = "google")[2])
impounds_2017$geoAddress[29372] <- as.character(geocode(impounds_2017$crossing[29372], output = "latlona", source = "google")[3])
impounds_2017[29372, ]

# all 2017 observations have correct state now
impounds_2017$state <- str_sub(impounds_2017$geoAddress, -13, -12)
impounds_2017[which(impounds_2017$state != "tx" & impounds_2017$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

#write.csv(impounds_2017, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2017_total_zipcodes.csv")



######################### 2016 ####################################

impounds_2016 <- impounds %>% filter(year == 2016) %>% filter(!is.na(crossing))
dim(impounds_2016) #6883 observations (0 missing values for crossing)

for(i in 5868:nrow(impounds_2016)){
  result <- geocode(impounds_2016$crossing[i], output = "latlona", source = "google")
  impounds_2016$lon[i] <- as.numeric(result[1])
  impounds_2016$lat[i] <- as.numeric(result[2])
  impounds_2016$geoAddress[i] <- as.character(result[3])
  print(i)
}
# problematic observations c(5867)

impounds_2016$crossing[5867] <- "9900 NEWHEART ST, dallas tx"
impounds_2016$lon[5867] <- as.numeric(geocode(impounds_2016$crossing[5867], output = "latlona", source = "google")[1])
impounds_2016$lat[5867] <- as.numeric(geocode(impounds_2016$crossing[5867], output = "latlona", source = "google")[2])
impounds_2016$geoAddress[5867] <- as.character(geocode(impounds_2016$crossing[5867], output = "latlona", source = "google")[3])
impounds_2016[5867, ]

#write.csv(impounds_2016, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2016.csv")

impounds_2016$state <- str_sub(impounds_2016$geoAddress, -13, -12)
impounds_2016[which(impounds_2016$state != "tx" & impounds_2016$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

impounds_2016$crossing[1804] <- "11800 CRESTLINE, dallas tx"
impounds_2016$lon[1804] <- as.numeric(geocode(impounds_2016$crossing[1804], output = "latlona", source = "google")[1])
impounds_2016$lat[1804] <- as.numeric(geocode(impounds_2016$crossing[1804], output = "latlona", source = "google")[2])
impounds_2016$geoAddress[1804] <- as.character(geocode(impounds_2016$crossing[1804], output = "latlona", source = "google")[3])
impounds_2016[1804, ]

# all 2016 observations have correct state back
impounds_2016$state <- str_sub(impounds_2016$geoAddress, -13, -12)
impounds_2016[which(impounds_2016$state != "tx" & impounds_2016$state != "ll"), c("crossing", "ZipCode", "geoAddress", "state")]

#write.csv(impounds_2016, file = "/Users/rob.pruette/Documents/SMU Fall 2020/STAT 6366/Consulting Project/impounds_2016_total_zipcodes.csv")
