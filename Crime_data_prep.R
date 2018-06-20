# Use this file to clean and prepare the 2017 crime dataset for use

# Load raw data
crime <- read.csv('C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Crime_Incidents_in_2017.csv') 


# install.packages('splitstackshape')
library(splitstackshape)
crime <- cSplit(crime, 'START_DATE', sep="T", type.convert=FALSE)    # split and clean dates of crimes
crime <- cSplit(crime, 'END_DATE', sep="T", type.convert=FALSE)      # focus on start date/time of each crime
names(crime)[names(crime) == 'START_DATE_2'] <- 'START_TIME'
names(crime)[names(crime) == 'START_DATE_1'] <- 'START_DATE'
names(crime)[names(crime) == 'END_DATE_2'] <- 'END_TIME'
names(crime)[names(crime) == 'END_DATE_1'] <- 'END_DATE'
names(crime)[names(crime) == 'BID'] <- 'HOOD'                    # clean confusingly named variables 
names(crime)[names(crime) == 'ï..X'] <- 'X'

crime$START_DATE <- as.Date(crime$START_DATE)           #continue formatting - separate day/date from time
crime$END_DATE <- as.Date(crime$END_DATE)
library(dplyr)
crime <- crime %>%
  mutate(start_month = format(START_DATE, "%m"), start_year = format(START_DATE, "%Y"))

crime <- cSplit(crime, 'START_TIME', sep=".", type.convert=FALSE)
names(crime)[names(crime) == 'START_TIME_1'] <- 'START_TIME'
crime$START_TIME_2 <- NULL

# install.packages('chron')
library(chron)
crime$START_TIME <- chron(times=crime$START_TIME, format = c(times = "h:m:s"))
crime$HOUR <- hours(crime$START_TIME)
crime$TIME_OF_DAY <- ifelse((crime$HOUR >= 5) & (crime$HOUR <= 12), "morning", ifelse((crime$HOUR > 12) & (crime$HOUR <= 20), "afternoon", "night"))

library(dplyr)
crime <- crime %>%
  mutate(start_day = format(START_DATE, "%d"), start_month = format(START_DATE, "%m"), start_year = format(START_DATE, "%Y"))

crime$start_day <- as.numeric(crime$start_day)
crime$start_month <- as.numeric(crime$start_month)          # set day of week values and corresponding levels 
crime$start_year <- as.numeric(crime$start_year)
crime$DoW <- day.of.week(crime$start_day, crime$start_month, crime$start_year)
crime$dow <- ifelse((crime$DoW == 0), "Sunday", ifelse((crime$DoW == 1), "Monday", ifelse((crime$DoW == 2), "Tuesday", ifelse((crime$DoW == 3), "Wednesday", ifelse((crime$DoW == 4), "Thursday", ifelse((crime$DoW == 5), "Friday", "Saturday"))))))
crime$dow <- factor(crime$dow, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# create new variable specifying geographic quadrant of the city 
# ('unclear' refers to crimes that took place on dividing streets, like North Capitol St.)
crime$quarter <- ifelse(grepl("NE", crime$BLOCK, ignore.case = T), "NE", 
                        ifelse(grepl("SW", crime$BLOCK, ignore.case = T), "SW", ifelse(grepl("NW", crime$BLOCK, ignore.case = T), "NW", ifelse(grepl("SE", crime$BLOCK, ignore.case = T), "SE", "unclear"))))

# write cleaned dataset to new file for use with Shiny
write.csv(crime, "C:\\Users\\Jordan\\Documents\\MSBA Coursework\\Programming for Analytics\\Individual Project\\DC Crime Shiny\\Crime_new.csv")
