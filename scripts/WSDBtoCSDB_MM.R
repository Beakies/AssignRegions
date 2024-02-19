# M. Murphy February 16th, 2024

#test
# This code is used to format data from WSDB into the form required for CSDB

# Download and install packages if not already installed: 

pacman::p_load(writexl, readxl, readr, tidyverse, lubridate, here, dplyr, tidyr)

# Load test data sheet 

file_path <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Input\representative_dataset.csv)" 
WSDB_data <- read_csv(file_path)

# Read data tables
Species_code_table <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Species code table.csv)"
Species_code_table <- read_csv(Species_code_table)

Activity_type_code_table <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Activity type code table.csv)"
Activity_type_code_table <- read_csv(Activity_type_code_table)

Effort_code_table <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Effort code table.csv)"
Effort_code_table <- read_csv(Effort_code_table)

Data_source_code_table  <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Data source code table.csv)"
Data_source_code_table <- read_csv(Data_source_code_table)

# CREATE NEW data frame called CSDB_data with correct column names (run this by Joy)

# Format Year Month and Day to pull from UTC_Year, UTC_Month, UTC_Day when available, if not use Reported_Year, Reported_Month, Reported_Day
# HAVING TROUBLE WITH THIS SYNTAX, what was the fix we did in the meeting? Also do I want to create just this new column in CSDB_data df, to keep things clear?

CSDB_data <- WSDB_data %>%
  mutate(Year = ifelse(is.null(UTC_Year),Reported_Year, UTC_Year),
         Month = ifelse(is.null(UTC_Month),Reported_Month, UTC_Month),
         Day = ifelse(is.null(UTC_Day),Reported_Day, Reported_Day))

# 1) Modify Regional_Primary_Key so that the first 2 digits are “11” (representing MAR Region) and the total number of digits is 11 (including the WS_EVENT_ID).
# example: if WS_EVENT_ID is 1234, then the Regional_Primary_Key should be 11000001234  

# Modify Regional_Primary_Key

# First 2 digits are "11" representing MAR Region

string1 <- 11

# Add 9 digits to string1 (totaling 11 digits)
# FIGURE OUT HOW TO MODIFY THIS TO WORK WITH VARYING NUMBERS OF DIGITS

string2 <- sprintf("%09d", WSDB_data$WS_EVENT_ID)
CSDB_data$Regional_Primary_Key = paste(string1,string2, sep= "")

# Verify Regional_Primary_Key is in the correct format (first 2 digits are "11" and the total number of digits is 11)
#print(Regional_Primary_Key) 

# 2) Change the format of UTC_Time and Reported_Time to hh:mm:ss
# CHANGE TO WS_TIME AND WS_TIME_UTC

suppressWarnings(CSDB_data <- WSDB_data %>%
                   mutate(UTC_Time = sprintf("%04d", UTC_Time),
                          UTC_Time = parse_date_time(UTC_Time, orders= "HM"),
                          UTC_Time = format(UTC_Time, format = "%H:%M:%S")))

suppressWarnings(CSDB_data <- WSDB_data %>%
                   mutate(Reported_Time = sprintf("%04d", Reported_Time),
                          Reported_Time = parse_date_time(Reported_Time,orders= "HM"),
                          Reported_Time = format(Reported_Time, format = "%H:%M:%S")))

# 3) Map LCQUECODES_CD to Location_Uncertainty_Code

CSDB_data <- mutate(Location_Uncertainty_Code = case_when(LCQECODE_CD == 4 ~ 3, 
                                                          LCQECODE_CD == 5 ~ 3, 
                                                          LCQECODE_CD == 6 ~ 3, 
                                                          LCQECODE_CD == 7 ~ 3,
                                                          is.na(LCQECODE_CD) ~ 0,
                                                          TRUE ~ as.numeric(LCQECODE_CD)))

# 4) Map LCQECODE_CD to Location_Uncertainty_Reason_Code

CSDB_data <- WSDB_data %>%
  mutate(Location_Uncertainty_Reason_Code = case_when(LCQECODE_CD == 3 ~ 1, 
                                                      LCQECODE_CD == 4 ~ 2, 
                                                      LCQECODE_CD == 5 ~ 3, 
                                                      LCQECODE_CD == 6 ~ 4, 
                                                      LCQECODE_CD == 7 ~ 5,
                                                      is.na(LCQECODE_CD) ~ 0,
                                                      LCQECODE_CD == 1 ~ NA_real_,
                                                      LCQECODE_CD == 2 ~ NA_real_,
                                                      TRUE ~ as.numeric(LCQECODE_CD)))

# 5 and 6) Map COMMONNAME to ITIS_Code and Species_code using Species_code_table
# IS IT OK TO DO THIS IN ONE STEP

CSDB_data %>% left_join(Species_code_table, CSDB_data, by = "COMMONNAME")

# 7) Map IDREL_CD to SpeciesID_Uncertainty_Code (put 0's in for null) 

CSDB_data <- WSDB_data %>%
  mutate(SpeciesID_Uncertainty_Code = case_when(IDREL_CD == 1 ~ 1, 
                                                IDREL_CD == 2 ~ 2, 
                                                IDREL_CD == 3 ~ 3,
                                                IDREL_CD == 9 ~ 0,
                                                is.na(IDREL_CD) ~ 0,
                                                TRUE ~ as.numeric(IDREL_CD)))

# 8) Map Count_Uncertainty_Code (put 0’s in for null)

CSDB_data <- WSDB_data %>%
  mutate(Count_Uncertainty_Code = coalesce(Count_Uncertainty_Code, 0))

# 9) Create column called Behaviour_Comments that concatenates the five BEHAVIOUR_DESC columns into the one column with hyphens in between.

# Remove 'NOT RECORDED' from concatenated BEHAVIOUR_CDs

WSDB_data["BEHAVIOUR_DESC"][WSDB_data["BEHAVIOUR_DESC"] == "NOT RECORDED"] <- NA
WSDB_data["BEHAVIOUR_DESC_1"][WSDB_data["BEHAVIOUR_DESC_1"] == "NOT RECORDED"] <- NA
WSDB_data["BEHAVIOUR_DESC_2"][WSDB_data["BEHAVIOUR_DESC_2"] == "NOT RECORDED"] <- NA
WSDB_data["BEHAVIOUR_DESC_3"][WSDB_data["BEHAVIOUR_DESC_3"] == "NOT RECORDED"] <- NA
WSDB_data["BEHAVIOUR_DESC_4"][WSDB_data["BEHAVIOUR_DESC_4"] == "NOT RECORDED"] <- NA

# Concatenate five BEHAVIOUR_DESC columns separated by hyphens, removing NA's

CSDB_data <- WSDB_data %>%
  unite('Behaviour_Comments', c('BEHAVIOUR_DESC','BEHAVIOUR_DESC_1','BEHAVIOUR_DESC_2','BEHAVIOUR_DESC_3','BEHAVIOUR_DESC_4'), sep=" - ", na.rm = TRUE)

# Reorder columns (note that COMMONNAME and BEHAVIOUR_DESC columns get removed)

#CSDB_data <- WSDB_data[, c("Regional_Primary_Key", "Year", "Month", "Day", "UTC_Time", "Reported_Time", "Latitude", "Longitude", "Location_Uncertainty_Code", "Location_Uncertainty_Reason_Code", "Species_Code", "ITIS_Code", "SpeciesID_Uncertainty_Code", "Species_Comments",
"Reported_Count", "Min_Count", "Max_Count", "Count_Uncertainty_Code", "Behaviour_Comments", "GEARIMPACT_CD", "Distance", "Reported_SeaState", "Platform_Type_Code", "Activity_Type_Code", "Effort", 
"Data_Source_Code", "Suspected_Data_Issue_Reason", "Comments")]

# 10) Map Animal_Status_Code using GEARIMPACT_CD and Behaviour_Comments

CSDB_data <- WSDB_data %>%
  mutate(Animal_Status_Code = case_when(GEARIMPACT_CD == 9 ~ 0,
                                        GEARIMPACT_CD == 5 | Behaviour_Comments == "VISIBLE INJURY"| Behaviour_Comments == "STRUCK BY VESSEL" ~ 2,
                                        GEARIMPACT_CD == 1 | GEARIMPACT_CD == 2 | Behaviour_Comments == "TANGLED IN FISHING GEAR"| Behaviour_Comments == "DISENTANGLED RELEASED ALIVE" ~ 3,
                                        GEARIMPACT_CD == 4 ~ 4,
                                        is.na(GEARIMPACT_CD) ~ 0,
                                        TRUE ~ as.numeric(GEARIMPACT_CD)))

# 11) Remove record if GEARIMPACT_CD = 3
# THIS WILL BE DONE AT THE QUERY LEVEL INSTEAD

# CSDB_data <- WSDB_data %>%
# filter(!grepl(3, GEARIMPACT_CD))

#CSDB_data <- WSDB_data[, c("Regional_Primary_Key","Year", "Month", "Day", "UTC_Time", "Reported_Time", "Latitude", "Longitude", "Location_Uncertainty_Code", "Location_Uncertainty_Reason_Code", "Species_Code", "ITIS_Code", "SpeciesID_Uncertainty_Code", "Species_Comments",
"Reported_Count", "Min_Count", "Max_Count", "Count_Uncertainty_Code", "Animal_Status_Code", "Behaviour_Comments", "Distance", "Reported_SeaState", "Platform_Type_Code", "Activity_Type_Code", "Effort", 
"Data_Source_Code", "Suspected_Data_Issue_Reason", "Comments")]

# 12) Add one decimal place to Reported_SeaState (always zero, #.0), **REMOVE NA's

CSDB_data <- WSDB_data %>%
  mutate(Reported_SeaState = sprintf(Reported_SeaState, fmt = '%.1f'))

# 13) Map Reported_SeaState and add one decimal place to Reported_SeaState (always zero, #.0)

my_data<- my_data %>%
  mutate(Reported_SeaState = case_when(Reported_SeaState == '13.0' ~ NA_character_,
                                       TRUE ~ as.character(Reported_SeaState)))

# 14) Map Platform_Type_Code

PLATFORM_TYPE_CD <- my_data$Platform_Type_Code

my_data <- my_data %>%
  mutate(Platform_Type_Code = case_when(PLATFORM_TYPE_CD == 4 ~ 0,
                                        is.na(PLATFORM_TYPE_CD) ~ 0,
                                        TRUE ~ as.numeric(PLATFORM_TYPE_CD)))

# 15) Map TRIPTYPE_CD to Activity_Type_Code using Trip_type_table

CSDB_data %>% left_join(Trip_type_table, CSDB_data, by = "TRIPTYPE_CD")

# 16) Map Effort using Effort_code_table

CSDB_data %>% left_join(Effort_code_table, CSDB_data, by = "DATA_TYPE_CD")

CSDB_data <- WSDB_data %>%
  mutate(is.na(DATA_TYPE_CD) ~ 0,
        TRUE ~ as.numeric(DATA_TYPE_CD))

# 17) Map Data_Source_Code using Data_source_table (note that this is ordered by CSDB code)

CSDB_data %>% left_join(Data_source_code_table, CSDB_data, by = "DATASOURCE_CD")

CSDB_data <- WSDB_data %>%
  mutate(Data_Source_Code = case_when(is.na(DATASOURCE_CD) ~ 0,
                                      TRUE ~ as.numeric(DATASOURCE_CD)))

# 18) Add a column called Suspected_Data_Issue before Suspected_Data_Issue_Reason, and populate with ‘Yes’ when the reason column is not null and ‘No’ when it is null. 

CSDB_data <- WSDB_data %>%
  mutate(Suspected_Data_Issue = case_when(is.na(Suspected_Data_Issue_Reason) ~ "No",
                                          TRUE ~ "Yes"))

# Reorder columns so would it be easier to do this just once?
CSDB_data <- WSDB_data[, c("Regional_Primary_Key","Year", "Month", "Day", "UTC_Time", "Reported_Time", "Latitude", "Longitude", "Location_Uncertainty_Code", "Location_Uncertainty_Reason_Code", "Species_Code", "ITIS_Code", "SpeciesID_Uncertainty_Code", "Species_Comments",
                       "Reported_Count", "Min_Count", "Max_Count", "Count_Uncertainty_Code", "Animal_Status_Code", "Behaviour_Comments", "Distance", "Reported_SeaState", "Platform_Type_Code", "Activity_Type_Code", "Effort", 
                       "Data_Source_Code", "Suspected_Data_Issue", "Suspected_Data_Issue_Reason", "Comments")]

# 19) Format Latitude and Longitude to four decimal places

CSDB_data <- WSDB_data %>%
  mutate(Latitude = sprintf(Latitude, fmt = '%.4f'))

CSDB_data <- WSDB_data %>%
  mutate(Longitude = sprintf(Longitude, fmt = '%.4f'))

# 20) Remove commas from Comments and Behaviour_Comments

CSDB_data$Comments <- gsub(",","",WSDB_data$Comments)

CSDB_data$Behaviour_Comments <- gsub(",","",WSDB_data$Behaviour_Comments)

# 21) Export as .xlsx including today's date

today <- Sys.Date()

output_file = paste0("CSDB ", year, " created ", today, ".xlsx")
write_xlsx(CSDB_data, here("Output", output_file))

view(CSDB_data)
