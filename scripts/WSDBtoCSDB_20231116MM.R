# M. Murphy March 15th, 2023

# This code is used to format data from WSDB into the form required for CSDB

# Download and install packages if not already installed: 

if (!require("writexl")) install.packages("writexl")
if (!require("readxl")) install.packages("readxl")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("here")) install.packages("here")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

# Then open the packages

library(writexl)
library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(tidyr)

# Load test data sheet 

file_path <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Yearly data exports.xlsx)" 
year <- "2020"
my_data <- read_excel(file_path, sheet = year)

# Format Year Month and Day to pull from UTC_Year, UTC_Month, UTC_Day when available, if not use Reported_Year, Reported_Month, Reported_Day

my_data <- my_data %>%
  mutate(Year = ifelse(is.null(my_data$UTC_Year),my_data$UTC_Year, my_data$Reported_Year),
         Month = ifelse(is.null(my_data$UTC_Month),my_data$UTC_Month, my_data$Reported_Month),
         Day = ifelse(is.null(my_data$UTC_Day),my_data$UTC_Day, my_data$Reported_Day))

# 1) Modify Regional_Primary_Key so that the first 2 digits are “11” (representing MAR Region) and the total number of digits is 11 (including the WS_EVENT_ID).
# example: if WS_EVENT_ID is 1234, then the Regional_Primary_Key should be 11000001234  

# Rename Regional_Primary_Key from WSDB "WS_EVENT_ID"

WS_EVENT_ID <- my_data$Regional_Primary_Key

list(WS_EVENT_ID)

# Modify Regional_Primary_Key
# First 2 digits are "11" representing MAR Region

string1 <- 11

# Add 9 digits to string1 (totaling 11 digits)

string2 <- sprintf("%09d", WS_EVENT_ID)
Regional_Primary_Key = paste(string1,string2, sep= "")

# Verify Regional_Primary_Key is in the correct format (first 2 digits are "11" and the total number of digits is 11)

print(Regional_Primary_Key) 

# Include the modified Regional_Primary_Key in the data frame

my_data$Regional_Primary_Key <- Regional_Primary_Key

# 2) Change the format of UTC_Time and Reported_Time to hh:mm:ss

suppressWarnings(my_data <- my_data %>%
  mutate(UTC_Time = sprintf("%04d", UTC_Time),
         UTC_Time = parse_date_time(UTC_Time, orders= "HM"),
         UTC_Time = format(UTC_Time, format = "%H:%M:%S")))

suppressWarnings(my_data <- my_data %>%
  mutate(Reported_Time = sprintf("%04d", Reported_Time),
         Reported_Time = parse_date_time(Reported_Time,orders= "HM"),
         Reported_Time = format(Reported_Time, format = "%H:%M:%S")))

# 3) Map LCQUECODES_CD to Location_Uncertainty_Code

LCQECODE_CD <- my_data$Location_Uncertainty_Code
list(LCQECODE_CD)

my_data <- my_data %>%
mutate(Location_Uncertainty_Code = case_when(LCQECODE_CD == 1 ~ 1, 
                                             LCQECODE_CD == 2 ~ 2, 
                                             LCQECODE_CD == 3 ~ 3, 
                                             LCQECODE_CD == 4 ~ 3, 
                                             LCQECODE_CD == 5 ~ 3, 
                                             LCQECODE_CD == 6 ~ 3, 
                                             LCQECODE_CD == 7 ~ 3,
                                             is.na(LCQECODE_CD) ~ 0,
                                             TRUE ~ as.numeric(LCQECODE_CD)))

# 4) Map LCQECODE_CD to Location_Uncertainty_Reason_Code

my_data <- my_data %>%
mutate(Location_Uncertainty_Reason_Code = case_when(LCQECODE_CD == 3 ~ 1, 
                                                      LCQECODE_CD == 4 ~ 2, 
                                                      LCQECODE_CD == 5 ~ 3, 
                                                      LCQECODE_CD == 6 ~ 4, 
                                                      LCQECODE_CD == 7 ~ 5,
                                                      is.na(LCQECODE_CD) ~ 0,
                                                      LCQECODE_CD == 1 ~ NA_real_,
                                                      LCQECODE_CD == 2 ~ NA_real_,
                                                      TRUE ~ as.numeric(LCQECODE_CD)))

# 5) Map COMMONNAME to ITIS_Code
my_data <- my_data %>%
  mutate(ITIS_Code = case_when(COMMONNAME == "DOLPHIN-ATLANTIC BOTTLENOSE" ~ "180426", 
                               COMMONNAME == "DOLPHIN-STRIPED" ~ "180434", 
                               COMMONNAME == "DOLPHIN-COMMON" ~ "180438", 
                               COMMONNAME == "DOLPHIN-FRASER'S" ~ "180440", 
                               COMMONNAME == "DOLPHIN-WHITE-BEAKED" ~ "180442", 
                               COMMONNAME == "DOLPHIN-ATLANTIC WHITE-SIDED" ~ "180443", 
                               COMMONNAME == "DOLPHIN-RISSO'S" ~ "180457", 
                               COMMONNAME == "FALSE KILLER WHALE" ~ "180463", 
                               COMMONNAME == "WHALE-KILLER" ~ "180469", 
                               COMMONNAME == "PORPOISE-HARBOUR" ~ "180473", 
                               COMMONNAME == "WHALE-BELUGA" ~ "180483", 
                               COMMONNAME == "WHALE-NARWHAL" ~ "180485", 
                               COMMONNAME == "WHALE-SPERM" ~ "180489", 
                               COMMONNAME == "PYGMY SPERM WHALE" ~ "180491", 
                               COMMONNAME == "Dwarf sperm whale" ~ "180492", 
                               COMMONNAME == "WHALE- CUVIER'S BEAKED" ~ "180498", 
                               COMMONNAME == "WHALE-NORTHERN BOTTLENOSE" ~ "180504", 
                               COMMONNAME == "WHALE-TRUES BEAKED" ~ "180508", 
                               COMMONNAME == "Gervais' beaked whale" ~ "180509", 
                               COMMONNAME == "WHALE-SOWERBY'S BEAKED" ~ "180515", 
                               COMMONNAME == "WHALE-BLAINVILLE'S BEAKED" ~ "180517", 
                               COMMONNAME == "WHALE-MINKE" ~ "180524", 
                               COMMONNAME == "WHALE-SEI" ~ "180526", 
                               COMMONNAME == "WHALE-FIN" ~ "180527", 
                               COMMONNAME == "WHALE-BLUE" ~ "180528", 
                               COMMONNAME == "WHALE-HUMPBACK" ~ "180530", 
                               COMMONNAME == "WHALE-BOWHEAD" ~ "180533", 
                               COMMONNAME == "WHALE-NORTH ATLANTIC RIGHT" ~ "180537", 
                               COMMONNAME == "DOLPHIN- ATLANTIC SPOTTED" ~ "552460", 
                               COMMONNAME == "WHALE-LONG-FINNED PILOT" ~ "552461", 
                               COMMONNAME == "WHALE-FIN/SEI" ~ NA_character_, 
                               COMMONNAME == "CETACEAN (NS)" ~ NA_character_, 
                               COMMONNAME == "WHALE-BALEEN (NS)" ~ NA_character_,
                               COMMONNAME == "WHALE-BEAKED (NS)" ~ NA_character_, 
                               COMMONNAME == "Unidentified dolphin" ~ NA_character_, 
                               COMMONNAME == "DOLPHINS/PORPOISE (NS)" ~ NA_character_, 
                               COMMONNAME == "Unidentified kogia" ~ NA_character_, 
                               COMMONNAME == "WHALE-MESOPLODONT (NS)" ~ NA_character_,
                               COMMONNAME == "WHALES (NS)" ~ NA_character_)) 

# 6) Map COMMONNAME to Species_Code
my_data <- my_data %>%
  mutate(Species_Code = case_when(COMMONNAME == "DOLPHIN-ATLANTIC BOTTLENOSE" ~ "BNDO", 
                                  COMMONNAME == "DOLPHIN-STRIPED" ~ "STDO", 
                                  COMMONNAME == "DOLPHIN-COMMON" ~ "SADO", 
                                  COMMONNAME == "DOLPHIN-FRASER'S" ~ "FRDO", 
                                  COMMONNAME == "DOLPHIN-WHITE-BEAKED" ~ "WBDO", 
                                  COMMONNAME == "DOLPHIN-ATLANTIC WHITE-SIDED" ~ "AWDO", 
                                  COMMONNAME == "DOLPHIN-RISSO'S" ~ "GRAM", 
                                  COMMONNAME == "FALSE KILLER WHALE" ~ "FKWH", 
                                  COMMONNAME == "WHALE-KILLER" ~ "KIWH", 
                                  COMMONNAME == "PORPOISE-HARBOUR" ~ "HAPO", 
                                  COMMONNAME == "WHALE-BELUGA" ~ "BELU", 
                                  COMMONNAME == "WHALE-NARWHAL" ~ "NRWH", 
                                  COMMONNAME == "WHALE-SPERM" ~ "SPWH", 
                                  COMMONNAME == "PYGMY SPERM WHALE" ~ "PSWH", 
                                  COMMONNAME == "Dwarf sperm whale" ~ "DSWH", 
                                  COMMONNAME == "WHALE- CUVIER'S BEAKED" ~ "CUBW", 
                                  COMMONNAME == "WHALE-NORTHERN BOTTLENOSE" ~ "NBWH", 
                                  COMMONNAME == "WHALE-TRUES BEAKED" ~ "TRBW", 
                                  COMMONNAME == "Gervais' beaked whale" ~ "GEBW", 
                                  COMMONNAME == "WHALE-SOWERBY'S BEAKED" ~ "SBWH", 
                                  COMMONNAME == "WHALE-BLAINVILLE'S BEAKED" ~ "BLBW", 
                                  COMMONNAME == "WHALE-MINKE" ~ "MIWH", 
                                  COMMONNAME == "WHALE-SEI" ~ "SEWH", 
                                  COMMONNAME == "WHALE-FIN" ~ "FIWH", 
                                  COMMONNAME == "WHALE-BLUE" ~ "BLWH", 
                                  COMMONNAME == "WHALE-HUMPBACK" ~ "HUWH", 
                                  COMMONNAME == "WHALE-BOWHEAD" ~ "BOWH", 
                                  COMMONNAME == "WHALE-NORTH ATLANTIC RIGHT" ~ "NARW", 
                                  COMMONNAME == "DOLPHIN-ATLANTIC SPOTTED" ~ "ASDO", 
                                  COMMONNAME == "WHALE-LONG-FINNED PILOT" ~ "LFPW", 
                                  COMMONNAME == "WHALE-FIN/SEI" ~ "SEFI", 
                                  COMMONNAME == "CETACEAN (NS)" ~ "UNKN", 
                                  COMMONNAME == "WHALES (NS)" ~ "UNKN",
                                  COMMONNAME == "WHALE-BALEEN (NS)" ~ "UNBA", 
                                  COMMONNAME == "WHALE-BEAKED (NS)" ~ "UNBW", 
                                  COMMONNAME == "Unidentified dolphin" ~ "UNDO", 
                                  COMMONNAME == "DOLPHINS/PORPOISE (NS)" ~ "UNDP", 
                                  COMMONNAME == "Unidentified kogia" ~ "UNKO", 
                                  COMMONNAME == "WHALE-MESOPLODONT (NS)" ~ "UNMP",)) 

# 7) Map IDREL_CD to SpeciesID_Uncertainty_Code (put 0's in for null) 

IDREL_CD <- my_data$SpeciesID_Uncertainty_Code

my_data <- my_data %>%
  mutate(SpeciesID_Uncertainty_Code = case_when(IDREL_CD == 1 ~ 1, 
                                                IDREL_CD == 2 ~ 2, 
                                                IDREL_CD == 3 ~ 3,
                                                IDREL_CD == 9 ~ 0,
                                                is.na(IDREL_CD) ~ 0,
                                                TRUE ~ as.numeric(IDREL_CD)))

# 8) Map Count_Uncertainty_Code (put 0’s in for null)

my_data <- my_data %>%
  mutate(Count_Uncertainty_Code = coalesce(Count_Uncertainty_Code, 0))

# 9) Create column called Behaviour_Comments that concatenates the five BEHAVIOUR_DESC columns into the one column with hyphens in between.

# Remove 'NOT RECORDED' from concatenated BEHAVIOUR_CDs

my_data["BEHAVIOUR_DESC"][my_data["BEHAVIOUR_DESC"] == "NOT RECORDED"] <- NA
my_data["BEHAVIOUR_DESC_1"][my_data["BEHAVIOUR_DESC_1"] == "NOT RECORDED"] <- NA
my_data["BEHAVIOUR_DESC_2"][my_data["BEHAVIOUR_DESC_2"] == "NOT RECORDED"] <- NA
my_data["BEHAVIOUR_DESC_3"][my_data["BEHAVIOUR_DESC_3"] == "NOT RECORDED"] <- NA
my_data["BEHAVIOUR_DESC_4"][my_data["BEHAVIOUR_DESC_4"] == "NOT RECORDED"] <- NA

# Concatenate five BEHAVIOUR_DESC columns separated by hyphens, removing NA's

my_data <- my_data %>%
  unite('Behaviour_Comments', c('BEHAVIOUR_DESC','BEHAVIOUR_DESC_1','BEHAVIOUR_DESC_2','BEHAVIOUR_DESC_3','BEHAVIOUR_DESC_4'), sep=" - ", na.rm = TRUE)

# Reorder columns (note that COMMONNAME and BEHAVIOUR_DESC columns get removed)

my_data <- my_data[, c("Regional_Primary_Key", "Year", "Month", "Day", "UTC_Time", "Reported_Time", "Latitude", "Longitude", "Location_Uncertainty_Code", "Location_Uncertainty_Reason_Code", "Species_Code", "ITIS_Code", "SpeciesID_Uncertainty_Code", "Species_Comments",
                       "Reported_Count", "Min_Count", "Max_Count", "Count_Uncertainty_Code", "Behaviour_Comments", "GEARIMPACT_CD", "Distance", "Reported_SeaState", "Platform_Type_Code", "Activity_Type_Code", "Effort", 
                       "Data_Source_Code", "Suspected_Data_Issue_Reason", "Comments")]

# 10) Map Animal_Status_Code using GEARIMPACT_CD and Behaviour_Comments

my_data <- my_data %>%
  mutate(Animal_Status_Code = case_when(GEARIMPACT_CD == 9 ~ 0,
                                        GEARIMPACT_CD == 5 | Behaviour_Comments == "VISIBLE INJURY"| Behaviour_Comments == "STRUCK BY VESSEL" ~ 2,
                                        GEARIMPACT_CD == 1 | GEARIMPACT_CD == 2 | Behaviour_Comments == "TANGLED IN FISHING GEAR"| Behaviour_Comments == "DISENTANGLED RELEASED ALIVE" ~ 3,
                                        GEARIMPACT_CD == 4 ~ 4,
                                        is.na(GEARIMPACT_CD) ~ 0,
                                        TRUE ~ as.numeric(GEARIMPACT_CD)))

# 11) Remove record if GEARIMPACT_CD = 3

my_data <- my_data %>%
  filter(!grepl(3, GEARIMPACT_CD))

my_data <- my_data[, c("Regional_Primary_Key","Year", "Month", "Day", "UTC_Time", "Reported_Time", "Latitude", "Longitude", "Location_Uncertainty_Code", "Location_Uncertainty_Reason_Code", "Species_Code", "ITIS_Code", "SpeciesID_Uncertainty_Code", "Species_Comments",
                       "Reported_Count", "Min_Count", "Max_Count", "Count_Uncertainty_Code", "Animal_Status_Code", "Behaviour_Comments", "Distance", "Reported_SeaState", "Platform_Type_Code", "Activity_Type_Code", "Effort", 
                       "Data_Source_Code", "Suspected_Data_Issue_Reason", "Comments")]

# 12) Add one decimal place to Reported_SeaState (always zero, #.0), **REMOVE NA's

my_data <- my_data %>%
  mutate(Reported_SeaState = sprintf(Reported_SeaState, fmt = '%.1f'))

# 13) Map Reported_SeaState and add one decimal place to Reported_SeaState (always zero, #.0)

my_data<- my_data %>%
  mutate(Reported_SeaState = case_when(Reported_SeaState == '1.0' ~ '1.0',
                                       Reported_SeaState == '2.0' ~ '2.0',
                                       Reported_SeaState == '3.0' ~ '3.0',
                                       Reported_SeaState == '4.0' ~ '4.0',
                                       Reported_SeaState == '5.0' ~ '5.0',
                                       Reported_SeaState == '6.0' ~ '6.0',
                                       Reported_SeaState == '7.0' ~ '7.0',
                                       Reported_SeaState == '8.0' ~ '8.0',
                                       Reported_SeaState == '9.0' ~ '9.0',
                                       Reported_SeaState == '10.0' ~ '10.0',
                                       Reported_SeaState == '11.0' ~ '11.0',
                                       Reported_SeaState == '12.0' ~ '12.0',
                                       Reported_SeaState == '13.0' ~ NA_character_,
                                       TRUE ~ as.character(Reported_SeaState)))

# 14) Map Platform_Type_Code

PLATFORM_TYPE_CD <- my_data$Platform_Type_Code

my_data <- my_data %>%
  mutate(Platform_Type_Code = case_when(PLATFORM_TYPE_CD == 1 ~ 1, 
                                        PLATFORM_TYPE_CD == 2 ~ 2, 
                                        PLATFORM_TYPE_CD == 3 ~ 3,
                                        PLATFORM_TYPE_CD == 4 ~ 0,
                                        is.na(PLATFORM_TYPE_CD) ~ 0,
                                        TRUE ~ as.numeric(PLATFORM_TYPE_CD)))

# 15) Map TRIPTYPE_CD to Activity_Type_Code

TRIPTYPE_CD <- my_data$Activity_Type_Code

my_data <- my_data %>%
  mutate(Activity_Type_Code = case_when(TRIPTYPE_CD == 16 ~ 0,
                                        TRIPTYPE_CD == 32 ~ 1,
                                        TRIPTYPE_CD == 2 ~ 2, 
                                        TRIPTYPE_CD == 34 ~ 3,
                                        TRIPTYPE_CD == 4 ~ 4,
                                        TRIPTYPE_CD == 5 ~ 5,
                                        TRIPTYPE_CD == 33 ~ 6,
                                        TRIPTYPE_CD == 23 ~ 7,
                                        TRIPTYPE_CD == 24 ~ 8,
                                        TRIPTYPE_CD == 25 ~ 9,
                                        TRIPTYPE_CD == 26 ~ 10,
                                        TRIPTYPE_CD == 27 ~ 11,
                                        TRIPTYPE_CD == 28 ~ 12,
                                        TRIPTYPE_CD == 29 ~ 13,
                                        TRIPTYPE_CD == 30 ~ 14,
                                        TRIPTYPE_CD == 31 ~ 15,
                                        TRIPTYPE_CD == 17 ~ 17,
                                        TRIPTYPE_CD == 18 ~ 18,
                                        TRIPTYPE_CD == 35 ~ 19,
                                        TRIPTYPE_CD == 36 ~ 20,
                                        TRUE ~ as.numeric(TRIPTYPE_CD)))

# 16) Map Effort 

DATA_TYPE_CD <- my_data$Effort

my_data <- my_data %>%
  mutate(Effort = case_when(DATA_TYPE_CD == 1 ~ 2,
                            DATA_TYPE_CD == 2 ~ 2,
                            DATA_TYPE_CD == 3 ~ 2, 
                            DATA_TYPE_CD == 4 ~ 2,
                            DATA_TYPE_CD == 5 ~ 1,
                            DATA_TYPE_CD == 6 ~ 2,
                            DATA_TYPE_CD == 7 ~ 1,
                            DATA_TYPE_CD == 8 ~ 1,
                            DATA_TYPE_CD == 9 ~ 1,
                            DATA_TYPE_CD == 10 ~ 2,
                            DATA_TYPE_CD == 11 ~ 2,
                            DATA_TYPE_CD == 12 ~ 1,
                            is.na(DATA_TYPE_CD) ~ 0,
                            TRUE ~ as.numeric(DATA_TYPE_CD)))

# 17) Map Data_Source_Code (note that this is ordered by CSDB code)

DATASOURCE_CD <- my_data$Data_Source_Code

my_data <- my_data %>%
  mutate(Data_Source_Code = case_when(DATASOURCE_CD == 1 ~ 1,
                                      DATASOURCE_CD == 2 ~ 2,
                                      DATASOURCE_CD == 40 ~ 3, 
                                      DATASOURCE_CD == 4 ~ 4,
                                      DATASOURCE_CD == 38 ~ 5,
                                      DATASOURCE_CD == 43 ~ 6,
                                      DATASOURCE_CD == 44 ~ 7,
                                      DATASOURCE_CD == 45 ~ 8,
                                      DATASOURCE_CD == 25 ~ 9,
                                      DATASOURCE_CD == 26 ~ 10,
                                      DATASOURCE_CD == 28 ~ 11,
                                      DATASOURCE_CD == 29 ~ 12,
                                      DATASOURCE_CD == 32 ~ 13,
                                      DATASOURCE_CD == 34 ~ 14,
                                      DATASOURCE_CD == 35 ~ 15,
                                      DATASOURCE_CD == 36 ~ 16,
                                      DATASOURCE_CD == 17 ~ 17,
                                      DATASOURCE_CD == 39 ~ 18,
                                      DATASOURCE_CD == 19 ~ 19,
                                      DATASOURCE_CD == 41 ~ 20,
                                      DATASOURCE_CD == 42 ~ 21,
                                      DATASOURCE_CD == 22 ~ 22,
                                      DATASOURCE_CD == 23 ~ 23,
                                      DATASOURCE_CD == 24 ~ 24,
                                      DATASOURCE_CD == 46 ~ 25,
                                      DATASOURCE_CD == 47 ~ 26,
                                      DATASOURCE_CD == 48 ~ 27,
                                      DATASOURCE_CD == 49 ~ 28,
                                      DATASOURCE_CD == 50 ~ 29,
                                      DATASOURCE_CD == 51 ~ 30,
                                      DATASOURCE_CD == 52 ~ 31,
                                      DATASOURCE_CD == 53 ~ 32,
                                      DATASOURCE_CD == 54 ~ 33,
                                      DATASOURCE_CD == 55 ~ 34,
                                      DATASOURCE_CD == 56 ~ 35,
                                      DATASOURCE_CD == 57 ~ 36,
                                      DATASOURCE_CD == 58 ~ 37,
                                      DATASOURCE_CD == 59 ~ 38,
                                      DATASOURCE_CD == 60 ~ 39,
                                      DATASOURCE_CD == 61 ~ 40,
                                      DATASOURCE_CD == 62 ~ 41,
                                      DATASOURCE_CD == 63 ~ 42,
                                      DATASOURCE_CD == 64 ~ 43,
                                      DATASOURCE_CD == 65 ~ 44,
                                      DATASOURCE_CD == 66 ~ 45,
                                      DATASOURCE_CD == 67 ~ 46,
                                      DATASOURCE_CD == 68 ~ 47,
                                      DATASOURCE_CD == 69 ~ 48,
                                      DATASOURCE_CD == 70 ~ 49,
                                      DATASOURCE_CD == 71 ~ 50,
                                      DATASOURCE_CD == 72 ~ 51,
                                      DATASOURCE_CD == 73 ~ 52,
                                      DATASOURCE_CD == 74 ~ 53,
                                      DATASOURCE_CD == 75 ~ 54,
                                      DATASOURCE_CD == 76 ~ 55,
                                      DATASOURCE_CD == 77 ~ 56,
                                      DATASOURCE_CD == 78 ~ 57,
                                      DATASOURCE_CD == 79 ~ 58,
                                      DATASOURCE_CD == 80 ~ 59,
                                      DATASOURCE_CD == 81 ~ 60,
                                      DATASOURCE_CD == 82 ~ 61,
                                      DATASOURCE_CD == 83 ~ 62,
                                      DATASOURCE_CD == 84 ~ 63,
                                      DATASOURCE_CD == 85 ~ 64,
                                      DATASOURCE_CD == 86 ~ 65,
                                      DATASOURCE_CD == 87 ~ 66,
                                      DATASOURCE_CD == 88 ~ 67,
                                      DATASOURCE_CD == 89 ~ 68,
                                      DATASOURCE_CD == 90 ~ 69,
                                      DATASOURCE_CD == 91 ~ 70,
                                      DATASOURCE_CD == 92 ~ 71,
                                      DATASOURCE_CD == 93 ~ 72,
                                      DATASOURCE_CD == 94 ~ 73,
                                      DATASOURCE_CD == 95 ~ 74,
                                      DATASOURCE_CD == 96 ~ 75,
                                      DATASOURCE_CD == 97 ~ 76,
                                      DATASOURCE_CD == 98 ~ 77,
                                      DATASOURCE_CD == 99 ~ 78,
                                      DATASOURCE_CD == 100 ~ 79,
                                      DATASOURCE_CD == 101 ~ 80,
                                      DATASOURCE_CD == 102 ~ 81,
                                      DATASOURCE_CD == 103 ~ 82,
                                      DATASOURCE_CD == 104 ~ 83,
                                      DATASOURCE_CD == 105 ~ 84,
                                      DATASOURCE_CD == 106 ~ 85,
                                      DATASOURCE_CD == 107 ~ 86,
                                      DATASOURCE_CD == 108 ~ 87,
                                      DATASOURCE_CD == 109 ~ 88,
                                      DATASOURCE_CD == 110 ~ 89,
                                      DATASOURCE_CD == 111 ~ 90,
                                      DATASOURCE_CD == 112 ~ 91,
                                      DATASOURCE_CD == 113 ~ 92,
                                      DATASOURCE_CD == 114 ~ 93,
                                      DATASOURCE_CD == 115 ~ 94,
                                      DATASOURCE_CD == 116 ~ 95,
                                      DATASOURCE_CD == 117 ~ 96,
                                      DATASOURCE_CD == 118 ~ 97,
                                      DATASOURCE_CD == 119 ~ 98,
                                      DATASOURCE_CD == 120 ~ 99,
                                      DATASOURCE_CD == 121 ~ 100,
                                      DATASOURCE_CD == 122 ~ 101,
                                      DATASOURCE_CD == 123 ~ 102,
                                      DATASOURCE_CD == 124 ~ 103,
                                      DATASOURCE_CD == 125 ~ 104,
                                      is.na(DATASOURCE_CD) ~ 0,
                                      TRUE ~ as.numeric(DATASOURCE_CD)))

# 18) Add a column called Suspected_Data_Issue before Suspected_Data_Issue_Reason, and populate with ‘Yes’ when the reason column is not null and ‘No’ when it is null. 

my_data <- my_data %>%
  mutate(Suspected_Data_Issue = case_when(is.na(Suspected_Data_Issue_Reason) ~ "No",
                                          TRUE ~ "Yes"))

# Reorder columns so would it be easier to do this just once?
my_data <- my_data[, c("Regional_Primary_Key","Year", "Month", "Day", "UTC_Time", "Reported_Time", "Latitude", "Longitude", "Location_Uncertainty_Code", "Location_Uncertainty_Reason_Code", "Species_Code", "ITIS_Code", "SpeciesID_Uncertainty_Code", "Species_Comments",
                       "Reported_Count", "Min_Count", "Max_Count", "Count_Uncertainty_Code", "Animal_Status_Code", "Behaviour_Comments", "Distance", "Reported_SeaState", "Platform_Type_Code", "Activity_Type_Code", "Effort", 
                       "Data_Source_Code", "Suspected_Data_Issue", "Suspected_Data_Issue_Reason", "Comments")]

# 19) Format Latitude and Longitude to four decimal places

my_data <- my_data %>%
  mutate(Latitude = sprintf(Latitude, fmt = '%.4f'))

my_data <- my_data %>%
  mutate(Longitude = sprintf(Longitude, fmt = '%.4f'))

# 20) Remove commas from Comments and Behaviour_Comments

my_data$Comments <- gsub(",","",my_data$Comments)

my_data$Behaviour_Comments <- gsub(",","",my_data$Behaviour_Comments)

# 21) Export as .xlsx including today's date

today <- Sys.Date()

output_file = paste0("CSDB ", year, " created ", today, ".xlsx")
write_xlsx(my_data, here("Output", output_file))

view(my_data)

