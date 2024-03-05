# M. Murphy February 16th, 2024
# This code is used to format data from WSDB into the form required for CSDB

# Download and install packages if not already installed: 
pacman::p_load(writexl, readxl, readr, tidyverse, lubridate, here)

# Load data sheet, change path as needed 
input_file <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Input\representative_dataset.csv)" 
WSDB_data <- read_csv(input_file)

# Read data tables
Species_code_table <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Species code table.csv)"
Species_code_table <- read_csv(Species_code_table)

Activity_type_code_table <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Activity type code table.csv)"
Activity_type_code_table <- read_csv(Activity_type_code_table)

Effort_code_table <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Effort code table.csv)"
Effort_code_table <- read_csv(Effort_code_table)

Data_source_code_table  <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Code tables\Data source code table.csv)"
Data_source_code_table <- read_csv(Data_source_code_table)

# Start CSDB dataframe----
# 1) Format Year Month and Day to pull from UTC_Year, UTC_Month, UTC_Day when available, if not use Reported_Year, Reported_Month, Reported_Day

CSDB_data <- WSDB_data %>%
  mutate(Year= ifelse(is.na(UTC_Year),Reported_Year, UTC_Year),
         Month= ifelse(is.na(UTC_Month),Reported_Month, UTC_Month),
         Day= ifelse(is.na(UTC_Day),Reported_Day, Reported_Day))

# 2) Modify WS_EVENT_ID to Regional_Primary_Key so that the first 2 digits are “11” (representing MAR Region) and the total number of digits is 11 (including the WS_EVENT_ID)
# example: if WS_EVENT_ID is 1234, then the Regional_Primary_Key should be 11000001234  
CSDB_data <- CSDB_data %>%
  mutate(Regional_Primary_Key = paste(11, sprintf("%09d", WS_EVENT_ID), sep= ""))

# 3) Change the format of UTC_Time and Reported_Time to hh:mm:ss
CSDB_data <- CSDB_data %>%
  mutate(UTC_Time = sprintf("%04d", WS_TIME_UTC),
         UTC_Time = parse_date_time(UTC_Time, orders= "HM", quiet = T),
         UTC_Time = format(UTC_Time, format = "%H:%M:%S"))

CSDB_data <- CSDB_data %>%
  mutate(Reported_Time = sprintf("%04d", WS_TIME),
         Reported_Time = parse_date_time(Reported_Time,orders= "HM", quiet = T),
         Reported_Time = format(Reported_Time, format = "%H:%M:%S"))

# 4) Map LCQECODE_CD to Location_Uncertainty_Code. Note: Location_Uncertainty_Codes that are identical to LCQECODE_CDs remain unchanged 
# example: LCQECODE_CD = 1, Location_Uncertainty_Code = 1
CSDB_data <- CSDB_data %>%
  mutate(Location_Uncertainty_Code = case_when(LCQECODE_CD == 4 ~ 3, 
                                               LCQECODE_CD == 5 ~ 3, 
                                               LCQECODE_CD == 6 ~ 3, 
                                               LCQECODE_CD == 7 ~ 3,
                                               is.na(LCQECODE_CD) ~ 0,
                                               TRUE ~ as.numeric(LCQECODE_CD)))

# 5) Map LCQECODE_CD to Location_Uncertainty_Reason_Code
CSDB_data <- CSDB_data %>%
  mutate(Location_Uncertainty_Reason_Code = case_when(LCQECODE_CD == 3 ~ 1, 
                                                      LCQECODE_CD == 4 ~ 2, 
                                                      LCQECODE_CD == 5 ~ 3, 
                                                      LCQECODE_CD == 6 ~ 4, 
                                                      LCQECODE_CD == 7 ~ 5,
                                                      is.na(LCQECODE_CD) ~ 0,
                                                      LCQECODE_CD == 1 ~ NA_real_,
                                                      LCQECODE_CD == 2 ~ NA_real_,
                                                      TRUE ~ as.numeric(LCQECODE_CD)))

# 6) Map COMMONNAME to ITIS_Code and Species_code using Species_code_table
CSDB_data <- CSDB_data %>%
  left_join(Species_code_table, by = "COMMONNAME")

# 7) Map TRIPTYPE_CD to Activity_Type_Code using Activity_type__code_table
CSDB_data <- CSDB_data %>% 
  left_join(Activity_type_code_table, by = "TRIPTYPE_CD")

# 8) Map DATA_TYPE_CD to Effort using Effort_code_table
CSDB_data <- CSDB_data %>%
  left_join(Effort_code_table, by = "DATA_TYPE_CD")

# 9) Map Data_Source_Code using Data_source_table (note that this is ordered by CSDB code)
CSDB_data <- CSDB_data %>% 
  left_join(Data_source_code_table, by = "DATASOURCE_CD")

# 10) Map IDREL_CD to SpeciesID_Uncertainty_Code (put 0's in for null). Note: IDREL_CDs that are identical to SpeciesID_Uncertainty_Codes remain unchanged 
# example: IDREL_CD = 1, SpeciesID_Uncertainty_Code = 1.
CSDB_data <- CSDB_data %>%
  mutate(SpeciesID_Uncertainty_Code = case_when(IDREL_CD == 9 ~ 0,
                                                is.na(IDREL_CD) ~ 0,
                                                TRUE ~ as.numeric(IDREL_CD)))

# 11) Map COUNT_UNCERTAINTY_CD to Count_Uncertainty_Code (put 0’s in for null)
CSDB_data <- CSDB_data %>%
  mutate(Count_Uncertainty_Code = coalesce(COUNT_UNCERTAINTY_CD, 0))

# 12) Create column called Behaviour_Comments that concatenates the five BEHAVIOUR_DESC columns into the one column with hyphens in between
#might be useful instead of the unite function
#%>%rowwise() %>%  # Apply operations row by row
#  mutate(
#    input_count = sum(c_across(27:32) == "Y", na.rm = TRUE))

# 12a) Change 'NOT RECORDED' to NA from BEHAVIOUR_DESC columns
CSDB_data = CSDB_data %>%
  mutate(BEHAVIOUR_DESC = case_when(BEHAVIOUR_DESC == "NOT RECORDED" ~ NA,
                                    .default = BEHAVIOUR_DESC),
         BEHAVIOUR_DESC_1 = case_when(BEHAVIOUR_DESC_1 == "NOT RECORDED" ~ NA,
                                      .default = BEHAVIOUR_DESC),
         BEHAVIOUR_DESC_2 = case_when(BEHAVIOUR_DESC_2 == "NOT RECORDED" ~ NA,
                                      .default = BEHAVIOUR_DESC),
         BEHAVIOUR_DESC_3 = case_when(BEHAVIOUR_DESC_3 == "NOT RECORDED" ~ NA,
                                      .default = BEHAVIOUR_DESC),
         BEHAVIOUR_DESC_4 = case_when(BEHAVIOUR_DESC_4 == "NOT RECORDED" ~ NA,
                                      .default = BEHAVIOUR_DESC))

# 12b) Concatenate the five BEHAVIOUR_DESC columns separated by hyphens into a new column called Behaviour_Comments, removing NA's
CSDB_data <- CSDB_data %>%
  unite('Behaviour_Comments', c('BEHAVIOUR_DESC','BEHAVIOUR_DESC_1','BEHAVIOUR_DESC_2','BEHAVIOUR_DESC_3','BEHAVIOUR_DESC_4'), sep=" - ", na.rm = TRUE)

# 13) Map Animal_Status_Code using GEARIMPACT_CD and Behaviour_Comments
CSDB_data <- CSDB_data %>%
  mutate(Animal_Status_Code = case_when(GEARIMPACT_CD == 9 ~ 0,
                                        GEARIMPACT_CD == 5 | Behaviour_Comments == "VISIBLE INJURY"| Behaviour_Comments == "STRUCK BY VESSEL" ~ 2,
                                        GEARIMPACT_CD == 1 | GEARIMPACT_CD == 2 | Behaviour_Comments == "TANGLED IN FISHING GEAR"| Behaviour_Comments == "DISENTANGLED RELEASED ALIVE" ~ 3,
                                        GEARIMPACT_CD == 4 ~ 4,
                                        is.na(GEARIMPACT_CD) ~ 0,
                                        TRUE ~ as.numeric(GEARIMPACT_CD)))

# 14) Map BEAUFORT_CD to Reported_SeaState and add one decimal place (always zero, #.0)
CSDB_data <- CSDB_data %>%
  mutate(Reported_SeaState = sprintf(BEAUFORT_CD, fmt = '%.1f'))

# 15) Map BEAUFORT_CD to Reported_SeaState and remove NA's. Note: BEAUFORT_CDs that are identical to Reported_SeaStates remain unchanged 
# example: BEAUFORT_CD = 7.0, Reported_SeaState = 7.0
CSDB_data <- CSDB_data %>%
  mutate(Reported_SeaState = case_when(Reported_SeaState == '13.0' ~ NA_character_,
                                       Reported_SeaState == 'NA' ~ NA_character_,
                                       TRUE ~ as.character(Reported_SeaState)))

# 16) Map PLATFORM_TYPE_CD to Platform_Type_Code
CSDB_data <- CSDB_data %>%
  mutate(Platform_Type_Code = case_when(PLATFORM_TYPE_CD == 4 ~ 0,
                                        is.na(PLATFORM_TYPE_CD) ~ 0,
                                        TRUE ~ as.numeric(PLATFORM_TYPE_CD)))

# 17a) Rename WSDB SUSPECTED_DATA_ISSUE to Suspected_Data_Issue_Reason 
CSDB_data$Suspected_Data_Issue_Reason = CSDB_data$SUSPECTED_DATA_ISSUE

# 17b) Add a column called Suspected_Data_Issue before Suspected_Data_Issue_Reason, and populate with ‘Yes’ when the reason column is not null and ‘No’ when it is null 
CSDB_data <- CSDB_data %>%
  mutate(Suspected_Data_Issue = case_when(is.na(Suspected_Data_Issue_Reason) ~ "No",
                                          TRUE ~ "Yes"))

# 18) Format Latitude and Longitude to four decimal places
CSDB_data <- CSDB_data %>%
  mutate(Latitude = sprintf(LATITUDE, fmt = '%.4f'))

CSDB_data <- CSDB_data %>%
  mutate(Longitude = sprintf(LONGITUDE, fmt = '%.4f'))

# 19) Remove commas from Comments and Behaviour_Comments
CSDB_data$Comments <- gsub(",","",CSDB_data$COMMENTS)
CSDB_data$Behaviour_Comments <- gsub(",","",CSDB_data$Behaviour_Comments)

# 20) Rename the following columns
CSDB_data$Species_Comments = CSDB_data$FEATURE_DESC
CSDB_data$Reported_Count = CSDB_data$BEST_COUNT
CSDB_data$Min_Count = CSDB_data$MIN_COUNT
CSDB_data$Max_Count = CSDB_data$MAX_COUNT
CSDB_data$Distance = CSDB_data$DISTANCE

# 21) Select only the columns needed for CSDB data output in the desired order
CSDB_data <- CSDB_data %>%
  dplyr::select(Regional_Primary_Key, Year, Month, Day, UTC_Time, Reported_Time, Latitude, Longitude, Location_Uncertainty_Code, Location_Uncertainty_Reason_Code, 
                Species_Code, ITIS_Code, SpeciesID_Uncertainty_Code, Species_Comments, Reported_Count, Min_Count, Max_Count, Count_Uncertainty_Code, Animal_Status_Code,
                Behaviour_Comments, Distance, Reported_SeaState, Platform_Type_Code, Activity_Type_Code, Effort, Data_Source_Code, Suspected_Data_Issue, Suspected_Data_Issue_Reason, Comments)

# 22) Export as .xlsx including the date of export
today <- Sys.Date()
output_file = paste0("CSDB ", "created ", today, ".xlsx")
write_xlsx(CSDB_data, paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB to CSDB mapping-TEMP\Output\)", output_file))
