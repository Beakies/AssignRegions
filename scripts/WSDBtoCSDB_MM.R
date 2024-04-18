# M. Murphy February 16th, 2024
# This code is used to format data from WSDB into the form required for CSDB

# Download and install packages if not already installed: 
pacman::p_load(writexl, readxl, tidyverse, lubridate, here)

# Load data sheet, change filename as needed---- 
filename <- "special_characters_full_CSDB_export"

input_file_path <- paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB_to_CSDB\Input\)", filename,".csv")
WSDB_data <- read_csv(input_file_path, locale = readr::locale(encoding = "Cp1252"))

# Read data tables
code_table_path <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB_to_CSDB\Code_tables\)"

Species_code_table <- read_csv(paste0(code_table_path, "Species_code_table.csv"))
Activity_type_code_table <- read_csv(paste0(code_table_path, "Activity_type_code_table.csv"))
Effort_code_table <- read_csv(paste0(code_table_path, "Effort_code_table.csv"))
Data_source_code_table <- read_csv(paste0(code_table_path, "Data_source_code_table.csv"))

# Start CSDB dataframe----
# 1) Format Year Month and Day to pull from UTC_Year, UTC_Month, UTC_Day when available, if not use Reported_Year, Reported_Month, Reported_Day

CSDB_data <- WSDB_data %>%
  mutate(Year= ifelse(is.na(UTC_Year),Reported_Year, UTC_Year),
         Month= ifelse(is.na(UTC_Month),Reported_Month, UTC_Month),
         Day= ifelse(is.na(UTC_Day),Reported_Day, UTC_Day))

# 2) Modify WS_EVENT_ID to Regional_Primary_Key so that the first 2 digits are “11” (representing MAR Region) and the total number of digits is 11 (including the WS_EVENT_ID)
# example: if WS_EVENT_ID is 1234, then the Regional_Primary_Key should be 11000001234  
CSDB_data <- CSDB_data %>%
  mutate(Regional_Primary_Key = paste(11, sprintf("%09d", WS_EVENT_ID), sep= ""))

# 3) Change the format of UTC_Time and Reported_Time to hh:mm:ss
CSDB_data <- CSDB_data %>%
  mutate(UTC_Time = ifelse(!is.na(WS_TIME_UTC), sprintf("%04d", WS_TIME_UTC), NA),
         UTC_Time = parse_date_time(UTC_Time, orders= "HM"),
         UTC_Time = format(UTC_Time, format = "%H:%M:%S"))

CSDB_data <- CSDB_data %>%
  mutate(Reported_Time = ifelse(!is.na(WS_TIME), sprintf("%04d", WS_TIME), NA),
         Reported_Time = parse_date_time(Reported_Time, orders= "HM"),
         Reported_Time = format(Reported_Time, format = "%H:%M:%S"))

# 4) Map LCQECODE_CD to Location_Uncertainty. Note: Location_Uncertainty codes that are identical to LCQECODE_CDs remain unchanged 
# example: LCQECODE_CD = 1, Location_Uncertainty = 1
CSDB_data <- CSDB_data %>%
  mutate(Location_Uncertainty = case_when(LCQECODE_CD == 4 ~ 3, 
                                          LCQECODE_CD == 5 ~ 3, 
                                          LCQECODE_CD == 6 ~ 3, 
                                          LCQECODE_CD == 7 ~ 3,
                                          is.na(LCQECODE_CD) ~ 0,
                                          TRUE ~ as.numeric(LCQECODE_CD)))

# 5) Map LCQECODE_CD to Location_Uncertainty_Reason
CSDB_data <- CSDB_data %>%
  mutate(Location_Uncertainty_Reason = case_when(LCQECODE_CD == 3 ~ 1, 
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

# 10) Map IDREL_CD to SpeciesID_Uncertainty (put 0's in for null). Note: IDREL_CDs that are identical to SpeciesID_Uncertainty_Codes remain unchanged 
# example: IDREL_CD = 1, SpeciesID_Uncertainty = 1.
CSDB_data <- CSDB_data %>%
  mutate(SpeciesID_Uncertainty = case_when(IDREL_CD == 9 ~ 0,
                                           is.na(IDREL_CD) ~ 0,
                                           TRUE ~ as.numeric(IDREL_CD)))

# 11) Map COUNT_UNCERTAINTY_CD to Count_Uncertainty (put 0’s in for null)
CSDB_data <- CSDB_data %>%
  mutate(Count_Uncertainty = coalesce(COUNT_UNCERTAINTY_CD, 0))

# 12) Create column called Behaviour_Comments that concatenates the five BEHAVIOUR_DESC columns into the one column with hyphens in between
# 12a) Change 'NOT RECORDED' to NA from BEHAVIOUR_DESC columns
CSDB_data = CSDB_data %>%
  
  ##JS: option to do this in fewer lines of code
  
  mutate(across(starts_with("BEHAVIOUR_DESC"),~ case_when(. == "NOT RECORDED" ~ NA_character_,
                                                          TRUE ~ .), .names = "new_{.col}"))

# 12b) Concatenate the five BEHAVIOUR_DESC columns separated by hyphens into a new column called Behaviour_Comments, removing NA's
CSDB_data <- CSDB_data %>%
  
  unite('Behaviour_Comments', c('new_BEHAVIOUR_DESC','new_BEHAVIOUR_DESC_1','new_BEHAVIOUR_DESC_2','new_BEHAVIOUR_DESC_3','new_BEHAVIOUR_DESC_4'), sep=" - ", na.rm = TRUE)

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

# 17) Rename the following columns 
CSDB_data <- CSDB_data %>%
  rename(Suspected_Data_Issue_Reason = SUSPECTED_DATA_ISSUE,
         Species_Comments = FEATURE_DESC,
         Reported_Count = BEST_COUNT,
         Min_Count = MIN_COUNT,
         Max_Count = MAX_COUNT,
         Distance = DISTANCE)

# 18) Add a column called Suspected_Data_Issue before Suspected_Data_Issue_Reason, and populate with ‘Yes’ when the reason column is not null and ‘No’ when it is null 
CSDB_data <- CSDB_data %>%
  mutate(Suspected_Data_Issue = case_when(is.na(Suspected_Data_Issue_Reason) ~ "No",
                                          TRUE ~ "Yes"))

# 19) Format Latitude and Longitude to four decimal places
CSDB_data <- CSDB_data %>%
  mutate(Latitude = sprintf(LATITUDE, fmt = '%.4f'))

CSDB_data <- CSDB_data %>%
  mutate(Longitude = sprintf(LONGITUDE, fmt = '%.4f')) %>% 
  
  # 20) Remove commas from Comments and Behaviour_Comments
  
  mutate(Comments = str_replace_all(COMMENTS,",",""),
         Behaviour_Comments = str_replace_all(Behaviour_Comments, ",",""))

# 21) Select only the columns needed for CSDB data output in the desired order
CSDB_data_final <- CSDB_data %>%
  dplyr::select(Regional_Primary_Key, Year, Month, Day, UTC_Time, Reported_Time, Latitude, Longitude, Location_Uncertainty, Location_Uncertainty_Reason, 
                Species_Code, ITIS_Code, SpeciesID_Uncertainty, Species_Comments, Reported_Count, Min_Count, Max_Count, Count_Uncertainty, Animal_Status_Code,
                Behaviour_Comments, Distance, Reported_SeaState, Platform_Type_Code, Activity_Type_Code, Effort, Data_Source_Code, Suspected_Data_Issue, Suspected_Data_Issue_Reason, Comments)

# 22) Export as .xlsx including the date of export
today <- Sys.Date()
output_file = paste0("CSDB_", filename, "_", today, ".xlsx")
write_xlsx(CSDB_data, paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_12\WSDB\WSDB_to_CSDB\Output\)", output_file))
