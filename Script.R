'This is the main script for cleaning the data used for the external validation of the CARSS model using TRFT data - some of this code might not be relevant to your analysis, depending on your trust and data'

install.packages("tidyverse")
library(dplyr)
library(tidyr)

cars_data <- read.csv("R:/CARSCalculationFullViv2.csv")
cars_full <- read.csv("R:/ObsTesting(Changed).csv")

#Converting 'NULL' strings into NA values
cars_data$NEWSScore[cars_data$NEWSScore == "NULL"] <- NA
cars_data$RespiratoryRate[cars_data$RespiratoryRate == "NULL"] <- NA
cars_data$Temperature[cars_data$Temperature == "NULL"] <- NA
cars_data$Temperature[cars_data$Temperature == "not recorded"] <- NA
cars_data$SystolicBloodPressure[cars_data$SystolicBloodPressure == "NULL"] <- NA
cars_data$DiastolicBloodPressure[cars_data$DiastolicBloodPressure == "NULL"] <- NA
cars_data$Pulse[cars_data$Pulse == "NULL"] <- NA
cars_data$OxygenSupplementation[cars_data$OxygenSupplementation == "NULL"] <- NA
cars_data$OxygenSats[cars_data$OxygenSats == "NULL"] <- NA
cars_data$OxygenSatsScale[cars_data$OxygenSatsScale == "NULL"] <- NA
cars_data$Alertness[cars_data$Alertness == "NULL"] <- NA
cars_data$FlowRate[cars_data$OxygenSupplementation != "On Air" & cars_data$FlowRate == "NULL"] <- NA
cars_data$FlowRate[cars_data$OxygenSupplementation == "On Air" & cars_data$FlowRate == "NULL"] <- 0

#Converting Data as Chr into DateTime
cars_full$ObservationTakenDateTime <- as.POSIXct(cars_full$ObservationTakenDateTime, format = "%d/%m/%y %H:%M")
cars_data$ObservationTakenDateTime <- as.POSIXct(cars_data$ObservationTakenDateTime, format = "%d/%m/%y %H:%M")

#Sorting the full dataset
sorted_cars <- cars_full %>%
  arrange(AccountNumber,ObservationTakenDateTime)

#Converting 'NULL' strings into NA values
sorted_cars$NEWSScore[sorted_cars$NEWSScore == "NULL"] <- NA
sorted_cars$RespiratoryRate[sorted_cars$RespiratoryRate == "NULL"] <- NA
sorted_cars$Temperature[sorted_cars$Temperature == "NULL"] <- NA
sorted_cars$Temperature[sorted_cars$Temperature == "not recorded"] <- NA
sorted_cars$SystolicBloodPressure[sorted_cars$SystolicBloodPressure == "NULL"] <- NA
sorted_cars$DiastolicBloodPressure[sorted_cars$DiastolicBloodPressure == "NULL"] <- NA
sorted_cars$Pulse[sorted_cars$Pulse == "NULL"] <- NA
sorted_cars$OxygenSupplementation[sorted_cars$OxygenSupplementation == "NULL"] <- NA
sorted_cars$OxygenSats[sorted_cars$OxygenSats == "NULL"] <- NA
sorted_cars$OxygenSatsScale[sorted_cars$OxygenSatsScale == "NULL"] <- NA
sorted_cars$Alertness[sorted_cars$Alertness == "NULL"] <- NA
sorted_cars$FlowRate[sorted_cars$OxygenSupplementation != "On Air" & sorted_cars$FlowRate == "NULL"] <- NA
sorted_cars$FlowRate[sorted_cars$OxygenSupplementation == "On Air" & sorted_cars$FlowRate == "NULL"] <- 0

#Grouping observations based on account number, and filling NA values with values below. 
filled_cars <- sorted_cars %>%
  group_by(AccountNumber) %>%
  fill(NEWSScore, RespiratoryRate, Temperature, SystolicBloodPressure, DiastolicBloodPressure, Pulse, OxygenSupplementation, OxygenSats, OxygenSatsScale, Alertness, FlowRate, .direction = "up")
#Filtering results for unique account numbers. 
filtered_cars <- filled_cars %>%
  distinct(AccountNumber, .keep_all = TRUE)
write.csv(filtered_cars, "R:/FilteredCars.csv")

#Joining filled and filtered observation data with blood test data based on patient account number
cars_merged <- left_join(cars_data, filtered_cars, by = "AccountNumber")
cars_final <- cars_merged %>%
  select(SpellLocalID, PatientLocalID, DischargeStatus, DischargeMethod, Gender, Age, AdmissionDateTime, DischargeDateTime, SpecialtyCodes, AccountNumber, NEWSScore.y, RespiratoryRate.y, Temperature.y, SystolicBloodPressure.y, DiastolicBloodPressure.y, Pulse.y, OxygenSupplementation.y, OxygenSats.y, OxygenSatsScale.y, Alertness.y, ObservationTakenDateTime.y, FlowRate.y, AccountNumberICE, PatientIDICE, Request_RequestDateTime, AlbuminComplete, Bloods_LatestDateTime, Haemoglobin, Creatinine, Potassium, Sodium, Urea, White.Cell.Count, AKI.Warning.Stage)

cars_final$FlowRate[cars_final$OxygenSupplementation != "On Air" & cars_final$FlowRate == "NULL"] <- NA
cars_final$FlowRate[cars_final$OxygenSupplementation == "On Air" & cars_final$FlowRate == "NULL"] <- 0
cars_final$FlowRate[cars_final$FlowRate == "NULL"] <- NA

write.csv(cars_final, "R:/FinalCARS1.csv")

#########################################################################################################
#Converting chr into num
cars_final$NEWSScore.y <- as.numeric(cars_final$NEWSScore.y)
cars_final$RespiratoryRate.y <- as.numeric(cars_final$RespiratoryRate.y)
cars_final$Temperature.y <- as.numeric(cars_final$Temperature.y)
cars_final$SystolicBloodPressure.y <- as.numeric(cars_final$SystolicBloodPressure.y)
cars_final$DiastolicBloodPressure.y <- as.numeric(cars_final$DiastolicBloodPressure.y)
cars_final$Pulse.y <- as.numeric(cars_final$Pulse.y)
cars_final$OxygenSats.y <- as.numeric(cars_final$OxygenSats.y)
cars_final$FlowRate.y <- as.numeric(cars_final$FlowRate.y)

cars_clean <- cars_final

#Cleaning Observations based on standard ranges
cars_clean$DischargeMethod[cars_clean$DischargeMethod == ""] <- NA
cars_clean$Gender[cars_clean$Gender == "Not Known"] <- NA
cars_clean$Age[cars_clean$Age == "100 or Over"] <- 100
cars_clean$SpecialtyCodes[cars_clean$SpecialtyCodes == "420"] <- NA
cars_clean$RespiratoryRate.y[cars_clean$RespiratoryRate.y < 5 | cars_clean$RespiratoryRate.y > 99] <- NA
cars_clean$Temperature.y[cars_clean$Temperature.y < 31 | cars_clean$Temperature.y > 41] <- NA
cars_clean$SystolicBloodPressure.y[cars_clean$SystolicBloodPressure.y < 40 | cars_clean$SystolicBloodPressure.y > 240] <- NA
cars_clean$DiastolicBloodPressure.y[cars_clean$DiastolicBloodPressure.y < 30 | cars_clean$DiastolicBloodPressure.y > 160] <- NA
cars_clean$Pulse.y[cars_clean$Pulse.y < 20 | cars_clean$Pulse.y > 220] <- NA
cars_clean$OxygenSats.y[cars_clean$OxygenSats.y < 0 | cars_clean$OxygenSats.y > 100] <- NA
cars_clean$OxygenSupplementation.y[cars_clean$OxygenSupplementation.y == "On Air"] <- 0
cars_clean$OxygenSupplementation.y[cars_clean$OxygenSupplementation.y != 0] <- 1
cars_clean$Gender[cars_clean$Gender == "Male"] <- 1
cars_clean$Gender[cars_clean$Gender != 1] <- 0

#Cleaning Blood Tests based on standard ranges
cars_clean$AlbuminComplete <- as.numeric(cars_clean$AlbuminComplete)
cars_clean$Creatinine <- as.numeric(cars_clean$Creatinine)
cars_clean$Haemoglobin <- as.numeric(cars_clean$Haemoglobin)
cars_clean$Potassium <- as.numeric(cars_clean$Potassium)
cars_clean$Sodium <- as.numeric(cars_clean$Sodium)
cars_clean$Urea <- as.numeric(cars_clean$Urea)
cars_clean$White.Cell.Count <- as.numeric(cars_clean$White.Cell.Count)
cars_clean$AlbuminComplete[cars_clean$AlbuminComplete < 0 | cars_clean$AlbuminComplete > 55] <- NA
cars_clean$Creatinine[cars_clean$Creatinine == "<10"] <- 10
cars_clean$Creatinine[cars_clean$Creatinine < 0 | cars_clean$Creatinine > 1800] <- NA
cars_clean$Haemoglobin[cars_clean$Haemoglobin < 0 | cars_clean$Haemoglobin > 220] <- NA
cars_clean$Potassium[cars_clean$Potassium < 0 | cars_clean$Potassium > 9] <- NA
cars_clean$Sodium[cars_clean$Sodium < 100 | cars_clean$Sodium > 190] <- NA
cars_clean$Urea[cars_clean$Urea == "<1.8"] <- 1.8
cars_clean$Urea[cars_clean$Urea < 0 | cars_clean$Urea > 90] <- NA
cars_clean$White.Cell.Count[cars_clean$White.Cell.Count < 0 | cars_clean$White.Cell.Count > 400] <- NA
cars_clean$AKI.Warning.Stage[cars_clean$AKI.Warning.Stage == "NULL"] <- 0
cars_clean$Bloods_LatestDateTime[cars_clean$Bloods_LatestDateTime == "NULL"] <- NA

#Making NEWS2 variables from NA to NULL - so it does not affect Flag variable
cars_clean <- cars_clean %>% 
  mutate(FlowRate.y = ifelse(is.na(FlowRate.y), "NULL", FlowRate.y))
cars_clean <- cars_clean %>% 
  mutate(FlowRate = ifelse(is.na(FlowRate), "NULL", FlowRate))
cars_clean <- cars_clean %>% 
  mutate(OxygenSatsScale.y = ifelse(is.na(OxygenSatsScale.y), "NULL", OxygenSatsScale.y))
#Flag Variable 1 = NA
cars_clean <- cars_clean %>%
  mutate(Flag = ifelse(rowSums(is.na(.)) > 0, 1, 0))
sum(cars_clean$Flag, na.rm = TRUE)

write.csv(cars_clean, "R:/CleanedCARS1.csv")


#Converting str dates to POSIXct
cars_clean$AdmissionDateTime <- as.POSIXct(cars_clean$AdmissionDateTime, format = "%d/%m/%Y %H:%M")
cars_clean$DischargeDateTime <- as.POSIXct(cars_clean$DischargeDateTime, format = "%d/%m/%Y %H:%M")
cars_clean$Bloods_LatestDateTime <- as.POSIXct(cars_clean$Bloods_LatestDateTime, format = "%d/%m/%Y %H:%M")
cars_clean$time_since_obs <- as.numeric(difftime(cars_clean$ObservationTakenDateTime.y, cars_clean$AdmissionDateTime, units = "hours"))
cars_clean$time_since_bts <- as.numeric(difftime(cars_clean$Bloods_LatestDateTime, cars_clean$AdmissionDateTime, units = "hours"))

#Filtering out blood tests with NA values
bts_columns <- c("AlbuminComplete","Bloods_LatestDateTime","Haemoglobin", "Creatinine", "Potassium", "Sodium", "Urea", "White.Cell.Count", "AKI.Warning.Stage")
obs_columns <- c("DischargeMethod","Gender","Age", "SpecialtyCodes", "NEWSScore.y", "RespiratoryRate.y", "Temperature.y", "SystolicBloodPressure.y", "DiastolicBloodPressure.y", "Pulse.y", "OxygenSupplementation.y", "OxygenSats.y", "Alertness.y", "ObservationTakenDateTime.y")
cars_clean$NA_flag <- ifelse(rowSums(is.na(cars_clean[bts_columns])) > 0, TRUE, FALSE)
cars_clean$NAObs_flag <- ifelse(rowSums(is.na(cars_clean[obs_columns])) > 0, TRUE, FALSE)
sum(cars_clean$NA_flag == TRUE) 
sum(cars_clean$time_since_bts > 96 & cars_clean$NA_flag == FALSE, na.rm = TRUE) 
sum(cars_clean$NAObs_flag == TRUE & cars_clean$NA_flag == FALSE & cars_clean$time_since_bts <= 96) 
sum(cars_clean$NAObs_flag == FALSE & cars_clean$NA_flag == FALSE & cars_clean$time_since_bts <= 96 & cars_clean$time_since_obs > 24, na.rm = TRUE) 
sum(cars_clean$time_since_bts > 96 | cars_clean$NA_flag == TRUE | cars_clean$NAObs_flag == TRUE | cars_clean$time_since_obs > 24, na.rm = TRUE)
#Filtering out delayed blood tests and observations
sum(cars_clean$time_since_obs > 24 | cars_clean$time_since_bts >96 | cars_clean$Flag==1, na.rm=TRUE)
sum(cars_clean$Flag == 1 & cars_clean$NA_flag == TRUE & cars_clean$NAObs_flag == TRUE)

#Calculating NEWS (NEWS1) values
cars_clean$resp_news <- ifelse((cars_clean$RespiratoryRate.y<=8 | cars_clean$RespiratoryRate.y>=25) ,3,
                               ifelse((cars_clean$RespiratoryRate.y>=21 & cars_clean$RespiratoryRate.y<=24),2,
                                      ifelse((cars_clean$RespiratoryRate.y>=9 & cars_clean$RespiratoryRate.y<=11),1,0)))

cars_clean$sat_news <- ifelse((cars_clean$OxygenSats.y<=91) ,3,
                              ifelse((cars_clean$OxygenSats.y==92 | cars_clean$OxygenSats.y==93),2,
                                     ifelse((cars_clean$OxygenSats.y==94 | cars_clean$OxygenSats.y==95),1,0)))

cars_clean$sup_news <- ifelse(cars_clean$OxygenSupplementation.y==1,2,0)

cars_clean$temp_news <- ifelse((cars_clean$Temperature.y<=35) ,3,
                               ifelse((cars_clean$Temperature.y>=39.1),2,
                                      ifelse(((cars_clean$Temperature.y>=35.1 & cars_clean$Temperature.y<=36) | (cars_clean$Temperature.y>=38.1 & cars_clean$Temperature.y<=39.1)),1,0)))

cars_clean$syst_news <- ifelse((cars_clean$SystolicBloodPressure.y<=90 | cars_clean$SystolicBloodPressure.y>=220) ,3,
                               ifelse((cars_clean$SystolicBloodPressure.y>=91 & cars_clean$SystolicBloodPressure.y<=100),2,
                                      ifelse((cars_clean$SystolicBloodPressure.y>=101 & cars_clean$SystolicBloodPressure.y<=110),1,0)))

cars_clean$pulse_news <- ifelse((cars_clean$Pulse.y<=40 | cars_clean$Pulse.y>=131) ,3,
                                ifelse((cars_clean$Pulse.y>=111 & cars_clean$Pulse.y<=130),2,
                                       ifelse(((cars_clean$Pulse.y>=91 & cars_clean$Pulse.y<=110) | (cars_clean$Pulse.y>=41 & cars_clean$Pulse.y<=50)) ,1,0)))

cars_clean$alert_news <- ifelse(cars_clean$Alertness.y != "Alert",3,0)
cars_clean$calc_news <- cars_clean$resp_news + cars_clean$sat_news + cars_clean$sup_news + cars_clean$temp_news + cars_clean$syst_news + cars_clean$pulse_news + cars_clean$alert_news
cars_clean$calc_news[is.na(cars_clean$calc_news)]=0

#Sum of late observations and blood tests
cars_clean <- cars_clean %>% 
  mutate(Flag = ifelse(Flag != 1 & (time_since_obs > 24 | time_since_bts > 96), 1, Flag))
sum(cars_clean$Flag == 1, na.rm = TRUE)
cars_complete <- subset(cars_clean, Flag ==0)
cars_incomplete <- subset(cars_clean, Flag ==1)

#Preparing data for equation transformations (Equation.R)
cars_complete <- cars_complete %>%
  rename(age = Age, NEWS = calc_news, male = Gender, resp = RespiratoryRate.y, temp = Temperature.y, sys = SystolicBloodPressure.y, dias = DiastolicBloodPressure.y, pulse = Pulse.y, sup = OxygenSupplementation.y, sat = OxygenSats.y, ALB = AlbuminComplete, HB = Haemoglobin, CRE = Creatinine, POT = Potassium, SOD = Sodium, URE = Urea, WBC = White.Cell.Count)
