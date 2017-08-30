library(dplyr)
library(datapkg)
library(tidyr)
library(stringr)

##################################################################
#
# Processing Script for Population Projections by Town (unofficial)
# Created by Jenna Daly
# On 08/30/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", raw_location))
data_location <- grep("data$", sub_folders, value=T)
path_to_data <- (paste0(getwd(), "/", data_location))
town_pop_proj_file <- dir(path_to_raw_data, recursive=T, pattern = "town_") 

town_pop_proj_df <- read.csv(paste0(path_to_raw_data, "/", town_pop_proj_file), stringsAsFactors = FALSE, header=T)

#Create CT level values
##Total
CT_total <- town_pop_proj_df %>% 
  group_by(Year) %>% 
  summarise(Projection = sum(Projection))
CT_total$Sex <- "All"
CT_total$Age_Group <- "All"
CT_total$Geography <- "Connecticut"
state_pop_proj <- CT_total

##Age
CT_age_total <- town_pop_proj_df %>% 
  group_by(Year, Age_Group) %>% 
  summarise(Projection = sum(Projection))
CT_age_total$Sex <- "All"
CT_age_total$Geography <- "Connecticut"
CT_age_total <- as.data.frame(CT_age_total)
state_pop_proj <- rbind(state_pop_proj, CT_age_total)

##Gender
CT_gender_total <- town_pop_proj_df %>% 
  group_by(Year, Sex) %>% 
  summarise(Projection = sum(Projection))
CT_gender_total$Age_Group <- "All"
CT_gender_total$Geography <- "Connecticut"
CT_gender_total <- as.data.frame(CT_gender_total)
state_pop_proj <- rbind(state_pop_proj, CT_gender_total)

##Groups
CT_groups_total <- town_pop_proj_df %>% 
  group_by(Year, Sex, Age_Group) %>% 
  summarise(Projection = sum(Projection))
CT_groups_total$Geography <- "Connecticut"
CT_groups_total <- as.data.frame(CT_groups_total)
state_pop_proj <- rbind(state_pop_proj, CT_groups_total)
###########################################################################

#Create town level total values
##Total
town_total <- town_pop_proj_df %>% 
  group_by(Year, Geography) %>% 
  summarise(Projection = sum(Projection))
town_total$Sex <- "All"
town_total$Age_Group <- "All"
town_total <- as.data.frame(town_total)
town_pop_proj <- rbind(town_pop_proj_df, town_total)

##Age
age_total <- town_pop_proj_df %>% 
  group_by(Year, Age_Group, Geography) %>% 
  summarise(Projection = sum(Projection))
age_total$Sex <- "All"
age_total <- as.data.frame(age_total)
town_pop_proj <- rbind(town_pop_proj, age_total)

##Gender
gender_total <- town_pop_proj_df %>% 
  group_by(Year, Sex, Geography) %>% 
  summarise(Projection = sum(Projection))
gender_total$Age_Group <- "All"
gender_total <- as.data.frame(gender_total)
town_pop_proj <- rbind(town_pop_proj, gender_total)
#####################################################################################


#Create CT gender totals dfs
CT_male_total <- CT_gender_total[CT_gender_total$Sex == "male",]
names(CT_male_total)[names(CT_male_total) == "Projection"] <- "Male Total Projection"
CT_male_total$Sex <- NULL
CT_male_total$Age_Group <- NULL

CT_female_total <- CT_gender_total[CT_gender_total$Sex == "female",]
names(CT_female_total)[names(CT_female_total) == "Projection"] <- "Female Total Projection"
CT_female_total$Sex <- NULL
CT_female_total$Age_Group <- NULL

CT_gender_totals <- merge(CT_male_total, CT_female_total, by = c("Year", "Geography"))

#Create town gender totals dfs
male_total <- gender_total[gender_total$Sex == "male",]
names(male_total)[names(male_total) == "Projection"] <- "Male Total Projection"
male_total$Sex <- NULL
male_total$Age_Group <- NULL

female_total <- gender_total[gender_total$Sex == "female",]
names(female_total)[names(female_total) == "Projection"] <- "Female Total Projection"
female_total$Sex <- NULL
female_total$Age_Group <- NULL

town_gender_totals <- merge(male_total, female_total, by = c("Year", "Geography"))

#################################################################################################################################################

#Isolate CT and Town totals in df
all_town_total <- town_pop_proj[town_pop_proj$Sex == "All" & town_pop_proj$Age_Group == "All" & town_pop_proj$Geography != "Connecticut",]
names(all_town_total)[names(all_town_total) == "Projection"] <- "Total Projection"
all_town_total$Sex <- NULL
all_town_total$Age_Group <- NULL

all_CT_total <- state_pop_proj[state_pop_proj$Sex == "All" & state_pop_proj$Age_Group == "All" & state_pop_proj$Geography == "Connecticut",]
names(all_CT_total)[names(all_CT_total) == "Projection"] <- "Total Projection"
all_CT_total$Sex <- NULL
all_CT_total$Age_Group <- NULL

#Merge together town totals
total_town <- merge(town_pop_proj, all_town_total, by = c("Year", "Geography"), all.x=T)

#Merge in state totals
total_state <- merge(state_pop_proj, all_CT_total, by = c("Year", "Geography"), all.x=T)

#Merge in town gender totals
total_town_with_gender_totals <- merge(total_town, town_gender_totals, by = c("Year", "Geography"), all.x=T)

#Merge in CT gender totals
total_state_with_gender_totals <- merge(total_state, CT_gender_totals, by = c("Year", "Geography"), all.x=T)

merge_state <- unique(total_state_with_gender_totals %>% select(Year, `Total Projection`))

colnames(merge_state)[2] <- "Total State Projection"

total_town_with_gender_totals <- merge(total_town_with_gender_totals, merge_state, by = "Year")

#Calculate Percent values

# Gender   | Age   | Denom  
# -----------------------------
# Male     | All   | Town Total
# Male     | X     | Male Total
# Female   | All   | Town Total
# Female   | X     | Female Total
# All      | All   | State Total
# All      | X     | Town Total

#Calculate percents
towns <- total_town_with_gender_totals %>% 
  group_by(Year, Geography) %>% 
  mutate(Percent = ifelse((Sex == "male" & Age_Group == "All"), (Projection / `Total Projection`)*100,
                   ifelse((Sex == "male" & Age_Group != "All"), (Projection / `Male Total Projection`)*100, 
                   ifelse((Sex == "female" & Age_Group == "All"), (Projection / `Total Projection`)*100,  
                   ifelse((Sex == "female" & Age_Group != "All"), (Projection / `Female Total Projection`)*100,  
                   ifelse((Sex == "All" & Age_Group == "All"), (Projection / `Total State Projection`)*100,  
                   ifelse((Sex == "All" & Age_Group != "All"), (Projection / `Total Projection`)*100, NA)))))))

state <- total_state_with_gender_totals %>% 
  group_by(Year) %>% 
  mutate(Percent = ifelse((Sex == "male" & Age_Group == "All"), (Projection / `Total Projection`)*100,
                   ifelse((Sex == "male" & Age_Group != "All"), (Projection / `Male Total Projection`)*100, 
                   ifelse((Sex == "female" & Age_Group == "All"), (Projection / `Total Projection`)*100,  
                   ifelse((Sex == "female" & Age_Group != "All"), (Projection / `Female Total Projection`)*100,  
                   ifelse((Sex == "All" & Age_Group == "All"), (Projection / `Total Projection`)*100,  
                   ifelse((Sex == "All" & Age_Group != "All"), (Projection / `Total Projection`)*100, NA)))))))

#Clean up dfs and combine
towns <- towns %>% 
  select(Geography, Year, Sex, Age_Group, Projection, Percent)

state <- state %>% 
  select(Geography, Year, Sex, Age_Group, Projection, Percent)

total_projections <- rbind(towns, state)

#Clean up age_group names
old_ages <- c("0_4", "5_9", "10_14", "15_19", 
              "20_24", "25_29", "30_34", "35_39", 
              "40_44", "45_49", "50_54", "55_59", 
              "60_64", "65_69", "70_74", "75_79", 
              "80_84", "85+", "All")

new_ages <- c("0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", 
              "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", 
              "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", 
              "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", 
              "80 to 84 years", "85 years and Over", "Total")

total_projections$Age_Group <- new_ages[match(total_projections$Age_Group, old_ages)]

#Clean up gender column
total_projections$Sex <- str_to_title(total_projections$Sex)

#reshape from wide to long format
cols_to_stack <- c("Projection", "Percent")

long_row_count = nrow(total_projections) * length(cols_to_stack)

total_projections_long <- reshape(total_projections,
                                    varying = cols_to_stack,
                                    v.names = "Value",
                                    timevar = "Measure Type",
                                    times = cols_to_stack,
                                    new.row.names = 1:long_row_count,
                                    direction = "long"
)

#remove ID column
total_projections_long$id <- NULL

options(scipen=999)

total_projections_long$Variable <- "Projected Population"
total_projections_long$`Measure Type`[which(grepl("Projection", total_projections_long$`Measure Type`))] <- "Number"

names(total_projections_long)[names(total_projections_long) == "Geography"] <- "Town"

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

total_projections_long_fips <- merge(total_projections_long, fips, by = "Town", all=T)

#set Age_Group as factor for sorting
total_projections_long_fips <- transform(total_projections_long_fips, Age_Group = factor(Age_Group, 
                                          levels = c("Total", "0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", 
                                                     "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", 
                                                     "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", 
                                                     "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", 
                                                     "80 to 84 years", "85 years and Over"), ordered=TRUE))  

total_projections_long_fips <- total_projections_long_fips %>% 
  select(Town, FIPS, Year, Sex, Age_Group, Variable, Measure.Type, Value) %>% 
  arrange(Town, Year, Sex, Age_Group, Measure.Type) %>% 
  rename(`Age Cohort` = Age_Group, Gender = Sex, `Measure Type` = Measure.Type)


total_projections_long_fips$Value <- round(total_projections_long_fips$Value, 2)

write.table(
  total_projections_long_fips,
  file.path(getwd(), "data", "population_projections_by_town_2017_unofficial.csv"),
  sep = ",",
  row.names = F
)
