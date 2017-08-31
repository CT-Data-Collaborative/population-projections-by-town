library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for Population Projections by Town
# Created by Jenna Daly
# On 07/19/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", raw_location))
data_location <- grep("data$", sub_folders, value=T)
path_to_data <- (paste0(getwd(), "/", data_location))
town_pop_proj_file <- dir(path_to_raw_data, recursive=T, pattern = "townct") 
state_pop_proj_file <- dir(path_to_raw_data, recursive=T, pattern = "statect") 

town_pop_proj_df <- read.csv(paste0(path_to_raw_data, "/", town_pop_proj_file), stringsAsFactors = FALSE, header=T)
state_pop_proj_df <- read.csv(paste0(path_to_raw_data, "/", state_pop_proj_file), stringsAsFactors = FALSE, header=T)

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
all_pop_prof_dfs <- grep("proj_df", dfs, value=T)

#Combine towns and CT total
total_pop_proj <- data.frame(stringsAsFactors = F)
for (i in 1:length(all_pop_prof_dfs)) {
  pop_proj_df <- get(all_pop_prof_dfs[i])
  #Fix column names
  colnames(pop_proj_df) <- gsub("X", "20", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("M", " Male", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("F", " Female", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("TOTAL", " Total", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("0004", " 0 to 4 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("0509", " 5 to 9 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("1014", " 10 to 14 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("1519", " 15 to 19 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("2024", " 20 to 24 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("2529", " 25 to 29 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("3034", " 30 to 34 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("3539", " 35 to 39 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("4044", " 40 to 44 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("4549", " 45 to 49 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("5054", " 50 to 54 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("5559", " 55 to 59 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("6064", " 60 to 64 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("6569", " 65 to 69 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("7074", " 70 to 74 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("7579", " 75 to 79 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("8084", " 80 to 84 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("8589", " 85 to 89 years", colnames(pop_proj_df))
  colnames(pop_proj_df) <- gsub("90OV", " 90 years and Over", colnames(pop_proj_df))
  pop_proj_df <- pop_proj_df[!is.na(pop_proj_df$`2015 Total`),]
  colnames(pop_proj_df)[1] <- "Town"
  total_pop_proj <- rbind(total_pop_proj, pop_proj_df)
}

#colnames(total_pop_proj)

#Calculate Total Percent columns
total_15_col <- grep("2015 Total", colnames(total_pop_proj), value=T)
total_20_col <- grep("2020 Total", colnames(total_pop_proj), value=T)
total_25_col <- grep("2025 Total", colnames(total_pop_proj), value=T)

gender_2015_cols <- c("2015 Male", "2015 Female")
gender_15_cols <- grep(paste(gender_2015_cols, collapse="|"), colnames(total_pop_proj), value=TRUE)

gender_2020_cols <- c("2020 Male", "2020 Female")
gender_20_cols <- grep(paste(gender_2020_cols, collapse="|"), colnames(total_pop_proj), value=TRUE)

gender_2025_cols <- c("2025 Male", "2025 Female")
gender_25_cols <- grep(paste(gender_2025_cols, collapse="|"), colnames(total_pop_proj), value=TRUE)


for (i in 1:length(gender_15_cols)) {
  col_label <- paste0("Percent ", gender_15_cols[i])
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[gender_15_cols[i]]] / total_pop_proj[[total_15_col]])*100, 2)
}

for (i in 1:length(gender_20_cols)) {
  col_label <- paste0("Percent ", gender_20_cols[i])
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[gender_20_cols[i]]] / total_pop_proj[[total_20_col]])*100, 2)
}

for (i in 1:length(gender_25_cols)) {
  col_label <- paste0("Percent ", gender_25_cols[i])
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[gender_25_cols[i]]] / total_pop_proj[[total_25_col]])*100, 2)
}

#colnames(total_pop_proj)

#Calculate Age Percent columns
dont_include1 <- c("Town", "2020", "2025", "Percent", "Total")
remove1 <- colnames(total_pop_proj)[grep(paste(dont_include1, collapse="|"), colnames(total_pop_proj))]
age_15_cols <- colnames(total_pop_proj)[!colnames(total_pop_proj) %in% remove1]

dont_include2 <- c("Town", "2015", "2025", "Percent", "Total")
remove2 <- colnames(total_pop_proj)[grep(paste(dont_include2, collapse="|"), colnames(total_pop_proj))]
age_20_cols <- colnames(total_pop_proj)[!colnames(total_pop_proj) %in% remove2]

dont_include3 <- c("Town", "2015", "2020", "Percent", "Total")
remove3 <- colnames(total_pop_proj)[grep(paste(dont_include3, collapse="|"), colnames(total_pop_proj))]
age_25_cols <- colnames(total_pop_proj)[!colnames(total_pop_proj) %in% remove3]


for (i in 1:length(age_15_cols)) {
  col_label <- paste0("Percent ", age_15_cols[i])
  gender <- unlist(strsplit(col_label, " "))[3]
  year <- unlist(strsplit(col_label, " "))[2]
  matches <- c(gender, year, "Total")
  denom_col_pos <- as.integer(which(Reduce(`&`, lapply(matches, grepl, colnames(total_pop_proj)))))[1]
  denom_col <- names(total_pop_proj[denom_col_pos])
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[age_15_cols[i]]] / total_pop_proj[[denom_col]])*100, 2)
}

for (i in 1:length(age_20_cols)) {
  col_label <- paste0("Percent ", age_20_cols[i])
  gender <- unlist(strsplit(col_label, " "))[3]
  year <- unlist(strsplit(col_label, " "))[2]
  matches <- c(gender, year, "Total")
  denom_col_pos <- as.integer(which(Reduce(`&`, lapply(matches, grepl, colnames(total_pop_proj)))))[1]
  denom_col <- names(total_pop_proj[denom_col_pos])
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[age_20_cols[i]]] / total_pop_proj[[denom_col]])*100, 2)
}

for (i in 1:length(age_25_cols)) {
  col_label <- paste0("Percent ", age_25_cols[i])
  gender <- unlist(strsplit(col_label, " "))[3]
  year <- unlist(strsplit(col_label, " "))[2]
  matches <- c(gender, year, "Total")
  denom_col_pos <- as.integer(which(Reduce(`&`, lapply(matches, grepl, colnames(total_pop_proj)))))[1]
  denom_col <- names(total_pop_proj[denom_col_pos])
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[age_25_cols[i]]] / total_pop_proj[[denom_col]])*100, 2)
}

#colnames(total_pop_proj)

#Calculate each town as % of state total
vars <- ls()[sapply(mget(ls(), .GlobalEnv), is.character)]
total_cols <- grep("^total", vars, value=T)

for (i in 1:length(total_cols)) {
  col_label <- paste0("Percent ", get(total_cols[i]))
  total_val <- as.numeric(total_pop_proj[, get(total_cols[i])][which(total_pop_proj$Town == "Connecticut")])
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[get(total_cols[i])]] / total_val)*100, 2)
}

#colnames(total_pop_proj)

#Calculate 'Gender=All' for all Age Cohorts (M+F for all age cohorts needs to be calculated)

#First calculate M+F columns
male_15_cols <- grep("Male", age_15_cols, value=T)
female_15_cols <- grep("Female", age_15_cols, value=T)

for (i in 1:length(male_15_cols)) {
  age_cohort <- paste(unlist(strsplit(male_15_cols[i], " "))[-c(1:2)], sep="", collapse=" ") 
  year <- unlist(strsplit(male_15_cols[i], " "))[1]
  col_label <- paste0(year, " Total ", age_cohort)
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- total_pop_proj[[male_15_cols[i]]] + total_pop_proj[[female_15_cols[i]]]
}

male_20_cols <- grep("Male", age_20_cols, value=T)
female_20_cols <- grep("Female", age_20_cols, value=T)

for (i in 1:length(male_20_cols)) {
  age_cohort <- paste(unlist(strsplit(male_20_cols[i], " "))[-c(1:2)], sep="", collapse=" ") 
  year <- unlist(strsplit(male_20_cols[i], " "))[1]
  col_label <- paste0(year, " Total ", age_cohort)
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- total_pop_proj[[male_20_cols[i]]] + total_pop_proj[[female_20_cols[i]]]
}


male_25_cols <- grep("Male", age_25_cols, value=T)
female_25_cols <- grep("Female", age_25_cols, value=T)

for (i in 1:length(male_25_cols)) {
  age_cohort <- paste(unlist(strsplit(male_25_cols[i], " "))[-c(1:2)], sep="", collapse=" ") 
  year <- unlist(strsplit(male_25_cols[i], " "))[1]
  col_label <- paste0(year, " Total ", age_cohort)
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- total_pop_proj[[male_25_cols[i]]] + total_pop_proj[[female_25_cols[i]]]
}

#Now, calculate Percent columns for gender=all, for all age cohorts
match_15 <- c("2015", "Total", "years")
total_15_age_cols <- (which(Reduce(`&`, lapply(match_15, grepl, colnames(total_pop_proj)))))

#test <- total_pop_proj[,total_15_age_cols]

for (i in 1:length(total_15_age_cols)) {
  col_label <- paste0("Percent ", names(total_pop_proj[total_15_age_cols[i]]))
  which_total_col <- paste0(match_15[1], " Total")
  #total_val <- as.numeric(total_pop_proj[, which_total_col])[which(total_pop_proj$Town == "Connecticut")]
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[total_15_age_cols[i]]] / total_pop_proj[, which_total_col])*100, 2)
}
match_20 <- c("2020", "Total", "years")
total_20_age_cols <- (which(Reduce(`&`, lapply(match_20, grepl, colnames(total_pop_proj)))))

for (i in 1:length(total_20_age_cols)) {
  col_label <- paste0("Percent ", names(total_pop_proj[total_20_age_cols[i]]))
  which_total_col <- paste0(match_20[1], " Total")
  #total_val <- as.numeric(total_pop_proj[, which_total_col])[which(total_pop_proj$Town == "Connecticut")]
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[total_20_age_cols[i]]] / total_pop_proj[, which_total_col])*100, 2)
}

match_25 <- c("2025", "Total", "years")
total_25_age_cols <- (which(Reduce(`&`, lapply(match_25, grepl, colnames(total_pop_proj)))))

for (i in 1:length(total_25_age_cols)) {
  col_label <- paste0("Percent ", names(total_pop_proj[total_25_age_cols[i]]))
  which_total_col <- paste0(match_25[1], " Total")
  #total_val <- as.numeric(total_pop_proj[, which_total_col])[which(total_pop_proj$Town == "Connecticut")]
  total_pop_proj[, col_label] <- NA
  total_pop_proj[, col_label] <- round((total_pop_proj[[total_25_age_cols[i]]] / total_pop_proj[, which_total_col])*100, 2)
}

colnames(total_pop_proj)

#Convert wide to long
total_pop_proj_long <- gather(total_pop_proj, Variable, Value, `2015 Total`:`Percent 2025 Total 90 years and Over`, factor_key=TRUE)

#Create new columns based on contents of Variable column
total_pop_proj_long$Year <- NA
total_pop_proj_long$Year[which(grepl("2015", total_pop_proj_long$Variable))] <- 2015
total_pop_proj_long$Year[which(grepl("2020", total_pop_proj_long$Variable))] <- 2020
total_pop_proj_long$Year[which(grepl("2025", total_pop_proj_long$Variable))] <- 2025

total_pop_proj_long$Gender <- "All"
total_pop_proj_long$Gender[which(grepl("Male", total_pop_proj_long$Variable))] <- "Male"
total_pop_proj_long$Gender[which(grepl("Female", total_pop_proj_long$Variable))] <- "Female"

total_pop_proj_long$`Age Cohort` <- "Total"
total_pop_proj_long$`Age Cohort`[which(grepl("0 to 4 years", total_pop_proj_long$Variable))] <- "0 to 4 years"
total_pop_proj_long$`Age Cohort`[which(grepl("5 to 9 years", total_pop_proj_long$Variable))] <- "5 to 9 years"      
total_pop_proj_long$`Age Cohort`[which(grepl("10 to 14 years", total_pop_proj_long$Variable))] <- "10 to 14 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("15 to 19 years", total_pop_proj_long$Variable))] <- "15 to 19 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("20 to 24 years", total_pop_proj_long$Variable))] <- "20 to 24 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("25 to 29 years", total_pop_proj_long$Variable))] <- "25 to 29 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("30 to 34 years", total_pop_proj_long$Variable))] <- "30 to 34 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("35 to 39 years", total_pop_proj_long$Variable))] <- "35 to 39 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("40 to 44 years", total_pop_proj_long$Variable))] <- "40 to 44 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("45 to 49 years", total_pop_proj_long$Variable))] <- "45 to 49 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("50 to 54 years", total_pop_proj_long$Variable))] <- "50 to 54 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("55 to 59 years", total_pop_proj_long$Variable))] <- "55 to 59 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("60 to 64 years", total_pop_proj_long$Variable))] <- "60 to 64 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("65 to 69 years", total_pop_proj_long$Variable))] <- "65 to 69 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("70 to 74 years", total_pop_proj_long$Variable))] <- "70 to 74 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("75 to 79 years", total_pop_proj_long$Variable))] <- "75 to 79 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("80 to 84 years", total_pop_proj_long$Variable))] <- "80 to 84 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("85 to 89 years", total_pop_proj_long$Variable))] <- "85 to 89 years" 
total_pop_proj_long$`Age Cohort`[which(grepl("90 years and Over", total_pop_proj_long$Variable))] <- "90 years and Over" 

total_pop_proj_long$`Measure Type` <- "Number"
total_pop_proj_long$`Measure Type`[which(grepl("Percent", total_pop_proj_long$Variable))] <- "Percent"

total_pop_proj_long$Variable <- "Projected Population"

#Remove " town" and anythng after it from `Town` column
total_pop_proj_long$Town <- sub(" town.*", "", total_pop_proj_long$Town)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

total_pop_proj_long_fips <- merge(total_pop_proj_long, fips, by = "Town", all=T)

#set `Age Cohort` as factor for sorting
total_pop_proj_long_fips <- transform(total_pop_proj_long_fips, `Age Cohort` = factor(`Age Cohort`, 
                                      levels = c("Total", "0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", 
                                                 "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", 
                                                 "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", 
                                                 "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", 
                                                 "80 to 84 years", "85 to 89 years", "90 years and Over"), ordered=TRUE))          
total_pop_proj_long_fips <- total_pop_proj_long_fips %>% 
  select(Town, FIPS, Year, Gender, Age.Cohort, Variable, Measure.Type, Value) %>% 
  arrange(Town, Year, Gender, Age.Cohort, Measure.Type) %>% 
  rename(`Age Cohort` = Age.Cohort, `Measure Type` = Measure.Type)


write.table(
  total_pop_proj_long_fips,
  file.path(getwd(), "data", "population_projections_by_town_2015-2025.csv"),
  sep = ",",
  row.names = F
)



