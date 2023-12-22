# merging ruff morph-types to encounter history data

# a vector of all the packages needed in the project's scripts
packages_required_in_project <- 
  c("RMark",
    "stringr",
    "dplyr",
    "openxlsx"
  )

# of the required packages, check if some need to be installed
new.packages <- 
  packages_required_in_project[!(packages_required_in_project %in% 
                                   installed.packages()[,"Package"])]

# install all packages that are not locally available
if(length(new.packages)) install.packages(new.packages)

# load all the packages into the current R session
lapply(packages_required_in_project, require, character.only = TRUE)

# read excel spreadsheets into R
ruff_surv <- read.xlsx(xlsxFile = "data/raw/from_Lucie/Ruff_mark-resight_data.xlsx", sheet = "Feuil1", rows = c(1:5498), cols = c(1,2,3,13), colNames = TRUE)
ruff_morphs_check <- read.xlsx(xlsxFile = "data/raw/from_Clemens/Ruff_morph-types_data.xlsx", sheet = "results short", rows = c(2:242), cols = c(1:16), colNames = TRUE)
ruff_morphs_total <- read.xlsx(xlsxFile = "data/raw/from_Clemens/Ruff_morph-types_data.xlsx", sheet = "sample selection", rows = c(5:145, 153:252, 258:6932), cols = c(1:43), colNames = TRUE)

ruff_morphs_total


# consolidate relevant columns in Sheffield subset and rename columns
ruff_morphs_check_sub <- ruff_morphs_check[ , c(1, 3, 4, 5, 6, 7, 15)]
colnames(ruff_morphs_check_sub) <- c("lab_ID", "Gron_phenotype", "Seew_morph", "Seew_sex", "mismatch_info", "mismatch_comment", "morph")

# consolidate relevant columns in full data and rename columns
ruff_morphs_total_sub <- ruff_morphs_total[, c(4, 5, 6, 10, 11, 13, 27, 28, 16)]
colnames(ruff_morphs_total_sub) <- c("lab_ID", "ring_ID", "color_ID", "Gron_phenotype", "old_sex_morph", "age", "Seew_morph", "Seew_sex", "field_comm")

ruff_morphs_total_sub %>% 
  filter(Gron_phenotype == "" & Seew_sex == "m")

# label capture data columns
colnames(ruff_surv) <- c("color_ID", "age_Lucie", "sex_Lucie", "ch")

# remove possible white spaces before merging
ruff_morphs_total_sub$ring_ID <- gsub(" ", "", ruff_morphs_total_sub$ring_ID)
ruff_morphs_total_sub$color_ID <- gsub(" ", "", ruff_morphs_total_sub$color_ID)
ruff_morphs_total_sub$lab_ID <- gsub(" ", "", ruff_morphs_total_sub$lab_ID)

# merge full dataset with Sheffield subset by lab_ID, Gron_phenotype, Seew_morph, and Seew_sex
ruff_morphs_joined <- left_join(ruff_morphs_total_sub, ruff_morphs_check_sub, by = c("lab_ID", "Gron_phenotype", "Seew_morph", "Seew_sex"))

# classify morphs using genetic and field data
ruff_morphs_joined$morph2 <- 
  # use morph from Sheffield subset if present
  ifelse(!is.na(ruff_morphs_joined$morph), ruff_morphs_joined$morph,
         # if no inversion found in Seewiesen, then "I"
         ifelse(ruff_morphs_joined$Seew_morph == "I", "I",
                # if inversion found in Seewiesen and sex was male and phenotype is faeder, then "F"
                ifelse(ruff_morphs_joined$Seew_morph == "S/F" & 
                         ruff_morphs_joined$Seew_sex == "m" & 
                         ruff_morphs_joined$Gron_phenotype == "faeder", "F",
                       # if inversion found in Seewiesen and sex was male and field notes mention possible faeder, then "F"
                       ifelse(ruff_morphs_joined$Seew_morph == "S/F" & 
                                ruff_morphs_joined$Seew_sex == "m" & 
                                (grepl("faeder", ruff_morphs_joined$field_comm) | 
                                   grepl("faer", ruff_morphs_joined$field_comm)), "F",
                              # if inversion found in Seewiesen and sex was male, then "S"
                              ifelse(ruff_morphs_joined$Seew_morph == "S/F" & 
                                       ruff_morphs_joined$Seew_sex == "m", "S", "XXX")))))

# check levels of morphs
levels(as.factor(ruff_morphs_joined$morph2))

# check which observations have "S/F" morph
ruff_morphs_joined[which(ruff_morphs_joined$morph2 == "S/F"),]

# this one bird is likely a "S"
ruff_morphs_joined[which(ruff_morphs_joined$ring_ID == "1501853"), "morph2"] <- "S"

# rename the sex column (genetic sex trumps the field sex)
ruff_morphs_joined$sex <- ruff_morphs_joined$Seew_sex

# merge the capture data with the morph data
ruff_surv_joined <- left_join(ruff_surv, ruff_morphs_joined[, c("lab_ID", "ring_ID", "color_ID", "age", "morph2", "sex")], by = "color_ID")

# remove rows that don't have a morph
ruff_surv_joined <- ruff_surv_joined[!is.na(ruff_surv_joined$morph2), ]

# change dutch abbreviations to english to keep consistent
ruff_surv_joined$age2 <- str_replace(ruff_surv_joined$age, pattern = "kj", replacement = "cy")

# classify age at first marking
ruff_surv_joined$age3 <- 
  # birds marked in 1st year are Juveniles
  ifelse(grepl("1", ruff_surv_joined$age2), "Juv",
         # birds marked after 2nd year are Adults
         ifelse(grepl(">2", ruff_surv_joined$age2), "Ad",
                # birds marked in 2nd year are Juveniles
                ifelse(grepl("2", ruff_surv_joined$age2), "Juv", 
                       # cases in which the Lucie had the age but not the master data
                       ifelse(ruff_surv_joined$age_Lucie == "2cy", "Juv", 
                              ifelse(ruff_surv_joined$age_Lucie == "ad", "Ad", "XXX")))))

# sexes that don't match
ruff_surv_joined[ruff_surv_joined$sex_Lucie != ruff_surv_joined$sex,]

# consolidate final dataset
ruff_surv_morph <- ruff_surv_joined[, c("ring_ID", "color_ID", "lab_ID", "sex", "age3", "morph2", "ch")]

# rename columns
colnames(ruff_surv_morph) <- c("ring_ID", "color_ID", "lab_ID", "mol_sex", "cap_age", "morph", "ch")

# consolidate dataset for mark-recapture analysis
ruff_inp_morph <- ruff_surv_morph[, c("ch", "mol_sex", "cap_age", "morph")]
colnames(ruff_inp_morph) <- c("ch", "sex", "age", "morph")
table(ruff_inp_morph$morph, ruff_inp_morph$sex)
prop.table(table(ruff_inp_morph$morph, ruff_inp_morph$sex), margin = 2)

# write.table(ruff_inp_morph, file = "data/cooked/ruff_inp_morph.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

# remove faeders due to low sample size
ruff_inp_sat_ind <- ruff_inp_morph[-which(ruff_inp_morph$morph == "F"), ]
ruff_inp_sat_ind_fad <- ruff_inp_morph[-which(ruff_inp_morph$sex == "f"), c(1, 3, 4)]
table(ruff_inp_sat_ind$morph, ruff_inp_sat_ind$sex)
table(ruff_inp_sat_ind_fad$morph, ruff_inp_sat_ind_fad$age)

ruff_inp_sat_ind$morph <- as.factor(ruff_inp_sat_ind$morph)
ruff_inp_sat_ind$sex <- as.factor(ruff_inp_sat_ind$sex)
ruff_inp_sat_ind$age <- as.factor(ruff_inp_sat_ind$age)
ruff_inp_sat_ind_fad$morph <- as.factor(ruff_inp_sat_ind_fad$morph)
ruff_inp_sat_ind_fad$age <- as.factor(ruff_inp_sat_ind_fad$age)

ruff.proc_age_sex_sat_ind <- RMark::process.data(ruff_inp_sat_ind, model = "CJS",
                                               groups = c("age", "morph", "sex"),
                                               begin.time = 2004, age.var = 2, 
                                               initial.age = c(1, 0))

ruff.proc_age_sex_sat_ind_fad <- RMark::process.data(ruff_inp_sat_ind_fad, model = "CJS",
                                               groups = c("age", "morph"),
                                               begin.time = 2004, age.var = 1, 
                                               initial.age = c(1, 0))

# groups ruff_inp_sat_ind:
# 1 = female, ad, ind
# 2 = female, juv, ind
# 3 = female, ad, sat
# 4 = female, juv, sat
# 5 = male, ad, ind
# 6 = male, juv, ind
# 7 = male, ad, sat
# 8 = male, juv, sat

# groups ruff_inp_sat_ind_fad:
# 1 = ad, fad
# 2 = juv, fad
# 3 = ad, ind
# 4 = juv, ind
# 5 = ad, sat
# 6 = juv, sat

export.chdata(data = ruff.proc_age_sex_sat_ind, filename = "data/cooked/HC_age_sex_morph", replace = TRUE)
export.chdata(data = ruff.proc_age_sex_sat_ind_fad, filename = "data/cooked/HC_age_males_sat_ind_fad", replace = TRUE)

read.table("data/cooked/HC_age_sex_morph.inp", colClasses = "character") %>% 
  bind_cols(ruff_inp_sat_ind, .) %>% 
  filter(sex == "m" & age == "Juv")

read.table("data/cooked/HC_age_males_sat_ind_fad.inp", colClasses = "character") %>% 
  bind_cols(ruff_inp_sat_ind_fad, .) %>% 
  filter(morph == "I" & age == "Ad") %>%
  rename(ch2 = V1,
         G1 = V2,
         G2 = V3,
         G3 = V4, 
         G4 = V5,
         G5 = V6,
         G6 = V7) %>% 
  head()
