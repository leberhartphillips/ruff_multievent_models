# visualizing Ruff morph-sex-specific multievent models from E-SURGE
# June 27, 2023
# Luke Eberhart-Hertel (luke.eberhart@bi.mpg.de)

# groups:
# 1 = female, ad, ind
# 2 = female, juv, ind
# 3 = female, ad, sat
# 4 = female, juv, sat
# 5 = male, ad, ind <- trap-happy based on U-CARE Test2.CT
# 6 = male, juv, ind <- trap-happy based on U-CARE Test2.CT
# 7 = male, ad, sat
# 8 = male, juv, sat

# Lucie's gemaco:
# group 1: Ad male
# group 2: Juv male
# group 3: Ad female
# group 4: Juv female

# p =
# H*(sex+t)
# [f(1 2, 3 4).to(1 3)].[sex+t]+others <- 

# libraries needed
#install.packages("readxl", dependencies = TRUE)
library(readxl)
#install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)

# Load model output from E-SURGE
mod1_par_est <- 
  read_excel(path = "data/model_output/sex_sat_ind_Model_1.xls", sheet = "Reduced Set of Parameters", 
             range = cell_cols("A:K"), col_names = TRUE)

p_est <- 
  mod1_par_est %>% 
  filter(Parameters == "p")
  
