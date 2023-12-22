# Plot of Ruff apparent survival for Clemens' ESEB talk
# Author: Luke J. Eberhart-Phillips
# August 15, 2018

# make sure packages are installed (uncomment uninstalled libraries)
# install.packages("RMark")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("RColorBrewer")

# load libraries
# install.packages("readxl")
library(readxl)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("patchwork")
library(patchwork)

# library(RMark)
# library(dplyr)
# library(ggplot2)
# library(RColorBrewer)
# library(readxl)

# import mark-recapture data and classify groups as factors
ruff_inp_morph <- 
  read.table("data/cooked/ruff_inp_morph.txt",
             header = TRUE, 
             colClasses = c("character","factor","factor","factor"))

# remove the single female faeder prior to modelling
ruff_inp_morph <- 
  ruff_inp_morph[-which(ruff_inp_morph$sex == "f" & 
                          ruff_inp_morph$morph == "F"), ]

# classify Independent morphs ("I") as "Ancestral", and Faeders ("F") and 
# Satellites ("S") as "Inversion"
ruff_inp_morph$inversion <- 
  as.factor(ifelse(ruff_inp_morph$morph == "I", "Ancestral", "Inversion"))

# process the capture history data to incorporate group structure
ruff.proc_inversion_age_sex <- 
  RMark::process.data(ruff_inp_morph, model = "CJS",
                      groups = c("age", "inversion", "sex"),
                      begin.time = 2004, age.var = 2, 
                      initial.age = c(1, 0))

# check levels of the group to see the ranking
names(ruff.proc_inversion_age_sex$freq)
# [1] "ageAd.inversionAncestral.sexf" 
# [2] "ageJuv.inversionAncestral.sexf"
# [3] "ageAd.inversionInversion.sexf" 
# [4] "ageJuv.inversionInversion.sexf"
# [5] "ageAd.inversionAncestral.sexm" 
# [6] "ageJuv.inversionAncestral.sexm"
# [7] "ageAd.inversionInversion.sexm" 
# [8] "ageJuv.inversionInversion.sexm"

# export the formatted MARK .inp file to the directory for import into E-SURGE
export.chdata(data = ruff.proc_inversion_age_sex, 
              filename = "Ruff_inversion_age_sex", 
              replace = TRUE)

############################ E-SURGE modelling ################################

# Number of occasions :   8
# Number of states :   5
# Number of events :   2
# Number of groups :   8
# Number of age classes :   2

# # of step for initial state :1 
# Phrase for step 1 : g(2 4 6 8,1 5,3 7)+t 
# Number of shortcuts : 1
# Pattern matrix :
#   i 	- 	* 	- 	
#   Name file for covariates : defaultfile  
# 
# # of step for transition : 3 
# Phrase for step 1 : f(1 3).to(5).[g(5 7).a(1)]+others  
# Number of shortcuts : 1
# Pattern matrix :
#   *	-	-	-	t	
# -	*	-	-	-	
#   -	-	*	-	t	
# -	-	-	*	-	
#   -	-	-	-	*	
#   Phrase for step 2 : f(1 2,3 4).inversion.sex+t+others 
# Number of shortcuts : 2
# sex ->   [g(1 2 3 4,5 6 7 8)] 
# inversion ->   [g(1 2 5 6,3 4 7 8)] 
# Pattern matrix :
#   s	-	-	-	*	
#   -	s	-	-	*	
#   -	-	s	-	*	
#   -	-	-	s	*	
#   -	-	-	-	*	
#   Phrase for step 3 : [f(1 2, 3 4).to(1 3)+f(1 2 3 4).to(1 3).t]+sex.inversion+others  
# Number of shortcuts : 2
# sex ->   [g(1 2 3 4,5 6 7 8)] 
# inversion ->   [g(1 2 5 6,3 4 7 8)] 
# Pattern matrix :
#   p	*	-	-	-	
#   p	*	-	-	-	
#   -	-	p	*	-	
#   -	-	p	*	-	
#   -	-	-	-	*	
#   Name file for covariates :  defaultfile  
# 
# # of step for encounter : 1 
# Phrase for step 1 : firste+nexte  
# Number of shortcuts : 1
# Pattern matrix :
#   - 	* 	
#   * 	- 	
#   - 	* 	
#   * 	- 	
#   * 	- 	
#   Name file for covariates :  defaultfile  

# GEMACO for E-SURGE
# The following structure applies to "Model 32" fo the candidate list:

# run model selection on transience parameter (τ), top model has following
# structure:
# τ [null]                  <- 	f(1 3).to(5).[g(5 7).a(1)]+others  (=0 in IVFV)

# run model selection on detection probability (p), top model has following
# structure:
# p [H+sex*inversion+t]     <- 	[f(1 2, 3 4).to(1 3)+f(1 2 3 4).to(1 3).t]+sex.inversion+others

# run model selection on detectability class membership (π), top model has following
# structure:
# π [(inversion*ad)+1y+t]	  <-  g(2 4 6 8,1 5,3 7)+t

# apparent survival (ɸ) model of interest is an interaction between sex and 
# inversion, while controlling for variation across time and detectability"
# ɸ [H*sex*inversion+t]     <-  f(1 2,3 4).inversion.sex+t+others 

#### wrangle output from E-Surge ----
# import E-SURGE output spreadsheet into R for plotting
model_out <-
  readxl::read_xls(path = "C:/Users/leberhart/Documents/ruff_demography/e-surge/2023/July_3/July_3_Phi/Model6.xls", 
                   sheet = "Reduced Set of Parameters", col_names = TRUE) %>% 
  rename(lcl = `CI-`,
         ucl = `CI+`) %>% 
  mutate(morph = ifelse(Group %in% c(1, 2, 5, 6), "independent", "satellite"),
         sex = ifelse(Group %in% c(1, 2, 3, 4), "female", "male"),
         age = ifelse(Group %in% c(1, 3, 5, 7), "adult", "first-year"),
         class = ifelse(From == 1, "high", "low"),
         parameter = ifelse(Parameters == "IS", "Pi",
                            ifelse(Parameters == "p", "p",
                                   ifelse(Parameters == "s", "Phi", "XXX"))),
         year = ifelse(Parameters != "s" & Time == 1, "2004",
                       ifelse(Parameters != "s" & Time == 2, "2005",
                              ifelse(Parameters != "s" & Time == 3, "2006",
                                     ifelse(Parameters != "s" & Time == 4, "2007",
                                            ifelse(Parameters != "s" & Time == 5, "2008",
                                                   ifelse(Parameters != "s" & Time == 6, "2009",
                                                          ifelse(Parameters != "s" & Time == 7, "2010",
                                                                 ifelse(Parameters != "s" & Time == 8, "2011",
                                                                        ifelse(Parameters == "s" & Time == 1, "2004-2005",
                                                                               ifelse(Parameters == "s" & Time == 2, "2005-2006",
                                                                                      ifelse(Parameters == "s" & Time == 3, "2006-2007",
                                                                                             ifelse(Parameters == "s" & Time == 4, "2007-2008",
                                                                                                    ifelse(Parameters == "s" & Time == 5, "2008-2009",
                                                                                                           ifelse(Parameters == "s" & Time == 6, "2009-2010",
                                                                                                                  ifelse(Parameters == "s" & Time == 7, "2010-2011", "XXX")))))))))))))))) %>% 
  mutate(year = as.factor(year),
         age_sex = paste(age, sex, sep = " "),
         Pi_fill = ifelse(Group == 5, "independent", ifelse(Group == 7, "satellite", "not applicable"))) %>% 
  # pull(age_sex)
  mutate(Pi_fill = factor(Pi_fill, levels = c("independent", "satellite", "not applicable")),
         age_sex = factor(age_sex, levels = c("first-year female", "first-year male", "adult female", "adult male")))
  

#### visualizing results ----
# facet labels for plot
facet_names <- c(
  `high` = "HD",
  `low` = "LD", 
  `male` = "male",
  `female` = "female",
  `adult` = "adult",
  `first-year` = "first year",
  `first-year female` = "first-year",
  `first-year male` = "first-year",
  `adult female` = "adult female",
  `adult male` = "adult male")

# plot apparent survival
Phi_plot <- 
  # subset data to extract the apparent survival estimates ("s")
  ggplot2::ggplot(data = dplyr::filter(model_out, 
                                       parameter == "Phi"), 
                  aes(y = Estimates, x = year, 
                      fill = morph, group = morph, ymin = lcl, ymax = ucl)) +
  geom_ribbon(#position = pd, 
              aes(ymin = lcl, ymax = ucl), alpha = 0.3) +
  geom_line(#position = pd,
            size = 0.75, na.rm = TRUE, aes(color = morph)) +
  geom_point(#position = pd, 
             shape = 21, colour = "black",  size = 4, 
             na.rm = TRUE) +
  # facet by sex (columns) and detection class (rows)
  facet_grid(class ~ sex, labeller = as_labeller(facet_names)) +
  theme_bw() +
  theme(text = element_text(size = 16, family="Franklin Gothic Book"),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 10, angle = 45, vjust = 1, 
                                    hjust = 1), 
        axis.title.y = element_text(size = 12, 
                                    margin = margin(0, 5, 0, 0)),
        axis.text.y = element_text(size = 10), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(linetype = "solid", colour = "grey")) +
  scale_fill_manual(values = Inversion_palette) +
  scale_color_manual(values = Inversion_palette) +
  ylab(expression(paste("Apparent survival (year" ^-1, " ± 95% CI)",
                        sep = ""))) +
  # scale_x_discrete(labels = c("1" = "2004-2005",
  #                             "2" = "2005-2006",
  #                             "3" = "2006-2007",
  #                             "4" = "2007-2008",
  #                             "5" = "2008-2009",
  #                             "6" = "2009-2010",
  #                             "7" = "2010-2011")) +
  scale_y_continuous(limits = c(0, 1))
Phi_plot

# # save plot to directory
# ggsave(Phi_sex_inversion_detectability_time_plot,
#        filename = "Ruff_Phi_sex_inversion_detectability_time_plot_ESEB_2018.jpg",
#        path = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/ruff_demography/plots/",
#        width = 8,
#        height = 4, units = "in",
#        dpi = 600,
#        scale = 1)

# plot capture probability
p_plot <- 
  # subset data to extract the apparent survival estimates ("p")
  ggplot2::ggplot(data = dplyr::filter(model_out, 
                                       parameter == "p"), 
                  aes(y = Estimates, x = year, 
                      fill = morph, group = morph, ymin = lcl, ymax = ucl)) +
  geom_ribbon(position = pd, aes(ymin = lcl, ymax = ucl), alpha = 0.3) +
  geom_line(position = pd,  size = 0.75, na.rm = TRUE, aes(color = morph)) +
  geom_point(position = pd, shape = 21, colour = "black",  size = 4, 
             na.rm = TRUE) +
  # facet by sex (columns) and detection class (rows)
  facet_grid(class ~ sex, labeller = as_labeller(facet_names)) +
  theme_bw() +
  theme(text = element_text(size = 16, family="Franklin Gothic Book"),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 10, angle = 45, vjust = 1, 
                                    hjust = 1), 
        axis.title.y = element_text(size = 12, 
                                    margin = margin(0, 5, 0, 0)),
        axis.text.y = element_text(size = 10), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(linetype = "solid", colour = "grey")) +
  scale_fill_manual(values = rep(Detectability_palette[3], 2)) +
  scale_color_manual(values = rep(Detectability_palette[3], 2)) +
  ylab(expression(paste("Detection probability (year" ^-1, " ± 95% CI)",
                        sep = ""))) +
  # scale_x_discrete(labels = c("1" = "2004-2005",
  #                             "2" = "2005-2006",
  #                             "3" = "2006-2007",
  #                             "4" = "2007-2008",
  #                             "5" = "2008-2009",
  #                             "6" = "2009-2010",
  #                             "7" = "2010-2011")) +
  scale_y_continuous(limits = c(0, 1))
p_plot

# # save plot to directory
# ggsave(p_sex_inversion_detectability_time_plot,
#        filename = "Ruff_p_sex_inversion_detectability_time_plot_ESEB_2018.jpg",
#        path = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/ruff_demography/plots/",
#        width = 8,
#        height = 4, units = "in",
#        dpi = 600,
#        scale = 1)
# 
# # merge Age and Inversion groups for Pi plot
# inversion_sex_time_detectability$Age_Inversion <- 
#   ifelse(inversion_sex_time_detectability$Age == "Adult",
#          paste(inversion_sex_time_detectability$Age, 
#                inversion_sex_time_detectability$Inversion, sep = " "),
#          inversion_sex_time_detectability$Age)

# plot probability of high detectability class membership
Pi_plot <- 
  # subset data to extract the apparent survival estimates ("IS")
  ggplot2::ggplot(data = dplyr::filter(model_out, 
                                       parameter == "Pi", year != "2011"), 
                  aes(y = Estimates, x = year, 
                      fill = Pi_fill, group = Pi_fill, ymin = lcl, ymax = ucl)) +
  geom_ribbon(position = pd, aes(ymin = lcl, ymax = ucl), alpha = 0.3) +
  geom_line(position = pd,  size = 0.75, na.rm = TRUE, aes(color = Pi_fill)) +
  geom_point(position = pd, shape = 21, colour = "black",  size = 4, 
             na.rm = TRUE) +
  # facet by sex (columns) and detection class (rows)
  # facet_grid(Detection_class ~ Sex, labeller = as_labeller(facet_names)) +
  facet_grid(class ~ age_sex, labeller = as_labeller(facet_names)) +
  theme_bw() +
  theme(text = element_text(size = 16, family="Franklin Gothic Book"),
        legend.title = element_blank(),
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 10, angle = 45, vjust = 1, 
                                    hjust = 1), 
        axis.title.y = element_text(size = 12, 
                                    margin = margin(0, 15, 0, 0),
                                    vjust = -3),
        axis.text.y = element_text(size = 10), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(linetype = "solid", colour = "grey")) +
  scale_fill_manual(values = Detectability_palette) +
  scale_color_manual(values = Detectability_palette) +
  ylab(expression(paste(" Probability of membership in the\nhigh detectability class (± 95% CI)",
                        sep = ""))) +
  # scale_x_discrete(labels = c("1" = "2004-2005",
  #                             "2" = "2005-2006",
  #                             "3" = "2006-2007",
  #                             "4" = "2007-2008",
  #                             "5" = "2008-2009",
  #                             "6" = "2009-2010",
  #                             "7" = "2010-2011")) +
  scale_y_continuous(limits = c(0, 1))
Pi_plot

Pi_plot / p_plot / Phi_plot

# save plot to directory
ggsave(Pi_sex_inversion_detectability_time_plot,
       filename = "Ruff_Pi_sex_inversion_detectability_time_plot_ESEB_2018.jpg",
       path = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/ruff_demography/plots/",
       width = 8,
       height = 4, units = "in",
       dpi = 600,
       scale = 1)


# rename confidence interval columns
colnames(model_out)[c(9, 10)] <- c("lcl", "ucl")

# classify groups for inversion
model_out$Inversion <- 
  ifelse(inversion_sex_time_detectability$Group == 1 | 
           inversion_sex_time_detectability$Group == 5, 
         "Ancestral", "Inversion")

# classify groups for age
inversion_sex_time_detectability$Age <- 
  ifelse(inversion_sex_time_detectability$Group == 1 | 
           inversion_sex_time_detectability$Group == 3 |
             inversion_sex_time_detectability$Group == 5 | 
               inversion_sex_time_detectability$Group == 7, "Adult", "Juvenile")

# merge age and Inversion for Pi plot
inversion_sex_time_detectability$Age_Inversion <- 
  paste(inversion_sex_time_detectability$Age, 
        inversion_sex_time_detectability$Inversion, sep = " ")

# classify groups for sex
inversion_sex_time_detectability$Sex <- 
  ifelse(inversion_sex_time_detectability$Group == 1 | 
           inversion_sex_time_detectability$Group == 3, "Female", "Male")

# classify groups for detectability class
inversion_sex_time_detectability$Detection_class <- 
  ifelse(inversion_sex_time_detectability$To == 3, "HD", "LD")

# make time a factor to help labelling the x-axis of the plot
inversion_sex_time_detectability$Time <- 
  as.factor(inversion_sex_time_detectability$Time)

# assign the color palette to be used in the plot
Inversion_palette <- RColorBrewer::brewer.pal(7, "Spectral")[c(7, 2)]
Detectability_palette <- c(RColorBrewer::brewer.pal(7, "Spectral")[c(7, 2)],
                           RColorBrewer::brewer.pal(7, "Greys")[5])



# plot apparent survival
Phi_sex_inversion_detectability_time_plot <- 
  # subset data to extract the apparent survival estimates ("s")
  ggplot2::ggplot(data = dplyr::filter(inversion_sex_time_detectability, 
                                       Parameters == "s"), 
                  aes(y = Estimates, x = Time, 
                      fill = Inversion, group = Inversion, ymin = lcl, ymax = ucl)) +
  geom_ribbon(position = pd, aes(ymin = lcl, ymax = ucl), alpha = 0.3) +
  geom_line(position = pd,  size = 0.75, na.rm = TRUE, aes(color = Inversion)) +
  geom_point(position = pd, shape = 21, colour = "black",  size = 4, 
             na.rm = TRUE) +
  # facet by sex (columns) and detection class (rows)
  facet_grid(Detection_class ~ Sex, labeller = as_labeller(facet_names)) +
  theme_bw() +
  theme(text = element_text(size = 16, family="Franklin Gothic Book"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 10, angle = 45, vjust = 1, 
                                    hjust = 1), 
        axis.title.y = element_text(size = 14, 
                                    margin = margin(0, 5, 0, 0)),
        axis.text.y = element_text(size = 10), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(linetype = "solid", colour = "grey")) +
  scale_fill_manual(values = Inversion_palette) +
  scale_color_manual(values = Inversion_palette) +
  ylab(expression(paste("Apparent survival (year" ^-1, " ± 95% CI)",
                        sep = ""))) +
  scale_x_discrete(labels = c("1" = "2004-2005",
                              "2" = "2005-2006",
                              "3" = "2006-2007",
                              "4" = "2007-2008",
                              "5" = "2008-2009",
                              "6" = "2009-2010",
                              "7" = "2010-2011")) +
  scale_y_continuous(limits = c(0, 1))
Phi_sex_inversion_detectability_time_plot

# save plot to directory
ggsave(Phi_sex_inversion_detectability_time_plot,
       filename = "Ruff_Phi_sex_inversion_detectability_time_plot_ESEB_2018.jpg",
       path = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/ruff_demography/plots/",
       width = 8,
       height = 4, units = "in",
       dpi = 600,
       scale = 1)

# plot capture probability
p_sex_inversion_detectability_time_plot <- 
  # subset data to extract the apparent survival estimates ("p")
  ggplot2::ggplot(data = dplyr::filter(inversion_sex_time_detectability, 
                                       Parameters == "p"), 
                  aes(y = Estimates, x = Time, 
                      fill = Inversion, group = Inversion, ymin = lcl, ymax = ucl)) +
  geom_ribbon(position = pd, aes(ymin = lcl, ymax = ucl), alpha = 0.3) +
  geom_line(position = pd,  size = 0.75, na.rm = TRUE, aes(color = Inversion)) +
  geom_point(position = pd, shape = 21, colour = "black",  size = 4, 
             na.rm = TRUE) +
  # facet by sex (columns) and detection class (rows)
  facet_grid(Detection_class ~ Sex, labeller = as_labeller(facet_names)) +
  theme_bw() +
  theme(text = element_text(size = 16, family="Franklin Gothic Book"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 10, angle = 45, vjust = 1, 
                                    hjust = 1), 
        axis.title.y = element_text(size = 14, 
                                    margin = margin(0, 5, 0, 0)),
        axis.text.y = element_text(size = 10), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(linetype = "solid", colour = "grey")) +
  scale_fill_manual(values = Inversion_palette) +
  scale_color_manual(values = Inversion_palette) +
  ylab(expression(paste("Detection probability (year" ^-1, " ± 95% CI)",
                        sep = ""))) +
  scale_x_discrete(labels = c("1" = "2004-2005",
                              "2" = "2005-2006",
                              "3" = "2006-2007",
                              "4" = "2007-2008",
                              "5" = "2008-2009",
                              "6" = "2009-2010",
                              "7" = "2010-2011")) +
  scale_y_continuous(limits = c(0, 1))
p_sex_inversion_detectability_time_plot

# save plot to directory
ggsave(p_sex_inversion_detectability_time_plot,
       filename = "Ruff_p_sex_inversion_detectability_time_plot_ESEB_2018.jpg",
       path = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/ruff_demography/plots/",
       width = 8,
       height = 4, units = "in",
       dpi = 600,
       scale = 1)

# merge Age and Inversion groups for Pi plot
inversion_sex_time_detectability$Age_Inversion <- 
  ifelse(inversion_sex_time_detectability$Age == "Adult",
         paste(inversion_sex_time_detectability$Age, 
               inversion_sex_time_detectability$Inversion, sep = " "),
         inversion_sex_time_detectability$Age)

# plot probability of high detectability class membership
Pi_sex_inversion_detectability_time_plot <- 
  # subset data to extract the apparent survival estimates ("IS")
  ggplot2::ggplot(data = dplyr::filter(inversion_sex_time_detectability, 
                                       Parameters == "IS", Time != "8"), 
                  aes(y = Estimates, x = Time, 
                      fill = Age_Inversion, group = Age_Inversion, ymin = lcl, ymax = ucl)) +
  geom_ribbon(position = pd, aes(ymin = lcl, ymax = ucl), alpha = 0.3) +
  geom_line(position = pd,  size = 0.75, na.rm = TRUE, aes(color = Age_Inversion)) +
  geom_point(position = pd, shape = 21, colour = "black",  size = 4, 
             na.rm = TRUE) +
  # facet by sex (columns) and detection class (rows)
  # facet_grid(Detection_class ~ Sex, labeller = as_labeller(facet_names)) +
  theme_bw() +
  theme(text = element_text(size = 16, family="Franklin Gothic Book"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(size = 10, angle = 45, vjust = 1, 
                                    hjust = 1), 
        axis.title.y = element_text(size = 14, 
                                    margin = margin(0, 15, 0, 0),
                                    vjust = -3),
        axis.text.y = element_text(size = 10), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.5, colour = "grey40"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(linetype = "solid", colour = "grey")) +
  scale_fill_manual(values = Detectability_palette) +
  scale_color_manual(values = Detectability_palette) +
  ylab(expression(paste(" Probability of membership in the\nhigh detectability class (± 95% CI)",
                        sep = ""))) +
  scale_x_discrete(labels = c("1" = "2004-2005",
                              "2" = "2005-2006",
                              "3" = "2006-2007",
                              "4" = "2007-2008",
                              "5" = "2008-2009",
                              "6" = "2009-2010",
                              "7" = "2010-2011")) +
  scale_y_continuous(limits = c(0, 1))
Pi_sex_inversion_detectability_time_plot

# save plot to directory
ggsave(Pi_sex_inversion_detectability_time_plot,
       filename = "Ruff_Pi_sex_inversion_detectability_time_plot_ESEB_2018.jpg",
       path = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/ruff_demography/plots/",
       width = 8,
       height = 4, units = "in",
       dpi = 600,
       scale = 1)
