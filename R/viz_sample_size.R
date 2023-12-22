## install dependent packages

# a vector of all the packages needed in the project's scripts
packages_required_in_project <- 
  c("tidyverse",
    "readxl",
    "gt",
    "patchwork",
    "packcircles",
    "RColorBrewer",
    "showtext"
  )

# of the required packages, check if some need to be installed
new.packages <- 
  packages_required_in_project[!(packages_required_in_project %in% 
                                   installed.packages()[,"Package"])]

# install all packages that are not locally available
if(length(new.packages)) install.packages(new.packages)

# load all the packages into the current R session
lapply(packages_required_in_project, require, character.only = TRUE)

# font_add_google("Libre Franklin")
# showtext_auto()

#### table of sample sizes across sex and morph ----
df <- 
  read.table("data/cooked/ruff_inp_morph.txt", colClasses = "character", header = TRUE) %>% 
  mutate(sex = ifelse(sex == "f", "female", "male"),
         morph = ifelse(morph == "F", "Faeder",
                        ifelse(morph == "I", "Independent", "Satellite")),
         age = ifelse(age == "Juv", "first-year", "adult")) %>% 
  mutate(morph = factor(morph, levels = c("Independent", "Satellite", "Faeder")))

table_df <- table(df$morph, df$sex)
table_df

nrow(df)

#### circle plot of sample sizez across sex and morph ----
# import capture histroy dataset
ruff_data <-
  read.table("data/cooked/ruff_inp_morph.txt", colClasses = "character", header = TRUE) %>% 
  mutate(sex = ifelse(sex == "f", "female", "male"),
         morph = ifelse(morph == "F", "Faeder",
                        ifelse(morph == "I", "Independent", "Satellite")),
         age = ifelse(age == "Juv", "first-year", "adult")) %>% 
  mutate(morph = factor(morph, levels = c("Independent", "Satellite", "Faeder"))) %>% 
  mutate(value = ifelse(morph == "Independent", 1.000001,
                        ifelse(morph == "Satellite", 1,
                               ifelse(morph == "Faeder", 0.999999, "XXX"))),
         sex_morph = paste(sex, morph, sep = "_"),
         fill = ifelse(morph == "Independent", "grey20",
                         ifelse(morph == "Satellite", "white",
                                ifelse(morph == "Faeder", brewer.pal(7, "Dark2")[7], "XXX")))) %>%
  arrange(value)

# determine packcircle layout for males
packing_m <- 
  ruff_data %>% 
  filter(sex == "male") %>% 
  select(value, sex_morph) %>% 
  circleProgressiveLayout(.)
dat.gg_m <- circleLayoutVertices(packing_m)

# determine packcircle layout for females
packing_f <- 
  ruff_data %>% 
  filter(sex == "female") %>% 
  select(value, sex_morph) %>% 
  circleProgressiveLayout(.)
dat.gg_f <- circleLayoutVertices(packing_f)

# draw male plot
male_sample <- 
  ggplot(data = dat.gg_m) +
  geom_polygon(aes(x, y, group = id, fill = factor(id)), 
                   color = "black",
               size = 0.5,
               show.legend = FALSE) +
  scale_fill_manual(values = ruff_data %>% 
                      filter(sex == "male") %>% 
                      pull(fill)) +
  theme_void() +
  coord_equal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-45, 45)) +
  scale_y_continuous(limits = c(-45, 45)) #+
  # ggtitle("males")

# draw female plot
female_sample <-
  female_sample <- 
  ggplot(data = dat.gg_f) +
  geom_polygon(aes(x, y, group = id, fill = factor(id)), 
               color = "black",
               size = 0.5,
               show.legend = FALSE) +
  scale_fill_manual(values = ruff_data %>% 
                      filter(sex == "female") %>% 
                      pull(fill)) +
  theme_void() +
  coord_equal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(-45, 45)) +
  scale_y_continuous(limits = c(-25, 65)) #+
  # ggtitle("females")

# mosaic plot
mosaicplot(table_df, main = NA, xlab = "morph", ylab = "sex", color = c(RColorBrewer::brewer.pal(7, "Accent")[c(2, 1)]))

#### plot connecting years of obervation ----
summary_sex <- 
  ruff_data %>% 
  group_by(sex) %>% 
  summarise(total_sex = n())

sex_split <-
  ruff_data %>% 
  bind_cols(., c(paste("bird", c(1:5430), sep = "_"))) %>% 
  mutate(ch = str_pad(as.character(as.numeric(ch)), 8, pad = 0, side = "right")) %>% 
  mutate(ch2 = ch) %>% 
  separate_wider_position(ch , c(y1 = 1,
                                 y2 = 1,
                                 y3 = 1,
                                 y4 = 1,
                                 y5 = 1,
                                 y6 = 1,
                                 y7 = 1,
                                 y8 = 1)) %>%
  rename(ID = `...8`) %>% 
  mutate(y = 0,
         sum_years = as.numeric(y1) + as.numeric(y2) + as.numeric(y3) + 
           as.numeric(y4) + as.numeric(y5) + as.numeric(y6) + 
           as.numeric(y7) + as.numeric(y8)) %>% 
  mutate(y1 = ifelse(y1 == 1, 1, 0),
         y2 = ifelse(y2 == 1, 2, 0),
         y3 = ifelse(y3 == 1, 3, 0),
         y4 = ifelse(y4 == 1, 4, 0),
         y5 = ifelse(y5 == 1, 5, 0),
         y6 = ifelse(y6 == 1, 6, 0),
         y7 = ifelse(y7 == 1, 7, 0),
         y8 = ifelse(y8 == 1, 8, 0)) %>% 
  mutate(second_year = str_sub(as.character(as.numeric(paste0(y2, y3, y4, y5, y6, y7, y8))), 1, 1),
         third_year = str_sub(as.character(as.numeric(paste0(y3, y4, y5, y6, y7, y8))), 1, 1),
         fourth_year = str_sub(as.character(as.numeric(paste0(y4, y5, y6, y7, y8))), 1, 1),
         fifth_year = str_sub(as.character(as.numeric(paste0(y5, y6, y7, y8))), 1, 1),
         sixth_year = str_sub(as.character(as.numeric(paste0(y6, y7, y8))), 1, 1),
         seventh_year = str_sub(as.character(as.numeric(paste0(y6, y7, y8))), 1, 1),
         eighth_year = str_sub(as.character(as.numeric(paste0(y8))), 1, 1)) %>% 
    mutate(third_year = as.numeric(ifelse(third_year == second_year | second_year == 0, 0, third_year)),
           fourth_year = as.numeric(ifelse(fourth_year == third_year | third_year == 0, 0, fourth_year)),
           fifth_year = as.numeric(ifelse(fifth_year == fourth_year | fourth_year == 0, 0, fifth_year)),
           sixth_year = as.numeric(ifelse(sixth_year == fifth_year | fifth_year == 0, 0, sixth_year)),
           seventh_year = as.numeric(ifelse(seventh_year == sixth_year | sixth_year == 0, 0, seventh_year)),
           eighth_year = as.numeric(ifelse(eighth_year == seventh_year | seventh_year == 0, 0, eighth_year)),
           second_year = as.numeric(second_year),
           first_year = 1) %>% 
    mutate(ch1_2 = paste0(first_year, second_year),
           ch2_3 = paste0(second_year, third_year),
           ch3_4 = paste0(third_year, fourth_year),
           ch4_5 = paste0(fourth_year, fifth_year),
           ch5_6 = paste0(fifth_year, sixth_year),
           ch6_7 = paste0(sixth_year, seventh_year),
           ch7_8 = paste0(seventh_year, eighth_year)) %>% 
  group_by(ch1_2, sex) %>% 
  mutate(n_ch1_2 = n()) %>% 
  ungroup() %>% 
  group_by(ch2_3, sex) %>% 
  mutate(n_ch2_3 = n()) %>% 
  ungroup() %>% 
  group_by(ch3_4, sex) %>% 
  mutate(n_ch3_4 = n()) %>% 
  ungroup() %>% 
  group_by(ch4_5, sex) %>% 
  mutate(n_ch4_5 = n()) %>% 
  ungroup() %>% 
  group_by(ch5_6, sex) %>% 
  mutate(n_ch5_6 = n()) %>% 
  ungroup() %>% 
  group_by(ch6_7, sex) %>% 
  mutate(n_ch6_7 = n()) %>% 
  ungroup() %>% 
  group_by(ch7_8, sex) %>% 
  mutate(n_ch7_8 = n()) %>% 
  ungroup() %>% 
    select(-ID, -age, -morph, -sex_morph, -value, -fill) %>% 
    distinct() %>% 
  left_join(., summary_sex)

sex_split_1_2 <- 
  sex_split %>% 
  select(sex, first_year, second_year, ch1_2, n_ch1_2) %>% 
  distinct() %>% 
  arrange(sex, ch1_2) %>% 
  left_join(summary_sex) %>% 
  mutate(prop_sex = n_ch1_2/total_sex)

sex_split_2_3 <- 
  sex_split %>% 
  select(sex, second_year, third_year, ch2_3, n_ch2_3) %>% 
  distinct() %>% 
  arrange(sex, ch2_3) %>% 
  left_join(summary_sex) %>% 
  mutate(prop_sex = n_ch2_3/total_sex)

sex_split_3_4 <- 
  sex_split %>% 
  select(sex, third_year, fourth_year, ch3_4, n_ch3_4) %>% 
  distinct() %>% 
  arrange(sex, ch3_4) %>% 
  left_join(summary_sex) %>% 
  mutate(prop_sex = n_ch3_4/total_sex)

sex_split_4_5 <- 
  sex_split %>% 
  select(sex, fourth_year, fifth_year, ch4_5, n_ch4_5) %>% 
  distinct() %>% 
  arrange(sex, ch4_5) %>% 
  left_join(summary_sex) %>% 
  mutate(prop_sex = n_ch4_5/total_sex)

sex_split_5_6 <- 
  sex_split %>% 
  select(sex, fifth_year, sixth_year, ch5_6, n_ch5_6) %>% 
  distinct() %>% 
  arrange(sex, ch5_6) %>% 
  left_join(summary_sex) %>% 
  mutate(prop_sex = n_ch5_6/total_sex)

sex_split_6_7 <- 
  sex_split %>% 
  select(sex, sixth_year, seventh_year, ch6_7, n_ch6_7) %>% 
  distinct() %>% 
  arrange(sex, ch6_7) %>% 
  left_join(summary_sex) %>% 
  mutate(prop_sex = n_ch6_7/total_sex)

sex_split_7_8 <- 
  sex_split %>% 
  select(sex, seventh_year, eighth_year, ch7_8, n_ch7_8) %>% 
  distinct() %>% 
  arrange(sex, ch7_8) %>% 
  left_join(summary_sex) %>% 
  mutate(prop_sex = n_ch7_8/total_sex)

sample_sizes_t_sex <- 
  bind_rows(filter(rename(select(sex_split_1_2, -ch1_2), 
                   t1 = first_year, t2 = second_year, n_sex = n_ch1_2), t1 != 0),
            filter(rename(select(sex_split_2_3, -ch2_3), 
                   t1 = second_year, t2 = third_year, n_sex = n_ch2_3), t1 != 0), 
            filter(rename(select(sex_split_3_4, -ch3_4), 
                   t1 = third_year, t2 = fourth_year, n_sex = n_ch3_4), t1 != 0),
            filter(rename(select(sex_split_4_5, -ch4_5), 
                   t1 = fourth_year, t2 = fifth_year, n_sex = n_ch4_5), t1 != 0),
            filter(rename(select(sex_split_5_6, -ch5_6), 
                   t1 = fifth_year, t2 = sixth_year, n_sex = n_ch5_6), t1 != 0),
            filter(rename(select(sex_split_6_7, -ch6_7), 
                   t1 = sixth_year, t2 = seventh_year, n_sex = n_ch6_7), t1 != 0),
            filter(rename(select(sex_split_7_8, -ch7_8), 
                   t1 = seventh_year, t2 = eighth_year, n_sex = n_ch7_8), t1 != 0)) %>% 
    group_by(sex, t1) %>% 
    summarise(sum_n = sum(n_sex)) %>% 
  mutate(shade = ifelse(t1 %in% c(1, 2, 3), "white", "black"))

time_points_df <- 
  data.frame(t = c(1, 2, 3, 4, 5, 6, 7, 8),
             name = c("capture", "encounter 1", 
                      "encounter 2", "encounter 3",
                      "encounter 4", "encounter 5",
                      "encounter 6", "encounter 7"))
  
male_sample_plot <- 
  ggplot() +
    ## 1_2
    geom_curve(data = filter(sex_split_1_2, sex == "male" & second_year != 0), 
               aes(x = first_year, xend = second_year, 
                   y = 0, yend = 0, 
                   size = prop_sex), 
               lineend = "round", curvature = -0.5, color = brewer.pal(8, "Blues")[8],
               alpha = 0.75) +
    ## 2_3
    geom_curve(data = filter(sex_split_2_3, sex == "male" & third_year != 0), 
               aes(x = second_year, xend = third_year, 
                   y = 0, yend = 0, 
                   size = prop_sex), 
               lineend = "round", curvature = 0.5, color = brewer.pal(8, "Blues")[7],
               alpha = 0.75) +
    ## 3_4
    geom_curve(data = filter(sex_split_3_4, sex == "male" & fourth_year != 0), 
               aes(x = third_year, xend = fourth_year, 
                   y = 0, yend = 0, 
                   size = prop_sex), 
               lineend = "round", curvature = -0.5, color = brewer.pal(8, "Blues")[6],
               alpha = 0.75) +
    ## 4_5
    geom_curve(data = filter(sex_split_4_5, sex == "male" & fifth_year != 0), 
               aes(x = fourth_year, xend = fifth_year, 
                   y = 0, yend = 0, 
                   size = prop_sex), 
               lineend = "round", curvature = 0.5, color = brewer.pal(8, "Blues")[5],
               alpha = 0.75) +
    ## 5_6
    geom_curve(data = filter(sex_split_5_6, sex == "male" & sixth_year != 0), 
               aes(x = fifth_year, xend = sixth_year, 
                   y = 0, yend = 0, 
                   size = prop_sex), 
               lineend = "round", curvature = -0.5, color = brewer.pal(8, "Blues")[4],
               alpha = 0.75) +
    ## 6_7
    geom_curve(data = filter(sex_split_6_7, sex == "male" & seventh_year != 0), 
               aes(x = sixth_year, xend = seventh_year, 
                   y = 0, yend = 0, 
                   size = prop_sex), 
               lineend = "round", curvature = 0.5, color = brewer.pal(8, "Blues")[3],
               alpha = 0.75) +
    ## 7_8
    geom_curve(data = filter(sex_split_7_8, sex == "male" & eighth_year != 0), 
               aes(x = seventh_year, xend = eighth_year, 
                   y = 0, yend = 0, 
                   size = prop_sex), 
               lineend = "round", curvature = -0.5, color = brewer.pal(8, "Blues")[2],
               alpha = 0.75) +
  geom_point(data = filter(sample_sizes_t_sex, sex == "male"), 
             aes(x = t1, y = 0, fill = as.factor(t1)), size = 13, shape = 21) +
  geom_text(data = filter(sample_sizes_t_sex, sex == "male"), 
            aes(x = t1, y = 0, label = sum_n, color = shade)) +
  # geom_text(data = time_points_df, aes(x = t-0.2, y = -0.05, label = name), 
  #           angle = 90, hjust = 0, color = "white") +
  # scale_y_continuous(limits = c(-0.025, 0.08)) +
  scale_fill_manual(values = rev(brewer.pal(8, "Blues"))) +
  scale_color_manual(values = c("black", "white")) +
  theme_void() +
    theme(legend.position = "none",
        panel.grid.minor.x = element_line(color = "grey90",
                                          size = 0.5,
                                          linetype = 1),
        panel.grid.major.x = element_line(color = "grey90",
                                          size = 0.5,
                                          linetype = 1))  
  
female_sample_plot <- 
  ggplot() +
  ## 1_2
    geom_curve(data = filter(sex_split_1_2, sex == "female" & second_year != 0),
               aes(x = first_year, xend = second_year,
                   y = 0, yend = 0,
                   size = prop_sex),
               lineend = "round", curvature = 0.5, color = brewer.pal(8, "Reds")[8],
               alpha = 0.75) +
    ## 2_3
    geom_curve(data = filter(sex_split_2_3, sex == "female" & third_year != 0),
               aes(x = second_year, xend = third_year,
                   y = 0, yend = 0,
                   size = prop_sex),
               lineend = "round", curvature = -0.5, color = brewer.pal(8, "Reds")[7],
               alpha = 0.75) +
    ## 3_4
    geom_curve(data = filter(sex_split_3_4, sex == "female" & fourth_year != 0),
               aes(x = third_year, xend = fourth_year,
                   y = 0, yend = 0,
                   size = prop_sex),
               lineend = "round", curvature = 0.5, color = brewer.pal(8, "Reds")[6],
               alpha = 0.75) +
    ## 4_5
    geom_curve(data = filter(sex_split_4_5, sex == "female" & fifth_year != 0),
               aes(x = fourth_year, xend = fifth_year,
                   y = 0, yend = 0,
                   size = prop_sex),
               lineend = "round", curvature = -0.5, color = brewer.pal(8, "Reds")[5],
               alpha = 0.75) +
    ## 5_6
    geom_curve(data = filter(sex_split_5_6, sex == "female" & sixth_year != 0),
               aes(x = fifth_year, xend = sixth_year,
                   y = 0, yend = 0,
                   size = prop_sex),
               lineend = "round", curvature = 0.5, color = brewer.pal(8, "Reds")[4],
               alpha = 0.75) +
    ## 6_7
    geom_curve(data = filter(sex_split_6_7, sex == "female" & seventh_year != 0),
               aes(x = sixth_year, xend = seventh_year,
                   y = 0, yend = 0,
                   size = prop_sex),
               lineend = "round", curvature = -0.5, color = brewer.pal(8, "Reds")[3],
               alpha = 0.75) +
    ## 7_8
    geom_curve(data = filter(sex_split_7_8, sex == "female" & eighth_year != 0),
               aes(x = seventh_year, xend = eighth_year,
                   y = 0, yend = 0,
                   size = prop_sex),
               lineend = "round", curvature = 0.5, color = brewer.pal(8, "Reds")[2],
               alpha = 0.75) +
    geom_point(data = filter(sample_sizes_t_sex, sex == "female"), 
               aes(x = t1, y = 0, fill = as.factor(t1)), size = 13, shape = 21) +
    geom_text(data = filter(sample_sizes_t_sex, sex == "female"), 
              aes(x = t1, y = 0, label = sum_n, color = factor(shade))) +
    # geom_text(data = time_points_df, aes(x = t-0.2, y = 0.05, label = name), 
    #           angle = 90, hjust = 0) +
    scale_fill_manual(values = rev(brewer.pal(8, "Reds"))) +
    scale_color_manual(values = c("black", "white")) +
    scale_y_continuous(limits = c(-0.025, 0.08)) +
    # scale_x_continuous(breaks = seq(1, 8, 1), labels = c("capture", "encounter 1",
    #                             "encounter 2", "encounter 3",
    #                             "encounter 4", "encounter 5",
    #                             "encounter 6", "encounter 7")) +
    theme_void() +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(color = "grey90",
                                            size = 0.5,
                                            linetype = 1),
          panel.grid.minor.x = element_line(color = "grey90",
                                            size = 0.5,
                                            linetype = 1),
          axis.text.x = element_blank())
          # axis.text.x = element_text(angle = 50, 
          #                            hjust = 1,
          #                            vjust = 0.9))

# patchwork the two plots together
sample_size_figure <- 
  (male_sample + male_sample_plot) / (female_sample + female_sample_plot)

# export plot to disk
ggsave(plot = sample_size_figure,
       filename = "figs_tables/sample_size_figure.jpg",
       width = 10,
       height = 10, 
       units = "in")

#### morph-sex split ----
summary_morph_sex <- 
  ruff_data %>% 
  group_by(sex, morph) %>% 
  summarise(total_sex_morph = n())

sex_morph_split <- 
  ruff_data %>% 
  bind_cols(., c(paste("bird", c(1:5430), sep = "_"))) %>% 
  mutate(ch = str_pad(as.character(as.numeric(ch)), 8, pad = 0, side = "right")) %>% 
  mutate(ch2 = ch) %>% 
  separate_wider_position(ch , c(y1 = 1,
                                 y2 = 1,
                                 y3 = 1,
                                 y4 = 1,
                                 y5 = 1,
                                 y6 = 1,
                                 y7 = 1,
                                 y8 = 1)) %>%
  rename(ID = `...8`) %>% 
  mutate(y = 0,
         sum_years = as.numeric(y1) + as.numeric(y2) + as.numeric(y3) + 
           as.numeric(y4) + as.numeric(y5) + as.numeric(y6) + 
           as.numeric(y7) + as.numeric(y8)) %>% 
  group_by(ch2, sex, morph) %>% 
  mutate(n_samples = n()) %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(y1 = ifelse(y1 == 1, 1, 0),
         y2 = ifelse(y2 == 1, 2, 0),
         y3 = ifelse(y3 == 1, 3, 0),
         y4 = ifelse(y4 == 1, 4, 0),
         y5 = ifelse(y5 == 1, 5, 0),
         y6 = ifelse(y6 == 1, 6, 0),
         y7 = ifelse(y7 == 1, 7, 0),
         y8 = ifelse(y8 == 1, 8, 0)) %>% 
  left_join(., summary_morph_sex) %>% 
  mutate(prop_sex_morph = n_samples/total_sex_morph)

ggplot(ruff_data_split, aes(group = ID)) +
  geom_curve(aes(x = y1, xend = y2, y = y, yend = y), size = 50, lineend = "round")

str_pad(as.character(ruff_data_split$ch), 8, pad = 0, side = "right")

ruff_data_04_05 <-
  ruff_data_split %>% 
  select(y_2004_, y_2005_, sex, age, morph, value, sex_morph, fill, ID) %>% 
  filter(y_2004_ != 0 & y_2005_ != 0) %>% 
  mutate(y = 0)

ruff_data_05_06 <-
  ruff_data_split %>% 
  select(y_2005_, y_2006_, sex, age, morph, value, sex_morph, fill, ID) %>% 
  filter(y_2005_ != 0 & y_2006_ != 0) %>% 
  mutate(y = 0)

ruff_data_06_07 <-
  ruff_data_split %>% 
  select(y_2006_, y_2007_, sex, age, morph, value, sex_morph, fill, ID) %>% 
  filter(y_2006_ != 0 & y_2007_ != 0) %>% 
  mutate(y = 0)

ruff_data_07_08 <-
  ruff_data_split %>% 
  select(y_2007_, y_2008_, sex, age, morph, value, sex_morph, fill, ID) %>% 
  filter(y_2007_ != 0 & y_2008_ != 0) %>% 
  mutate(y = 0)

ruff_data_08_09 <-
  ruff_data_split %>% 
  select(y_2008_, y_2009_, sex, age, morph, value, sex_morph, fill, ID) %>% 
  filter(y_2008_ != 0 & y_2009_ != 0) %>% 
  mutate(y = 0)

ruff_data_09_10 <-
  ruff_data_split %>% 
  select(y_2009_, y_2010_, sex, age, morph, value, sex_morph, fill, ID) %>% 
  filter(y_2009_ != 0 & y_2010_ != 0) %>% 
  mutate(y = 0)

ruff_data_10_11 <-
  ruff_data_split %>% 
  select(y_2010_, y_2011_, sex, age, morph, value, sex_morph, fill, ID) %>% 
  filter(y_2010_ != 0 & y_2011_ != 0) %>% 
  mutate(y = 0)


