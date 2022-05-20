# R script to wrangle, model, and plot Hooded Plover nest survival

# code: Luke Eberhart-Hertel (luke.eberhart@orn.mpg.de)
# data: Lucinda Plowman, Grainne Maguire, and Mike Weston

# 4 May, 2022

#### setup R environment ----
# load and install packages
# install.packages(tidyverse) 
# install.packages(readxl)
# install.packages(RMark)
# install.packages(RColorBrewer)
# install.packages(patchwork)
library(tidyverse)
library(readxl)
library(RMark)
library(RColorBrewer)
library(patchwork)

# specify personal theme for ggplots
luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.x  = element_text(size = 8), 
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.5, colour = "grey40"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

#### inport data ----
# import the nest history file and clean up NAs
nest_data <-
  read_excel("data/MP  nesting summary for Honours.xlsx", 
             sheet = "MP 2020_2021", col_types = "text") %>% 
  
  # make a nest ID
  mutate(nest_ID = str_remove_all(paste0(`Pair ID (Bird 1)`, 
                                         `Pair ID (Bird 2)`, 
                                         `Attempt #`), " ")) %>% 
  
  # consolidate columns
  select(nest_ID, `Date found`, `Date last seen alive (nest)`, 
         `Nest fail date`, `Hatch?`) %>% 
  
  # simplify column names
  rename(first_found = `Date found`,
         last_alive = `Date last seen alive (nest)`,
         last_checked = `Nest fail date`, 
         Fate = `Hatch?`) %>% 
  
  # wrangle: if date last alive is "Unk." make it "NA"
  mutate(last_alive = ifelse(str_detect(last_alive, "Unk."), NA, last_alive),
         # change Fate to 1 or 0 (1 = failed, 0 = hatched)
         Fate = ifelse(Fate == "Y", 0, 1)) %>%
  mutate(
    # wrangle: if last_alive has a date and last_checked is NA, then change 
    # last_checked to the date in last_alive
    last_checked = ifelse(!is.na(last_alive) & is.na(last_checked),
                          last_alive,
                          # if both last_alive and last_checked is "NA", then
                          # change last_checked to the first_found date
                          ifelse(is.na(last_alive) & is.na(last_checked),
                                 first_found,
                                 last_checked))) %>%
  mutate(
    # wrangle: if last_alive is NA and the nest hatched and last_checked has a
    # date, then specify last_alive as the date from last_checked
    last_alive = ifelse(is.na(last_alive) & Fate == "0" & !is.na(last_checked),
                        last_checked,
                        # if the last_alive is NA and the nest failed and 
                        # last_checked has a date, then specify last_alive as the
                        # date from first_found
                        ifelse(is.na(last_alive) & Fate == "1" & !is.na(last_checked),
                               first_found,
                               last_alive))) %>%
  # specify date columns as a date string
  mutate(first_found2 = as.Date(as.numeric(first_found), 
                               origin = "1899-12-30"),
         last_alive2 = as.Date(as.numeric(last_alive), 
                              origin = "1899-12-30"),
         last_checked2 = as.Date(as.numeric(last_checked), 
                                origin = "1899-12-30")) %>%
  # rename column headers for RMark nest survival model
  mutate(FirstFound = as.numeric(first_found2 - min(first_found2) + 1),
         LastPresent = as.numeric(last_alive2 - min(first_found2) + 1),
         LastChecked =  as.numeric(last_checked2 - min(first_found2) + 1)) %>% 
  # remove all nests that have unknown fate
  filter(!is.na(Fate))

#### prepare nest surivial data for RMark ----
# define the number of occasions
occ <- max(nest_data$LastChecked)

filter(nest_data, LastPresent > occ | LastPresent < 1 | is.na(LastPresent))

# create processed RMARK data format as NestSurvival with Year as group
nest_data.processed <- RMark::process.data(nest_data, 
                                           model = "Nest",
                                           nocc = occ)

# create the design data
nest_fate.ddl <- RMark::make.design.data(nest_data.processed)

# add a new variable to the design data that is the quadratic transformation of
# time
time <- c(0:(occ-1))
Cubic <- time^3
Quadratic <- time^2
quad_time <- data.frame(time, Quadratic, Cubic)
quad_time$time <- c(1:occ)
nest_fate.ddl$S <- 
  RMark::merge_design.covariates(nest_fate.ddl$S, quad_time, 
                                 bygroup = FALSE, bytime = TRUE)

#### specify models to test ----
# Create model list as function
plover_nest_survival <- function()
{
  
  # Specify models to test
  # constant daily survival rate (DSR)
  S.dot <- 
    list(formula = ~1)
  
  # Linear trend in DSR
  S.Time <-
    list(formula = ~Time)
  
  # Quadratic trend in DSR
  S.Quadratic_Time <- 
    list(formula = ~Time + Quadratic)
  
  # Cubic trend in DSR
  S.Cubic_Time <- 
    list(formula = ~Time + Quadratic + Cubic)
  
  # specify to run as a nest survival model in program MARK
  cml <- RMark::create.model.list("Nest")
  
  # run model list in MARK. Supress generation of MARK files.
  model.list <- RMark::mark.wrapper(cml,
                                    data = nest_data.processed, 
                                    ddl = nest_fate.ddl,
                                    threads = 4, 
                                    brief = TRUE, 
                                    delete = TRUE) 
  
  # store completed model list
  return(model.list)
}

#### run model selection in RMark ----
# run the model list
plover_nest_survival_run <- plover_nest_survival()

#### extract and wrangle modeling results ----
# examine AIC table
plover_nest_survival_run

# Extract estimates of survival from Cubic model (performs as well as constant model)
plover_nest_survival_reals <- 
  plover_nest_survival_run[[1]]$results$real

# wrangle dataframe to tidy up model predictions in prep for plotting
Groups <- data.frame(
  str_split_fixed(rownames(plover_nest_survival_reals), " ", n = 4))
plover_nest_survival_reals <- cbind(Groups, plover_nest_survival_reals)
plover_nest_survival_reals$day_of_season <- 
  as.numeric(unlist(substr(plover_nest_survival_reals$X4, 2, 4)))
plover_nest_survival_reals <- 
  plover_nest_survival_reals[1:nrow(plover_nest_survival_reals) - 1,]

#### plotting ----
# make a dataframe of dates from start to end of season for plot
dates_for_plot <- 
  data.frame(date = as.Date(min(as.numeric(nest_data$first_found)):
                              max(as.numeric(nest_data$last_checked)), 
                            origin = "1899-12-30"),
             day_of_season = c(1:160))

plover_nest_survival_reals <- 
  left_join(plover_nest_survival_reals, dates_for_plot, by = "day_of_season")

# plot the seasonal variation in daily nest survival
hooded_plover_nest_survival_season_20_21_plot <-
  ggplot(data = plover_nest_survival_reals) +
  geom_ribbon(aes(x = date, ymin = lcl, ymax = ucl),
              fill = brewer.pal(8, "Set1")[c(2)], alpha = 0.3) +
  geom_line(aes(x = date, y = estimate), 
            color = brewer.pal(8, "Set1")[c(2)],
            size = 1) +
  scale_x_date(date_labels = "%B", 
               expand = c(0.01, 0.01), 
               date_breaks = "1 months") +
  ylab("daily nest survival ± 95% CI") +
  scale_y_continuous(limits = c(0.6, 1)) +
  luke_theme +
  theme(legend.position = "none",
        panel.grid.major = element_line(colour = "grey70", 
                                        size = 0.15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1))

# plot the seasonal variation in daily nest discovery
hooded_plover_nest_discovery_season_20_21_plot <- 
  ggplot(nest_data, aes(FirstFound)) +
  geom_histogram(bins = 22,
                 fill = brewer.pal(8, "Set1")[c(2)], alpha = 0.5) +
  ylab("nests found\nweekly") +
  scale_y_continuous(breaks = c(2, 4, 6)) +
  luke_theme +
  theme(legend.position = "none",
        panel.grid.major = element_line(colour = "grey70", 
                                        size = 0.15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# merge plots together
hooded_plover_nest_plot_20_21 <- 
  hooded_plover_nest_discovery_season_20_21_plot + 
  hooded_plover_nest_survival_season_20_21_plot +
  plot_layout(widths = c(5), 
              heights = unit(c(0.75, 3), c('in', 'in'))) +
  plot_annotation(tag_levels = 'A', title = "Hooded Plovers", 
                  subtitle = "2020-2021 breeding season")

# export plots
ggsave(plot = hooded_plover_nest_plot_20_21,
       filename = "tabs_figs/hooded_plover_nest_plot_20_21.jpg",
       width = 4,
       height = 6, units = "in",
       dpi = 300)