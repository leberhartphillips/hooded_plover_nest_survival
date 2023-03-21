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
# function to import Lucinda's data from excel and format it for RMark

lucinda_nest_import <-
  function(year_1, year_2, file_name) {
    read_excel(paste0("data/", file_name), 
               sheet = paste0("MP ", year_1, "_", str_sub(year_2, 3, 4), " Nest summary"), 
               col_types = "text") %>% 
      
      # consolidate columns
      select(Season, `Nest ID`, `Date found`, `Date last seen alive (nest)`, 
             `Date last checked (failed)`, `Hatch?`, `Nest habitat`, `Nest managed?`) %>% 
      
      # simplify column names
      rename(first_found = `Date found`,
             last_alive = `Date last seen alive (nest)`,
             last_checked = `Date last checked (failed)`, 
             Fate = `Hatch?`,
             season = Season,
             nest_ID = `Nest ID`,
             nest_habitat = `Nest habitat`,
             management_status = `Nest managed?`) %>% 
      
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
      # julian dates
      mutate(FirstFound = as.numeric(format(first_found2 + 180, "%j")),
             LastPresent = as.numeric(format(last_alive2 + 180, "%j")),
             LastChecked = as.numeric(format(last_checked2 + 180, "%j"))) %>% 
      # rename column headers for RMark nest survival model
      # mutate(FirstFound = as.numeric(FirstFound_J - min(FirstFound_J, na.rm = TRUE) + 1),
      #        LastPresent = as.numeric(LastPresent_J - min(FirstFound_J, na.rm = TRUE) + 1),
      #        LastChecked =  as.numeric(LastChecked_J - min(FirstFound_J, na.rm = TRUE) + 1)) %>%
      # remove all nests that have unknown fate
      filter(!is.na(Fate))
  }

# import the nest history file and clean up NAs
nest_data <-
  bind_rows(
    lucinda_nest_import(year_1 = "2020", year_2 = "2021", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2019", year_2 = "2020", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2018", year_2 = "2019", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2017", year_2 = "2018", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2016", year_2 = "2017", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2015", year_2 = "2016", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2014", year_2 = "2015", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2013", year_2 = "2014", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx"),
    lucinda_nest_import(year_1 = "2012", year_2 = "2013", 
                        file_name = "MP nesting summary for Honours for all seasons NEW.xlsx")
    ) %>% 
  group_by(season) %>% 
  mutate(nocc = max(max(LastChecked, na.rm = TRUE), max(LastPresent, na.rm = TRUE)),
         season = as.factor(season),
         nest_habitat = as.factor(nest_habitat),
         management_status = as.factor(management_status)) %>% 
  filter(!is.na(FirstFound) & !is.na(LastPresent) & !is.na(LastChecked)) %>% 
  filter(management_status %in% c("Y", "N")) %>% 
  filter(nest_habitat %in% c("Beach", "Dune", "Foredune/face"))
  
#### prepare nest surivial data for RMark ----
# define the number of occasions
occ <- max(nest_data$LastChecked, na.rm = TRUE)

unique(nest_data$nest_habitat)
unique(nest_data$management_status)

filter(nest_data, LastPresent > nocc | LastPresent < 1 | is.na(LastPresent))
filter(nest_data, FirstFound > nocc | FirstFound < 1)
filter(nest_data, FirstFound > LastPresent | FirstFound > LastChecked)

# create processed RMARK data format as NestSurvival with Year as group
nest_data.processed <- 
  RMark::process.data(nest_data, 
                      model = "Nest",
                      nocc = occ, groups = c("season", 
                                             "nest_habitat", 
                                             "management_status"))

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
  
  # Linear trend in DSR
  S.season <-
    list(formula = ~season)
  
  # # Linear trend in DSR
  # S.Time_season <-
  #   list(formula = ~Time + season)
  # 
  # # Quadratic trend in DSR
  # S.Quadratic_Time_season <-
  #   list(formula = ~Time + Quadratic + season)
  # 
  # # Cubic trend in DSR
  # S.Cubic_Time_season <-
  #   list(formula = ~Time + Quadratic + Cubic + season)
  
  S.habitat <-
    list(formula = ~nest_habitat)
  
  # Linear trend in DSR
  S.Time_habitat <-
    list(formula = ~Time + nest_habitat)

  # Quadratic trend in DSR
  S.Quadratic_Time_habitat <-
    list(formula = ~Time + Quadratic + nest_habitat)

  # Cubic trend in DSR
  S.Cubic_Time_habitat <-
    list(formula = ~Time + Quadratic + Cubic + nest_habitat)
  
  S.management <-
    list(formula = ~management_status)
  
  # Linear trend in DSR
  S.Time_management <-
    list(formula = ~Time + management_status)

  # Quadratic trend in DSR
  S.Quadratic_Time_management <-
    list(formula = ~Time + Quadratic + management_status)
  
  # Cubic trend in DSR
  S.Cubic_Time_management <-
    list(formula = ~Time + Quadratic + Cubic + management_status)

  # Cubic trend in DSR
  S.Cubic_Time_management_x_habitat <-
    list(formula = ~Time + Quadratic + Cubic + management_status*nest_habitat)
  
  S.management_x_habitat <-
    list(formula = ~management_status*nest_habitat)
  
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
  plover_nest_survival_run[[3]]$results$real

# wrangle dataframe to tidy up model predictions in prep for plotting
Groups <- data.frame(
  str_split_fixed(rownames(plover_nest_survival_reals), " ", n = 4))
plover_nest_survival_reals <- cbind(Groups, plover_nest_survival_reals)
plover_nest_survival_reals$day_of_season <- 
  as.numeric(unlist(substr(plover_nest_survival_reals$X4, 2, 4)))
plover_nest_survival_reals$management_status <- 
  as.factor(str_sub(plover_nest_survival_reals$X2, 
                    nchar(plover_nest_survival_reals$X2), nchar(plover_nest_survival_reals$X2)))
plover_nest_survival_reals$nest_habitat <- 
  gsub(x = plover_nest_survival_reals$X2, pattern = "[^a-zA-Z]", replacement = "") %>% 
  str_sub(., start = 2, end = nchar(.)-1) %>% as.factor()
plover_nest_survival_reals <- 
  plover_nest_survival_reals[1:nrow(plover_nest_survival_reals) - 1,]

#### plotting ----
# make a dataframe of dates from start to end of season for plot
dates_for_plot <- 
  data.frame(date = as.Date(min(as.numeric(nest_data$FirstFound)):
                              max(max(as.numeric(nest_data$LastChecked)), 
                                  max(as.numeric(nest_data$LastPresent))), 
                            origin = "2023-01-01") - 180,
             day_of_season = c(0:(max(max(as.numeric(nest_data$LastChecked)), 
                                     max(as.numeric(nest_data$LastPresent))) - min(as.numeric(nest_data$FirstFound)))))

plover_nest_survival_reals <- 
  left_join(plover_nest_survival_reals, dates_for_plot, by = "day_of_season")

# plot the seasonal variation in daily nest survival
hooded_plover_nest_survival_season_20_21_plot <-
  ggplot(data = plover_nest_survival_reals, aes(group = management_status)) +
  geom_ribbon(aes(x = date, ymin = lcl, ymax = ucl, fill = management_status),
              # fill = brewer.pal(8, "Set1")[c(2, 1)], 
              alpha = 0.3) +
  geom_line(aes(x = date, y = estimate, color = management_status), 
            # color = brewer.pal(8, "Set1")[c(2, 1)],
            size = 1) +
  scale_x_date(date_labels = "%B", 
               expand = c(0.01, 0.01), 
               date_breaks = "1 months") +
  ylab("daily nest survival ± 95% CI") +
  scale_y_continuous(limits = c(0.4, 1)) +
  scale_fill_manual(values = brewer.pal(8, "Set1")[c(2, 1)],
                    labels = c("Unmanaged", "Managed")) +
  scale_color_manual(values = brewer.pal(8, "Set1")[c(2, 1)],
                     labels = c("Unmanaged", "Managed")) +
  luke_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey70", 
                                        size = 0.15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1))

# plot the seasonal variation in daily nest discovery
hooded_plover_nest_discovery_season_20_21_plot <- 
  ggplot(nest_data, aes(as.Date(FirstFound, origin = "2023-01-01") - 180,
                        fill = management_status)) +
  geom_histogram(bins = 30,
                 # fill = brewer.pal(8, "Set1")[c(2)], 
                 alpha = 0.5) +
  scale_fill_manual(values = brewer.pal(8, "Set1")[c(2, 1)],
                    labels = c("Unmanaged", "Managed")) +
  ylab("nests found\nweekly") +
  scale_x_date(date_labels = "%B", 
               expand = c(0.01, 0.01), 
               date_breaks = "1 months", limits = c(min(plover_nest_survival_reals$date, na.rm = TRUE), 
                                                    max(plover_nest_survival_reals$date, na.rm = TRUE))) +
  scale_y_continuous(breaks = c(10, 20, 30, 40)) +
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
                  subtitle = "2012-2021 breeding seasons")

hooded_plover_nest_plot_20_21

# export plots
ggsave(plot = hooded_plover_nest_plot_20_21,
       filename = "tabs_figs/hooded_plover_nest_plot_20_21.jpg",
       width = 4,
       height = 6, units = "in",
       dpi = 300)