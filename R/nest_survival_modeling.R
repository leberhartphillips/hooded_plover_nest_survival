# R script to wrangle, model, and plot Hooded Plover nest survival at 
# Mornington Peninsula (2006-2021), Bellarine / Surf Coast (2006-2021), and Fleurieu Peninsula (2019-2021)

# code: Luke Eberhart-Hertel (luke.eberhart@bi.mpg.de)
# data: Lucinda Plowman, Grainne Maguire, and Mike Weston

# 8 November, 2023

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
    axis.ticks = element_line(size = 0.2, colour = "grey40"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

#### import data ----
# function to import Lucinda's data from excel and format it for RMark
lucinda_nest_import <-
  function(year_1, year_2, file_name, site, extra_text = NULL) {
    if(is.null(extra_text)){
      file <- 
        read_excel(paste0("data/", file_name), 
                   sheet = paste0(site, " ", year_1, "_", str_sub(year_2, 3, 4)), 
                   col_types = "text")
    }
    else{
    file <- 
      read_excel(paste0("data/", file_name), 
                 sheet = paste0(site, " ", year_1, "_", str_sub(year_2, 3, 4), extra_text), 
                 col_types = "text")
    }
    file %>% 
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
      # remove all nests that have unknown fate
      filter(!is.na(Fate))
  }

# import the nest history file and clean up NAs
##### Mornington Peninsula ----
nest_data_MP <-
  bind_rows(
    lucinda_nest_import(year_1 = "2020", year_2 = "2021", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2019", year_2 = "2020", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2018", year_2 = "2019", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2017", year_2 = "2018", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2016", year_2 = "2017", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2015", year_2 = "2016", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2014", year_2 = "2015", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2013", year_2 = "2014", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2012", year_2 = "2013", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2011", year_2 = "2012", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2010", year_2 = "2011", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2009", year_2 = "2010", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2008", year_2 = "2009", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2007", year_2 = "2008", 
                        file_name = "MP nesting summary 2020_21 to 2006_07 FOR LUKE.xlsx", site = "MP", extra_text = " Nest summary"),
    ) %>% 
  group_by(season) %>% 
  mutate(nocc = max(max(LastChecked, na.rm = TRUE), max(LastPresent, na.rm = TRUE)),
         season = as.factor(season),
         nest_habitat = as.factor(nest_habitat),
         management_status = as.factor(management_status)) %>% 
  filter(!is.na(FirstFound) & !is.na(LastPresent) & !is.na(LastChecked)) %>% 
  filter(management_status %in% c("Y", "N")) %>% 
  filter(nest_habitat %in% c("Beach", "Dune", "Foredune/face"))

##### Bellarine / Surf Coast ----
nest_data_BSC <-
  bind_rows(
    lucinda_nest_import(year_1 = "2020", year_2 = "2021", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2019", year_2 = "2020", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2018", year_2 = "2019", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2017", year_2 = "2018", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2016", year_2 = "2017", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2015", year_2 = "2016", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2014", year_2 = "2015", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2013", year_2 = "2014", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2012", year_2 = "2013", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2011", year_2 = "2012", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2010", year_2 = "2011", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2009", year_2 = "2010", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2008", year_2 = "2009", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2007", year_2 = "2008", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
    lucinda_nest_import(year_1 = "2006", year_2 = "2007", 
                        file_name = "Bellarine_Surf Coast Nesting summary FOR LUKE.xlsx", site = "BellSurfCoast"),
  ) %>% 
  group_by(season) %>% 
  mutate(nocc = max(max(LastChecked, na.rm = TRUE), max(LastPresent, na.rm = TRUE)),
         season = as.factor(season),
         nest_habitat = as.factor(nest_habitat),
         management_status = as.factor(management_status)) %>% 
  filter(!is.na(FirstFound) & !is.na(LastPresent) & !is.na(LastChecked)) %>% 
  filter(management_status %in% c("Y", "N")) %>% 
  filter(nest_habitat %in% c("Beach", "Dune", "Foredune/face"))

##### Fleurieu Peninsula ----
nest_data_FP <-
  bind_rows(
    lucinda_nest_import(year_1 = "2020", year_2 = "2021", 
                        file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2019", year_2 = "2020", 
                        file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    lucinda_nest_import(year_1 = "2018", year_2 = "2019", 
                        file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2017", year_2 = "2018",
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2016", year_2 = "2017",
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2015", year_2 = "2016", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2014", year_2 = "2015", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2013", year_2 = "2014", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2012", year_2 = "2013", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2011", year_2 = "2012", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2010", year_2 = "2011", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2009", year_2 = "2010", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2008", year_2 = "2009", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2007", year_2 = "2008", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
    # lucinda_nest_import(year_1 = "2006", year_2 = "2007", 
    #                     file_name = "FP Nesting summary FOR LUKE.xlsx", site = "FP", extra_text = " Nest summary"),
  ) %>% 
  group_by(season) %>% 
  mutate(nocc = max(max(LastChecked, na.rm = TRUE), max(LastPresent, na.rm = TRUE)),
         season = as.factor(season),
         nest_habitat = as.factor(nest_habitat),
         management_status = as.factor(management_status)) %>% 
  filter(!is.na(FirstFound) & !is.na(LastPresent) & !is.na(LastChecked)) %>% 
  filter(management_status %in% c("Y", "N")) %>% 
  filter(nest_habitat %in% c("Beach", "Dune", "Foredune/face"))

#### prepare nest survival data for RMark ----
##### define the number of occasions ----
occ_MP <- max(nest_data_MP$LastChecked, na.rm = TRUE)
occ_BSC <- max(nest_data_BSC$LastChecked, na.rm = TRUE)
occ_FP <- max(nest_data_FP$LastChecked, na.rm = TRUE)

##### run a few data checks ----
###### Mornington Peninsula ----
# check the nest habitat values (only "Beach", "Foredune/face", and "Dune")
unique(nest_data_MP$nest_habitat)
# check the management values (only "Y" or "N")
unique(nest_data_MP$management_status)
# check if there are any data in which the last alive date is a) beyond the nocc, b) less than 1, or c) NA 
filter(nest_data_MP, LastPresent > nocc | LastPresent < 1 | is.na(LastPresent)) # should be nothing if correct
# check if there are any data in which the found date is a) beyond the nocc, or b) less than 1
filter(nest_data_MP, FirstFound > nocc | FirstFound < 1) # should be nothing if correct
# check if there are any data in which the found date is a) after the last alive or b) after the last checked date 
filter(nest_data_MP, FirstFound > LastPresent | FirstFound > LastChecked) # should be nothing if correct

###### Bellarine / Surf Coast ----
# check the nest habitat values (only "Beach", "Foredune/face", and "Dune")
unique(nest_data_BSC$nest_habitat)
# check the management values (only "Y" or "N")
unique(nest_data_BSC$management_status)
# check if there are any data in which the last alive date is a) beyond the nocc, b) less than 1, or c) NA 
filter(nest_data_BSC, LastPresent > nocc | LastPresent < 1 | is.na(LastPresent)) # should be nothing if correct
# check if there are any data in which the found date is a) beyond the nocc, or b) less than 1
filter(nest_data_BSC, FirstFound > nocc | FirstFound < 1) # should be nothing if correct
# check if there are any data in which the found date is a) after the last alive or b) after the last checked date 
filter(nest_data_BSC, FirstFound > LastPresent | FirstFound > LastChecked) # should be nothing if correct

###### Fleurieu Peninsula ----
# check the nest habitat values (only "Beach", "Foredune/face", and "Dune")
unique(nest_data_FP$nest_habitat)
# check the management values (only "Y" or "N")
unique(nest_data_FP$management_status)
# check if there are any data in which the last alive date is a) beyond the nocc, b) less than 1, or c) NA 
filter(nest_data_FP, LastPresent > nocc | LastPresent < 1 | is.na(LastPresent)) # should be nothing if correct
# check if there are any data in which the found date is a) beyond the nocc, or b) less than 1
filter(nest_data_FP, FirstFound > nocc | FirstFound < 1) # should be nothing if correct
# check if there are any data in which the found date is a) after the last alive or b) after the last checked date 
filter(nest_data_FP, FirstFound > LastPresent | FirstFound > LastChecked) # should be nothing if correct

#### process data for RMark and make design data ----
RMark_nest_survival_prep <- 
  function(hoodie_nest_data, occ){
    # create processed RMARK data format as NestSurvival with Year as group
    nest_data.processed <- 
      RMark::process.data(hoodie_nest_data, 
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
    
    RMark_data <- list(nest_data.processed = nest_data.processed,
                              nest_fate.ddl = nest_fate.ddl)
    RMark_data
  }

RMark_data_MP <- 
  RMark_nest_survival_prep(hoodie_nest_data = nest_data_MP, occ = occ_MP)
RMark_data_BSC <- 
  RMark_nest_survival_prep(hoodie_nest_data = nest_data_BSC, occ = occ_BSC)
RMark_data_FP <- 
  RMark_nest_survival_prep(hoodie_nest_data = nest_data_FP, occ = occ_FP)

#### specify models to test ----
# Create model list as function
nest_survival_MP <- function()
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
  
  # annual variation in DSR
  S.season <-
    list(formula = ~season)
  
  # Linear trend and annual variation in DSR
  S.Time_season <-
    list(formula = ~Time + season)

  # Quadratic trend and annual variation DSR
  S.Quadratic_Time_season <-
    list(formula = ~Time + Quadratic + season)

  # Cubic trend and annual variation DSR
  S.Cubic_Time_season <-
    list(formula = ~Time + Quadratic + Cubic + season)
  
  # habitat-specific variation in DSR
  S.habitat <-
    list(formula = ~nest_habitat)
  
  # Linear trend habitat-specific variation in DSR
  S.Time_habitat <-
    list(formula = ~Time + nest_habitat)

  # Quadratic trend habitat-specific variation in DSR
  S.Quadratic_Time_habitat <-
    list(formula = ~Time + Quadratic + nest_habitat)

  # Cubic trend habitat-specific variation in DSR
  S.Cubic_Time_habitat <-
    list(formula = ~Time + Quadratic + Cubic + nest_habitat)
  
  # managment-specific variation in DSR
  S.management <-
    list(formula = ~management_status)
  
  # Linear trend managment-specific variation in DSR
  S.Time_management <-
    list(formula = ~Time + management_status)

  # Quadratic trend managment-specific variation in DSR
  S.Quadratic_Time_management <-
    list(formula = ~Time + Quadratic + management_status)
  
  # Cubic trend managment-specific variation in DSR
  S.Cubic_Time_management <-
    list(formula = ~Time + Quadratic + Cubic + management_status)

  # Cubic trend and interaction between habitat and managment on DSR
  S.Cubic_Time_management_x_habitat <-
    list(formula = ~Time + Quadratic + Cubic + management_status*nest_habitat)
  
  # interaction between habitat and managment on DSR
  S.management_x_habitat <-
    list(formula = ~management_status*nest_habitat)
  
  # specify to run as a nest survival model in program MARK
  cml <- RMark::create.model.list("Nest")
  
  # run model list in MARK. Supress generation of MARK files.
  model.list <- RMark::mark.wrapper(cml,
                                    data = RMark_data_MP$nest_data.processed, 
                                    ddl = RMark_data_MP$nest_fate.ddl,
                                    threads = 4, 
                                    brief = TRUE, 
                                    delete = TRUE) 
  
  # store completed model list
  return(model.list)
}

nest_survival_BSC <- function()
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
  
  # annual variation in DSR
  S.season <-
    list(formula = ~season)
  
  # Linear trend and annual variation in DSR
  S.Time_season <-
    list(formula = ~Time + season)
  
  # Quadratic trend and annual variation DSR
  S.Quadratic_Time_season <-
    list(formula = ~Time + Quadratic + season)
  
  # Cubic trend and annual variation DSR
  S.Cubic_Time_season <-
    list(formula = ~Time + Quadratic + Cubic + season)
  
  # habitat-specific variation in DSR
  S.habitat <-
    list(formula = ~nest_habitat)
  
  # Linear trend habitat-specific variation in DSR
  S.Time_habitat <-
    list(formula = ~Time + nest_habitat)
  
  # Quadratic trend habitat-specific variation in DSR
  S.Quadratic_Time_habitat <-
    list(formula = ~Time + Quadratic + nest_habitat)
  
  # Cubic trend habitat-specific variation in DSR
  S.Cubic_Time_habitat <-
    list(formula = ~Time + Quadratic + Cubic + nest_habitat)
  
  # managment-specific variation in DSR
  S.management <-
    list(formula = ~management_status)
  
  # Linear trend managment-specific variation in DSR
  S.Time_management <-
    list(formula = ~Time + management_status)
  
  # Quadratic trend managment-specific variation in DSR
  S.Quadratic_Time_management <-
    list(formula = ~Time + Quadratic + management_status)
  
  # Cubic trend managment-specific variation in DSR
  S.Cubic_Time_management <-
    list(formula = ~Time + Quadratic + Cubic + management_status)
  
  # Cubic trend and interaction between habitat and managment on DSR
  S.Cubic_Time_management_x_habitat <-
    list(formula = ~Time + Quadratic + Cubic + management_status*nest_habitat)
  
  # interaction between habitat and managment on DSR
  S.management_x_habitat <-
    list(formula = ~management_status*nest_habitat)
  
  # specify to run as a nest survival model in program MARK
  cml <- RMark::create.model.list("Nest")
  
  # run model list in MARK. Supress generation of MARK files.
  model.list <- RMark::mark.wrapper(cml,
                                    data = RMark_data_BSC$nest_data.processed, 
                                    ddl = RMark_data_BSC$nest_fate.ddl,
                                    threads = 4, 
                                    brief = TRUE, 
                                    delete = TRUE) 
  
  # store completed model list
  return(model.list)
}

nest_survival_FP <- function()
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
  
  # annual variation in DSR
  S.season <-
    list(formula = ~season)
  
  # Linear trend and annual variation in DSR
  S.Time_season <-
    list(formula = ~Time + season)
  
  # Quadratic trend and annual variation DSR
  S.Quadratic_Time_season <-
    list(formula = ~Time + Quadratic + season)
  
  # Cubic trend and annual variation DSR
  S.Cubic_Time_season <-
    list(formula = ~Time + Quadratic + Cubic + season)
  
  # habitat-specific variation in DSR
  S.habitat <-
    list(formula = ~nest_habitat)
  
  # Linear trend habitat-specific variation in DSR
  S.Time_habitat <-
    list(formula = ~Time + nest_habitat)
  
  # Quadratic trend habitat-specific variation in DSR
  S.Quadratic_Time_habitat <-
    list(formula = ~Time + Quadratic + nest_habitat)
  
  # Cubic trend habitat-specific variation in DSR
  S.Cubic_Time_habitat <-
    list(formula = ~Time + Quadratic + Cubic + nest_habitat)
  
  # managment-specific variation in DSR
  S.management <-
    list(formula = ~management_status)
  
  # Linear trend managment-specific variation in DSR
  S.Time_management <-
    list(formula = ~Time + management_status)
  
  # Quadratic trend managment-specific variation in DSR
  S.Quadratic_Time_management <-
    list(formula = ~Time + Quadratic + management_status)
  
  # Cubic trend managment-specific variation in DSR
  S.Cubic_Time_management <-
    list(formula = ~Time + Quadratic + Cubic + management_status)
  
  # Cubic trend and interaction between habitat and managment on DSR
  S.Cubic_Time_management_x_habitat <-
    list(formula = ~Time + Quadratic + Cubic + management_status*nest_habitat)
  
  # interaction between habitat and managment on DSR
  S.management_x_habitat <-
    list(formula = ~management_status*nest_habitat)
  
  # specify to run as a nest survival model in program MARK
  cml <- RMark::create.model.list("Nest")
  
  # run model list in MARK. Supress generation of MARK files.
  model.list <- RMark::mark.wrapper(cml,
                                    data = RMark_data_FP$nest_data.processed, 
                                    ddl = RMark_data_FP$nest_fate.ddl,
                                    threads = 4, 
                                    brief = TRUE, 
                                    delete = TRUE) 
  
  # store completed model list
  return(model.list)
}

#### run model selection in RMark ----
# run the model list (uncomment the following 3 lines to run...takes ~15 mins each!)
# nest_survival_run_MP <- nest_survival_MP()
# nest_survival_run_BSC <- nest_survival_BSC()
# nest_survival_run_FP <- nest_survival_FP()

# save model output to save time with future sessions (uncomment if you ran the previous 3 lines)
# saveRDS(nest_survival_run_MP, file = "output/nest_survival_run_MP.rds")
# saveRDS(nest_survival_run_BSC, file = "output/nest_survival_run_BSC.rds")
# saveRDS(nest_survival_run_FP, file = "output/nest_survival_run_FP.rds")

# load the saved results into R
nest_survival_run_MP_out <- readRDS(file = "output/nest_survival_run_MP.rds")
nest_survival_run_BSC_out <- readRDS(file = "output/nest_survival_run_BSC.rds")
nest_survival_run_FP_out <- readRDS(file = "output/nest_survival_run_FP.rds")

#### extract and wrangle modeling results ----
# examine AIC table
nest_survival_run_MP_out
nest_survival_run_BSC_out
nest_survival_run_FP_out

# Extract estimates of survival from Cubic model with management 
# (non-linear season variation and management effect)
nest_survival_reals_MP <- 
  nest_survival_run_MP_out[[3]]$results$real

nest_survival_reals_BSC <- 
  nest_survival_run_BSC_out[[3]]$results$real

nest_survival_reals_FP <- 
  nest_survival_run_FP_out[[3]]$results$real

# wrangle dataframe to tidy up model predictions in prep for plotting
RMark_pred_tidy <- 
  function(nest_survival_reals){
    Groups <- data.frame(
      str_split_fixed(rownames(nest_survival_reals), " ", n = 4))
    nest_survival_reals <- cbind(Groups, nest_survival_reals)
    nest_survival_reals$day_of_season <- 
      as.numeric(unlist(substr(nest_survival_reals$X4, 2, 4)))
    nest_survival_reals$management_status <- 
      as.factor(str_sub(nest_survival_reals$X2, 
                        nchar(nest_survival_reals$X2), nchar(nest_survival_reals$X2)))
    nest_survival_reals$nest_habitat <- 
      gsub(x = nest_survival_reals$X2, pattern = "[^a-zA-Z]", replacement = "") %>% 
      str_sub(., start = 2, end = nchar(.)-1) %>% as.factor()
    nest_survival_reals <- 
      nest_survival_reals[1:nrow(nest_survival_reals) - 1,]
    nest_survival_reals
  }

nest_survival_reals_tidy_MP <- RMark_pred_tidy(nest_survival_reals_MP)
nest_survival_reals_tidy_BSC <- RMark_pred_tidy(nest_survival_reals_BSC)
nest_survival_reals_tidy_FP <- RMark_pred_tidy(nest_survival_reals_FP)

#### plotting ----
# make a dataframe of dates from start to end of season for plot
nest_survival_plotR <- 
  function(nest_data, 
           nest_survival_reals_tidy, 
           plot_title = "2006-2021 breeding seasons at Mornington Peninsula"){
    dates_for_plot <- 
      data.frame(date = as.Date(min(as.numeric(nest_data$FirstFound)):
                                  max(max(as.numeric(nest_data$LastChecked)), 
                                      max(as.numeric(nest_data$LastPresent))), 
                                origin = "2023-01-01") - 180,
                 day_of_season = c(0:(max(max(as.numeric(nest_data$LastChecked)), 
                                          max(as.numeric(nest_data$LastPresent))) - 
                                        min(as.numeric(nest_data$FirstFound)))))
    
    nest_survival_reals_tidy_dates <- 
      left_join(nest_survival_reals_tidy, dates_for_plot, by = "day_of_season")
    
    # plot the seasonal variation in daily nest survival
    nest_survival_season_plot <-
      ggplot(data = nest_survival_reals_tidy_dates, aes(group = management_status)) +
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
    nest_discovery_season_plot <- 
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
                   date_breaks = "1 months", limits = c(min(nest_survival_reals_tidy_dates$date, na.rm = TRUE), 
                                                        max(nest_survival_reals_tidy_dates$date, na.rm = TRUE))) +
      scale_y_continuous(breaks = c(10, 20, 30, 40)) +
      luke_theme +
      theme(legend.position = "none",
            panel.grid.major = element_line(colour = "grey70", 
                                            size = 0.15),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
    # merge plots together
    hooded_plover_nest_plot <- 
      nest_discovery_season_plot + 
      nest_survival_season_plot +
      plot_layout(widths = c(5), 
                  heights = unit(c(0.75, 3), c('in', 'in'))) +
      plot_annotation(tag_levels = 'A', title = "Hooded Plover Nest Survival", 
                      subtitle = plot_title)
    
    hooded_plover_nest_plot
  }

nest_survival_plot_MP <- 
  nest_survival_plotR(nest_data = nest_data_MP, 
                      nest_survival_reals_tidy = nest_survival_reals_tidy_MP, 
                      plot_title = "2006-2021 breeding seasons at Mornington Peninsula")

nest_survival_plot_BSC <- 
  nest_survival_plotR(nest_data = nest_data_BSC, 
                      nest_survival_reals_tidy = nest_survival_reals_tidy_BSC, 
                      plot_title = "2006-2021 breeding seasons at Bellarine / Surf Coast")

nest_survival_plot_FP <- 
  nest_survival_plotR(nest_data = nest_data_FP, 
                      nest_survival_reals_tidy = nest_survival_reals_tidy_FP, 
                      plot_title = "2019-2021 breeding seasons at Fleurieu Peninsula")

# export plots
ggsave(plot = nest_survival_plot_MP,
       filename = "tabs_figs/nest_survival_plot_MP.jpg",
       width = 5,
       height = 7, units = "in",
       dpi = 300)

# export plots
ggsave(plot = nest_survival_plot_BSC,
       filename = "tabs_figs/nest_survival_plot_BSC.jpg",
       width = 5,
       height = 7, units = "in",
       dpi = 300)

# export plots
ggsave(plot = nest_survival_plot_FP,
       filename = "tabs_figs/nest_survival_plot_FP.jpg",
       width = 5,
       height = 7, units = "in",
       dpi = 300)