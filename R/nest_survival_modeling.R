# install.packages(RMark) 
# install.packages(stringr)
# install.packages(ggplot2)
# install.packages(extrafont)
# install.packages(dplyr)
# install.packages(reshape2)
# install.packages(popbio)
library(tidyverse)
library(readxl)
library(RMark)

library(stringr)
library(ggplot2)
library(extrafont)
library(dplyr)
library(reshape2)
library(popbio)

# import the nest history file
nest_data <-
  read_excel("data/MP  nesting summary for Honours.xlsx", 
             sheet = "MP 2020_2021", col_types = "text") %>% 
  mutate(nest_ID = str_remove_all(paste0(`Pair ID (Bird 1)`, 
                                         `Pair ID (Bird 2)`, 
                                         `Attempt #`), " ")) %>% 
  select(nest_ID, `Date found`, `Date last seen alive (nest)`, 
         `Nest fail date`, `Hatch?`) %>% 
  rename(first_found = `Date found`,
         last_alive = `Date last seen alive (nest)`,
         last_checked = `Nest fail date`, 
         Fate = `Hatch?`) %>% 
  mutate(last_alive = ifelse(str_detect(last_alive, "Unk."), NA, last_alive),
         Fate = ifelse(Fate == "Y", 0, 1)) %>%
  mutate(
    last_checked = ifelse(!is.na(last_alive) & is.na(last_checked),
                          last_alive,
                          ifelse(is.na(last_alive) & is.na(last_checked),
                                 first_found,
                                 last_checked))) %>%
  mutate(
    last_alive = ifelse(is.na(last_alive) & Fate == "0" & !is.na(last_checked),
                        last_checked,
                        ifelse(is.na(last_alive) & Fate == "1" & !is.na(last_checked),
                               first_found,
                               last_alive))) %>%
  mutate(first_found = as.Date(as.numeric(first_found), 
                               origin = "1899-12-30"),
         last_alive = as.Date(as.numeric(last_alive), 
                              origin = "1899-12-30"),
         last_checked = as.Date(as.numeric(last_checked), 
                                origin = "1899-12-30")) %>%
  mutate(FirstFound = as.numeric(first_found - min(first_found) + 1),
         LastPresent = as.numeric(last_alive - min(first_found) + 1),
         LastChecked =  as.numeric(last_checked - min(first_found) + 1)) %>% 
  filter(!is.na(Fate))

nest_data %>% 
  filter(Fate == 0)
  # filter(FirstFound > LastPresent | LastPresent > LastChecked)
  # filter(last_checked == "07/01/21 or 09-01-2021?")
  filter(is.na(last_checked) | is.na(last_alive) | is.na(first_found) | is.na(Fate))
  # filter(nest_ID == "BMRight(Orange)Unbanded1")
  # filter(Fate == 1)

levels(as.factor(nest_data$LastPresent))

# define the number of occasions
occ <- max(nest_data$LastChecked)

filter(nest_data, LastPresent > occ | LastPresent < 1 | is.na(LastPresent))

# create processed RMARK data format as NestSurvival with Year as group
nest_fate.processed <- RMark::process.data(nest_data, 
                                           model = "Nest",
                                           nocc = occ)

# create the design data
nest_fate.ddl <- RMark::make.design.data(nest_data.processed)

# add a new variable to the design data that is the quadratic transformation of
# time
time <- c(0:(occ-1))
Quadratic <- time^2
quad_time <- data.frame(time, Quadratic)
quad_time$time <- c(1:occ)
nest_fate.ddl$S <- 
  RMark::merge_design.covariates(nest_fate.ddl$S, quad_time, 
                                 bygroup = FALSE, bytime = TRUE)
# Create model list as function
plover_nest_survival <- function()
{
  
  # Specify models to test
  # constant daily survival rate (DSR)
  S.dot <- 
    list(formula = ~1)
  
  # DSR varies with Time (i.e., linearly across the season)
  S.Time <-
    list(formula = ~Time)
  
  # DSR varies quadratically with Time
  S.Quadratic_Time <- 
    list(formula = ~Quadratic)
  
  # # DSR varies with standardized time
  # S.Std_time <- 
  #   list(formula = ~std_time)
  
  # # DSR varies with nest age
  # S.Age <- 
  #   list(formula = ~NestAge)
  
  # # DSR varies year and time
  # S.yearTime <- 
  #   list(formula = ~Year + Time)
  # 
  # # DSR varies year and time
  # S.yearxTime <- 
  #   list(formula = ~Year * Time)
  # 
  # # DSR varies with year and nest age
  # S.yearAge <- 
  #   list(formula = ~Year + NestAge)
  # 
  # # DSR varies with year and nest age
  # S.yearxAge <- 
  #   list(formula = ~Year * NestAge)
  # 
  # # DSR varies with time and nest age
  # S.TimeAge <-
  #   list(formula = ~Time + NestAge)
  # 
  # # DSR varies with time and nest age
  # S.TimexAge <-
  #   list(formula = ~Time * NestAge)
  # 
  # # DSR varies with year, habitat, time and nest age
  # S.global <- 
  #   list(formula = ~Year + Time + NestAge)
  # 
  # # DSR varies with year, habitat, time and nest age
  # S.globalx <- 
  #   list(formula = ~Year * Time * NestAge)
  
  # specify to run as a nest survival model in program MARK
  cml <- RMark::create.model.list("Nest")
  
  # run model list in MARK. Supress generation of MARK files.
  model.list <- RMark::mark.wrapper(cml,
                                    data = nest_fate.processed, ddl = nest_fate.ddl,
                                    threads = 4, brief = TRUE, delete = TRUE) 
  
  # store completed model list
  return(model.list)
}

# run the model list
plover_nest_survival_run <- plover_nest_survival()

# examine AIC table
plover_nest_survival_run

# Extract estimates of survival from top model
plover_nest_survival_reals <- 
  plover_nest_survival_run[[3]]$results$real

# wrangle dataframe to get annual estimates (grouped by "Year")
Groups <- data.frame(
  str_split_fixed(rownames(Nest_survival_reals), " ", n = 4))
Nest_survival_reals <- cbind(Groups, Nest_survival_reals)
Nest_survival_reals$Year <- unlist(substr(Nest_survival_reals$X2, 2, 5))
# summarize the seasonal estimates by averaging within Year
Yearly_nest_survival_avgs <- Nest_survival_reals %>%
  group_by(Year) %>%
  summarise_each(funs(mean))
# calculate the apparent nest survival by raising daily nest survival to ^25
Yearly_nest_survival_avgs$Year_apparent <- 
  Yearly_nest_survival_avgs$estimate^25
Yearly_nest_survival_avgs$Year_apparent_lcl <- 
  Yearly_nest_survival_avgs$lcl^25
Yearly_nest_survival_avgs$Year_apparent_ucl <- 
  Yearly_nest_survival_avgs$ucl^25