library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(DT)


ACAD <- read_excel("Data/ACAD.n396.annotations&summaries.v03.xlsx", 
                     sheet = "sample_by_spp.n396") %>% 
  dplyr::select(-fileID) %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  tidyr::pivot_longer(cols = 3:68, names_to = "AOU", values_to ="Calls") %>% add_column(Park = "ACAD")



MABI <- read_excel("Data/MABI.n477.annotations&summaries.v02.xlsx", 
                      sheet = "sample_by_spp.n477") %>% 
  dplyr::select(-fileID) %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  tidyr::pivot_longer(cols = 3:80, names_to = "AOU", values_to ="Calls") %>% add_column(Park = "MABI")


long<- bind_rows(ACAD, MABI) %>% mutate(month_day = format(date, "%m-%d")) %>%  # Format dates to "day-month"
        mutate(year= lubridate::year(date))

AOU_list <- unique(long$AOU)

Park_list <- unique(long$Park)
