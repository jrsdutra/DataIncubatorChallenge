setwd('~/Downloads')
library(tidyverse)
library(lubridate)
Incid <- read_csv('Incidents_Responded_to_by_Fire_Companies.csv') %>%
  mutate(ZIP_CODE = as.character(ZIP_CODE)) %>%
  mutate_at(vars(ends_with('TIME')), mdy_hms)
  
sprintf("%.10f",)
Incid %>%
  group_by(INCIDENT_TYPE_DESC) %>%
  summarise(Frequency_type = n()) %>%
  arrange(desc(Frequency_type)) %>%
  mutate(Proportion = sprintf("%.10f",Frequency_type/ sum(Frequency_type)))

vec <- Incid %>%
  filter(INCIDENT_TYPE_DESC %in% c('111 - Building fire','651 - Smoke scare, odor of smoke')) %>%
  group_by(INCIDENT_TYPE_DESC) %>%
  summarise(Avg_units = mean(UNITS_ONSCENE, na.rm = TRUE)) %>%
  select(Avg_units) %>% mutate(Avg_units = as.numeric(Avg_units)) %>%
  as.matrix()
sprintf("%.10f",vec[1,]/ vec[2,])

vec <- Incid %>%
  filter(INCIDENT_TYPE_DESC == '710 - Malicious, mischievous false call, other' &
           BOROUGH_DESC %in% c('1 - Manhattan','3 - Staten Island')) %>%
  group_by(BOROUGH_DESC) %>%
  summarise(N_calls = n())
sprintf("%.10f",vec[1,2]/vec[2,2])

  
sprintf("%.10f",Incid %>%
  filter(INCIDENT_TYPE_DESC == '111 - Building fire')%>%
  mutate(Time_to_help = time_length(ARRIVAL_DATE_TIME - INCIDENT_DATE_TIME, "minute"))%>%
  summarise(Q3 = quantile(Time_to_help, probs=0.75, na.rm = TRUE)) %>%
  as.numeric())

N_incidents <- Incid %>%
  mutate(Time_Incid_rounddown = hour(floor_date(INCIDENT_DATE_TIME, "hour"))) %>%
  select(Time_Incid_rounddown, INCIDENT_TYPE_DESC) %>%
  group_by(Time_Incid_rounddown)%>%
  summarise(N_incidents = n())

Incid %>%
  mutate(Time_Incid_rounddown = hour(floor_date(INCIDENT_DATE_TIME, "hour"))) %>%
  group_by(INCIDENT_TYPE_DESC, Time_Incid_rounddown)%>%
  summarise(N_incidents_type = n()) %>%
  filter(INCIDENT_TYPE_DESC == '113 - Cooking fire, confined to container')%>%
  full_join(N_incidents, by = "Time_Incid_rounddown") %>%
  mutate(Freq_incident = sprintf("%.10f",N_incidents_type/N_incidents)) %>%
  arrange(desc(Freq_incident))
