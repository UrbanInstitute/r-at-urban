#load libraries

library(readxl)
library(tidyverse)
library(janitor)
library(tidycensus)
library(urbnthemes)
library(urbnmapr)

#set urban style for mapping
set_urbn_defaults("map")

#load in function that creates better labels from factors created by cut and cut_number 
source("gen_cut_labels.R")

options(scipen = 999)

#read in data, clean fips and names, create logical variable that meets eviction moratorium standards from 
#https://www.cdc.gov/coronavirus/2019-ncov/covid-eviction-declaration.html
#Counties experiencing substantial transmission levels are experiencing (1) 50.99-99.99 new cases in the county in the past 7 days divided by the population in the county multiplied by 100,000; and (2) 8.00-9.99% positive nucleic acid amplification tests in the past 7 days (number of positive tests in the county during the past 7 days divided by the total number of tests performed in the county during the past 7 days). Christie A, Brooks JT, Hicks LA, et al. Guidance for Implementing COVID-19 Prevention Strategies in the Context of Varying Community Transmission Levels and Vaccination Coverage. MMWR Morb Mortal Wkly Rep 2021;70:1044–1047. DOI: http://dx.doi.org/10.15585/mmwr.mm7030e2. See COVID-19 Integrated County View, Centers for Disease Control and Prevention; https://covid.cdc.gov/covid-data-tracker/#county-view (last updated August 1, 2021).

#Looking at inspect page of this URL,
# May want to autoomate reading in of data by sending GET request to this URL: https://healthdata.gov/api/views/gqxm-d9w9/files/5bc6ffd3-809f-4800-bfd0-3f386acbe8d1?download=true&filename=Community%20Profile%20Report%2020210804.xlsx
# Got the URL from inspecting page of above URL

cdc<- read_excel("Community_Profile_Report_20210803.xlsx", sheet = "Counties", skip = 1) %>% 
  clean_names() %>% 
  mutate(fips_code = str_pad(fips_code, 5, "left", "0")) %>% 
  select(fips_code, population, community_transmission_level_last_7_days) %>% 
  mutate(is_protected = community_transmission_level_last_7_days %in% c("High", "Substantial"))

#get number of renter households and total households from acs5
# AN: Do we know they are using acs5 year households and not acs 1 year estimates to arrive at the cdc numbers?
renter_hhs <- get_acs("county", 
                    # AN: Checked these variables and I think they're right!
                   variables = c(total_households= "B25003_001", 
                                 renter_households = "B25003_003"),
                   year = 2019, 
                   survey = "acs5", 
                   output = "wide") %>% 
   clean_names() %>% 
   select(-ends_with("m"))

#make variable that contains total number of renter households protected by moratorium and summarise by state
final<- cdc %>% 
  left_join(renter_hhs, by = c("fips_code" = "geoid")) %>% 
  mutate(st = str_sub(fips_code, 1, 2), 
         # AN: There are 52 rows where renter_households_e is NA from the ACS 5 year survey. Right not my_num for those 52 counties is also NA. Do we know how those counties should be handled?
         my_num = renter_households_e * is_protected) %>% 
  # AN: I would move the summarize and ungroup into a sepearte object in case you decide to make vizzes/maps with the county level data in the future
  group_by(st) %>% 
  summarise(total = sum(renter_households_e, na.rm = TRUE), 
            protected = sum(my_num, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(not_protected = total - protected,
         # AN: This won't happen in state pop. counts so don't worry for now, but should account for when num or denom is 0 (base / will return NAs)
         perc = protected / total, 
         # AN: Any reason you chose these breaks? Seem a little arbitrary but fine. 
         my_cat = cut(perc, breaks = c(.3, .5, .75, .85, .95, 1)), 
         num_cat = cut(protected, breaks = c(35000, 160000, 400000, 600000, 1000000, 6000000), dig.lab = 50), 
         not_num_cat = cut(not_protected, breaks = c(0, 100, 2500, 14000, 40000, 650000), dig.lab = 50, include.lowest = TRUE))

#create labels for graphs
my_labels <- gen_cut_labels(final$my_cat, type = "percent")
my_num_labels <- gen_cut_labels(final$num_cat)
not_num_labels <- gen_cut_labels(final$not_num_cat)

#get state map from urbnmapr package and join to data
my_states <- get_urbn_map(sf = TRUE) %>% 
  left_join(final, by = c("state_fips" = "st"))

#plot for percent of renter households protected by CDC eviction
plot_perc<- ggplot(my_states) +
  geom_sf(mapping = aes(fill = my_cat), color = "#ffffff", size = .4) + 
  # AN: Little bit of personal prefernces but I would flip the color scale. In my mind dark = higher colors and light = lower colors.
  scale_fill_manual(values = palette_urbn_cyan[c(8, 7, 5, 3, 1)], labels = my_labels) + 
  labs(fill = NULL, title = "Percent of Renter Households Protected by CDC Eviction Morotorium")

ggsave("renter_hh_protected_cdc_eviction_perc.png", plot_perc)
  
#plot for number of renter households protected
plot_num<- ggplot(my_states) +
  geom_sf(mapping = aes(fill = num_cat), color = "#ffffff", size = .4) + 
  scale_fill_manual(values = palette_urbn_cyan[c(8, 7, 5, 3, 1)], labels = my_num_labels) + 
  labs(fill = NULL, title = "Number of Renter Households Protected by CDC Eviction Morotorium")

ggsave("renter_hh_protected_cdc_eviction_num.png", plot_num)

#plot for number of renter households not protected
plot_not_num<- ggplot(my_states) +
  geom_sf(mapping = aes(fill = not_num_cat), color = "#ffffff", size = .4) + 
  scale_fill_manual(values = palette_urbn_cyan[c(1, 3, 5, 7, 8)], labels = not_num_labels) + 
  labs(fill = NULL, title = "Number of Renter Households Not Protected by CDC Eviction Morotorium")

ggsave("renter_hh_protected_cdc_eviction_not_num.png", plot_not_num)

# AN: Not sure exactly what the ask was, but why not number of renter households who were unprotected?



##Ajjit stop here for now, unless you have time
###Pulse Data
#get unique pulse files and sheets that are states/US
pulse_files <- list.files(pattern = "housing")
my_pulse_sheets <- excel_sheets(pulse_files[1])
my_pulse_sheets <- my_pulse_sheets[str_length(my_pulse_sheets)==2]

pulse_dir<- expand_grid(pulse_files, my_pulse_sheets)

#read in pulse data. This function was originally written with more uses in mind, but ported it here just to get certain variables
read_in_pulse <- function(filepath, sheet){
  
  if(str_detect(filepath, "housing1")){
    my_df <-  read_excel(filepath, sheet =sheet) %>% 
      rename(var = 1) %>%
      filter(var == "Total" | 
               startsWith(var, "Less than $")|startsWith(var, "$")) %>% 
      select(var,
             yes = 4, 
             no= 5
      ) %>% 
      
      mutate(across(-var, .fns = as.numeric),
             total = no + yes, 
             value = no,
             geo = sheet, 
             week = str_extract(filepath, "week[0-9][0-9]"), 
             my_type = "Caught up on Rent")
  } else{
    my_df <-  read_excel(filepath, sheet =sheet) %>% 
      rename(var = 1) %>%
      filter(var == "Total" | 
               startsWith(var, "Less than $")|startsWith(var, "$")) %>% 
      select(var,
             very = 3, 
             likely= 4, 
             not_likely= 5, 
             def_no = 6) %>% 
      
      mutate(across(-var, .fns = as.numeric),
             total = rowSums(cbind(very, likely, not_likely, def_no), na.rm = TRUE),
             value = rowSums(cbind(very, likely), na.rm = TRUE), 
             geo = sheet, 
             week = str_extract(filepath, "week[0-9][0-9]"), 
             my_type = "Likelihood to be Evicted")
  }
  
  
  
}

#read in pulse data on each state
my_pulse<- map2_dfr(pulse_dir$pulse_files, pulse_dir$my_pulse_sheets, read_in_pulse) %>% 
  filter(var == "Total") %>% 
  mutate(my_perc = value / total)
#split data into caught up on rent and liklihood to be evicted
my_pulse_c <- filter(my_pulse, my_type == "Caught up on Rent")
my_pulse_e <- filter(my_pulse, my_type == "Likelihood to be Evicted") %>% 
  select(-total)

my_pulse_means<- my_pulse_c %>% 
  select(geo, var, total) %>% 
  left_join( my_pulse_e, .) %>% 
  bind_rows(my_pulse_c) %>% 
  mutate(perc = value/ total)

pulse_recent<- my_pulse_means %>% 
  select(geo, my_type, perc) %>%
  pivot_wider(values_from = perc, names_from = my_type) %>% 
  clean_names() %>% 
  filter(geo != "US")


my_states_2<- my_states %>% 
  select(-perc) %>% 
  left_join(pulse_recent, by= c("state_abbv" = "geo")) %>% 
  mutate(evicted_protected = protected * likelihood_to_be_evicted, 
         not_caught_up_protected = protected * caught_up_on_rent, 
         total_evicted = total * likelihood_to_be_evicted, 
         total_not_caught_up = total * caught_up_on_rent, 
         evic_cat = cut(evicted_protected, breaks = c(300, 7500, 23000, 45000, 90000, 460000), dig.lab = 50), 
         not_cat = cut(not_caught_up_protected, breaks = c(2750, 18000, 50000, 87500, 190000, 800000), dig.lab = 50))

evic_labels <- gen_cut_labels(my_states_2$evic_cat)
not_labels <- gen_cut_labels(my_states_2$not_cat)


plot_num_e<- ggplot(my_states_2) +
  geom_sf(mapping = aes(fill = evic_cat), color = "#ffffff", size = .4) + 
  scale_fill_manual(values = palette_urbn_cyan[c(8, 7, 5, 3, 1)], labels = evic_labels) + 
  labs(fill = NULL, title = "Number of Renter Households Likely to be Evicted and Protected by CDC Eviction Morotorium")

ggsave("renter_hh_protected_cdc_eviction_num_evicted.png", plot_num_e)

plot_num_n<- ggplot(my_states_2) +
  geom_sf(mapping = aes(fill = not_cat), color = "#ffffff", size = .4) + 
  scale_fill_manual(values = palette_urbn_cyan[c(8, 7, 5, 3, 1)], labels = not_labels) + 
  labs(fill = NULL, title = "Number of Renter Households Not Caught up on Rent and Protected by CDC Eviction Morotorium")

ggsave("renter_hh_protected_cdc_eviction_num_not_caught_up.png", plot_num_n)



