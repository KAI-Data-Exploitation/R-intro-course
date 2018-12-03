library(tidyverse)


# Fill in the blanks - Filter ---------------------------------------------

fsm_ex <- read_csv("data/free_school_meals_identifier.csv")

YHS <- filter(fsm_ex,  
              school_name ==  "York High School")


# Mutate ------------------------------------------------------------------

fsm_ex <- read_csv("data/free_school_meals_identifier.csv")

mean_ex <- mutate(fsm_ex,  
                  diff_mean_pupils =  round(total_pupils - mean(total_pupils),0))


# Summarise ---------------------------------------------------------------

fsm_ex <- read_csv("data/free_school_meals_identifier.csv")

school_sum <- group_by(fsm_ex,  
                       school_phase, year )

school_sum2 <- summarise(school_sum ,
                         number =  n(), 
                         sum_pupils = sum(total_pupils),
                         mean_pupils = mean(total_pupils)
)


# Pipes -------------------------------------------------------------------

fsm_ex <- read_csv("data/free_school_meals_identifier.csv")

school_year_summary <- fsm_ex  %>%
  group_by(school_phase , year) %>%
  summarise( number =  n(), 
           sum_pupils = sum(total_pupils),
           mean_pupils = mean(total_pupils)
  )  %>%
ungroup()


# Bar chart ---------------------------------------------------------------

fsm_ex <- read_csv("data/free_school_meals_identifier.csv")

school_year_summary <- 
  fsm_ex   %>% 
  group_by(school_phase  , year) %>%
  summarise( number =  n(), 
             sum_pupils = sum(total_pupils),
             mean_pupils = mean(total_pupils)
  )  %>%
  ungroup()


school_year_summary %>% 
  ggplot(aes(x = year , 
             y = school_phase, 
             group = school_phase, 
             fill = school_phase)) +
geom_bar(stat="identity", position="dodge") +   
  labs(x = "Year",
       y = "Mean number of pupils")



# Scatter graph -----------------------------------------------------------

combined_schools_14_ex <- readRDS("data/combined_schools_14.rds") %>%
  mutate(fsm_perc = fsm_perc*100) # change from proportion to perc


ggplot(data = combined_schools_14_ex, 
       aes(x = absence_perc, 
           y = fsm_perc, 
           col = spend_per_pupil ,
           size = total_pupils
       )) + 
  geom_point() +
  theme(legend.position = "bottom") +
  labs(x = "Absence percentage (%)",
       y = "Free school meal takeup(%)")




# Line graph --------------------------------------------------------------

# Data you might need to rerun
fsm_ex <- read_csv("data/free_school_meals_identifier.csv")

school_year_summary <- 
  fsm_ex   %>% 
  group_by(school_phase  , year) %>%
  summarise( number =  n(), 
             sum_pupils = sum(total_pupils),
             mean_pupils = mean(total_pupils)
  )  %>%
  ungroup()


ggplot(data = school_year_summary, 
       aes(x = year, 
           y = mean_pupils, 
           group = school_phase, 
           colour = school_phase)) +
geom_line() + 
  labs(title = "Average pupil numbers between 2014 and 2017",
       x = "School type",
       y = "Count")


# Advanced - Facet Wrap ---------------------------------------------------

fsm_ex <- read_csv("data/free_school_meals_identifier.csv")
data_add_ex <- read_csv("data/york_additional_clean.csv")

combined_schools <- fsm_ex %>%
  mutate(fsm_taken_clean = case_when(
    fsm_taken == 9999  ~ 0,
    TRUE ~ as.numeric(fsm_taken)
  ),
  fsm_perc = fsm_taken_clean/total_pupils) %>%
  filter( school_phase != "Nursery") %>%
  left_join(data_add_ex, by = c("laestab", "year")) %>%
  mutate(spend_band = case_when(spend_per_pupil > 0 & spend_per_pupil < 3667 ~ "1 - Low",
                                spend_per_pupil >= 3667 & spend_per_pupil < 4953 ~ "2 - Medium",
                                spend_per_pupil >= 4953 & spend_per_pupil < 7000 ~ "3 - High",
                                spend_per_pupil >= 7000 ~ "4 - Very-high",
                                          TRUE ~ "Blank"),
         absence_perc = absence_perc/100) 


combined_schools %>%
  ggplot(aes(x = fsm_perc, 
             y = absence_perc, 
             col = school_phase )) + 
  geom_point(size = 2) +
  labs(x = "Percentage of pupils on free school meals",
       y = "Absence rate",
       colour = "School phase") +
facet_wrap(~spend_band, nrow = 2) 
