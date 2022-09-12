# Setting up the environment

library(tidyverse)  # Data import and wrangling
library(ggplot2)    # For data Visualization
library(dplyr)
library(tidyr)

library(scales)   # For transforming numbers in percentage

getwd()   # Displays the working directory (so we know the location of our files)


##  Importing the datasets

daily_activity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

daily_calories <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

daily_intensities <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")

daily_steps <- read_csv("Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")

daily_sleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

weight_info <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")


## Preview datasets for overview and inspection
View(daily_activity)
View(daily_calories)
View(daily_intensities)
View(daily_sleep)
View(daily_steps)
View(weight_info)

## Some Basic Cleaning and Formatting

rm(daily_calories, daily_intensities, daily_steps) #(removing tables)


# Cleaning the variables
daily_activity <- daily_activity %>% 
  rename(Date = ActivityDate) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

daily_sleep <- daily_sleep %>% 
  rename(Date = SleepDay) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

weight_info <- weight_info %>% 
  select(-LogId) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  mutate(IsManualReport = as.factor(IsManualReport))

## Merging the Datasets
final_data <- merge(merge(daily_activity, daily_sleep, by=c('Id','Date'), all = TRUE), weight_info, by = c('Id','Date'), all = TRUE)

## Viewing the Merged dataframe (final_data)
View(final_data)

## Removing extra/irrelevant variables
final_data <- final_data %>% 
  select(-c(TrackerDistance, LoggedActivitiesDistance, TotalSleepRecords, WeightPounds, Fat, BMI, IsManualReport))

## Viewing the Merged dataframe (final_data) again after removing unwanted variables
View(final_data)

## Checking the variables & data types
str(final_data)

summary(final_data)

## Now with data merged, we can check for Users daily activities in a simple plot
final_data %>% 
  mutate(weekdays = weekdays(Date)) %>% 
  select(weekdays, TotalSteps) %>% 
  mutate(weekdays = factor(weekdays, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>% 
  drop_na() %>% 
  ggplot(aes(weekdays, TotalSteps, fill = weekdays)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set2") +
  theme(legend.position="none") +
  labs(title = "Users' activity by day",x = "Day of the week",y = "Steps",
    caption = 'Data Source: FitBit Fitness Tracker Data')


## Check for calories calories burned by steps (i.e Calories vs Total Steps)
final_data %>% 
  group_by(TotalSteps, Calories) %>% 
  ggplot(aes(x = TotalSteps, y = Calories, color = Calories)) +
  geom_point() +
  geom_smooth() + 
  theme(legend.position = c(.8, .3),
        legend.spacing.y = unit(1, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  labs(title = 'Calories burned by total steps taken',y = 'Calories',
       x = 'Total Steps',caption = 'Data Source: FitBit Fitness Tracker Data')

## The more steps taken in a day, the more calories burned

## We can further check for this correlation by using the Pearson Correlation Coefficient
cor.test(final_data$TotalSteps, final_data$Calories, method = 'pearson', conf.level = 0.95)

##  Check for intensity of excercise activity
final_data %>% 
  select(VeryActiveDistance, 
         ModeratelyActiveDistance, 
         LightActiveDistance) %>% 
  summarise(across(everything(), list(sum))) %>% 
  gather(activities, value) %>% 
  mutate(ratio = value / sum(value),
         label = percent(ratio %>% round(4))) %>% 
  mutate(activities = factor(activities,labels = c('Light Activity','Moderate Activity', 'Heavy Activity'))) %>% 
  ggplot(aes(x = (activities),y = value,label = label,fill = activities)) +
  geom_bar(stat='identity') +
  geom_label(aes(label = label),fill = "beige", colour = "black",vjust = 0.5) +
  scale_fill_brewer(palette="Accent") +
  theme(legend.position="none") +
  labs(title = "Intensity of exercise activity",x = "Activity level",
    y = "Distance", caption = 'Data Source: FitBit Fitness Tracker Data')

## From the analysis above, the most common level of activity during exercise is light.


## Sleep Distribution
  final_data %>% 
  select(TotalMinutesAsleep) %>% 
  drop_na() %>% 
  mutate(sleep_quality = ifelse(TotalMinutesAsleep <= 420, 'Less than 7h',
                                ifelse(TotalMinutesAsleep <= 540, '7h to 9h', 
                                       'More than 9h'))) %>%
  mutate(sleep_quality = factor(sleep_quality, 
                                levels = c('Less than 7h','7h to 9h',
                                           'More than 9h'))) %>% 
  ggplot(aes(x = TotalMinutesAsleep, fill = sleep_quality)) +
  geom_histogram(position = 'dodge', bins = 30) +
  scale_fill_manual(values=c("tan1", "#66CC99", "lightcoral")) +
  theme(legend.position = c(.80, .80),legend.title = element_blank(),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),legend.box.background = element_rect(colour = "black")) +
    labs(title = "Sleep distribution",x = "Time slept (minutes)",y = "Count",
    caption = 'Data Source: FitBit Fitness Tracker Data')


## Sleep Vs Distance covered
  final_data %>% 
    select(Id, TotalDistance, TotalMinutesAsleep) %>% 
    group_by(Id) %>% 
    summarise_all(list(~mean(., na.rm=TRUE))) %>% 
    drop_na() %>% 
    mutate(Id = factor(Id)) %>% 
    ggplot() +
    geom_bar(aes(x = Id, y = TotalDistance), stat = "identity", fill = 'lightblue', alpha = 0.7) +
    geom_point(aes(x = Id, y = TotalMinutesAsleep/60), color = 'gold4') +
    geom_segment(aes(x = Id, xend = Id, y = 0, yend = TotalMinutesAsleep/60), color = 'gold4' ,group = 1) +
    scale_y_continuous(limits=c(0, 12), name = "Total Distance", 
                       sec.axis = sec_axis(~.*60, name = "Sleep in minutes")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y.right = element_text(color = "gold4"),axis.ticks.y.right = element_line(color = "gold4"),
          axis.text.y.right = element_text(color = "gold4")) +
    labs(
      title = "Average distance vs average sleep by user",x = "Users",
      caption = 'Data Source: FitBit Fitness Tracker Data')


## further confirmation by breaking sleeping hours by steps
  final_data %>% 
    select(TotalMinutesAsleep, TotalSteps) %>% 
    mutate(sleep_quality = ifelse(TotalMinutesAsleep <= 420, 'Less than 7h',
                                  ifelse(TotalMinutesAsleep <= 540, '7h to 9h', 
                                         'More than 9h'))) %>% 
    mutate(active_level = ifelse(TotalSteps >= 15000,'More than 15,000 steps',
                                 ifelse(TotalSteps >= 10000,'10,000 to 14,999 steps',
                                        ifelse(TotalSteps >= 5000, '5,000 to 9,999 steps',
                                               'Less than 4,999 steps')))) %>% 
    select(-c(TotalMinutesAsleep, TotalSteps)) %>% 
    drop_na() %>% 
    group_by(sleep_quality, active_level) %>% 
    summarise(counts = n()) %>% 
    mutate(active_level = factor(active_level, 
                                 levels = c('Less than 4,999 steps',
                                            '5,000 to 9,999 steps',
                                            '10,000 to 14,999 steps',
                                            'More than 15,000 steps'))) %>% 
    mutate(sleep_quality = factor(sleep_quality, 
                                  levels = c('Less than 7h','7h to 9h',
                                             'More than 9h'))) %>% 
    ggplot(aes(x = sleep_quality, 
               y = counts, 
               fill = sleep_quality)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("tan1", "#66CC99", "lightcoral")) +
    facet_wrap(~active_level, nrow = 1) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.text = element_text(colour = 'black', size = 8)) +
    theme(strip.background = element_rect(fill = "beige", color = 'black'))+
    labs(
      title = "Sleep quality by steps",
      x = "Sleep quality",
      y = "Count",
      caption = 'Data Source: FitBit Fitness Tracker Data')


## Weight Vs Distance covered
  final_data %>% 
    select(Id, WeightKg, TotalDistance) %>% 
    group_by(Id) %>% 
    summarise_all(list(~mean(., na.rm=TRUE))) %>% 
    drop_na() %>% 
    mutate(Id = factor(Id)) %>% 
    ggplot(aes(WeightKg, TotalDistance, fill = Id)) +
    geom_point(aes(color = Id, size = WeightKg), alpha = 0.5) +
    scale_size(range = c(5, 20)) +
    theme(legend.position = "none") +
    labs(
      title = "Weight (kg) vs distance covered",
      x = "Kilograms",
      y = "Total Distance",
      caption = 'Data Source: FitBit Fitness Tracker Data')
  





