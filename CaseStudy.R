### Norman Lew
### Fitbit Analysis Project

library(here)
library(skimr)
library(janitor)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(data.table)

# The dailyActivity_merged.csv file is the primary Fitbit file.  It has the daily tracking numbers for steps taken,
# distance travelled, time spent in each activity and calories burned.
dailyActivity_merged <- read.csv("dailyActivity_merged.csv")

str(dailyActivity_merged)

head(dailyActivity_merged)

skim_without_charts(dailyActivity_merged)

glimpse(dailyActivity_merged)

# There are 940 records on file.
nrow(dailyActivity_merged)

# There are no duplicate rows in the dailyActivity_merged dataset.
nrow(distinct(dailyActivity_merged))


# There are 33 different Ids (persons) where their activity data is provided.
n_distinct(dailyActivity_merged$Id)

# Find out how many days worth of data there are for each person
# The maximum number of days of data for a person are 31 (The period of measurement was over 31 consecutive days,
# April to May, in 2016).  
number_of_days_by_id <- dailyActivity_merged %>% count(Id)

# A bar chart showing the distribution of the number of days of data for each person
# The most number of people used a Fitbit for the entire 31 day period.
ggplot(data = number_of_days_by_id) +
  geom_bar(mapping = aes(x = n)) +
  labs(title = "Number of Days of Data per Person",
      x = "Number of Days",
      y = "Number of People")


# There are 77 records with 0 TotalSteps recorded. 
sum(dailyActivity_merged$TotalSteps == 0)

# There are. 79 records with 1440 Sedentary Minutes Recorded (60 minutes X 24 hours = 1440 minutes). 
sum(dailyActivity_merged$SedentaryMinutes == 1440)

### Number of records (days) where records minutes do not sum to a full day (1440 minutes)
# First, Add a column that sums all minutes
dailyActivity_merged <- dailyActivity_merged %>% 
  mutate(sum_of_minutes = rowSums(across(c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes))))

# There are 462 rows where the sum of all minutes is not equal to 1440.
sum(dailyActivity_merged$sum_of_minutes != 1440)

# There are 478 rows where the sum of all minutes is equal to 1440.
sum(dailyActivity_merged$sum_of_minutes == 1440)

### Create a dataframe for only those records where there was a logged activity
### A logged activity is an activity that the person wanted to log, as opposed to all activity that is continuously
### monitored automatically while the fitbit is on
dailyActivity_with_logged_activity <- dailyActivity_merged %>%
  filter(LoggedActivitiesDistance > 0)

# There are 32 rows where an activity was logged
nrow(dailyActivity_with_logged_activity)

# See how many logged activities each user did
num_logged_activity_by_id <- dailyActivity_with_logged_activity %>% count(Id)

# Not many people recorded Logged Activities.  Only two people recorded more than 3.
ggplot(data = num_logged_activity_by_id) +
  geom_bar(mapping = aes(x = n)) +
  labs(title = "Number of Logged Activities per Person",
       x = "Number of Logged Activities",
       y = "Number of People")

# 4 users logged at least one activity
n_distinct(dailyActivity_with_logged_activity$Id)

# ----------------------------- #

# The ActivityDate column is currently a character.  This changes it to a date format
dailyActivity <- dailyActivity_merged %>%
  mutate(ActivityDate = mdy(ActivityDate))

### When I look at the dailyActivity_merged file, there are days where there were 0 TotalSteps recorded for 
### a given person.  I considered whether to delete these records (cleaning).  Additionally, there are days 
### where there were 1440 minutes of SedentaryMinutes recorded, and most of these records show 0 TotalDistance
### and 0 TotalSteps recorded.  However, there were a handful of records where even with 1440 minutes of 
### SedentaryMinutes recorded, there were also TotalSteps and TotalDistance measurements that are greater than 0.
### Additionally, there are days where a very small number of steps are recorded, like, 4, 8, 9.  
### So on the issue of data cleaning, I had to consider whether to omit any of these records (those with little or
### no recorded activity).  For those records with no activity recorded whatsover, I can think of several posibilities
### as to what had occured.  Maybe they had the FitBit turned on but were not wearing it (put it on the table for the
### day and forgot about it).  Maybe they were bedridden and had taken no steps.

# The days where there is no activity recorded do not provide much useful information.  So for the analysis, I 
# eliminate all records that have 0 TotalSteps recorded.
dailyActivity <- dailyActivity %>%
  filter(TotalSteps > 0)

# There are now 863 records as opposed to the original 940 after we filtered out records with no recorded Steps
nrow(dailyActivity)

# As I pointed out earlier, there are some records with 1440 minutes of sedentary activity but with thousands of 
# steps still being recorded.  I do not know what is going on here, but I eliminated these records as well.
dailyActivity <- dailyActivity %>%
  filter(!(TotalSteps > 0 & SedentaryMinutes == 1440))

# There are now 856 records as opposed to 863 records after we filtered out records with 1440 SedentaryMinutes
# with TotalSteps recorded greater than 0
nrow(dailyActivity)

# test

# The number of rows with SedentaryActiveDistance equal to 0 is 774
count_zero_SedentaryActiveDistance = nrow(subset(dailyActivity, SedentaryActiveDistance == 0))
print(count_zero_SedentaryActiveDistance)

# The maximum SedentaryActiveDistance is only .109999.
max_SedentaryDistance <- max(dailyActivity$SedentaryActiveDistance)
print(max_SedentaryMinutes)

# As most SedentaryActiveDistance entries are equal to zero, as they should be, and with the rest
# being of insignificant value, though they should be zero as well, I am excluding SedentaryActiveDistance
# in calculations and visualizations altogether.


# Create a new column with total active minutes recorded.  These are all Active Minutes, but not SedentaryMinutes.
dailyActivity <- dailyActivity %>%
  mutate(TotalActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)


# str(dailyActivity)

#looking_for_rows_to_delete <- dailyActivity %>%
  #filter(TotalSteps < 100)

# glimpse(dailyActivity)
# dailyActivity$test_Activitydate

# View(dailyActivity$test_ActivityDate)

### Steps
# Look at the mean total steps recorded for each Id
# mean_steps_by_id <- dailyActivity %>%
#   group_by(Id) %>%
#   summarize(mean_steps = mean(TotalSteps, na.rm = TRUE))


# ggplot(data = mean_steps_by_id) +
#   geom_col(mapping = aes(x = Id, y = mean_steps))

# dailyActivity <- dmy(dailyActivity_merged$ActivityDate)

# glimpse(dailyActivity)

# A histogram of the average number of steps per day
# As we can see, more people take between 6000 and 1000 steps per day
# There are a few who take more than 1000.  More power to them!
# result <- hist(mean_steps_by_id$mean_steps,
#                main = "Average Number of Steps Per Day",
#                xlab = "Average Steps Per Day",
#                ylab = "Number of People",
#                col = "Purple")

# print(result)

# The mean distance recorded, by Id
# mean_total_distance_by_id <- dailyActivity %>%
#   group_by(Id) %>%
#   summarize(mean_distance = mean(TotalDistance, na.rm = TRUE)) 

# A histogram of the mean total distance by Id
# Most people walk between 2 and 8 miles a day.  
# result <- hist(mean_total_distance_by_id$mean_distance,
#                main = "Average Distance Per Day",
#                xlab = "Average Distance",
#                ylab = "Number of People",
#                col = "Red")

### Combining averages per ID in one data frame
# means_by_id <- dailyActivity %>%
#   group_by(Id) %>%
#   summarize(mean_steps = mean(TotalSteps, na.rm = TRUE),
#             mean_distance = mean(TotalDistance, na.rm = TRUE),
#             mean_calories = mean(Calories, na.rm = TRUE))

# A histogram of the mean calories burined by Id
# result <- hist(means_by_id$mean_calories,
#                main = "Average Calories Burned Per Day",
#                xlab = "Average Calories",
#                ylab = "Number of People",
#                col = "Red")


### All Summary Stats by ID
summary_stats_by_id <- dailyActivity %>%
  group_by(Id) %>%
  summarize(mean_TotalSteps = mean(TotalSteps, na.rm = TRUE),
            min_TotalSteps = min(TotalSteps, na.rm = TRUE),
            max_TotalSteps = max(TotalSteps, na.rm = TRUE),
            sum_TotalSteps = sum(TotalSteps, na.rm = TRUE),
            mean_TotalDistance = mean(TotalDistance, na.rm = TRUE),
            min_TotalDistance = min(TotalDistance, na.rm = TRUE),
            max_TotalDistance = max(TotalDistance, na.rm = TRUE),
            sum_TotalDistance = sum(TotalDistance , na.rm = TRUE),
            mean_VeryActiveDistance = mean(VeryActiveDistance, na.rm = TRUE),
            max_VeryActiveDistance = max(VeryActiveDistance, na.rm = TRUE),
            min_VeryActiveDistance = min(VeryActiveDistance, na.rm = TRUE),
            sum_VeryActiveDistance = sum(VeryActiveDistance, na.rm = TRUE),
            mean_ModeratelyActiveDistance = mean(ModeratelyActiveDistance, na.rm = TRUE),
            max_ModeratelyActiveDistance = max(ModeratelyActiveDistance, na.rm = TRUE),
            min_ModeratelyActiveDistance = min(ModeratelyActiveDistance, na.rm = TRUE),
            sum_ModeratelyActiveDistance = sum(ModeratelyActiveDistance, na.rm = TRUE),
            mean_LightActiveDistance = mean(LightActiveDistance, na.rm = TRUE),
            max_LightActiveDistance = max(LightActiveDistance, na.rm = TRUE),
            min_LightActiveDistance = min(LightActiveDistance, na.rm = TRUE),
            sum_LightActiveDistance = sum(LightActiveDistance, na.rm = TRUE),
            sum_TotalActiveMinutes = sum(TotalActiveMinutes, na.rm = TRUE),
            mean_VeryActiveMinutes = mean(VeryActiveMinutes, na.rm = TRUE),
            max_VeryActiveMinutes = max(VeryActiveMinutes, na.rm = TRUE),
            min_VeryActiveMinutes = min(VeryActiveMinutes, na.rm = TRUE),
            sum_VeryActiveMinutes = sum(VeryActiveMinutes, na.rm = TRUE),
            mean_FairlyActiveMinutes = mean(FairlyActiveMinutes, na.rm = TRUE),
            max_FairlyActiveMinutes = max(FairlyActiveMinutes, na.rm = TRUE),
            min_FairlyActiveMinutes = min(FairlyActiveMinutes, na.rm = TRUE),
            sum_FairlyActiveMinutes = sum(FairlyActiveMinutes, na.rm = TRUE),
            mean_LightlyActiveMinutes = mean(LightlyActiveMinutes, na.rm = TRUE),
            max_LightlyActiveMinutes = max(LightlyActiveMinutes, na.rm = TRUE),
            min_LightlyActiveMinutes = min(LightlyActiveMinutes, na.rm = TRUE),
            sum_LightlyActiveMinutes = sum(LightlyActiveMinutes, na.rm = TRUE),
            mean_SedentaryMinutes = mean(SedentaryMinutes, na.rm = TRUE),
            max_SedentaryMinutes = max(SedentaryMinutes, na.rm = TRUE),
            min_SedentaryMinutes = min(SedentaryMinutes, na.rm = TRUE),
            sum_SedentaryMinutes = sum(SedentaryMinutes, na.rm = TRUE),
            mean_calories = mean(Calories, na.rm = TRUE),
            min_calories = min(Calories, na.rm = TRUE),
            max_calories = max(Calories, na.rm = TRUE),
            sum_calories = sum(Calories, na.rm = TRUE))

str(summary_stats_by_id)

# A histogram of the average number of steps per day
# As we can see, more people take between 6000 and 10000 steps per day
# There are 7 who take more than 10000, an oft cited number of recommended
# steps per day.  More power to them!
result <- hist(summary_stats_by_id$mean_TotalSteps,
               main = "Average Number of Steps Per Day",
               xlab = "Average Steps Per Day",
               ylab = "Number of People",
               col = "Purple")

# A histogram of the mean total distance by Id.
# Most people walk between 2 and 8 miles a day.
result <- hist(summary_stats_by_id$mean_TotalDistance,
               main = "Average Distance Per Day",
               xlab = "Average Distance",
               ylab = "Number of People",
               col = "Magenta")

# A histogram of the mean calories burned per day by Id.
# Most people burn between 1500 and 3000 calories per day.
result <- hist(summary_stats_by_id$mean_calories,
               main = "Average Calories Burned Per Day",
               xlab = "Average Calories",
               ylab = "Number of People",
               col = "Green")


# A histogram for average number of steps recorded per day, per Id
# ggplot(data = summary_stats_by_id) +
#   geom_histogram(mapping = aes(x = mean_TotalSteps))

# Another histogram for average number of steps recorded per day, per Id
# result <- hist(summary_stats_by_id$mean_TotalSteps,
#                main = "Average Steps Per Day",
#                xlab = "Average Number of Steps",
#                ylab = "Number of People",
#                col = "Violet")

# A histogram for mean Total Distance for each ID
# ggplot(data = summary_stats_by_id) +
#   geom_histogram(mapping = aes(x = mean_TotalDistance ))

# Another histogram for mean Total Distance for each ID
# result <- hist(summary_stats_by_id$mean_TotalDistance,
#                main = "Average Distance Per Day",
#                xlab = "Average Distance",
#                ylab = "Number of People",
#                col = "Yellow")



# Here is a more granular histogram for the average number of calories burned per Id
ggplot(data = summary_stats_by_id) +
  geom_histogram(mapping = aes(x = mean_calories )) +
  labs(title = "Average Calories Burned Per Day",
       subtitle = "Per Id",
       x = "Calories Burned Per Day",
       y = "Number of People")

# Another histogram for average number of calories burned per Id
# result <- hist(summary_stats_by_id$mean_calories,
#                main = "Average Calories Burned Per Day",
#                xlab = "Average Calories",
#                ylab = "Number of People",
#                col = "Yellow")


############
### For each ID, create two different bar graphs, one showing percentage breakdown of distance types,
### the other showing percentage breakdown of activity types.
### Create new dataframe taking data from summary_stats_by_id in percentage form
summary_stats_by_id3 <- summary_stats_by_id %>%
  select(Id, sum_VeryActiveDistance, sum_ModeratelyActiveDistance, sum_LightActiveDistance,
         sum_VeryActiveMinutes, sum_FairlyActiveMinutes, sum_LightlyActiveMinutes, sum_SedentaryMinutes)

str(summary_stats_by_id3)

summary_stats_by_id3 <- summary_stats_by_id3 %>%
  mutate(percent_VeryActiveDistance = sum_VeryActiveDistance /
           (sum_VeryActiveDistance + sum_ModeratelyActiveDistance + sum_LightActiveDistance),
         percent_ModeratelyActiveDistance = sum_ModeratelyActiveDistance /
           (sum_VeryActiveDistance + sum_ModeratelyActiveDistance + sum_LightActiveDistance),
         percent_LightActiveDistance = sum_LightActiveDistance /
           (sum_VeryActiveDistance + sum_ModeratelyActiveDistance + sum_LightActiveDistance),
         percent_VeryActiveMinutes = sum_VeryActiveMinutes  /
           (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes +
              sum_SedentaryMinutes),
         percent_FairlyActiveMinutes = sum_FairlyActiveMinutes  /
           (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes +
              sum_SedentaryMinutes),
         percent_LightlyActiveMinutes = sum_LightlyActiveMinutes  /
           (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes +
              sum_SedentaryMinutes),
         percent_SedentaryMinutes = sum_SedentaryMinutes  /
           (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes +
              sum_SedentaryMinutes)
         )

str(summary_stats_by_id3)

summary_stats_by_id3 <- summary_stats_by_id3 %>%
  select(Id, percent_VeryActiveDistance, percent_ModeratelyActiveDistance,percent_LightActiveDistance,
         percent_VeryActiveMinutes, percent_FairlyActiveMinutes, percent_LightlyActiveMinutes,
         percent_SedentaryMinutes)

distance_percentage_by_id3_long <- gather(summary_stats_by_id3, distance_category, percentage_distance,
                                    percent_VeryActiveDistance:percent_LightActiveDistance)

distance_percentage_by_id3_long <- distance_percentage_by_id3_long %>%
  select(Id, distance_category, percentage_distance)

activity_percentage_by_id3_long <- gather(summary_stats_by_id3, activity_category, percentage_activity,
                                    percent_VeryActiveMinutes:percent_SedentaryMinutes)

activity_percentage_by_id3_long <- activity_percentage_by_id3_long %>%
  select(Id, activity_category, percentage_activity)

distance_percentage_by_id3_long <- distance_percentage_by_id3_long %>%
  mutate(distance_category = recode(distance_category, percent_VeryActiveDistance = 'Very',
                                    percent_ModeratelyActiveDistance = 'Moderately',
                                    percent_LightActiveDistance =  'Lightly'))

activity_percentage_by_id3_long <- activity_percentage_by_id3_long %>%
  mutate(activity_category = recode(activity_category, percent_VeryActiveMinutes = 'Very',
                                    percent_FairlyActiveMinutes = 'Fairly',
                                    percent_LightlyActiveMinutes =  'Lightly',
                                    percent_SedentaryMinutes = "Sedentary"))

# For each Id, plot the percentage spent in each distance category.
# The majority of people have their highest percentage in the Lightly distance category .
# And interestingly, a handful of people have their highest percentage in the Very category.
ggplot(data = distance_percentage_by_id3_long, aes (x = distance_category, y = percentage_distance)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  labs(title = "Percentage Spent in Each Distance Category",
       subtitle = "Per Id",
       x = "Distance Category",
       y = "Percentage") +
  facet_wrap(~Id)


# For each Id, plot the percentage spent in each activity category.
# As expected, everyone spent the most amount of time in the Sedentary category.
ggplot(data = activity_percentage_by_id3_long, aes (x = activity_category, y = percentage_activity)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  labs(title = "Percentage Spent in Each Activity Category",
       subtitle = "Per Id",
       x = "Activity",
       y = "Percentage") +
  facet_wrap(~Id)




#############


### Put summary_stats_by_id activity distances into long format.
summary_stats_by_id2 <- select(summary_stats_by_id, Id, mean_VeryActiveDistance, mean_ModeratelyActiveDistance, mean_LightActiveDistance)
summary_stats_by_id3 <- gather(summary_stats_by_id2,activity, average, mean_VeryActiveDistance:mean_LightActiveDistance)
summary_stats_by_id4 <- summary_stats_by_id3 %>%
  mutate(activity = recode(activity, mean_VeryActiveDistance = 'Very', mean_ModeratelyActiveDistance = 'Moderate', mean_LightActiveDistance =  'Light' ))

# Plot the histogram, for each Id, the average distances walked in each distance category.
# As expected, most people spent the majority of their distance activity in the Light category.
# Interestingly, a few spent the majority of the distiance activity in the Very category.
ggplot(data = summary_stats_by_id4, aes (x = activity, y = average)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  labs(title = "Average Distances Spent in Each Distance Category",
       subtitle = "Per Id",
       x = "Activity Category",
       y = "Average Distance") +
  facet_wrap(~Id)

# Scatter plot showing the relationship between totalsteps taken and total distance travelled
# As we can see, the more steps taken, the greater the total distance travelled.
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = TotalDistance)) +
  labs("title" = "Total Steps vs Total Distance",
       x = "Total Steps",
       y = "Total Distance")

# Show the scatter plot for TotalSteps vs TotalDistance for each Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = TotalDistance)) +
  labs("title" = "Total Steps vs Total Distance",
       subtitle = "Per Id",
       x = "Total Steps",
       y = "Total Distance") + 
  facet_wrap(~Id)

#ggplot(data = dailyActivity) +
  #geom_point(mapping = aes(x = TotalSteps, y = TotalDistance, color = Id))

# Plot the relationship between TotalSteps taken and Calories burned.
# There is a positive correlation between TotalSteps taken and Calories burned.
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories)) +
  labs("title" = "Total Steps vs Total Calories",
       x = "Total Steps",
       y = "Calories")

# Plot the relation between TotalSteps taken and Calories burned for each Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories)) +
  labs("title" = "Total Steps vs Total Calories",
       subtitle = "Per Id",
       x = "Total Steps",
       y = "Calories") +
  facet_wrap(~Id)

#ggplot(data = dailyActivity) +
  #geom_line(mapping = aes(x = TotalSteps, y = Calories, color = Id))

# Plot the relationship between TotalDistance and  Calories burned.
# As we can see, as the Total Distance goes up, so does Calories
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalDistance, y = Calories)) +
  labs(title = "Total Distance vs Total Calories",
       x = "Total Distance",
       y = "Calories")

# Plot the relationship between TotalDistance and  Calories burned for each Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalDistance, y = Calories)) +
  labs(title = "Total Distance vs Total Calories", 
       subtitle = "Per Id",
       x = "Total Distance",
       y = "Calories") +
  facet_wrap(~Id)

# Plot the relationship between SedentaryMinutes and Calories burned
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = SedentaryMinutes, y = Calories)) +
  labs(title = "Sedentary Minutes vs Calories",
       x = "Sedentary Minutes",
       y = "Calories")


# Plot the relationship between VeryActiveMinutes and Calories burned
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = Calories)) +
  labs(title = "Very Active Minutes vs Calories",
       x = "Very Active Minutes",
       y = "Calories")

# Plot the relationship between VeryActiveMinutes and Calories burned per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = Calories)) +
  labs(title = "Very Active Minutes vs Calories",
       subtitle = "Per Id",
       x = "Very Active Minutes",
       y = "Calories") +
  facet_wrap(~Id)

# Plot the relationship between VeryActiveMinutes and VeryActiveDistance.
# As we can see, as the number of Very Active Minutes goes up, so does Very Active Distance
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = VeryActiveDistance)) +
  labs(title = "Very Active Minutes vs Very Active Distance",
       x = "Very Active Minutes",
       y = "Very Active Distance")

# Plot the relationship between VeryActiveMinutes and VeryActiveDistance per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = VeryActiveDistance)) +
  labs(title = "Very Active Minutes vs Very Active Distance",
       subtitle = "Per Id",
       x = "Very Active Minutes",
       y = "Very Active Distance") +
  facet_wrap(~Id)

# Plot the relationship between FairlyActiveMinutes and ModeratelyActiveDistance.
# As the number of Fairly Active Minutes goes up, so does the amount of distance travelled in
# the Moderately Active Distance category
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = FairlyActiveMinutes, y = ModeratelyActiveDistance)) +
  labs(title = "Fairly Active Minutes vs Moderately Active Distance",
       x = "Fairly Active Minutes",
       y = "Moderately Active Distance")

# Plot the relationship between FairlyActiveMinutes and ModeratelyActiveDistance per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = FairlyActiveMinutes, y = ModeratelyActiveDistance)) +
  labs(title = "Fairly Active Minutes vs Moderately Active Distance", 
       subtitle = "Per Id",
       x = "Fairly Active Minutes",
       y = "Moderately Active Distance") +
  facet_wrap(~Id)

# Plot the relationship between LightlyActiveMinutes and LightActiveDistance.
# The more time spent in doing Lightly Active activities, the greater the distance covered in the
# Lightly Active Distance category
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = LightlyActiveMinutes, y = LightActiveDistance)) +
  labs(title = "Lightly Active Minutes vs Light Active Distance",
       x = "Lightly Active Minutes",
       y = "Lightly Active Distance")

# Plot the relationship between LightlyActiveMinutes and LightActiveDistance per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = LightlyActiveMinutes, y = LightActiveDistance)) + 
  labs(title = "Lightly Active Minutes vs Light Active Distance",
       subtitle = "Per Id",
       x = "Lightly Active Minutes",
       y = "Lightly Active Distance") +
  facet_wrap(~Id)

### 

# Plot the relationship between TotalSteps and SedentaryMinutes
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = SedentaryMinutes)) +
  labs(title = "Total Steps vs Sedentary Minutes",
       x = "Total Steps",
       y = "Sedentary Minutes")

# Plot the relationship between TotalSteps and SedentaryMinutes per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = SedentaryMinutes)) +
  labs(title = "Total Steps vs Sedentary Minutes",
       subtitle = "Per Id",
       x = "Total Steps",
       y = "Sedentary Minutes") +
  facet_wrap(~Id)

# Plot the relationship between TotalDistance and SedentaryMinutes
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalDistance, y = SedentaryMinutes)) +
  labs(title = "Total Distance vs Sedentary Minutes",
       x = "Total Distance",
       y = "Sedentary Minutes")


# Plot the relationship between TotalDistance and SedentaryMinutes per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalDistance, y = SedentaryMinutes)) +
  labs(title = "Total Distance vs Sedentary Minutes",
       subtitle = "Per Id",
       x = "Total Distance",
       y = "Sedentary Minutes") +
  facet_wrap(~Id)

# Plot the relationship between the sum_TotalSteps and sum_calories.
# There is a positive correlation between the Sum of Total Steps and the
# Sum of Total Calories
ggplot(data = summary_stats_by_id) +
  geom_point(mapping = aes(x = sum_TotalSteps, y = sum_calories)) +
  labs(title = "Sum of Total Steps vs Sum of Total Calories",
       x = "Sum of Total Steps",
       y = "Sum of Total Calories")

# Plot the relationship between sum_TotalDistance and sum_calories
# There is a positive correlation between the Sum of Total Distance and the 
# Sum of total Calories
ggplot(data = summary_stats_by_id) +
  geom_point(mapping = aes(x = sum_TotalDistance, y = sum_calories)) +
  labs(title = "Sum of Total Distance vs Sum of Total Calories",
       x = "Sum of Total Distance",
       y = "Sum of Total Calories")


##################### SLEEP ############################

# Store the sleepDay_Merged.csv file in a dataframe
sleepDay_merged <- read.csv("sleepDay_Merged.csv")

# There are 413 rows in the sleepDay_merged dataframe
nrow(sleepDay_merged)

# There are 410 distinct rows in the sleepDay_merged dataframe
nrow(distinct(sleepDay_merged))

duplicated(sleepDay_merged)

duplicates <- sleepDay_merged[duplicated(sleepDay_merged), ]
duplicates
str(sleepDay_merged)

# Remove duplicate rows from the sleepDay_merged dataframe
# There are now 410 rows in the sleepDay_merged data frame
sleepDay_merged <- unique(sleepDay_merged)
nrow(sleepDay_merged)


# Convert character representation of date and time to POSIXlt
sleepDay_merged <- sleepDay_merged %>% 
  mutate(SleepDay = strptime(SleepDay, "%m/%d/%Y %H:%M:%S"))

# Convert SleepDay to Date format
sleepDay_merged <- sleepDay_merged %>% 
  mutate(SleepDay = as.Date(SleepDay))

# Add a column calculating sleep percentage
sleepDay_merged <- sleepDay_merged %>% 
  mutate(sleepPercentage = TotalMinutesAsleep / TotalTimeInBed)

# Create a new dataframe that shows summary stats per Id
sleep_stats_by_id <- sleepDay_merged %>%
  group_by(Id) %>%
  summarize(
            sum_TotalSleepRecords = sum(TotalSleepRecords, na.rm = TRUE),
            mean_TotalMinutesAsleep = mean(TotalMinutesAsleep, na.rm = TRUE),
            min_TotalMinutesAsleep = min(TotalMinutesAsleep, na.rm = TRUE),
            max_TotalMinutesAsleep = max(TotalMinutesAsleep, na.rm = TRUE),
            sum_TotalMinutesAsleep = sum(TotalMinutesAsleep , na.rm = TRUE),
            mean_TotalTimeInBed  = mean(TotalTimeInBed, na.rm = TRUE),
            max_TotalTimeInBed  = max(TotalTimeInBed, na.rm = TRUE),
            min_TotalTimeInBed  = min(TotalTimeInBed , na.rm = TRUE),
            sum_TotalTimeInBed  = sum(TotalTimeInBed , na.rm = TRUE),
            )

# Add a column for avg_percentage_asleep_in_bed
sleep_stats_by_id <- sleep_stats_by_id %>%
  mutate(percentage_asleep_in_bed = sum_TotalMinutesAsleep / sum_TotalTimeInBed)

#all_stats = cbind(summary_stats_by_id, sleep_stats_by_id)

# A plot of the percentage of time asleep while in bed.
ggplot(sleep_stats_by_id) +
  geom_histogram(mapping = aes(x = percentage_asleep_in_bed)) +
  labs(title = "Percentage of Time Asleep While in Bed",
       x = "Percentage of Time",
       y = "Number of People")

# Another histogram for a count of the percentage of time asleep while in bed.
# Most people spend 80% or more of the time in bed asleep.
result <- hist(sleep_stats_by_id$percentage_asleep_in_bed,
               main = "Proportion of Time Asleep While In Bed",
               xlab = "Percentage of Time Asleep",
               ylab = "Number of People",
               col = "Cyan")

### Create a dataframe for minuteSleep_merged.csv
minuteSleep_merged <- read.csv("minuteSleep_merged.csv")

nrow(minuteSleep_merged)


nrow(distinct(minuteSleep_merged))

duplicates <- minuteSleep_merged[duplicated(minuteSleep_merged), ]
duplicates

str(minuteSleep_merged)

# Calculate percentage spent in each sleep stage for each Id
minuteSleep_stats <- minuteSleep_merged %>%
  group_by(Id) %>% 
  summarize(sum_stage1 = sum(value == 1), 
            sum_stage2 = sum(value == 2),
            sum_stage3 = sum(value == 3))

minuteSleep_stats <- minuteSleep_stats %>% 
  mutate(percentage_stage1 = sum_stage1 / (sum_stage1 + sum_stage2 + sum_stage3),
         percentage_stage2 = sum_stage2 / (sum_stage1 + sum_stage2 + sum_stage3),
         percentage_stage3 = sum_stage3 / (sum_stage1 + sum_stage2 + sum_stage3))
  
# ggplot(data = minuteSleep_stats) +
# geom_bar(mapping = aes(x = distribution_channel,fill=deposit_type ))

# minuteSleep_stats_long <- gather(minuteSleep_stats, stage, percent, percentage_stage1:percentage_stage3)

# minuteSleep_stats_long <- minuteSleep_stats_long %>%
#   select(Id, stage, percent)
# 
# ggplot(data = minuteSleep_stats_long) +
#   geom_bar(mapping = aes(x = stage)) +
#   facet_grid(~Id)
# 
# ggplot(data = minuteSleep_stats_long) +
#   geom_histogram(mapping = aes(x = stage)) +
#   facet_wrap(~Id)

# ggplot(data = all_stats) +
#   geom_histogram(mapping = aes(x = avg_percentage_asleep_in_bed))

# Show breakdown of minutes spent in each sleep stage
minuteSleep_stats_long2 <- gather(minuteSleep_stats, stage, num_minutes, sum_stage1:sum_stage3)

minuteSleep_stats_long2 <- minuteSleep_stats_long2 %>% 
  select(Id, num_minutes, stage)

minuteSleep_stats_long2 <- minuteSleep_stats_long2 %>%
  mutate(stage = recode(stage, sum_stage1 = 'Stage_1', sum_stage2 = 'Stage_2', sum_stage3 =  'Stage_3' ))

# ggplot(data = minuteSleep_stats_long2) +
#   geom_bar(mapping = aes (x = stage)) +
#   facet_wrap(~Id)
# 
# ggplot(data = minuteSleep_stats_long2) +
#   geom_histogram(mapping = aes (x = num_minutes)) +
#   facet_wrap(~Id)

# Plot breakdown of time in each sleep stage for each Id
ggplot(data = minuteSleep_stats_long2, aes (x = stage, y = num_minutes)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  labs(title = "Number of Minutes Spent in Each Sleep Stage",
       subtitle = "Per Id",
       x = "Stage",
       y = "Number of Minutes") +
  facet_wrap(~Id)

# A breakdown of aggregate time in each sleep stage
ggplot(data = minuteSleep_stats_long2) +
  geom_col(mapping = aes(x = stage, y = num_minutes)) +
  labs(title = "Number of Minutes Spent in Each Sleep Stage",
       subtitle = "Per Id",
       x = "Stage",
       y = "Number of Minutes")

# Show  breakdown of percentage of time spent in each sleep stage
minuteSleep_stats_percentages_long <- gather(minuteSleep_stats, stage, percentage, percentage_stage1:percentage_stage3)

minuteSleep_stats_percentages_long <- minuteSleep_stats_percentages_long %>%
  select(Id, stage, percentage)

minuteSleep_stats_percentages_long <- minuteSleep_stats_percentages_long %>%
  mutate(stage = recode(stage, percentage_stage1 = 'Stage_1', percentage_stage2 = 'Stage_2', percentage_stage3 =  'Stage_3'))

# Plot breakdown of percentage of time spent in each sleep stage by Id
ggplot(data = minuteSleep_stats_percentages_long, aes (x = stage, y = percentage)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  labs(title = "Percentage of Time Spent in Each Sleep Stage",
       subtitle = "Per Id",
       x = "Stage",
       y = "Percentage of Time") +
  facet_wrap(~Id)

# ### For each ID, create two different bar graphs, one showing percentage breakdown of distance types, 
# ### the other showing percentage breakdown of activity types.
# ### Create new dataframe taking data from summary_stats_by_id in percentage form
# summary_stats_by_id3 <- summary_stats_by_id %>%
#   select(Id, sum_VeryActiveDistance, sum_ModeratelyActiveDistance, sum_LightActiveDistance,
#          sum_VeryActiveMinutes, sum_FairlyActiveMinutes, sum_LightlyActiveMinutes, sum_SedentaryMinutes)
# 
# str(summary_stats_by_id3)
# 
# summary_stats_by_id3 <- summary_stats_by_id3 %>%
#   mutate(percent_VeryActiveDistance = sum_VeryActiveDistance / 
#            (sum_VeryActiveDistance + sum_ModeratelyActiveDistance + sum_LightActiveDistance),
#          percent_ModeratelyActiveDistance = sum_ModeratelyActiveDistance / 
#            (sum_VeryActiveDistance + sum_ModeratelyActiveDistance + sum_LightActiveDistance),
#          percent_LightActiveDistance = sum_LightActiveDistance / 
#            (sum_VeryActiveDistance + sum_ModeratelyActiveDistance + sum_LightActiveDistance),
#          percent_VeryActiveMinutes = sum_VeryActiveMinutes  / 
#            (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes + 
#               sum_SedentaryMinutes),
#          percent_FairlyActiveMinutes = sum_FairlyActiveMinutes  / 
#            (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes + 
#               sum_SedentaryMinutes),
#          percent_LightlyActiveMinutes = sum_LightlyActiveMinutes  / 
#            (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes + 
#               sum_SedentaryMinutes),
#          percent_SedentaryMinutes = sum_SedentaryMinutes  / 
#            (sum_VeryActiveMinutes + sum_FairlyActiveMinutes + sum_LightlyActiveMinutes + 
#               sum_SedentaryMinutes)
#          )
# 
# str(summary_stats_by_id3)
# 
# # Stopped here
# summary_stats_by_id3 <- summary_stats_by_id3 %>%
#   select(Id, percent_VeryActiveDistance, percent_ModeratelyActiveDistance,percent_LightActiveDistance,
#          percent_VeryActiveMinutes, percent_FairlyActiveMinutes, percent_LightlyActiveMinutes,
#          percent_SedentaryMinutes)
#          
# distance_percentage_by_id3_long <- gather(summary_stats_by_id3, distance_category, percentage_distance, 
#                                     percent_VeryActiveDistance:percent_LightActiveDistance)
# 
# distance_percentage_by_id3_long <- distance_percentage_by_id3_long %>%
#   select(Id, distance_category, percentage_distance)
# 
# activity_percentage_by_id3_long <- gather(summary_stats_by_id3, activity_category, percentage_activity, 
#                                     percent_VeryActiveMinutes:percent_SedentaryMinutes)
# 
# activity_percentage_by_id3_long <- activity_percentage_by_id3_long %>%
#   select(Id, activity_category, percentage_activity)
# 
# distance_percentage_by_id3_long <- distance_percentage_by_id3_long %>%
#   mutate(distance_category = recode(distance_category, percent_VeryActiveDistance = 'Very', 
#                                     percent_ModeratelyActiveDistance = 'Moderately', 
#                                     percent_LightActiveDistance =  'Lightly'))
# 
# activity_percentage_by_id3_long <- activity_percentage_by_id3_long %>%
#   mutate(activity_category = recode(activity_category, percent_VeryActiveMinutes = 'Very', 
#                                     percent_FairlyActiveMinutes = 'Fairly', 
#                                     percent_LightlyActiveMinutes =  'Lightly',
#                                     percent_SedentaryMinutes = "Sedentary"))
# 
# # For each Id, plot the percentage spent in each distance category
# ggplot(data = distance_percentage_by_id3_long, aes (x = distance_category, y = percentage_distance)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 270)) +
#   labs(title = "Percentage Spent in Each Distance Category",
#        subtitle = "Per Id",
#        x = "Distance Category",
#        y = "Percentage") +
#   facet_wrap(~Id)
# 
# # For each Id, plot the percentage spent in each activity category
# ggplot(data = activity_percentage_by_id3_long, aes (x = activity_category, y = percentage_activity)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 270)) +
#   labs(title = "Percentage Spent in Each Activity Category",
#        subtitle = "Per Id",
#        x = "Activity",
#        y = "Percentage") + 
#   facet_wrap(~Id)


### Read in weightLogInfo_merged.csv to a dataframe
weightLogInfo_merged <- read.csv("weightLogInfo_merged.csv")

# There are 67 records in the weightLogInfo_merged dataframe
nrow(weightLogInfo_merged)

# There are no duplicate rows in the weightLogInfo_merged dataframe
nrow(distinct(weightLogInfo_merged))

# There are 8 people who recorded their weight
n_distinct(weightLogInfo_merged$Id)

str(weightLogInfo_merged)

# Number of times each person recorded their weight
weightLog_count <- table(weightLogInfo_merged$Id)
weightLog_count


### Read in number of steps by minute, narrow merged 
minuteStepsNarrow_merged <- read.csv("minuteStepsNarrow_merged.csv")

# There are 1325580 rows in the minuteStepsNarrow_merged dataframe
nrow(minuteStepsNarrow_merged)

# There are no duplicate rows in the minuteStepsNarrow_merged dataframe
nrow(distinct(minuteStepsNarrow_merged))

str(minuteStepsNarrow_merged)

# Convert ActivityMinute from character to date/time
minuteSteps <- minuteStepsNarrow_merged %>%
  mutate(ActivityMinute = mdy_hms(ActivityMinute))

#minuteSteps = subset(minuteSteps, select = -c(ActivityMinute))

#colnames(minuteSteps)[3] <- "ActivityMinute"

str(minuteSteps)

# We are only concerned about hours in the time since we will calculate steps per hour
minuteSteps <- minuteSteps %>%
  mutate(ActivityMinute = hour(ActivityMinute))

str(minuteSteps)

minuteSteps <- minuteSteps %>%
  rename(Hour = ActivityMinute)

# dates2 <- hour(minuteSteps$ActivityMinute)
# 
# head(dates2)
# 
# n_distinct(dates2)
# 
# dates <- as.POSIXct(minuteSteps$ActivityMinute, format = "%H")
# head(dates)
# 
# dates <- hour(dates)
# 
# n_distinct(dates)
# 
# typeof(dates)
# time <- format(minuteStepsNarrow_merged$ActivityMinute, format = "%H:%M:%S")
# 
# print(time)
# 
# minuteStepsNarrow_merged$time <- as.ITime(minuteStepsNarrow_merged$ActivityMinute)
# 
# str(minuteStepsNarrow_merged)
# 
# typeof(minuteSteps$ActivityMinute)

# ggplot(data = minuteSteps) +
#   geom_histogram(mapping = aes(x = ActivityMinute))
# 
# ggplot(data = minuteSteps, aes (x = ActivityMinute)) +
#   geom_histogram(stat = "identity") + 
#   facet_wrap(~Id)

#ggplot(data = minuteSteps, aes (x = ActivityMinute, y = Steps)) +
 # geom_bar()

# Create new dataframe showing total steps per hour for each Id
minuteStepsSummary <- minuteSteps %>%
  group_by(Id, Hour) %>%
  summarize(TotalSteps = sum(Steps, na.rm = TRUE))

str(minuteStepsSummary)

#ggplot(data = summary_stats_by_id2, aes (x = activity, y = average)) +
  #geom_bar(stat = "identity") +
  #theme(axis.text.x = element_text(angle = 315)) +
  #facet_wrap(~Id)

# Create a bar chart showing total steps during each hour of the day for each Id
ggplot(data = minuteStepsSummary, aes (x = Hour, y = TotalSteps)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Steps Taken By Hour",
       subtitle = "Per Id") +
  facet_wrap(~Id)

# Show step distribution in the aggregate
ggplot(data = minuteStepsSummary) +
  geom_col(mapping = aes(x = Hour, y = TotalSteps)) +
  labs (title = "Total Steps Taken By Hour")

# Create new data frame showing total_steps_by_hour in the aggregate
total_steps_by_hour <- minuteSteps %>%
  group_by(Hour) %>%
  summarize(TotalSteps = sum(Steps, na.rm = TRUE))

# Another way to show aggregate TotalSteps by hour
ggplot(data = total_steps_by_hour, aes (x = Hour, y = TotalSteps)) +
  geom_col() +
  scale_y_continuous(breaks=c(0,100000,200000,300000,400000,500000, 600000, 700000))