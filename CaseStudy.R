

library(here)
library(skimr)
library(janitor)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(data.table)

dailyActivity_merged <- read.csv("dailyActivity_merged.csv")

str(dailyActivity_merged)

head(dailyActivity_merged)

### Number of records, total
nrow(dailyActivity_merged)

# There are 33 different Ids (persons) where their activity data is provided
n_distinct(dailyActivity_merged$Id)

# Find out how many days worth of data there are for each person
number_of_days_by_id <- dailyActivity_merged %>% count(Id)

skim_without_charts(dailyActivity_merged)

glimpse(dailyActivity_merged)


### Number of records with 0 TotalSteps recorded
sum(dailyActivity_merged$TotalSteps == 0)

### Number of records with 1440 Sedentary Minutes Recorded (60 minutes X 24 hours = 1440 minutes)
sum(dailyActivity_merged$SedentaryMinutes == 1440)

### Number of records (days) where records minutes do not sum to a full day (1440 minutes)

# First, Add a column that sums all minutes
dailyActivity_merged <- dailyActivity_merged %>% 
  mutate(sum_of_minutes = rowSums(across(c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes))))

# Then, Count up the number of rows where the sum of all minutes is not equal to 1440
sum(dailyActivity_merged$sum_of_minutes != 1440)

# Count the number of rows where the sum of all minutes is equal to 1440
sum(dailyActivity_merged$sum_of_minutes == 1440)

### Create a dataframe for only those records where there was a logged activity
### A logged activity is an activity that the person wanted to log, as opposed to all activity that is continuous
### monitored automatically while the fitbit is on
dailyActivity_with_logged_activity <- dailyActivity_merged %>%
  filter(LoggedActivitiesDistance > 0)

# The number of rows where an activity was logged
nrow(dailyActivity_with_logged_activity)

# See how many logged activities each user did
num_logged_activity_by_id <- dailyActivity_with_logged_activity %>% count(Id)

# The number of users that logged at least one activity
n_distinct(dailyActivity_with_logged_activity$Id)

### The ActivityDate column is currently a character.  This changes it to a date format
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
### day and forgot about it).  Maybe there were bedridden and had took no steps.

### The days where there is no activity recorded do not provide much useful information.  So for the analysis, I 
### eliminate all records that have 0 TotalSteps recorded.
dailyActivity <- dailyActivity %>%
  filter(TotalSteps > 0)

### There are now 863 records as opposed to the original 940 after we filtered out records with no recorded Steps
nrow(dailyActivity)

### Create a new column with total active minutes recorded
dailyActivity <- dailyActivity %>%
  mutate(TotalActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)


# str(dailyActivity)

#looking_for_rows_to_delete <- dailyActivity %>%
  #filter(TotalSteps < 100)

# glimpse(dailyActivity)
# dailyActivity$test_Activitydate

View(dailyActivity$test_ActivityDate)

### Steps
# Look at the mean total steps recorded for each Id
mean_steps_by_id <- dailyActivity %>%
  group_by(Id) %>%
  summarize(mean_steps = mean(TotalSteps, na.rm = TRUE))


ggplot(data = mean_steps_by_id) +
  geom_col(mapping = aes(x = Id, y = mean_steps))

# dailyActivity <- dmy(dailyActivity_merged$ActivityDate)

# glimpse(dailyActivity)

# A histogram of the average number of steps per day
# As we can see, more people take between 6000 and 1000 steps per day
# There are a few who take more than 1000.  More power to them!
result <- hist(mean_steps_by_id$mean_steps,
               main = "Average Number of Steps Per Day",
               xlab = "Average Steps Per Day",
               ylab = "Number of People",
               col = "Magenta")

print(result)


### Distance
# The mean distance recorded, by Id
mean_total_distance_by_id <- dailyActivity %>%
  group_by(Id) %>%
  summarize(mean_distance = mean(TotalDistance, na.rm = TRUE)) 

# A histogram of the mean total distance by Id
# Most people walk between 2 and 8 miles a day.  
result <- hist(mean_total_distance_by_id$mean_distance,
               main = "Average Distance Per Day",
               xlab = "Average Distance",
               ylab = "Number of People",
               col = "Magenta")

### Combining averages per ID in one data frame
means_by_id <- dailyActivity %>%
  group_by(Id) %>%
  summarize(mean_steps = mean(TotalSteps, na.rm = TRUE),
            mean_distance = mean(TotalDistance, na.rm = TRUE),
            mean_calories = mean(Calories, na.rm = TRUE))

# A histogram of the mean calories burined by Id
result <- hist(means_by_id$mean_calories,
               main = "Average Calories Burned Per Day",
               xlab = "Average Calories",
               ylab = "Number of People",
               col = "Green")

# print(result)


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


# A histogram for average number of steps recorded per day, per Id
ggplot(data = summary_stats_by_id) +
  geom_histogram(mapping = aes(x = mean_TotalSteps))

# Another histogram for average number of steps recorded per day, per Id
result <- hist(summary_stats_by_id$mean_TotalSteps,
               main = "Average Steps Per Day",
               xlab = "Average Number of Steps",
               ylab = "Number of People",
               col = "Violet")

# A histogram for mean Total Distance for each ID
ggplot(data = summary_stats_by_id) +
  geom_histogram(mapping = aes(x = mean_TotalDistance ))

# Another histogram for mean Total Distance for each ID
result <- hist(summary_stats_by_id$mean_TotalDistance,
               main = "Average Distance Per Day",
               xlab = "Average Distance",
               ylab = "Number of People",
               col = "Yellow")



# A histogram for the average number of calories burned per Id
ggplot(data = summary_stats_by_id) +
  geom_histogram(mapping = aes(x = mean_calories ))

# Another histogram for average number of calories burned per Id
result <- hist(summary_stats_by_id$mean_calories,
               main = "Average Calories Burned Per Day",
               xlab = "Average Calories",
               ylab = "Number of People",
               col = "Yellow")


### Put summary_stats_by_id activity distances into long format
summary_stats_by_id2 <- select(summary_stats_by_id, Id, mean_VeryActiveDistance, mean_ModeratelyActiveDistance, mean_LightActiveDistance)
summary_stats_by_id3 <- gather(summary_stats_by_id2,activity, average, mean_VeryActiveDistance:mean_LightActiveDistance)
summary_stats_by_id4 <- summary_stats_by_id3 %>%
  mutate(activity = recode(activity, mean_VeryActiveDistance = 'Very', mean_ModeratelyActiveDistance = 'Moderate', mean_LightActiveDistance =  'Light' ))

# Plot the histogram, for each Id, the average distances walked in each distance category
ggplot(data = summary_stats_by_id4, aes (x = activity, y = average)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  facet_wrap(~Id)

# Scatter plot showing the relationship between totalsteps taken and total distance travelled
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = TotalDistance))

# Show the scatter plot for totalsteps vs totalDistance for each Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = TotalDistance)) +
  facet_wrap(~Id)

#ggplot(data = dailyActivity) +
  #geom_point(mapping = aes(x = TotalSteps, y = TotalDistance, color = Id))

# Plot the relationship between TotalSteps taken and Calories burned
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories))

# Plot the relation between TotalSteps taken and Calories burned for each Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories)) +
  facet_wrap(~Id)

#ggplot(data = dailyActivity) +
  #geom_line(mapping = aes(x = TotalSteps, y = Calories, color = Id))

# Plot the relationship between TotalDistance and  Calories burned
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalDistance, y = Calories))

# Plot the relationship between TotalDistance and  Calories burned for each Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalDistance, y = Calories)) +
  facet_wrap(~Id)

# Plot the relationship between SedentaryMinutes and Calories burned
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = SedentaryMinutes, y = Calories))


# Plot the relationship between VeryActiveMinutes and Calories burned
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = Calories))

# Plot the relationship between VeryActiveMinutes and Calories burned per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = Calories)) +
  facet_wrap(~Id)

# Plot the relationship between VeryActiveMinutes and VeryActiveDistance
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = VeryActiveDistance))

# Plot the relationship between VeryActiveMinutes and VeryActiveDistance per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = VeryActiveMinutes, y = VeryActiveDistance)) +
  facet_wrap(~Id)

# Plot the relationship between FairlyActiveMinutes and ModeratelyActiveDistance
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = FairlyActiveMinutes, y = ModeratelyActiveDistance))

# Plot the relationship between FairlyActiveMinutes and ModeratelyActiveDistance per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = FairlyActiveMinutes, y = ModeratelyActiveDistance)) +
  facet_wrap(~Id)

# Plot the relationship between LightlyActiveMinutes and LightActiveDistance
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = LightlyActiveMinutes, y = LightActiveDistance))

# Plot the relationship between LightlyActiveMinutes and LightActiveDistance per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = LightlyActiveMinutes, y = LightActiveDistance)) + 
  facet_wrap(~Id)

# Plot the relationship between TotalSteps and SedentaryMinutes
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = SedentaryMinutes))

# Plot the relationship between TotalSteps and SedentaryMinutes per Id
ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = SedentaryMinutes)) +
  facet_wrap(~Id)

ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalSteps, y = SedentaryMinutes)) +
  facet_wrap(~Id)

ggplot(data = dailyActivity) +
  geom_point(mapping = aes(x = TotalDistance, y = SedentaryMinutes)) +
  facet_wrap(~Id)

#####################
ggplot(data = summary_stats_by_id) +
  geom_point(mapping = aes(x = sum_TotalSteps, y = sum_calories))

#####################
ggplot(data = summary_stats_by_id) +
  geom_point(mapping = aes(x = sum_TotalDistance, y = sum_calories))


##################### SLEEP ############################
sleepDay_merged <- read.csv("sleepDay_Merged.csv")

str(sleepDay_merged)

sleepDay_merged <- sleepDay_merged %>% 
  mutate(SleepDay = strptime(SleepDay, "%m/%d/%Y %H:%M:%S"))

sleepDay_merged <- sleepDay_merged %>% 
  mutate(SleepDay = as.Date(SleepDay))

sleepDay_merged <- sleepDay_merged %>% 
  mutate(sleepPercentage = TotalMinutesAsleep / TotalTimeInBed)

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
  
sleep_stats_by_id <- sleep_stats_by_id %>%
  mutate(avg_percentage_asleep_in_bed = sum_TotalMinutesAsleep / sum_TotalTimeInBed)

all_stats = cbind(summary_stats_by_id, sleep_stats_by_id)

ggplot(sleep_stats_by_id) +
  geom_histogram(mapping = aes(x = avg_percentage_asleep_in_bed))

minuteSleep_merged <- read.csv("minuteSleep_merged.csv")

minuteSleep_stats <- minuteSleep_merged %>%
  group_by(Id) %>% 
  summarize(sum_stage1 = sum(value == 1), 
            sum_stage2 = sum(value == 2),
            sum_stage3 = sum(value == 3))

minuteSleep_stats <- minuteSleep_stats %>% 
  mutate(percentage_stage1 = sum_stage1 / (sum_stage1 + sum_stage2 + sum_stage3),
         percentage_stage2 = sum_stage2 / (sum_stage1 + sum_stage2 + sum_stage3),
         percentage_stage3 = sum_stage3 / (sum_stage1 + sum_stage2 + sum_stage3))
  
ggplot(data = minuteSleep_stats) +
  geom_bar(mapping = aes(x = distribution_channel,fill=deposit_type ))

minuteSleep_stats_long <- gather(minuteSleep_stats, stage, percent, percentage_stage1:percentage_stage3)

minuteSleep_stats_long <- minuteSleep_stats_long %>% 
  select(Id, stage, percent)

ggplot(data = minuteSleep_stats_long) +
  geom_bar(mapping = aes(x = stage)) +
  facet_grid(~Id)

ggplot(data = minuteSleep_stats_long) +
  geom_histogram(mapping = aes(x = stage)) +
  facet_wrap(~Id)

ggplot(data = all_stats) +
  geom_histogram(mapping = aes(x = avg_percentage_asleep_in_bed))

minuteSleep_stats_long2 <- gather(minuteSleep_stats, stage, num_minutes, sum_stage1:sum_stage3)

minuteSleep_stats_long2 <- minuteSleep_stats_long2 %>% 
  select(Id, num_minutes, stage)

minuteSleep_stats_long2 <- minuteSleep_stats_long2 %>%
  mutate(stage = recode(stage, sum_stage1 = 'Stage_1', sum_stage2 = 'Stage_2', sum_stage3 =  'Stage_3' ))

ggplot(data = minuteSleep_stats_long2) +
  geom_bar(mapping = aes (x = stage)) +
  facet_wrap(~Id)

ggplot(data = minuteSleep_stats_long2) +
  geom_histogram(mapping = aes (x = num_minutes)) +
  facet_wrap(~Id)

ggplot(data = minuteSleep_stats_long2, aes (x = stage, y = num_minutes)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 315)) +
  facet_wrap(~Id)

minuteSleep_stats_percentages_long <- gather(minuteSleep_stats, stage, percentage, percentage_stage1:percentage_stage3)

minuteSleep_stats_percentages_long <- minuteSleep_stats_percentages_long %>%
  select(Id, stage, percentage)

minuteSleep_stats_percentages_long <- minuteSleep_stats_percentages_long %>%
  mutate(stage = recode(stage, percentage_stage1 = 'Stage_1', percentage_stage2 = 'Stage_2', percentage_stage3 =  'Stage_3'))

ggplot(data = minuteSleep_stats_percentages_long, aes (x = stage, y = percentage)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 315)) +
  facet_wrap(~Id)


### For each ID, create two different bar graphs, one showing breakdown of distance types, 
### the other showing breakdown of activity types
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

ggplot(data = distance_percentage_by_id3_long, aes (x = distance_category, y = percentage_distance)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  facet_wrap(~Id)

ggplot(data = activity_percentage_by_id3_long, aes (x = activity_category, y = percentage_activity)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 270)) +
  facet_wrap(~Id)


### Weight Info ###
weightLogInfo_merged <- read.csv("weightLogInfo_merged.csv")

# number of people who recorded their weight
n_distinct(weightLogInfo_merged$Id)

str(weightLogInfo_merged)

# number of times each person recorded their weight
table(weightLogInfo_merged$Id)


### Read in number of steps by minute, narrow merged ###
minuteStepsNarrow_merged <- read.csv("minuteStepsNarrow_merged.csv")

str(minuteStepsNarrow_merged)

minuteSteps <- minuteStepsNarrow_merged %>%
  mutate(ActivityMinute, mdy_hms(ActivityMinute))

minuteSteps = subset(minuteSteps, select = -c(ActivityMinute))

colnames(minuteSteps)[3] <- "ActivityMinute"

str(minuteSteps)

minuteSteps <- minuteSteps %>%
  mutate(ActivityMinute = hour(ActivityMinute))

str(minuteSteps)

dates2 <- hour(minuteSteps$ActivityMinute)

head(dates2)

n_distinct(dates2)

dates <- as.POSIXct(minuteSteps$ActivityMinute, format = "%H")
head(dates)

dates <- hour(dates)

n_distinct(dates)

typeof(dates)
#time <- format(minuteStepsNarrow_merged$ActivityMinute, format = "%H:%M:%S")

print(time)

minuteStepsNarrow_merged$time <- as.ITime(minuteStepsNarrow_merged$ActivityMinute)

str(minuteStepsNarrow_merged)

typeof(minuteSteps$ActivityMinute)

#ggplot(data = minuteSteps, aes (x = ActivityMinute)) +
#  geom_histogram(stat = "identity")+
 # facet_wrap(~Id)

#ggplot(data = minuteSteps, aes (x = ActivityMinute, y = Steps)) +
 # geom_bar()

minuteStepsSummary <- minuteSteps %>%
  group_by(Id, ActivityMinute) %>%
  summarize(TotalSteps = sum(Steps, na.rm = TRUE))

str(minuteStepsSummary)

#ggplot(data = summary_stats_by_id2, aes (x = activity, y = average)) +
  #geom_bar(stat = "identity") +
  #theme(axis.text.x = element_text(angle = 315)) +
  #facet_wrap(~Id)

ggplot(data = minuteStepsSummary, aes (x = ActivityMinute, y = TotalSteps)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Id)

total_steps_by_hour <- minuteSteps %>%
  group_by(ActivityMinute) %>%
  summarize(totalSteps = sum(Steps, na.rm = TRUE))

ggplot(data = total_steps_by_hour, aes (x = ActivityMinute, y = totalSteps)) +
  geom_col() +
  scale_y_continuous(breaks=c(0,100000,200000,300000,400000,500000, 600000, 700000))