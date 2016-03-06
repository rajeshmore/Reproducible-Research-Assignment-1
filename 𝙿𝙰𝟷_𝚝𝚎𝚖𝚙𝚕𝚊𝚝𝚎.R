##
## Objective : 
##

library(lattice)
# 1. Loading and preprocessing the data
# Read raw data into input_raw_data varable
# Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
# Process/transform the data (if necessary) into a format suitable for your analysis
input_raw_data <- read.csv("activity.csv",header = TRUE,sep = ",")
input_raw_data$date <- as.Date(input_raw_data$date, "%Y-%m-%d")
input_raw_data_new <- input_raw_data
summary(input_raw_data)

# 2. What is mean total number of steps taken per day?
# Calculate the total number of steps taken per day. Get total steps per day - using tapply (group by date and find sum for steps  )
totstepsbydate <- tapply(input_raw_data$steps, input_raw_data$date, sum)

# Check bar plot - barplot(totstepsbydate)
#Plot histogram
hist(totstepsbydate,  xlab = "Steps Per Day", ylab = "Interval", main = "Steps per day")

# remove missing values (remove .NA)
totstepsbydate <- totstepsbydate[!is.na(totstepsbydate)]
hist(totstepsbydate,  xlab = "Steps Per Day", ylab = "Interval", main = "Steps per day")
# Calculate and report the mean and median of the total number of steps taken per day
# Find mean and median value
meanvalue <- mean(totstepsbydate)
meanvalue
medianvalue <- median(totstepsbydate)
medianvalue

# 3. What is the average daily activity pattern?
# To find average get mean 
meansteps <- tapply(input_raw_data$steps, input_raw_data$interval, mean,na.rm = TRUE)
# Plot line graph for Average daily activity pattern
plot(row.names(meansteps), meansteps, type = "l", xlab = "5 min interval", 
 ylab = "Average steps/day", main = "Average Daily Activity Pattern", col = "dark blue", lwd = 2 )

mean(meansteps)
# 4. Imputing missing values
# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with 𝙽𝙰s)
# Use complete.cases - it return a logical vector indicating which cases are complete, i.e., have no missing values.
# get NA set with !
nrow(input_raw_data_new[!complete.cases(input_raw_data_new),])

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.

for (i in 1:dim(input_raw_data_new)[1]) {
   ifelse (is.na(input_raw_data_new$steps[i]) ,   input_raw_data_new$steps[i] <- mean(meansteps) ,NA)
  #ifelse (is.na(input_raw_data_new$steps[i]) ,   input_raw_data_new$steps[i] <- (ifelse (is.na(meansteps[i]),meanvalue,meansteps[i])) ,NA)
  }

nrow(input_raw_data[!complete.cases(input_raw_data),])
nrow(input_raw_data_new[!complete.cases(input_raw_data_new),])

#####
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

totstepsbydate_new <- tapply(input_raw_data_new$steps, input_raw_data_new$date, sum)

hist(totstepsbydate_new,  xlab = "Steps Per Day", ylab = "Interval", main = "Steps per day")
# Calculate and report the mean and median of the total number of steps taken per day
# Find mean and median value
meanvalue_new <- mean(totstepsbydate_new)
meanvalue
meanvalue_new
medianvalue_new <- median(totstepsbydate_new)
medianvalue
medianvalue_new

#Are there differences in activity patterns between weekdays and weekends?
# Check weekday vs weekend

day <- weekdays(input_raw_data_new$date)
week_weekend <- vector()
for (i in 1:nrow(input_raw_data_new)) {
  if (day[i] == "Saturday") {
    week_weekend[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    week_weekend[i] <- "Weekend"
  } else {
    week_weekend[i] <- "Weekday"
  }
}

# Add daylevel to input_raw_data_new
input_raw_data_new$week_weekend <- week_weekend
input_raw_data_new$week_weekend <- factor(input_raw_data_new$week_weekend)

# split data and find mean steps for interval/day
stepsperday <- aggregate(steps ~ interval + week_weekend, data = input_raw_data_new, mean)
names(stepsperday) <- c("interval", "week_weekend", "steps")
# Plot week vs weekend chart
xyplot(steps ~ interval | week_weekend, stepsperday, type = "l", layout = c(1, 2),xlab = "interval", ylab = "steps")

