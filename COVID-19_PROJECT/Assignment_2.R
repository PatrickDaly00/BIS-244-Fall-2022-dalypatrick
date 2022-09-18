#Assignment_2 
#Patrick Daly
#9/17/2022

# Clear out Console and Environment
rm(list=ls(all=TRUE))
cat("\014")

# Let's read in the us-counties file from covid-19-data

# We'll use package "readr", which is part of the tidyverse
library(tidyverse)

# Storing the path of the current working directory
Temp <- getwd()

# Switching the working directory to the covid-19-data subfolder
setwd("./covid-19-data/")

# Reading the us.states.csv in as a data frame
STATES <- read_csv("us-states.csv")

# Switching the working directory back to the project folder
setwd(Temp)

## Alternative way to access subfolders
library(here)
STATES <- read_csv(here("covid-19-data","us-states.csv"))

# Examining the data
View(STATES)

# Using filter()to get just PA
PENNSYLVANIA <- filter(STATES, state=="Pennsylvania" 
                      )
View(PENNSYLVANIA)

# Set n to legth of data set
n <- length(PENNSYLVANIA$date)

# Initialize new variable in data frame
PENNSYLVANIA$incr_cases <- 1
PENNSYLVANIA$incr_deaths <- 1

View(PENNSYLVANIA)

# Calculate values for other than first row using FOR loop

for (i in 2:n) {
  PENNSYLVANIA$incr_cases[i] <- (PENNSYLVANIA$cases[i]-PENNSYLVANIA$cases[i-1]) 
}

View(PENNSYLVANIA)

# Plot what we've got

p <- ggplot(data = PENNSYLVANIA,
            mapping = aes(x = date, y = incr_cases))

p + geom_point() +
  labs(x = "Dates", y = "Incremental Cases",
       title = "COVID-19 Cases in PA",
       subtitle = "Data points are incremental new confirmed cases",
       caption = "Source: NY Times")

# Let's replace 0 values with NA using IF..ELSE statement

for (i in 1:n) {
  if (PENNSYLVANIA$incr_cases[i]==0) {
    PENNSYLVANIA$incr_cases[i] <- NA
  } else {}
}

View(PENNSYLVANIA)

# Replot

p <- ggplot(data = PENNSYLVANIA,
            mapping = aes(x = date, y = incr_cases))

p + geom_point() +
  labs(x = "Dates", y = "Incremental Cases",
       title = "COVID-19 Cases in PA",
       subtitle = "Data points are incremental new confirmed cases",
       caption = "Source: NY Times")

# Remember, we have replaced some value of incr_cases with NA, so...

mean(PENNSYLVANIA$incr_cases)

# There IS a workaround for some commands, such as mean()
mean(PENNSYLVANIA$incr_cases, na.rm=TRUE)
meancases <- mean(PENNSYLVANIA$incr_cases, na.rm=TRUE)

# Initialize seperate vectors for cases above and below average
PENNSYLVANIA$above_cases <- 0
PENNSYLVANIA$below_cases <- 0

# But there is no easy workaround for logical tests, such below where
# we have "NA" in a variable in a logical test inside an IF() statement

for (i in 1:n) {
  if(PENNSYLVANIA$incr_cases[i]>=meancases) {
    PENNSYLVANIA$above_cases[i] <- PENNSYLVANIA$incr_cases[i]
  } else {
    PENNSYLVANIA$below_cases[i] <- PENNSYLVANIA$incr_cases[i]
  }
}

# Return incr_cases to what it was when we first computed it

PENNSYLVANIA$incr_cases <- 1
for (i in 2:n) {
  PENNSYLVANIA$incr_cases[i] <- (PENNSYLVANIA$cases[i]-PENNSYLVANIA$cases[i-1]) 
}

View(PENNSYLVANIA)

# Assign values to above_cases and below_cases based on whether incr_cases
# is greater than or less than average

for (i in 1:n) {
  if(PENNSYLVANIA$incr_cases[i]>=meancases) {
    PENNSYLVANIA$above_cases[i] <- PENNSYLVANIA$incr_cases[i]
  } else {
    PENNSYLVANIA$below_cases[i] <- PENNSYLVANIA$incr_cases[i]
  }
}

View(PENNSYLVANIA)

# Plot what we've got

p = ggplot() + 
  geom_point(data = PENNSYLVANIA, aes(x = date, y = above_cases), color = "red") +
  geom_point(data = PENNSYLVANIA, aes(x = date, y = below_cases), color = "green") +
  labs(x = "Dates", y = "Incremental Cases",
       title = "Incremental COVID-19 Cases in PA",
       subtitle = "Red = Above Average, Green = Below Average",
       caption = "Source: NY Times")
p

# Now, the values on the x-axis are really ugly, so, replace 0 values with "NA"

for (i in 1:n) {
  if(PENNSYLVANIA$above_cases[i]==0) {
    PENNSYLVANIA$above_cases[i] <- NA
  } else {}
  if(PENNSYLVANIA$below_cases[i]==0) {
    PENNSYLVANIA$below_cases[i] <- NA
  } else {}
}

# Now, our plot will look much better

p = ggplot() + 
  geom_point(data = PENNSYLVANIA, aes(x = date, y = above_cases), color = "red") +
  geom_point(data = PENNSYLVANIA, aes(x = date, y = below_cases), color = "green") +
  labs(x = "Dates", y = "Incremental Cases",
       title = "Incremental COVID-19 Cases in PA",
       subtitle = "Red = Above Average, Green = Below Average",
       caption = "Source: NY Times")
p





#look into incre_deaths



# Calculate values for other than first row using FOR loop

for (i in 2:n) {
  PENNSYLVANIA$incr_deaths[i] <- (PENNSYLVANIA$deaths[i]-PENNSYLVANIA$deaths[i-1]) 
}

View(PENNSYLVANIA)

# Plot what we've got

p <- ggplot(data = PENNSYLVANIA,
            mapping = aes(x = date, y = incr_deaths))

p + geom_point() +
  labs(x = "Dates", y = "Incremental Deaths",
       title = "COVID-19 Deaths in PA",
       subtitle = "Data points are incremental new confirmed deaths",
       caption = "Source: NY Times")

# Let's replace 0 values with NA using IF..ELSE statement

for (i in 1:n) {
  if (PENNSYLVANIA$incr_deaths[i]==0) {
    PENNSYLVANIA$incr_deaths[i] <- NA
  } else {}
}

View(PENNSYLVANIA)

# Replot

p <- ggplot(data = PENNSYLVANIA,
            mapping = aes(x = date, y = incr_deaths))

p + geom_point() +
  labs(x = "Dates", y = "Incremental Deaths",
       title = "COVID-19 Deaths in PA",
       subtitle = "Data points are incremental new confirmed deaths",
       caption = "Source: NY Times")

# Remember, we have replaced some value of incr_deaths with NA, so...

mean(PENNSYLVANIA$incr_deaths)

# There IS a workaround for some commands, such as mean()
mean(PENNSYLVANIA$incr_deaths, na.rm=TRUE)
meandeaths <- mean(PENNSYLVANIA$incr_deaths, na.rm=TRUE)

# Initialize seperate vectors for deaths above and below average
PENNSYLVANIA$above_deaths <- 0
PENNSYLVANIA$below_deaths <- 0

# But there is no easy workaround for logical tests, such below where
# we have "NA" in a variable in a logical test inside an IF() statement

for (i in 1:n) {
  if(PENNSYLVANIA$incr_deaths[i]>=meandeaths) {
    PENNSYLVANIA$above_deaths[i] <- PENNSYLVANIA$incr_deaths[i]
  } else {
    PENNSYLVANIA$below_deaths[i] <- PENNSYLVANIA$incr_deaths[i]
  }
}

# Return incr_deaths to what it was when we first computed it

PENNSYLVANIA$incr_deaths <- 1
for (i in 2:n) {
  PENNSYLVANIA$incr_deaths[i] <- (PENNSYLVANIA$deaths[i]-PENNSYLVANIA$deaths[i-1]) 
}

View(PENNSYLVANIA)

# Assign values to above_deaths and below_deaths based on whether incr_deaths
# is greater than or less than average

for (i in 1:n) {
  if(PENNSYLVANIA$incr_deaths[i]>=meandeaths) {
    PENNSYLVANIA$above_deaths[i] <- PENNSYLVANIA$incr_deaths[i]
  } else {
    PENNSYLVANIA$below_deaths[i] <- PENNSYLVANIA$incr_deaths[i]
  }
}

View(PENNSYLVANIA)

# Plot what we've got

p = ggplot() + 
  geom_point(data = PENNSYLVANIA, aes(x = date, y = above_deaths), color = "red") +
  geom_point(data = PENNSYLVANIA, aes(x = date, y = below_deaths), color = "green") +
  labs(x = "Dates", y = "Incremental Deaths",
       title = "Incremental COVID-19 Deaths in PA",
       subtitle = "Red = Above Average, Green = Below Average",
       caption = "Source: NY Times")
p

# Now, the values on the x-axis are really ugly, so, replace 0 values with "NA"

for (i in 1:n) {
  if(PENNSYLVANIA$above_deaths[i]==0) {
    PENNSYLVANIA$above_deaths[i] <- NA
  } else {}
  if(PENNSYLVANIA$below_deaths[i]==0) {
    PENNSYLVANIA$below_deaths[i] <- NA
  } else {}
}

# Now, our plot will look much better

p = ggplot() + 
  geom_point(data = PENNSYLVANIA, aes(x = date, y = above_deaths), color = "red") +
  geom_point(data = PENNSYLVANIA, aes(x = date, y = below_deaths), color = "green") +
  labs(x = "Dates", y = "Incremental Deaths",
       title = "Incremental COVID-19 Deaths in PA",
       subtitle = "Red = Above Average, Green = Below Average",
       caption = "Source: NY Times")
p


#print out sd of deaths and cases
sd(PENNSYLVANIA$incr_cases)
sd(PENNSYLVANIA$incr_deaths)

