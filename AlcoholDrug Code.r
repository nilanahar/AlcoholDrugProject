library(mosaic)

# Read in the comma seperated dataset
CADUMS<-read.csv(file = "ProjectData.csv", header = TRUE, sep = ",")

# Attach the data frame
attach(CADUMS)

# Look at column names
names(CADUMS)

# Look at frequency table of alc10 (Drinking alcohol over the past week)
table(alc10)

# Look at frequency table of how soon people smoke after waking up
# Note: 1 = Within 5 minutes, 2 = 6 to 30 minutes, 3 = 31 to 60 minutes
#       4 = More than 60 minutes
table(smok4)

# Remove unnecessary responses and clean up the data for alcohol consumers
alcCleaned <- CADUMS[which(alc10 <= 1),]
# Remove unnecessary responses and clean up the data for smoking consumers
smokCleaned <- CADUMS[which(smok4 <= 4),]


# Detach the old data frame
detach(CADUMS)

# Attach the alcohol cleaned data frame
attach(alcCleaned)

# Look at new cleaned distribution table
table(alc10)

# Detach the cleaned alcahol consumer data frame
detach(alcCleaned)

# Attach the cleaned smoking data frame
attach(smokCleaned)

# Look at new cleaned distribution table
table(smok4)

## The data is now cleaned and we will do out first question ##

# Detach the cleaned smoking data frame
detach(smokCleaned)

# Attach the cleaned alcahol consumer data frame
attach(alcCleaned)

# In our first point, we will see if there's an association between
# whether or not people had a drink the past week and their sex

# Using these bar plots we can easily see the amount of males and females that had drinks

# within the past week
drinkM <- (alcCleaned[alcCleaned[, 2] == 1,])
drinkF <- (alcCleaned[alcCleaned[, 2] == 2,])

barplot(table(drinkM$alc10),
        main = "Drinks had by Males",
        xlab = "Had a drink within the last week",
        ylab = "Counts",
        col = rainbow(2),
        names.arg = c("No", "Yes"))

barplot(table(drinkF$alc10),
        main = "Drinks had by Females",
        xlab = "Had a drink within the last week",
        ylab = "Counts",
        col = rainbow(2),
        names.arg = c("No", "Yes"))

# First, we'll create a better meaningful name
drinkAlc <- alc10

# Likewise, for the sex variable, we want to attach value labels
# 1 = Male, 2 = Female
sexAlc<-factor(sex,
               levels = c(1,2), 
               labels = c("Male", "Female"))

# Here is the frequency distribution of the table for sex
addmargins(table(sexAlc))

# Like before, for the alcohol variable, we want to attach value labels
# 0 = No, 1 = Yes
drinkAlc<-factor(drinkAlc,
                 levels = c(0,1), 
                 labels = c("No", "Yes"))

# Here is the frequency distribution of the table for drinking alcohol

# Next, we create a contingency tabl
Contingency.Table.drinkAlc <- table(sexAlc,drinkAlc)

# Add margins to the contingency table.
addmargins(Contingency.Table.drinkAlc)

# To check for association, we will conduct a chi-squared test of independence 
chisq.test(Contingency.Table.drinkAlc)

# Next, we'll create a 95% CI (with continuity correction) for the difference between the two proportions.
prop.test(c(723,1228),c(3009,3683))

# This can be interpreted as we're 95% confident that the percent reported of 
# people that don't drink for females is between 7.1% and 11.5% higher than for the males

# Conversely, we can conduct it for people that do drink
prop.test(c(2286,2455),c(3009,3683))

# Like before, this can be interpreted as We're 95% confident that the percent reported of
# people that do drink for males is between 7.1% and 11.5% higher than for the females

detach (alcCleaned)

### We are now done with the first question ###

# For our second question, we'll compare the mean initial smoking time between males and
# females
attach(smokCleaned)

# Like before, we'll create a better meaningful name
smokTime <- smok4

# Likewise, for the sex variable, We want to attach value labels
# 1 = Male, 2 = Female
sexSmok<-factor(sex,
                levels = c(1,2), 
                labels = c("Male", "Female"))

# Here is the frequency distribution of the table for sex
addmargins(table(sexSmok))

# This is used to simply obtain some summary statistics for the time of respondents first
# smoke by the sex of the respondent
favstats(smokTime ~ sexSmok)

# We can create a boxplot of the time of first smoke after waking up
# A boxplot is great to use in this scenario because we get a good indication of how the
# values are spread out
boxplot(smokTime ~ sexSmok,
        ylab = "Time of first smoke after waking up",
        xlab = "Sex of respondent",
        main = "Smoking times based on sex",
        col = rainbow(2))

# Just by looking at the boxplot, they look very identical so it's safe to assume the
# means are about the same regardless of sex We can confirm this further by doing a 
# t-test

t.test(smokTime ~ sexSmok)

### We are now done with the second question ###
