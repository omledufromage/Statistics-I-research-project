# import the dataset and name it "owid covid data 1" (without quotes)
# Mark headers as included.
# Switch NA to #N/A

# Data from database: World Development Indicators	""	""	""	""
# Last Updated: 02/17/2021	""	""	""	""

attach(owid covid data 1)

day <- "2020-07-18"
# Individual countries (no regions). Choose the date here. Don't change anything else.
country.name <- location[date==day & location != "World" & location != "Africa" & location != "Europe" & location != "Oceania" & location != "North America" & location != "International" & location != "European Union" & location != "Asia" & location != "South America"]

# If you want to change to daily deaths, thats also possible. Just change from "total_deaths_per_million" to "new_deaths_per_million". Don't change the rest.
country.deathrate <- total_deaths_per_million[date==day & location != "World" & location != "Africa" & location != "Europe" & location != "Oceania" & location != "North America" & location != "International" & location != "European Union" & location != "Asia" & location != "South America"] 
# Creating a dataframe. Just better to save the data on. Not necessary though. 
df <- data.frame(country.name, country.deathrate, check.names = TRUE, check.rows = TRUE)
df

#functions. Explore them:
sort(country.deathrate)
length(country.deathrate)
summary(country.deathrate)

#calculating IQR, SD and where is Q3
q3 <- quantile(country.deathrate, 0.75, na.rm = TRUE)
q3
sd(country.deathrate, na.rm = TRUE)
IQR(country.deathrate, na.rm = TRUE)

# Higher than this number will be the outliers.
outlier.limit = q3 + 1.5*IQR(country.deathrate, na.rm = TRUE)
outlier.limit

# Finding Outliers 
outlier.deathrate <- boxplot(country.deathrate, plot=FALSE)$out
outlier.name <- sort(country.name[country.deathrate > q3 + 1.5*IQR(country.deathrate, na.rm = TRUE)])
outliers.df <- data.frame(outlier.name, outlier.deathrate)
outliers.df

# What is the optimal number of breaks? Explore
b = 20

hist(country.deathrate, breaks = b, plot=FALSE)
hist(country.deathrate, las=1, col="cyan", breaks = b,
     main = "Histogram of the Covid-19 Deaths per million",
     xlab = "Number of deaths per million", 
     ylab = "No. of countries")


boxplot(country.deathrate, plot=FALSE)
boxplot(country.deathrate, las=1, horizontal=TRUE,
        col = "lightblue",
        border = "blue4",
        main = "Boxplot of the Covid-19 deaths per million",
        showmeans=TRUE,
        # notch = TRUE,
        xlab = "Covid-19 deaths per million",
        # xlim = range(0:40), yaxs = "i",
        frame = FALSE)
#axis(1, at=seq(0, 40, 4))

summary(country.deathrate)
outliers.df

# Counts for the frequency table (Chose the same amount of breaks!):
hist(country.deathrate, breaks = b, plot=FALSE)$counts