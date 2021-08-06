# import the dataset and name it "owid covid data 1" (without quotes)
# Mark headers as included.
# Switch NA to #N/A

# Data from database: World Development Indicators	""	""	""	""
# Last Updated: 02/17/2021	""	""	""	""

attach(Gini_Data)

# Individual countries (no regions). Choose the date here. Don't change anything else.
country.name <- Country.Name[1:169]
# If you want to change to daily deaths, thats also possible. Just change from "total_deaths_per_million" to "new_deaths_per_million". Don't change the rest.
country.gini <- X2018..YR2018.[1:169]
df <- data.frame(country.name, country.gini, check.names = TRUE, check.rows = TRUE)
df

#functions. Explore them:
sort(country.gini)
length(country.gini)
summary(country.gini)

#calculating IQR, SD and where is Q3
q3 <- quantile(country.gini, 0.75, na.rm = TRUE)
q3
sd(country.gini, na.rm = TRUE)
IQR(country.gini, na.rm = TRUE)

# Higher than this number will be the outliers.
outlier.limit = q3 + 1.5*IQR(country.gini, na.rm = TRUE)
outlier.limit

# Finding Outliers 
outlier.gini <- boxplot(country.gini, plot=FALSE)$out
outlier.name <- sort(country.name[country.gini > q3 + 1.5*IQR(country.gini, na.rm = TRUE)])
outliers.df <- data.frame(outlier.name, outlier.gini)
outliers.df

# What is the optimal number of breaks? Explore
b = 20

hist(country.gini, breaks = b, plot=FALSE)
hist(country.gini, las=1, col="cyan", breaks = b,
     main = "Histogram of the Gini coefficient",
     xlab = "Gini coefficient (100 means completely unequal. 0 means total equality)", 
     ylab = "No. of countries")


boxplot(country.gini, plot=FALSE)
boxplot(country.gini, las=1, horizontal=TRUE,
        col = "lightblue",
        border = "blue4",
        main = "Boxplot for countries according to Gini coefficient",
        showmeans=TRUE,
        # notch = TRUE,
        xlab = "Covid-19 deaths per million",
        # xlim = range(0:40), yaxs = "i",
        frame = FALSE)
#axis(1, at=seq(0, 40, 4))

summary(country.gini)
outliers.df

# Counts for the frequency table (Chose the same amount of breaks!):
hist(country.gini, breaks = b, plot=FALSE)$counts