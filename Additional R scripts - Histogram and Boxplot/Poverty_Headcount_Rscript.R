
attach(Poverty_Headcount_Data)

# Individual countries (no regions). Choose the date here. Don't change anything else.
country.name <- Country.Name[1:169]
# If you want to change to daily deaths, thats also possible. Just change from "total_deaths_per_million" to "new_deaths_per_million". Don't change the rest.
country.poverty <- X2018..YR2018.[1:169]
df <- data.frame(country.name, country.poverty, check.names = TRUE, check.rows = TRUE)
df

#functions. Explore them:
sort(country.poverty)
length(country.poverty)
summary(country.poverty)

#calculating IQR, SD and where is Q3
q3 <- quantile(country.poverty, 0.75, na.rm = TRUE)
q3
sd(country.poverty, na.rm = TRUE)
IQR(country.poverty, na.rm = TRUE)

# Higher than this number will be the outliers.
outlier.limit = q3 + 1.5*IQR(country.poverty, na.rm = TRUE)
outlier.limit

# Finding Outliers 
outlier.poverty <- boxplot(country.poverty, plot=FALSE)$out
outlier.name <- sort(country.name[country.poverty > q3 + 1.5*IQR(country.poverty, na.rm = TRUE)])
outliers.df <- data.frame(outlier.name, outlier.poverty)
outliers.df

# What is the optimal number of breaks? Explore
b = 40

hist(country.poverty, breaks = b, plot=FALSE)
hist(country.poverty, las=1, col="cyan", breaks = b,
     main = "Histogram of the poverty headcount (% of the population)",
     xlab = "% of the population living below US$1.90 (2011 PPPs)", 
     ylab = "No. of countries")


boxplot(country.poverty, plot=FALSE)
boxplot(country.poverty, las=1, horizontal=TRUE,
        col = "lightblue",
        border = "blue4",
        main = "Boxplot for Poverty Headcount (% of the population)",
        showmeans=TRUE,
        # notch = TRUE,
        xlab = "% of the population living below US$1.90 (2011 PPPs)",
        # xlim = range(0:40), yaxs = "i",
        frame = FALSE)
#axis(1, at=seq(0, 40, 4))

summary(country.poverty)
outliers.df

# Counts for the frequency table (Chose the same amount of breaks!):
hist(country.poverty, breaks = b, plot=FALSE)$counts
sum(hist(country.poverty, breaks = b, plot=FALSE)$counts)