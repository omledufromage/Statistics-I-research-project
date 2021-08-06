# import the dataset and name it "2016-CO2-emissions-per-capita" (without quotes)
# Mark headers as included.
# Switch NA to #N/A

# Data from database: World Development Indicators	""	""	""	""
# Last Updated: 02/17/2021	""	""	""	""

attach(`2016-CO2-emissions-per-capita`)

# Individual countries go up until row 217:
Country.Name[104] <- "Korea, Dem. Peoples Rep."
country.name <- Country.Name[1:217]
country.emission <- X2016..YR2016.[1:217]
df <- data.frame(country.name, country.emission, check.names = TRUE, check.rows = TRUE)
df

sort(country.emission)
length(country.emission)
#Ln
summary(log(country.emission), na.rm = TRUE)
summary(country.emission, na.rm = TRUE)

q3 <- quantile(country.emission, 0.75, na.rm = TRUE)
q1 <- quantile(log(country.emission), 0.25, na.rm = TRUE)
q3
sd(country.emission, na.rm = TRUE)
IQR(country.emission, na.rm = TRUE)

outlier.limit = q3 + 1.5*IQR(country.emission, na.rm = TRUE)
outlier.limit

outlier.emission <- boxplot(log(country.emission), plot=FALSE)$out
outlier.name <- sort(country.name[log(country.emission) < q1 - 1.5*IQR(log(country.emission), na.rm = TRUE)])
outliers.df <- data.frame(outlier.name, outlier.emission)
outliers.df

Chebyshev = 3*sd(country.emission, na.rm = TRUE) + summary(country.emission, na.rm = TRUE)[4]
sort(country.name[country.emission > Chebyshev])
sort(country.name[country.emission > quantile(country.emission, 0.25, na.rm = TRUE) & country.emission < quantile(country.emission, 0.75, na.rm = TRUE)])
sort(country.name[country.emission < quantile(country.emission, 0.25, na.rm = TRUE)])
sort(country.name[country.emission > quantile(country.emission, 0.75, na.rm = TRUE)])

# What is the optimal number of breaks? 
hist(country.emission, right=TRUE, breaks = 20, plot=FALSE)
hist(country.emission, right=TRUE, las=1, col="cyan", breaks = 20,
     main = "",
     xlab = expression("CO"[2]*" emissions (metric tons per capita)"), 
     ylab = "No. of countries")

# Still didn't explore the boxplot.

boxplot(country.emission, plot=FALSE)
boxplot(country.emission, las=1, horizontal=TRUE,
        col = "lavender",
        border = "darkblue",
        main = "",
        showmeans=TRUE,
        # notch = TRUE,
        xlab = expression("CO"[2]*" emissions (metric tons per capita)"),
        # xlim = range(0:40), yaxs = "i",
        frame = FALSE)
#axis(1, at=seq(0, 40, 5))

# What is the optimal number of breaks? 
hist(log(country.emission), right=TRUE, breaks = 20, plot=FALSE)
hist(log(country.emission), right=TRUE, las=1, col="cyan", breaks = 20,
     main = "",
     xlab = expression("CO"[2]*" emissions (metric tons per capita) - Logarithmic scale"), 
     ylab = "No. of countries")

boxplot(log(country.emission), plot=FALSE)
boxplot(log(country.emission), las=1, horizontal=TRUE,
        col = "lavender",
        border = "darkblue",
        main = "",
        showmeans=TRUE,
        # notch = TRUE,
        xlab = expression("CO"[2]*" emissions (metric tons per capita) - Logarithmic scale"),
        # xlim = range(0:40), yaxs = "i",
        frame = FALSE)
#axis(1, at=seq(0, 40, 5))

summary(country.emission)
outliers.df