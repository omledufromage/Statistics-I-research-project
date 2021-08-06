# A macro-economic model of the open economy in the long run: Switzerland, 1950-2019 
# The data are from https://data.oecd.org > Finance > Conversion rates > Exchange rates
# OECD (2020), Exchange rates (indicator). doi: 10.1787/037ed317-en (Accessed on 17 April 2020)
# OECD only has *annual* exchange rate data.
# The data file is:      OECD-exchange-rate.csv 
# Download the data file from the Canvas page of the course:
# Inleiding tot de Macro-economie / Introduction to Macroeconomics
#     Module: A macroeconomic model of the open economy
# Import the data set: File > Import Dataset > From Text (base)...

# Check which values the qualitative variables take:
levels(OECD.exchange.rate$LOCATION)      # countries (ISO codes)
levels(OECD.exchange.rate$INDICATOR)     # "EXCH": exchange rate
levels(OECD.exchange.rate$SUBJECT)       # "TOT"
levels(OECD.exchange.rate$MEASURE)       # "NATUSD": national currency units per USD
levels(OECD.exchange.rate$FREQUENCY)     # "A": Annual (OECD only has *annual* exchange rate data)

# The three-letter country code for Switzerland is CHE (https://unstats.un.org/unsd/tradekb/Knowledgebase/Country-Code)
# Subset the OECD data to LOCATION == "CHE":
CHE.exchange.rate   <- subset(OECD.exchange.rate, LOCATION == "CHE" & SUBJECT == "TOT" & MEASURE == "NATUSD"  & FREQUENCY == "A" )
head(CHE.exchange.rate) # display the first couple of lines of the data set

# Create the time series:
CHE.exchange.rate.ts <- ts(CHE.exchange.rate$Value, start=1950,frequency=1)  # only annual data of exchange rate
CHE.exchange.rate.ts   # display the time series

# Plot the time series of CHE.exchange.rate:
plot(CHE.exchange.rate.ts,frame.plot=FALSE,xlim=c(1950,2020), lwd=1,col="blue",xlab="", ylab="Exchange rate (Swiss francs per US dollar)",las=1)
# (Saved as CHF-per-USD-exchange-rate.pdf)
