#importing the libraries 
#install.packages("tinytex")
library(tinytex)
library(readxl)
library(psych)
library(pastecs)
library(dplyr)
library(tidyverse)
library(ggplot2)
#install.packages("pdftools")
library(pdftools)
library(plyr)
library(openxlsx)
library(data.table)
library(stringr)
library(data.table)
library(caTools)
#Importing our databases 

#Using a loop to load all Firearm trace data from 2014 to 2020
for (x in 14:20){
  #creating a variable dbA to store a year-based names ike firearms_2014, firearms_2015
  dbA <- paste("firearms_20",x,".xlsx", sep = "")  
  #extracting file with the name stored in the variable abover and reading it into the datavase variable
  database <- read_excel(dbA, range = "B2:BE17", col_names = TRUE) 
  #Providing the first column with an identifying name
  colnames(database)[1] <- "Firearm_type"
  #Ensuring last column of all databases is called TOTAL
  colnames(database)[ncol(database)] <- "TOTAL"
  #Sorting out the database in the alphabetical order of firearms 
  database <- database[order(database$Firearm_type),]
  #Identifying each firearm type as either Hand Gun, Long Gun or Others
  database$Category <- c( "Others", "Long Gun","Hand Gun","Others","Hand Gun", "Long Gun", "Hand Gun","Others","Hand Gun","Long Gun","Long Gun","Others","Long Gun","Total", "Others" )
  #Performing Vector Factorisation
  database$Category = factor(database$Category,
                             levels <- c("Long Gun", "Hand Gun", "Others"),
                             labels <- c(0, 1, 2)
  )
  #Create Yearwise dataframes from firearms excels of the year 2014-20
  dbB<- paste("firearms_20",x, sep = "")
  assign(dbB, database)
  #Building a smaller database with the imported datavases 
  dbC <- data.frame(database[, c("Firearm_type", "TOTAL", "Category")])
  #Creating a variable to store the newly created databases
  y <- paste("FASub", x, sep = "")
  assign(y, dbC)
}

#CODE ENDS  

#BUILDING DATAFRAME FOR TIME SERIES PLOTS

#Creating a list of dataframes
Year_data = list(FASub14, FASub15, FASub16, FASub17,
                 FASub18, FASub19, FASub20)

#Initialising a new datframe to contain yearwise data of handguns, long guns and other firearms
Annual_data <-data.frame(matrix(nrow = 7, ncol = 5))
colnames(Annual_data) <- c("Year", "Long_Gun", "Hand_Gun", "Others", "Total" )
Annual_data$Year <- c(2014:2020)

#Building a loop to extract values 
y = 1
#Picks up the first dataframe in our list of dataframes
for (x in Year_data) {
  x <- as.data.frame(x)
  #Stores sum of all values in the database FASub14 where Category is 0 or Long Guns
  Annual_data$Long_Gun[y] <- sum(x[which(x$Category == "0"), 2])
  #Stores sum of all values in the database FASub14 where Category is 1 or Hnd Guns
  Annual_data$Hand_Gun[y] <- sum(x[which(x$Category == "1"), 2])
  #Stores sum of all values in the database FASub14 where Category is 2 or Others
  Annual_data$Others[y] <- sum(x[which(x$Category == "2"), 2])
  #Stores sum of all values in the database FASub14 except the Total row where Category is NA
  Annual_data$Total[y] <- sum(x[which(x$Category == "2" | x$Category == "1" | x$Category == "0"), 2])
  y = y + 1
}

#Calculates the share of HandGuns and Long Guns in the data
Annual_data$sharepc <- (Annual_data$Long_Gun + Annual_data$Hand_Gun)/Annual_data$Total*100

#stores the existing data in '000s for ease of representation 
Annual_data <- mutate(Annual_data,
                      Long_Gun = Long_Gun/1000,
                      Hand_Gun = Hand_Gun/1000,
                      Others = Others/1000,
                      Total = Total/1000
)

#TimeSeries Plot
#Plot of Hand Guns or Long Guns
ggplot(data = Annual_data) + 
  geom_point(aes(x = Year, y = Long_Gun), color = "red") +
  geom_line(aes(x = Year, y = Long_Gun),) +
  geom_point(aes(x = Year, y = Hand_Gun), color = "Blue") +
  geom_line(aes(x = Year, y = Hand_Gun)) +
  ggtitle("Traces per year for Hand Guns (in Blue) and Long Guns (in Red)") +
  xlab("Years")+
  ylab("Number of Traces(in '000s) ")

#Plot of share of Hand Guns or Long Guns
ggplot(data = Annual_data) + 
  geom_point(aes(x = Year, y = sharepc), color = "red") +
  geom_line(aes(x = Year, y = sharepc)) +
  ggtitle("Share of traces per year of Hand Guns and Long Guns ") +
  xlab("Years")+
  ylab("Share of Traces")


#END OF QUESTION 1

#QUESTION 2

#Code to extract data from the NICS pdf file and store it into variable BGC_pdf
BGC_pdf <-pdf_text("NICS_BGChecks.pdf") %>%
  readr::read_lines()
#printing pdf and identifying rows that contain 2019 data
BGC_pdf
#removing all other rows from the data
BGC_PDF <- BGC_pdf[-c(1:5, 7:28, 30:40)] 
#storing headers and 2019 data into another variable
BGC_pdf19 <- BGC_PDF
BGC_pdf19

#Storing 2019 data into another variable after removing additional spaces 
#and separating them using single spaces left 
BG19_data <- BGC_pdf19[2] %>%
  str_squish() %>%
  strsplit(" ")
BG19_data
#Following the same procedure for the month headers 
BGC19_titles <- BGC_pdf19[1] %>%
  str_squish() %>%
  strsplit(" ")
BGC19_titles

#Building dataframes out of the lists extracted above
BGC19_df <- plyr::ldply(BG19_data)
BGC19_df <- BGC19_df[2:13]
BGC19_Mon <- plyr::ldply(BGC19_titles)
BGC19_Mon <- BGC19_Mon[2:13]

#Combining both data frames to create a single data frame
BGC19_df <-rbind(BGC19_df, BGC19_Mon)
#Transposng BGC_19 to represent 2 columns and 12 month-wise rows
BGC19_df <- t(BGC19_df)
#Assigning names to the two columns
colnames(BGC19_df) <- c("BG_Checks", "Months")
BGC19_df <- as.data.frame(BGC19_df)
#Factorising the month variable
BGC19_df$Months <- factor(BGC19_df$Months,
                          levels = month.abb
)


#Building a GGPLOT of the monthly background checks carried out in 2019

ggplot(data = BGC19_df, aes(x = Months, y = BG_Checks, group =1)) + 
  geom_point(aes(x = Months, y = BG_Checks), color = "red")+
  geom_line() +
  ggtitle("Monthly Background Checks in 2019") +
  xlab("Months")+
  ylab("Number of Background Checks")

#END OF QUESTION 2

#QUESTION 3

#LOADING XLSX file into the system 
#and fillng merged cells to avoid blank cells where cells in the excel were merged
crime_rates <- read.xlsx("Crime Rates.xlsx", 
                         startRow = 4, 
                         fillMergedCells = TRUE, 
                         colNames = TRUE)
#creating a database of only those rows where the third column contains value "Rate per 100,000 inhabitamts 
#because that row represents statewise crime rates across the data 
crime_rates <- subset(crime_rates,  X3 == "Rate per 100,000 inhabitants")
#creating a smaller dataframe to only contain state name and crime rates
crime_rates_final <- data.frame(crime_rates$State, crime_rates$Violent.crime1)
crime_rates_final <- rename(crime_rates_final, c("crime_rates.State" = "ATFs_State"))
#remove any number found in the same name 
crime_rates_final$ATFs_State <- gsub('[0-9]', "", crime_rates_final$ATFs_State)

#Transposing the 2019 firearms data 
firearms_2019_trans <- t(firearms_2019)
#Storing statewise total firearm trace data into the dataframe ATF_Traces
ATF_traces <- data.frame(firearms_2019_trans[,14])
#Storing state names in a new variable  
ATFs_State <- row.names(ATF_traces)
#Cleaning the data frame to onlu contain the states and their traces
ATF_traces <- ATF_traces[-c(1, 56:57),]
#Converting list of State Names into a data frame
ATFs_State <- as.data.frame(ATFs_State)
#Cleaning the data frame to only contain State Names
ATFs_State <- ATFs_State[-c(1, 56:57),]
ATF_traces <- as.data.frame(ATF_traces)
ATFs_State <- as.data.frame(ATFs_State)
#Uniquely identifying both databases with a column ID same as their index numbers
ATFs_State$ID <- as.numeric((row.names(ATFs_State)))
ATF_traces$ID <- as.numeric(row.names(ATF_traces))

#Merging rows of both dataframes by unique column ID
ATF_traces <- merge(ATF_traces, ATFs_State, by = "ID")
ATF_traces <- setorder(ATF_traces, ATFs_State)

#Merging the crime rate and ATF Trace data by using the ATFs_State column
ATF_CrimeRate <- merge(ATF_traces, crime_rates_final, by = "ATFs_State")
#Changing data type of crime rate and traces to numeric
ATF_CrimeRate$ATF_traces <- as.numeric(ATF_CrimeRate$ATF_traces)
ATF_CrimeRate$crime_rates.Violent.crime1 <- as.numeric((ATF_CrimeRate$crime_rates.Violent.crime1))
#Calculating log of traces
ATF_CrimeRate$logtraces <- log(ATF_CrimeRate$ATF_traces)

set.seed(123)
#Performing Linear Regression 

regressor = lm(formula = crime_rates.Violent.crime1 ~ logtraces, 
               data = ATF_CrimeRate)
summary(regressor)

#Creating a gglot with log(traces) and crime rate as well as the regression line calculated
intercept <- coefficients(regressor)[1]
slope <- coefficients(regressor)[2]
print(intercept)


ggplot(ATF_CrimeRate,  aes(logtraces, crime_rates.Violent.crime1)) +
  geom_point()+
  geom_abline(slope = slope, intercept = intercept, color = "red")  


ggplot(ATF_CrimeRate,  aes(ATF_traces, crime_rates.Violent.crime1)) +
  geom_point()  
#END OF QUESTION 3
