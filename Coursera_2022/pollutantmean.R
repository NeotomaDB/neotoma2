# Coursera assignment 1
#Final code
setwd("/Users/donaldcharles/Dropbox/Computer/R - Coursera 2022")
sitedata <- read.csv ("specdata/001.csv", header = TRUE)
mean_sulf <- mean(sitedata$sulfate, na.rm = TRUE)

# Reading in multiple files from specdata directory

##Read files named 001.csv, 002.csv, etc.
filenames <- list.files(
  path="/Users/donaldcharles/Dropbox/Computer/R - Coursera 2022/specdata",
                        pattern="*.csv")

##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,3)

###Load all files
for(i in names){
  filepath <- file.path(
    "/Users/donaldcharles/Dropbox/Computer/R - Coursera 2022/specdata",
    paste(i,".csv",sep=""))
  assign(i, read.delim(filepath,
                       colClasses=c("character","factor",rep("numeric",4)),
                       sep = "\t"))
}read.delim()





for(i in 1:332 {                              # Head of for-loop
                                     # Read and store data frames
         read.csv(paste0("specdata",
                          specdata[i])))
#
sitesulf <- sitedata$sulfate
mean_sulf <- mean(sitesulf, na.rm = TRUEme)
sitesulf
# figure out how to substutute id for the first part of the file name.
# subset the table to include only a particular pollutant
# calculate mean of the subset, not including the null values
#  read about the MEAN function; how to specify no null values
# figure out how to calculate means for multiple data files for loop?

#pollutantmean <- function (directory, pollutant, id = 1:332){NA = FALSE}
# directory is where the data files are located; 
#   specify name of directory within the working directory
# pollutant is the name of the pollutant, in quotes
# id specifies the csv file associated with a monitoring site of the same number
pollutantmean("specdata", "sulfate", 1:10)
#----------------
For completeness here is my final answer for loading any number of (tab) delimited files, in this case with 6 columns of data each where column 1 is characters, 2 is factor, and remainder numeric:
  
  ##Read files named xyz1111.csv, xyz2222.csv, etc.
  filenames <- list.files(path="../Data/original_data",
                          pattern="xyz+.*csv")

##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,7)

###Load all files
for(i in names){
  filepath <- file.path("../Data/original_data/",paste(i,".csv",sep=""))
  assign(i, read.delim(filepath,
                       colClasses=c("character","factor",rep("numeric",4)),
                       sep = "\t"))
}


#-----------------



print(R.version.string)
## [1] "R version 3.4.0 (2017-04-21)"
source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064128
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706047
pollutantmean("specdata", "nitrate", 23)
## [1] 1.280833