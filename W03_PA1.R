downloadACSData <- function() {
    ## setwd("/_Data/Research/JHU-DataMining/Assignments/03-CleaningData")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(fileUrl, destfile="acs06.csv", method="curl")
    acs06 <- read.csv2("acs06.csv")
    subset(acs06, VAL>1000000)
}
    
readACSData <- function() {
    ##fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
    ##download.file(fileUrl, destfile="acs06_2.csv", method="curl")
    
    acs06 <- fread("acs06_2.csv", sep=",")
    acs06
}

timeACSData <- function() {
    DT <- readACSData()
    cat(sprintf("\"%f\" \"%f\" \"%f\" \"%f\" \"%f\" \"%f\" \n", 
    system.time(tapply(DT$pwgtp15,DT$SEX,mean))[3],
    system.time(DT[,mean(pwgtp15),by=SEX])[3],
    system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))[3],
    system.time(mean(DT$pwgtp15,by=DT$SEX)),
    system.time({rowMeans(DT[DT$SEX==1,pwgtp15]); rowMeans(DT)[DT$SEX==2]})[3],
    system.time({mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)})[3]
    ))
    
}

## Downloads file and returns file name.
downloadNGAData <- function() {
    ## install.packages("xlsx")
    ## library(xlsx)
    fileName <- "ngaData.xlsx"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
    download.file(fileUrl, destfile=fileName, method="curl")
    fileName
}

readNGAFile <- function(fileNm) {
    dat <- read.xlsx(fileNm, 1, rowIndex=18:23, colIndex=7:15)
    sum(dat$Zip*dat$Ext,na.rm=T) 
}

downloadRestauraunts <- function() {
    fileName <- "baltimoreRes.xml"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml "
    download.file(fileUrl, destfile=fileName, method="curl")
    fileName
}

readRestaurants <- function(fileNm) {
    xmlParse(fileNm)
}

