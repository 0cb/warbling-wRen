# Read in Test
# > Test Rscript to read input from .csv
# > Set input as variables/ understand how to read from .csv
#
# Author: 0cb (01.23.18)    Christian Bowman

mydata <- read.table("~/Bioinf0cb/Rscripts/smaRtMath_data/Beef.csv",
                     colClasses = c("character", "character"), 
                     header = TRUE, sep = ",", dec = ".")
head(mydata)
value1 <- mydata$m207
value1          #reads whole col
value2 <- mydata[1,1]
value2          #reads spec val
value3 <- mydata[,2]
value3          #reads spec col + all rows of col
