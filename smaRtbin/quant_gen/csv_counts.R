# .csv data counter
# > Determine SUM(col#)
# > For .csv with large amounts of data
#
# Author: 0cb (01.20.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/Beef.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

c1 <- colnames(data)[1]
c2 <- colnames(data)[2]
c3 <- colnames(data)[3]
c4 <- colnames(data)[4]
c5 <- colnames(data)[5]

# counts # of occurences per value per column
count <- lapply(data, table)
count

