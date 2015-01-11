##
## plot1.R
##
## Written by Richard Sobota as part of progrmming assignment 
## in Exploratory Data Analysis course.
##
## Function uses packages dplyr and lubridate.
##

plot1 <- function() {
    ## read data from file
    csv.data <- read.csv("household_power_consumption.txt",
                         sep=";",
                         na.strings=c("?"),
                         stringsAsFactors=FALSE)

    ## use PNG file as graphics device    
    png(file = "plot1.png")
    
    ## convert data to table
    tbl_df(csv.data) %>%
    
        ##convert date from string to Date
        mutate(Date = dmy(Date)) %>%
        
        ## select required interval
        filter(Date >= dmy("01/02/2007"), Date <= dmy("02/02/2007")) %>%
        
        ## convert data to numbers
        mutate(Global_active_power = as.numeric(Global_active_power)) %>%
        
        ## draw plot
        with(hist(Global_active_power,
                  main="Global Active Power",
                  xlab="Global Active Power (kilowatts)",
                  col="red"))
    
    dev.off()
}