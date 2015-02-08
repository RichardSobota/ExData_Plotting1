##
## plot2.R
##
## Written by Richard Sobota as part of programming assignment 
## in Exploratory Data Analysis course.
##
## Function uses packages dplyr and lubridate.
##

plot2 <- function() {
    ## read data from file
    csv.data <- read.csv("household_power_consumption.txt",
                         sep=";",
                         na.strings=c("?"),
                         stringsAsFactors=FALSE)
    
    
    ## use PNG file as graphics device    
    png(file = "plot2.png")
    
    ## convert data to table
    tbl_df(csv.data) %>%

        ## convert date from string to Date
        mutate(Date = dmy(Date)) %>%
        
        ## select required interval
        filter(Date >= dmy("01/02/2007"), Date <= dmy("02/02/2007")) %>%
        
        ## add datetime
        mutate(datetime = Date + hms(Time)) %>%

        ## convert data to numbers where needed
        mutate(Global_active_power = as.numeric(Global_active_power)) %>%
        
        ## draw plot
        with(plot(datetime,
                  Global_active_power,
                  type="l",
                  main="",
                  ylab="Global Active Power (kilowatts)",
                  xlab=""))
    
    dev.off()
}