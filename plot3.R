##
## plot3.R
##
## Written by Richard Sobota as part of programming assignment 
## in Exploratory Data Analysis course.
##
## Function uses packages dplyr and lubridate.
##

plot3 <- function() {
    ## read data from file
    csv.data <- read.csv("household_power_consumption.txt",
                         sep=";",
                         na.strings=c("?"),
                         stringsAsFactors=FALSE)
    
    
    ## use PNG file as graphics device    
    png(file = "plot3.png")
    
    ## convert data to table
    tbl_df(csv.data) %>%

        ##convert date from string to Date
        mutate(Date = dmy(Date)) %>%
        
        ## select required interval
        filter(Date >= dmy("01/02/2007"), Date <= dmy("02/02/2007")) %>%

        ## add datetime
        mutate(datetime = Date + hms(Time)) %>%

        ## convert data to numbers where needed
        mutate(Sub_metering_1 = as.numeric(Sub_metering_1),
               Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
        
        ## draw plot
        with({
            plot(datetime,
                 Sub_metering_1,
                 type="l",
                 xlab="",
                 ylab='Energy sub metering',
                 col="black")
            
            lines(datetime,
                 Sub_metering_2,
                 type="l",
                 xlab="",
                 ylab="",
                 col="red")
            
            lines(datetime,
                 Sub_metering_3,
                 type="l",
                 xlab="",
                 ylab="",
                 col="blue")
            
            ## draw legend
            legend("topright",
                   c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
                   col=c("black", "red", "blue"),
                   lty=c(1, 1, 1))
        })
    
    dev.off()
}