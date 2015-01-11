##
## plot4.R
##
## Written by Richard Sobota as part of progrmming assignment 
## in Exploratory Data Analysis course.
##
## Function uses packages dplyr and lubridate.
##

plot4 <- function() {
    ## read data from file
    csv.data <- read.csv("household_power_consumption.txt",
                         sep=";",
                         na.strings=c("?"),
                         stringsAsFactors=FALSE)
    
    
    ## use PNG file as graphics device    
    png(file = "plot4.png")

    ## use multiple plots
    par(mfrow=c(2,2))
    
    ## convert data to table
    tbl_df(csv.data) %>%

        ##convert date from string to Date
        mutate(Date = dmy(Date)) %>%
        
        ## select required interval
        filter(Date >= dmy("01/02/2007"), Date <= dmy("02/02/2007")) %>%

        ## add datetime
        mutate(datetime = Date + hms(Time)) %>%

        ## convert data to numbers where needed
        mutate(Global_active_power = as.numeric(Global_active_power),
               Sub_metering_1 = as.numeric(Sub_metering_1),
               Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
        
        with( {
            ## draw first plot (Global active power)
            plot(datetime,
                 Global_active_power,
                 type="l",
                 main="",
                 ylab="Global Active Power",
                 xlab="")
    
            ## draw second plot (Voltage)
            plot(datetime,
                 Voltage,
                 type="l",
                 main="",
                 ylab="Voltage")

            ## draw third plot (Sub meterings)
            plot(datetime,
                 Sub_metering_1,
                 type="l",
                 xlab="",
                 ylab="Energy sub metering",
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
            
            ## draw legend for sub meterings
            legend("topright",
                   c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
                   col=c("black", "red", "blue"),
                   lty=c(1, 1, 1),
                   bty="n")
            
            ## draw fourth plot (Global reactive power)
            plot(datetime,
                 Global_reactive_power,
                 type="l",
                 main="",
                 ylab="Global_reactive_power")
        })
    
    dev.off()
}