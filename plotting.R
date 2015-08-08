##Generic Initiation of plot functions === Start
a<-find.package("dplyr", lib.loc = NULL, quiet = T,
                verbose = getOption("verbose"));
#If dplyr is not found, install it, since it's needed.
if(length(a)==0){
        install.packages("dplyr")   
}
library("dplyr");

dest <- getwd();
dest <- paste(dest,"/household_power_consumption.txt", sep="");
#Load the whole dataset, change the Date variable to a valid one and subset
M<-as_data_frame(read.csv2(dest));
M$Date<-as.Date(M$Date, format="%d/%m/%Y");
Mset<-filter(group_by(M,Date), Date>="2007-02-01" && Date<="2007-02-02");
Mset["POSIX"]<-NA;
Mset$POSIX<-as.POSIXlt(paste(Mset$Date, Mset$Time, sep=" "));
##Generic Initiation of plot functions === END
#Beguin plot1
plot1 <- function(){
        par(mar=c(4,4,2,1), mfrow= c(1,1), bg="transparent");
        png(
                "plot1.png",
                width     = 480,
                height    = 480,
                units     = "px",
        );
        hist(as.numeric(as.character(Mset$Global_active_power)), main="Global Active Power", xlab = "Global Active Power (kilowatts)", ylab="Frequency", font.lab=1, font.main=2, cex.lab=0.9, cex.main=1.05, col="red")
        dev.off();
};
plot2 <- function(){

        par(mar=c(4,4,2,1), mfrow= c(1,1), bg="transparent");
        png(
                "plot2.png",
                width     = 480,
                height    = 480,
                units     = "px",
        );
        plot(Mset$POSIX,as.numeric(as.character(Mset$Global_active_power)), main=NA, xlab = NA, ylab = "Global Active Power (kilowatts)", font.lab=1, font.main=2, cex.lab=0.9, cex.main=1.05, col="transparent");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Global_active_power)), type="l");
        dev.off();
}
plot3 <- function(){

        
        png(
                "plot3.png",
                width     = 480,
                height    = 480,
                units     = "px",
        );
        par(mar=c(4,4,2,1), mfrow= c(1,1), bg="transparent");
        plot(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_1)), main=NA, xlab = NA, ylab = "Energy sub Metering", font.lab=1, font.main=2, cex.lab=0.9, cex.main=1.05, col="transparent");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_1)), type="l", col="black");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_2)), type="l", col="red");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_3)), type="l", col="blue");
        legend("topright", pch="-", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"));
        dev.off();
}
plot4 <- function(){
        
       png(
                "plot4.png",
                width     = 480,
                height    = 480,
                units     = "px",
        );
        par(mar=c(4,4,2,1), mfrow= c(2,2), bg="transparent");
       
        ##First Plot
        plot(Mset$POSIX,as.numeric(as.character(Mset$Global_active_power)), main=NA, xlab = NA, ylab = "Global Active Power", font.lab=1, font.main=2, cex.lab=0.9, cex.main=1.05, col="transparent");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Global_active_power)), type="l");
        
        ##Second Plot
        plot(Mset$POSIX,as.numeric(as.character(Mset$Voltage)), main=NA, xlab = "datetime", ylab = "Voltage", font.lab=1, font.main=2, cex.lab=0.9, cex.main=1.05, col="transparent");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Voltage)), type="l", col="black");
        
        ##Third Plot
        plot(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_1)), main=NA, xlab = NA, ylab = "Energy sub Metering", font.lab=1, font.main=2, cex.lab=0.9, cex.main=1.05, col="transparent");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_1)), type="l", col="black");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_2)), type="l", col="red");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Sub_metering_3)), type="l", col="blue");
        legend("topright", pch="-", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"));
        
        ##Fourth Plot
        plot(Mset$POSIX,as.numeric(as.character(Mset$Global_reactive_power)), main=NA, xlab = "datetime", ylab = "Global_reactive_power", font.lab=1, font.main=2, cex.lab=0.9, cex.main=1.05, col="transparent");
        lines(Mset$POSIX,as.numeric(as.character(Mset$Global_reactive_power)), type="l", col="black");
        
        dev.off();
}