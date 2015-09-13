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
