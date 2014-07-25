setwd("~/OECD/Decomposition")

#All values in domestic curencies

aus <- read.csv("Australia.csv")
aus$Fuel.Excise <- rowSums(aus[,c(5,8)])
aus$Vehicle.Registration <- rep(0,nrow(aus))
aus$Road.Use <- aus[,7]
aus$Tolls <- rep(0,nrow(aus))
aus$Air <- rep(0,nrow(aus))
aus$Maritime <- rep(0,nrow(aus))
aus$Other <- rowSums(aus[,c(4,6)])
aus$Total.Revenue <- aus[,3]


ausc <- subset(aus, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
ausc$TR.Change <- c(0,diff(ausc$Total.Revenue)) 
ausc$Transport.Revenue <- rowSums(ausc[,c(3:9)])
ausc$TransR.Change <- c(0,diff(ausc$Transport.Revenue)) 

can <- read.csv("Canada.csv")
can$Fuel.Excise <- rowSums(can[, c(20,23)])
can$Vehicle.Registration <- rep(0,nrow(can))
can$Road.Use <- rowSums(can[ , c(12:13,15:16,21,25:26)])
can$Tolls <- rep(0,nrow(can))
can$Air <- rowSums(can[, c(2, 3, 4, 5)])
can$Maritime <- rowSums(can[, c(7:11)])
can$Other <- rowSums(can[, c(16,19,22)])
can$Total.Revenue <- can[,29]

canc <- subset(can, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
canc$TR.Change <- c(0,diff(canc$Total.Revenue)) 
canc$Transport.Revenue <- rowSums(canc[,c(3:9)])
canc$TransR.Change <- c(0,diff(canc$Transport.Revenue)) 


ire <- read.csv("Ireland.csv")
ire$Fuel.Excise <- rowSums(ire[, c(4,5)])
ire$Vehicle.Registration <- ire[,3]
ire$Road.Use <- ire[,7]
ire$Tolls <- ire[,8]
ire$Air <- rep(0,nrow(ire))
ire$Maritime <- rep(0,nrow(ire))
ire$Other <- rep(0,nrow(ire))
ire$Total.Revenue <- ire[,2]

irec <- subset(ire, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
irec <- subset(irec, Year > 2002)
irec$TR.Change <- c(0,diff(irec$Total.Revenue)) 
irec$Transport.Revenue <- rowSums(irec[,c(3:9)])
irec$TransR.Change <- c(0,diff(irec$Transport.Revenue)) 


nzl <- read.csv("New Zealand.csv")
nzl$Fuel.Excise <- nzl[,2]
nzl$Vehicle.Registration <- nzl[,3]
nzl$Road.Use <- nzl[,4]
nzl$Tolls <- nzl[,5]
nzl$Air <- rep(0,nrow(nzl))
nzl$Maritime <- nzl[,6]
nzl$Other <- rep(0,nrow(nzl))
nzl$Total.Revenue <- nzl[,8]

nzlc <- subset(nzl, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
nzlc$TR.Change <- c(0,diff(nzlc$Total.Revenue)) 
nzlc$Transport.Revenue <- rowSums(nzlc[,c(3:9)])
nzlc$TransR.Change <- c(0,diff(nzlc$Transport.Revenue)) 


swe <- read.csv("Sweden.csv")
swe$Fuel.Excise <- swe[, 7]
swe$Vehicle.Registration <- rep(0,nrow(swe))
swe$Road.Use <- rowSums(swe[,c(3,4)])
swe$Tolls <- swe[,5]
swe$Air <- rep(0,nrow(swe))
swe$Maritime <- rep(0,nrow(swe))
swe$Other <- swe[, 6]
swe$Total.Revenue <- swe[,2]

swec <- subset(swe, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
swec$TR.Change <- c(0,diff(swec$Total.Revenue)) 
swec$Transport.Revenue <- rowSums(swec[,c(3:9)])
swec$TransR.Change <- c(0,diff(swec$Transport.Revenue)) 


uk <- read.csv("uk.csv")
uk$Fuel.Excise <- uk[, 2]
uk$Vehicle.Registration <- rep(0,nrow(uk))
uk$Road.Use <- uk[,4]
uk$Tolls <- uk[,5]
uk$Air <- uk[,3]
uk$Maritime <- rep(0,nrow(uk))
uk$Other <- rep(0,nrow(uk))
uk$Total.Revenue <- uk[,7]

ukc <- subset(uk, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
ukc$TR.Change <- c(0,diff(ukc$Total.Revenue)) 
ukc$Transport.Revenue <- rowSums(ukc[,c(3:9)])
ukc$TransR.Change <- c(0,diff(ukc$Transport.Revenue)) 

fin <- read.csv("finland.csv")
fin$Fuel.Excise <- fin[, 3]
fin$Vehicle.Registration <- fin[,4]
fin$Road.Use <- fin[,5]
fin$Tolls <- rep(0,nrow(fin))
fin$Air <- rep(0,nrow(fin))
fin$Maritime <- fin[,8]
fin$Other <- fin[,9]
fin$Total.Revenue <- fin[,10]

finc <- subset(fin, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
finc$TR.Change <- c(0,diff(finc$Total.Revenue)) 
finc$Transport.Revenue <- rowSums(finc[,c(3:9)])
finc$TransR.Change <- c(0,diff(finc$Transport.Revenue))                

usa <- read.csv("usa.csv")
usa$Fuel.Excise <- usa[, 3]
usa$Vehicle.Registration <- rep(0,nrow(usa))
usa$Road.Use <- rep(0,nrow(usa))
usa$Tolls <- usa[,4]
usa$Air <- usa[,5]
usa$Maritime <- usa[,8]
usa$Other <- rowSums(usa[, c(6, 7, 9, 10)])
usa$Total.Revenue <- usa[,12]

usac <- subset(usa, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
usac$TR.Change <- c(0,diff(usac$Total.Revenue)) 
usac$Transport.Revenue <- rowSums(usac[,c(3:9)])
usac$TransR.Change <- c(0,diff(usac$Transport.Revenue)) 


den <- read.csv("denmark.csv")
den$Fuel.Excise <- den[, 7]
den$Vehicle.Registration <- rowSums(den[,c(8, 9)])
den$Road.Use <- rowSums(den[,c(4,5)])
den$Tolls <- rep(0,nrow(den))
den$Air <- rep(0,nrow(den))
den$Maritime <- den[,3]
den$Other <- den[, 6]
den$Total.Revenue <- den[,2]

denc <- subset(den, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
denc$TR.Change <- c(0,diff(denc$Total.Revenue)) 
denc$Transport.Revenue <- rowSums(denc[,c(3:9)])
denc$TransR.Change <- c(0,diff(denc$Transport.Revenue)) 

ned <- read.csv("netherlands.csv")
nedr <- read.csv("netherlandsrevenue.csv")
ned <- merge(ned, nedr)
ned$Heavy.Vehicles <- ned[,23] - ned[,22]
  
ned$Fuel.Excise <- rowSums(ned[,c(13,30)])
ned$Vehicle.Registration <- ned[,34]
ned$Road.Use <- rowSums(ned[,c(22, 55)])
ned$Tolls <- rep(0,nrow(ned))
ned$Air <- rep(0,nrow(ned))
ned$Maritime <- rep(0,nrow(ned))
ned$Other <- rep(0,nrow(ned))
ned$Total.Revenue <- ned[,53]
ned$Country <- rep("Netherlands", nrow(ned))

nedc <- subset(ned, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
nedc$TR.Change <- c(0,diff(nedc$Total.Revenue)) 
nedc$Transport.Revenue <- rowSums(nedc[,c(3:9)])
nedc$TransR.Change <- c(0,diff(nedc$Transport.Revenue)) 

nor <- read.csv("norway.csv")
nor$Country <- rep("Norway", nrow(nor))
colnames(nor)[1] <- "Year"
nor$Fuel.Excise <- rowSums(nor[, c(5,6)])
nor$Vehicle.Registration <- nor[,4]
nor$Road.Use <- rowSums(nor[,c(2, 3)])
nor$Tolls <- rep(0,nrow(nor))
nor$Air <- nor[,7]
nor$Maritime <- rep(0, nrow(nor))
nor$Other <- rep(0, nrow(nor))
nor$Total.Revenue <- nor[,8]

norc <- subset(nor, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
norc$TR.Change <- c(0,diff(norc$Total.Revenue)) 
norc$Transport.Revenue <- rowSums(norc[,c(3:9)])
norc$TransR.Change <- c(0,diff(norc$Transport.Revenue)) 

est <- read.csv("estonia.csv")
est$Country <- rep("Estonia", nrow(est))
est$Fuel.Excise <- est[,3]
est$Vehicle.Registration <- rowSums(est[,c(4, 5)])
est$Road.Use <- rowSums(est[,c(6,7)])
est$Tolls <- rep(0,nrow(est))
est$Air <- rep(0,nrow(est))
est$Maritime <- est[,8]
est$Other <- rep(0, nrow(est))
est$Total.Revenue <- est[,2]

estc <- subset(est, select = c(Year, Country, Fuel.Excise, Vehicle.Registration, Road.Use, Tolls, Air, Maritime, Other, Total.Revenue))
estc$TR.Change <- c(0,diff(estc$Total.Revenue)) 
estc$Transport.Revenue <- rowSums(estc[,c(3:9)])
estc$TransR.Change <- c(0,diff(estc$Transport.Revenue)) 
               
oecd <- rbind(ausc, canc, irec, nzlc, swec, ukc, usac, finc, denc, nedc, norc, estc)

oecd$Share.of.Rev <- oecd$Transport.Revenue/oecd$Total.Revenue

oecd[,1] <- paste("01-01-",as.numeric(oecd[,1]), sep = "") 
oecd[,1] <- as.Date(oecd[,1],'%d-%m-%Y' )

oecd[,1] <- as.POSIXlt(oecd[,1])$year+1900 

library(ggplot2)
library(scales)
p <- qplot(data = subset(oecd, Year > 2003 & Year < 2013), y = Share.of.Rev, x = Year, colour = Country) + geom_line()
q <- ggplot(data=subset(oecd, Year >2003 & Year < 2013), aes(Year, Share.of.Rev, colour = Country)) +
  geom_line() +
  scale_y_continuous(labels = percent)
  
oecd$Change.Ratio <- oecd$TransR.Change/oecd$TR.Change

l <- ggplot(data=subset(oecd, Year >2003 & Year < 2013), aes(Year, Change.Ratio, colour = Country)) +
  geom_line() +
  scale_y_continuous(labels = percent)

library(reshape2)
library(manipulate)

oecdm <- oecd[,1:9]
oecdm <- melt(oecdm, id = c("Country", "Year"))


#Slider to select year, all countries for which data available in given year.
OECDOT <- function(yr){
  oecdm2 <- subset(oecdm, Year == yr)
  ggplot(oecdm2, aes(x = Country, y = value, fill = variable))+
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent_format()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.05))
}
manipulate(OECDOT(yr), yr = slider(2002, 2013, step = 1, label = "Year"))

x <- as.list(as.character(unique(oecd$Country)))
x <- sort.list(x, method="quick")


#Picker to select country, all years for which data available.
CountryOT <- function(cy){
  oecdm3 <- subset(oecdm, Country == cy)
  ggplot(oecdm3, aes(x = Year, y = value, fill = variable))+
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent_format()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.05))
}
ByCountry <- manipulate(CountryOT(cy), cy = picker(x, label = "Country"))

CountryOTnom <- function(cy){
  oecdm4 <- subset(oecdm, Country == cy)
  ggplot(oecdm4, aes(x = Year, y = value, fill = variable))+
    geom_bar(position = "stack", stat = "identity") +
    scale_y_continuous() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -0.05))
}
ByCountrynom <- manipulate(CountryOTnom(cy), cy = picker(x, label = "Country"))

#write.csv(oecd, "oecd.csv")
