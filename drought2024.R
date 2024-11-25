#PART 1: LOAD, PLAY AROUND AND VISUALIZE DATA
data=read.csv("calpmdi23.csv") #add file to blank "data" slot

head(data) #show first few rows of the data
summary(data) #check summary

plot(data$pmdi~data$Year,type="b",cex=0.2, xlab="Year",ylab="PMDI",xaxt="n") #initial plot
axis(1,at=data$Year,labels=data$Year)

#plot lines for each pmdi value, label by color and pattern (preset)
abline(0,0,lty=1,col="black") 
abline(4,0,lty=2,col ="blue")
abline(3,0,lty =3,col="blue")
abline(-3,0,lty =2,col="red")
abline(-4,0,lty =3,col ="red")

pmdi_lr=lm(data$pmdi~data$Year) #linear regression (check whether drought changed over time)
summary(pmdi_lr)

abline(pmdi_lr,lty=1,lwd=3) #add regression line



##PART 2: IF 2024 WAS A WET YEAR
data24wet<-rbind(data,c(2024,4.0)) #add 2024 as wet year

#try the plots and analyses again to see what changes
plot(data24wet$pmdi~data24wet$Year,type="b",cex=0.2,xlab="Year",ylab="PMDI",xaxt="n")
axis(1,at=data24wet$Year,labels=data24wet$Year)

#plot the same preset
abline(0,0,lty =1,col="black") 
abline(4,0,lty=2,col="blue")
abline(3,0,lty=3,col="blue")
abline(-3,0,lty=2,col="red")
abline(-4,0,lty=3,col="red")

#linear regression for model with wet 2024
pmdi_lr_wet=lm(data24wet$pmdi~data24wet$Year) 
summary(pmdi_lr_wet)

abline(pmdi_lr_wet,lty=1,lwd=3) #then regression line 



##PART 3: IF 2024 WAS A DRY YEAR
data24dry<-rbind(data,c(2024,-4.0)) #add 2024 as dry year

#plot again with new 2024 data
plot(data24dry$pmdi~data24dry$Year,type="b",cex=0.2,xlab="Year",ylab="PMDI",xaxt="n")
axis(1,at=data24dry$Year,labels=data24dry$Year)

#plot same preset
abline(0,0,lty=1,col="black")
abline(4,0,lty=2,col="blue")
abline(3,0,lty=3,col="blue")
abline(-3,0,lty=2,col="red")
abline(-4,0,lty=3,col="red")

#linear regression for model with dry 2024
pmdi_lr_dry=lm(data24dry$pmdi~data24dry$Year) 
summary(pmdi_lr_dry)

abline(pmdi_lr_dry,lty=1,lwd=3) #then regression line

