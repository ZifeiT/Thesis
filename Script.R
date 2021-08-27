install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(sp)
library(dplyr)
library(fields) #for colour palette
library(rgdal)
library(inlabru)
library(GADMTools)
library(sf)
library(car)

pm2.5_hourly_with_missing =  read.csv("C:/Users/Tzf/Desktop/RProject/26_stations_hourly+Dundee=28.csv",
                                      header = TRUE,
                                      sep = ",",
                                      row.names = NULL,
                                      na.strings=c("No data")
)

pm2.5_hourly_with_missing$Date <- strptime(pm2.5_hourly_with_missing$Date,format="%d/%m/%Y")


pm2.5_daily_with_missing = pm2.5_hourly_with_missing %>% 
  group_by(Date) %>% 
  summarise_at(vars(East.Dunbartonshire.Kirkintilloch,Edinburgh.St.John.s.Road,Edinburgh.St.Leonards,Falkirk.Banknock,Falkirk.West.Bridge.Street,Fife.Cupar,Fife.Dunfermline,Fife.Kirkcaldy,Fife.Rosyth,Glasgow.Byres.Road,Glasgow.Dumbarton.Road,Glasgow.High.Street,Glasgow.Nithsdale.Road,Glasgow.Townhead,Glasgow.Waulkmillglen.Reservoir,Grangemouth,N.Lanarkshire.Chapelhall,Renfrewshire.Johnston,South.Lanarkshire.Cambuslang,South.Lanarkshire.East.Kilbride,South.Lanarkshire.Hamilton,South.Lanarkshire.Lanark,South.Lanarkshire.Rutherglen,South.Lanarkshire.Uddingston,West.Lothian.Broxburn,West.Lothian.Linlithgow.High.Street.2,Dundee.Lochee.Road,Dundee.Mains.Loan), function(x) ifelse(sum(is.na(x))<7, mean(x, na.rm = T), NA))

Pm2.5_daily_with_missing = as.data.frame(pm2.5_daily_with_missing)


library(VIM)
matrixplot(Pm2.5_daily_with_missing,xlab = "28 Monitoring Sites",labels = FALSE)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Pm2.5_daily_with_missing,2,pMiss)

#delete sites with more than 10% missing values
x = Pm2.5_daily_with_missing %>%
  select(-c(East.Dunbartonshire.Kirkintilloch,
            Falkirk.West.Bridge.Street,
            Glasgow.Nithsdale.Road,
            Dundee.Lochee.Road))

#imputation
library(mice)
miceData <- mice(x[,2:25],m=5,maxit=50,meth='pmm',seed=500)
micedata <- complete(miceData,2)
densityplot(miceData)

newcolumn <- x$Date
complete_data <- cbind(Date=newcolumn,micedata)

#15 stations for estimation
FIFstations <- complete_data[, -which(colnames(complete_data) %in% c("Edinburgh.St.John.s.Road",
                                                                     "Fife.Cupar",
                                                                     "Fife.Dunfermline",
                                                                     "Grangemouth",
                                                                     "N.Lanarkshire.Chapelhall",
                                                                     "Renfrewshire.Johnston",
                                                                     "South.Lanarkshire.East.Kilbride",
                                                                     "South.Lanarkshire.Lanark",
                                                                     "South.Lanarkshire.Rutherglen"))]


#9 stations for validation
NINEstations <- complete_data[, -which(colnames(complete_data) %in% c("Edinburgh.St.Leonards",
                                                                      "Falkirk.Banknock",
                                                                      "Fife.Kirkcaldy",
                                                                      "Fife.Rosyth",
                                                                      "Glasgow.Byres.Road",
                                                                      "Glasgow.Dumbarton.Road",
                                                                      "Glasgow.High.Street",
                                                                      "Glasgow.Townhead",
                                                                      "Glasgow.Waulkmillglen.Reservoir",
                                                                      "South.Lanarkshire.Cambuslang",
                                                                      "South.Lanarkshire.Hamilton",
                                                                      "South.Lanarkshire.Uddingston",
                                                                      "West.Lothian.Broxburn",
                                                                      "West.Lothian.Linlithgow.High.Street.2",
                                                                      "Dundee.Mains.Loan"))]



##--- Latitude and Longitude for the 15 stations and the 9 validation stations
lalo15 <-read.csv("C:/Users/Tzf/Desktop/lalo15.csv")
lalo9 <-read.csv("C:/Users/Tzf/Desktop/lalo9.csv")
names(lalo15)[names(lalo15) == "﻿Latitude"] <- "Latitude"
names(lalo9)[names(lalo9) == "﻿Latitude"] <- "Latitude"

##--- Complete PM2.5 measurements and covariate information 
##--- for the 15 stations and 365 days
Scotland_data = read.table("C:/Users/Tzf/Desktop/output_15stations(1).csv",
                           header=TRUE,sep=",")

##--- for the 9 validation stations and 365 days
Scotland_data_validation = read.table("C:/Users/Tzf/Desktop/output_9stations(1).csv",
                                      header=TRUE,sep=",")

n_stations = length(lalo15$Longitude) ## 15
n_stations_val = length(lalo9$Longitude) ## 9
n_data = length(Scotland_data$Station) ## 5475
n_days = as.integer(n_data/n_stations) ## 365

# log transformtion
Scotland_data$logPM2.5 = log(2+Scotland_data$PM2.5)
Scotland_data_validation$logPM2.5 = log(2+Scotland_data_validation$PM2.5)

Scotland_data$time = rep(1:n_days, each = n_stations)
Scotland_data_validation$time = rep(1:n_days, each = n_stations_val)
Scotland_data$Station.ID = rep(1:15, each = 1)
Scotland_data_validation$Station.ID = rep(1:9, each = 1)
Scotland_data$Longitude = lalo15[rep(1:15,365),2]
Scotland_data$Latitude = lalo15[rep(1:15,365),1]
Scotland_data_validation$Longitude = lalo9[rep(1:9,365),2]
Scotland_data_validation$Latitude = lalo9[rep(1:9,365),1]


##--- Primary Data exploration
##---Box plot
#Box plot of PM2.5 concentration for each month
FIFstations$Month <- substr(FIFstations$Date,6,7)
Pm2.5_monthly <- aggregate(FIFstations[,2:16],by=list(Month=FIFstations$Month),FUN=mean)
pm2.5_15 <- melt(Pm2.5_monthly, id.vars = c(colnames(Pm2.5_monthly)[1]))

monthly_mean <- aggregate(pm2.5_15[,3],by=list(Month=pm2.5_15$Month),FUN=mean)

ggplot(pm2.5_15[,c(1,3)], aes(x=Month, y=value)) + 
  geom_boxplot(outlier.colour="grey", outlier.shape=1,outlier.alpha=0.6)+
  labs(x="Month", y = "PM2.5 concentration(ug/m^3)")+
  geom_point(data=monthly_mean,aes(x=Month, y=x),col="purple")



#Box plot shows the distribution of PM2.5 for each station
m <- matrix(nr=365,nc=15)
for (j in 1:15) {
  m[,j] <- subset(Scotland_data[,c(16,13)],Station.ID==j)[,2]
  
}
#the PM2.5 concentration mean for each station
m_means <- colMeans(m, na.rm = TRUE)
Station.ID <- c(1:15) 
means <- data.frame(Station.ID=Station.ID,PM2.5=m_means)

ggplot(Scotland_data[,c(13,16)], aes(x=Station.ID, y=PM2.5,group=Station.ID)) + 
  geom_boxplot(outlier.colour="grey", outlier.shape=1,outlier.alpha=0.6)+
  labs(x="Station ID", y = "PM2.5 concentration(ug/m^3)")+
  geom_point(data=means,aes(x=Station.ID, y=PM2.5),col="orange")

#Find outliers for each station according to the box plot 
#and replace them with the median
for (i in 1:15) {
  Scotland_data[which(Scotland_data$Station.ID==i&
                        Scotland_data$time%in%
                        which(m[,i]%in%boxplot.stats(m[,i])$out)),13]<-median(m[,i])
}

#histogram and QQplot for the PM2.5 data after getting rid of outliers
par(mfrow=c(1,2))
hist(Scotland_data[,13],breaks = seq(0,17,1),freq = FALSE,xlab = "PM2.5 concentration",main = "Histogram of PM2.5 data")
lines(density(Scotland_data[,13]),col = "blue",lwd = 1)
qqnorm(Scotland_data[,13],col="blue")
qqline(Scotland_data[,13],col="blue")

#histogram and QQplot for the log PM2.5 data
par(mfrow=c(1,2))
hist(Scotland_data[,14],breaks = seq(0,4,0.2),freq = FALSE,xlab = "logPM2.5 concentration",main = "Histogram of logPM2.5")
lines(density(Scotland_data[,14]),col = "blue",lwd = 1)
qqnorm(Scotland_data[,14],col="blue")
qqline(Scotland_data[,14],col="blue")

#Box-Cox
bc <- Scotland_data[,13]
p=powerTransform(Scotland_data[,13])
bc=bcPower(bc,p$lambda)

par(mfrow=c(1,2))
hist(bc,breaks = seq(-1,3.5,0.2),freq = FALSE,xlab = "PM2.5 concentration after Box-Cox",main = "Histogram of bcPM2.5")
lines(density(bc),col = "blue",lwd = 1)
qqnorm(bc,col="blue")
qqline(bc,col="blue")

#Because Box-Cox transformation performs better than simple
#logarithm transformation, replace logPM2.5 with bcPM2.5 in
#the following analysis
colnames(Scotland_data)[14]<-"bcPM2.5"
Scotland_data[,14]<-data.frame(bc)

#repeat the operation to the validation stations
#get rid of outliers
q1<-quantile(Scotland_data_validation[,13], 0.01)        
q99<-quantile(Scotland_data_validation[,13], 0.99)       
Scotland_data_validation[Scotland_data_validation[,13]<q1,]$PM2.5<-q1
Scotland_data_validation[Scotland_data_validation[,13]>q99,]$PM2.5<-q99


#Box-Cox
bc_val <- Scotland_data_validation[,13]
p_val=powerTransform(bc_val)
bc_val=bcPower(bc_val,p_val$lambda)

par(mfrow=c(1,2))
hist(bc_val,breaks = seq(0,3,0.2),freq = FALSE,xlab = "PM2.5 concentration after Box-Cox",main = "Histogram of bcPM2.5")
lines(density(bc_val),col = "blue",lwd = 1)
qqnorm(bc_val,col="blue")
qqline(bc_val,col="blue")

colnames(Scotland_data_validation)[14]<-"bcPM2.5"
Scotland_data_validation[,14]<-data.frame(bc_val)






##--- Time series
#Time series plot over 365 days
par(mfrow=c(2,2))
#City of Edinburgh
ts1 <- ts(data=m[,1], start=1, end=365, frequency=1)
plot(ts1,xlab="Time",ylab="PM2.5 concentration",
     main="in-situ measurements in City of Edinburgh")

#Falkirk
ts2 <- ts(data=m[,2], start=1, end=365, frequency=1)
plot(ts2,xlab="Time",ylab="PM2.5 concentration",
     main="in-situ measurements in Falkirk")

#Fife
ts3 <- ts(data=m[,3], start=1, end=365, frequency=1)
ts4 <- ts(data=m[,4], start=1, end=365, frequency=1)
ts.plot(ts3,ts4,gpars=list(col=c("deepskyblue4","orangered")),
        ylab="PM2.5 concentration",
        main="in-situ measurements in Fife")

#City of Glasgow
ts5 <- ts(data=m[,5], start=1, end=365, frequency=1)
ts6 <- ts(data=m[,6], start=1, end=365, frequency=1)
ts7 <- ts(data=m[,7], start=1, end=365, frequency=1)
ts8 <- ts(data=m[,8], start=1, end=365, frequency=1)
ts9 <- ts(data=m[,9], start=1, end=365, frequency=1)
ts.plot(ts5,ts6,ts7,ts8,ts9,gpars=list(col=c(
  "blue4","violetred","orangered","skyblue","indianred")),
  ylab="PM2.5 concentration",
  main="in-situ measurements in City of Glasgow")

#South Lanarkshire
ts10 <- ts(data=m[,10], start=1, end=365, frequency=1)
ts11 <- ts(data=m[,11], start=1, end=365, frequency=1)
ts12 <- ts(data=m[,12], start=1, end=365, frequency=1)
ts.plot(ts10,ts11,ts12,gpars=list(col=c(
  "orangered","skyblue","violetred3")),
  ylab="PM2.5 concentration",
  main="in-situ measurements in South Lanarkshire")

#West Lothian
ts13 <- ts(data=m[,13], start=1, end=365, frequency=1)
ts14 <- ts(data=m[,14], start=1, end=365, frequency=1)
ts.plot(ts13,ts14,gpars=list(col=c("deepskyblue4","orangered")),
        ylab="PM2.5 concentration",
        main="in-situ measurements in West Lothian")

#Dundee City
ts15 <- ts(data=m[,15], start=1, end=365, frequency=1)
plot(ts15,xlab="Time",ylab="PM2.5 concentration",
     main="in-situ measurements in Dundee City")

#Overall
ts.plot(ts1,ts2,ts3,ts4,ts5,ts6,ts7,ts8,
        ts9,ts10,ts11,ts12,ts13,ts14,ts15,
        col="grey",
        ylab="PM2.5 concentration",
        main="in-situ measurements at all monitoring stations")


#acf of bcPM2.5 time series
q <- matrix(nr=365,nc=15)
for (j in 1:15) {
  q[,j] <- subset(Scotland_data[,c(16,14)],Station.ID==j)[,2]
  
}
ts.bc <- ts(data=q, start=1, end=365, frequency=1)
lag <- c(0:25)
acf.bc.matrix <- matrix(nr=26,nc=15)
for (i in 1:15) {
  acf.bc.matrix[,i]<-acf(ts.bc[,i],plot=FALSE)[["acf"]]
}
acf.bc<-data.frame(lag,acf.bc.matrix)
acf.bc <- melt(acf.bc, id.vars = c(colnames(acf.bc)[1]))
#95% Confidence interval(blue bands)
clim.acf<-qnorm((1+0.95)/2)/sqrt(365)
ggplot(acf.bc[,c(1,3)], aes(x=lag, y=value,group=lag)) + 
  geom_boxplot(outlier.alpha = 0)+
  labs(x="Lag", y = "Sample Autocorrelation")+
  geom_hline(yintercept = c(clim.acf,-clim.acf),color="blue",
             linetype=2)


#pacf of bcPM2.5 time series
lag.pacf<-c(1:25)
pacf.bc.matrix <- matrix(nr=25,nc=15)
for (i in 1:15) {
  pacf.bc.matrix[,i]<-pacf(ts.bc[,i],plot=FALSE)[["acf"]]
}
pacf.bc<-data.frame(lag.pacf,pacf.bc.matrix)
pacf.bc <- melt(pacf.bc, id.vars = c(colnames(pacf.bc)[1]))
#95% Confidence interval(blue bands)
clim.pacf<-qnorm((1+0.95)/2)/sqrt(365)
ggplot(pacf.bc[,c(1,3)], aes(x=lag.pacf, y=value,group=lag.pacf)) + 
  geom_boxplot(outlier.alpha = 0)+
  labs(x="Lag", y = "Sample Partial Autocorrelation")+
  geom_hline(yintercept = c(clim.pacf,-clim.pacf),color="blue",
             linetype=2)



##--- Explanatory Data Exploration

#BLH,WS,BLD and TP tend to be skewed, 
#so Box-Cox transformation is needed

##--- 9 validation stations
#for BLH
bc_val_BLH <- Scotland_data_validation[,8]
p_val_BLH=powerTransform(bc_val_BLH)
bc_val_BLH=bcPower(bc_val_BLH,p_val_BLH$lambda)

par(mfrow=c(1,2))
hist(bc_val_BLH)
qqnorm(bc_val_BLH,col="blue")
qqline(bc_val_BLH,col="blue")

Scotland_data_validation$bcBLH<-bc_val_BLH

#for WS
bc_val_WS <- Scotland_data_validation[,9]
p_val_WS=powerTransform(bc_val_WS)
bc_val_WS=bcPower(bc_val_WS,p_val_WS$lambda)

par(mfrow=c(1,2))
hist(bc_val_WS)
qqnorm(bc_val_WS,col="blue")
qqline(bc_val_WS,col="blue")

Scotland_data_validation$bcWS<-bc_val_WS

#for BLD
bc_val_BLD <- Scotland_data_validation[,7]
p_val_BLD=powerTransform(bc_val_BLD)
bc_val_BLD=bcPower(bc_val_BLD,p_val_BLD$lambda)

par(mfrow=c(1,2))
hist(bc_val_BLD)
qqnorm(bc_val_BLD,col="blue")
qqline(bc_val_BLD,col="blue")

Scotland_data_validation$bcBLD<-bc_val_BLD

#for TP
library(faux)
#outliers(TP value < 0) is detected,
#firstly outliers need to be removed
Scotland_data_validation[which(Scotland_data_validation$TP<0),12]<-0

#Because daily precipitation follows a Gamma distribution,
#gamma2norm() in package "faux" is used
#but still written as bc_val_TP to keep consistency
bc_val_TP <- Scotland_data_validation[,12]
bc_val_TP <- gamma2norm(bc_val_TP)

par(mfrow=c(1,2))
hist(bc_val_TP)

Scotland_data_validation$bcTP<-bc_val_TP




##--- 15 estimation stations
#for BLH
bc_BLH <- Scotland_data[,8]
p_BLH=powerTransform(bc_BLH)
bc_BLH=bcPower(bc_BLH,p_BLH$lambda)

par(mfrow=c(1,2))
hist(bc_BLH)
qqnorm(bc_BLH,col="blue")
qqline(bc_BLH,col="blue")

Scotland_data$bcBLH<-bc_BLH

#for WS
bc_WS <- Scotland_data[,9]
p_WS=powerTransform(bc_WS)
bc_WS=bcPower(bc_WS,p_WS$lambda)

par(mfrow=c(1,2))
hist(bc_WS)
qqnorm(bc_WS,col="blue")
qqline(bc_WS,col="blue")

Scotland_data$bcWS<-bc_WS

#for BLD
bc_BLD <- Scotland_data[,7]
p_BLD=powerTransform(bc_BLD)
bc_BLD=bcPower(bc_BLD,p_BLD$lambda)

par(mfrow=c(1,2))
hist(bc_BLD)
qqnorm(bc_BLD,col="blue")
qqline(bc_BLD,col="blue")

Scotland_data$bcBLD<-bc_BLD

#for TP
#outliers(TP value < 0) is detected,
#firstly outliers need to be removed
Scotland_data[which(Scotland_data$TP<0),12]<-0

#Because daily precipitation follows a Gamma distribution,
#gamma2norm() in package "faux" is used
#but still written as bc_TP to keep consistency
bc_TP <- Scotland_data[,12]
bc_TP <- gamma2norm(bc_TP)

par(mfrow=c(1,2))
hist(bc_TP)

Scotland_data$bcTP<-bc_TP

is.finite(Scotland_data$bcTP)#FALSE's exist
is.finite(Scotland_data_validation$bcTP)#FALSE's exist
Scotland_data[which(Scotland_data$bcTP==-Inf|
                      Scotland_data$bcTP==Inf),22]<-median(Scotland_data$bcTP)
Scotland_data_validation[which(Scotland_data_validation$bcTP==-Inf|
                      Scotland_data_validation$bcTP==Inf),22]<-median(Scotland_data_validation$bcTP)


##---Standardisation
#covariates(#=10) standardisation for 15 stations
mean_covariates = apply(Scotland_data[,c(3:12,17:22)],2,mean)
sd_covariates = apply(Scotland_data[,c(3:12,17:22)],2,sd)
Scotland_data[,c(3:12,17:22)] = scale(Scotland_data[,c(3:12,17:22)],
                                      mean_covariates, sd_covariates)

#covariates(#=10) standardisation for 9 validation stations
Scotland_data_validation[,c(3:12,17:22)] = scale(Scotland_data_validation[,c(3:12,17:22)],
                                                 mean_covariates, sd_covariates)




##--- Decide on covariates
#Now we have 10 candidate covariates(Altitude,SP,T2M,D2M,
#Longitude,Latitude,bcBLH,bcWS,bcBLD and bcTP)

#Regression analysis to check collinearity
regress<-data.frame(Scotland_data$bcPM2.5,Scotland_data$Altitude,
                    Scotland_data$SP,Scotland_data$T2M,
                    Scotland_data$D2M,Scotland_data$Longitude,
                    Scotland_data$Latitude,Scotland_data$bcBLH,
                    Scotland_data$bcWS,Scotland_data$bcBLD,
                    Scotland_data$bcTP)

lm1 <- lm(Scotland_data.bcPM2.5 ~ ., data = regress)
summary(lm1)

#check variance inflation factors(VIF)
vif(lm1, digits = 3)
#covariate selection
lm1.step <- step(lm1, direction = "backward")
summary(lm1.step)


#remove SP and T2M(VIF>10)
regress2<-data.frame(Scotland_data$bcPM2.5,Scotland_data$Altitude,
                     Scotland_data$D2M,Scotland_data$Longitude,
                     Scotland_data$Latitude,Scotland_data$bcBLH,
                     Scotland_data$bcWS,Scotland_data$bcBLD,
                     Scotland_data$bcTP)
lm2 <- lm(Scotland_data.bcPM2.5 ~ ., data = regress2)
summary(lm2)
vif(lm2, digits = 3)
lm2.step <- step(lm2, direction = "backward")
summary(lm2.step)

#8 covariates (Altitude,D2M,
#Longitude,Latitude,bcBLH,bcWS,bcBLD and bcTP) are kept




##--- INLA
#mesh
library(rgeos)
require(maptools)


poly <- rgdal::readOGR("C:/Users/Tzf/Desktop/mygeodata.lalo/scotland_ca_2010_05.shp")
border <- unionSpatialPolygons(poly,rep(1,nrow(poly)))
bdry <- inla.sp2segment(border)
mesh <- inla.mesh.create.helper(points = cbind(lalo15$Longitude,
                                               lalo15$Latitude),
                                boundary = bdry, cutoff = 0.1,
                                max.edge = c(0.1,1)
)
plot(mesh)
points(lalo15$Longitude,lalo15$Latitude,
       pch=20,cex=1, col=2)
points(lalo9$Longitude,lalo9$Latitude,
       pch=20,cex=1,col=4)




#map of study region
boundary <- st_read("C:/Users/Tzf/Desktop/mygeodata.lalo/scotland_ca_2010_05.shp")

ggplot() + 
  geom_sf(data = boundary, size=0.3,fill = "grey") + 
  ggtitle("") + 
  coord_sf() +
  geom_point(data=lalo15,size=2,aes(x=Longitude,y=Latitude),col="red")+
  geom_point(data=lalo9,size=2,aes(x=Longitude,y=Latitude),col="blue")


#SPDE
spde = inla.spde2.matern(mesh=mesh, alpha=2)

write.table(lalo15[Scotland_data$Station.ID,
                   c("Longitude","Latitude")],"C:/Users/Tzf/Desktop/loc15.csv",row.names=FALSE,col.names=TRUE,sep=",")
loc15 = read.table("C:/Users/Tzf/Desktop/loc15.csv",header=TRUE,sep=",")

write.table(lalo9[Scotland_data_validation$Station.ID,
                  c("Longitude","Latitude")],"C:/Users/Tzf/Desktop/loc9.csv",row.names=FALSE,col.names=TRUE,sep=",")
loc9 = read.table("C:/Users/Tzf/Desktop/loc9.csv",header=TRUE,sep=",")






A.est = inla.spde.make.A(mesh,
                         loc=as.matrix(loc15),
                         group=Scotland_data$time,
                         n.group=n_days)
A.val = inla.spde.make.A(mesh,
                         loc=as.matrix(loc9),
                         group=Scotland_data_validation$time,
                         n.group=n_days)

#observation matrix for prediction of day 365
i_day = 3
A.pred = inla.spde.make.A(mesh, group=i_day, n.group=n_days)

field.indices = inla.spde.make.index("field", n.spde=mesh$n, n.group=n_days)




list.est <- as.data.frame(Scotland_data[,c(3,11,17:22)])
list.val <- as.data.frame(Scotland_data_validation[,c(3,11,17:22)])

stack.est =
  inla.stack(data=list(bcPM2.5=Scotland_data$bcPM2.5),
             A=list(A.est, 1),
             effects=
               list(c(field.indices,
                      list(Intercept=1)),
                    list(list.est)),
             tag="est")


stack.val = 
  inla.stack(data=list(bcPM2.5=Scotland_data_validation$bcPM2.5),
             A=list(A.val, 1),
             effects=
               list(c(field.indices,
                      list(Intercept=1)),
                    list(list.val)),
             tag="val")




scaled.mesh.loc =
  list(Longitude=(rep(scale(mesh$loc[,1],
                            mean_covariates["Longitude"],
                            sd_covariates["Longitude"]), n_days)),
       Latitude=(rep(scale(mesh$loc[,2],
                           mean_covariates["Latitude"],
                           sd_covariates["Latitude"]), n_days)))


stack.pred =
  inla.stack(data=list(bcPM2.5=NA),
             A=list(A.pred),
             effects=list(c(field.indices,
                            scaled.mesh.loc,
                            list(Intercept=1))),
             tag="pred")
stack = inla.stack(stack.est, stack.val, stack.pred)





formula <- (bcPM2.5 ~ -1 + Intercept +
              Altitude+D2M+Longitude+Latitude+bcBLH+bcWS+bcBLD+bcTP+
              f(field, model = spde, group = field.group,
                control.group = list(model="ar1")))

result = inla(formula,
              data = inla.stack.data(stack, spde=spde),
              family = "gaussian",
              control.predictor = list(A=inla.stack.A(stack), compute=TRUE))



