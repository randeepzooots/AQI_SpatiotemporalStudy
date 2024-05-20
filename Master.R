rm(list = ls())

library(stats)
library(rgdal)
library(raster)
library(sp)
library(TTR)
library(forecast)
library(tseries)
library(rworldmap)
library(spData)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(arrangeGrob)
library(gridExtra)
library(grid)
library(descr)
library(dplyr)
library(clifro)  #for rose chart


#importing data
JU <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Jodhpur.csv",header = T)
KO <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Kota.csv",header = T)
JP <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Jaipur_PC.csv",header = T)
UD <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Udaipur.csv",header = T)



#changing to date format
JU$From.Date <- as.Date(JU$From.Date,"%d-%m-%Y")
KO$From.Date <- as.Date(KO$From.Date,"%d-%m-%Y")
JP$From.Date <- as.Date(JP$From.Date,"%d-%m-%Y")
UD$From.Date <- as.Date(UD$From.Date,"%d-%m-%Y")
DATE <- as.data.frame(JU$From.Date);colnames(DATE) <- "Date"
Season <- as.data.frame(as.factor(JP$Season));colnames(Season) <- "Season"

############# Rose Chart #############
rose <- function(D,S)
{
  D <- na.omit(D)
  D$From.Date <- as.Date(D$From.Date,"%d-%m-%Y")
  if(S=="Pre-Monsoon")
  {
    D <- D[D$From.Date>"2018-02-28" & D$From.Date<"2018-06-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5)
  }
  else if(S=="Monsoon")
  {
    D <- D[D$From.Date>"2018-05-31" & D$From.Date<"2018-10-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5)
  }
  else if(S=="Post-Monsoon")
  {
    D <- D[D$From.Date>"2018-09-30" & D$From.Date<"2019-01-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5)
  }
  else if(S=="Winter")
  {
    D <- D[D$From.Date>"2018-12-31" & D$From.Date<"2019-03-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5)
  }
}

rose(JU,"Pre-Monsoon")
rose(JP,"Monsoon")
rose(JP,"Post-Monsoon")
rose(JP,"Winter")


######## AQI ############

PM10.f <- function(C)
{
  check <- rep()
  C[is.na(C)] <- 0
  SI <- ifelse(C<=50,SI <- C,ifelse(C>50 & C<=100,SI <- C,ifelse(C>100 & C<=250,SI <- 100+(C-100)*100/150,
                                                                 ifelse(C>250 & C<=350,SI <- 200+(C-250),ifelse(C>350 & C<=430,SI <- 300+(C-350)*100/80,
                                                                                                                ifelse(C>430,SI <- 400+(C-430)*100/80,SI <- 400+(C-430)*100/80))))))
  check <- ifelse(SI<=0,0,1)
  data.frame("PM10"=SI,"PM10 Check"=check)
}

PM2.5.f <- function(C)
{
  check <- rep()
  C[is.na(C)] <- 0
  SI <- ifelse(C<=30,SI <- C*50/30,ifelse(C>30 & C<=60,SI <- 50+(C-30)*50/30,
                                          ifelse(C>60 & C<=90,SI <- 100+(C-60)*100/30,ifelse(C>90 & C<=120,SI <- 200+(C-90)*100/30,
                                                                                             ifelse(C>120 & C<=250,SI <- 300+(C-120)*100/130,ifelse(C>250,SI <- 400+(C-250)*100/130,SI <- 400+(C-250)*100/130))))))
  check <- ifelse(SI<=0,0,1)
  return(data.frame("PM2.5"=SI,"PM2.5 Check"=check))
}

SO2.f <- function(C)
{
  check <- rep()
  C[is.na(C)] <- 0
  SI <- ifelse(C<=40,SI <- C*50/40,ifelse(C>40 & C<=80,SI <- 50+(C-40)*50/40,
                                          ifelse(C>80 & C<=380,SI <- 100+(C-80)*100/300,ifelse(C>380 & C<=800,SI <- 200+(C-300)*100/420,
                                                                                               ifelse(C>800 & C<=1600,SI <- 300+(C-800)*100/800,ifelse(C>1600,SI <- 400+(C-1600)*100/800,SI <- 400+(C-1600)*100/800))))))
  check <- ifelse(SI<=0,0,1)
  return(data.frame("SO2"=SI,"SO2 Check"=check))
}

NOx.f <- function(C)
{
  check <- rep()
  C[is.na(C)] <- 0
  SI <- ifelse(C<=40,SI <- C*50/40,ifelse(C>40 & C<=80,SI <- 50+(C-40)*50/40,
                                          ifelse(C>80 & C<=180,SI <- 100+(C-80)*100/100,ifelse(C>180 & C<=280,SI <- 200+(C-180)*100/100,
                                                                                               ifelse(C>280 & C<=400,SI <- 300+(C-280)*100/120,ifelse(C>400,SI <- 400+(C-400)*100/120,SI <- 400+(C-400)*100/120))))))
  check <- ifelse(SI<=0,0,1)
  return(data.frame("NOx"=SI,"NOx Check"=check))
}

CO.f <- function(C)
{
  check <- rep()
  C[is.na(C)] <- 0
  SI <- ifelse(C<=1,SI <- C*50/1,ifelse(C>1 & C<=2,SI <- 50+(C-1)*50/1,
                                        ifelse(C>2 & C<=10,SI <- 100+(C-2)*100/8,ifelse(C>10 & C<=17,SI <- 200+(C-10)*100/7,
                                                                                        ifelse(C>17 & C<=34,SI <- 300+(C-17)*100/17,ifelse(C>34,SI <- 400+(C-34)*100/17,SI <- 400+(C-34)*100/17))))))
  check <- ifelse(SI<=0,0,1)
  return(data.frame("CO"=SI,"CO Check"=check))
}

O3.f <- function(C)
{
  check <- rep()
  C[is.na(C)] <- 0
  SI <- ifelse(C<=50,SI <- C*50/50,ifelse(C>50 & C<=100,SI <- 50+(C-50)*50/50,
                                          ifelse(C>100 & C<=168,SI <- 100+(C-100)*100/68,ifelse(C>168 & C<=208,SI <- 200+(C-168)*100/40,
                                                                                                ifelse(C>208 & C<=748,SI <- 300+(C-208)*100/539,ifelse(C>748,SI <- 400+(C-400)*100/539,SI <- 400+(C-400)*100/539))))))
  check <- ifelse(SI<=0,0,1)
  return(data.frame("O3"=SI,"O3 Check"=check))
}

NH3.f <- function(C)
{
  check <- rep()
  C[is.na(C)] <- 0
  SI <- ifelse(C<=200,SI <- C*50/200,ifelse(C>200 & C<=400,SI <- 50+(C-200)*50/200,
                                            ifelse(C>400 & C<=800,SI <- 100+(C-400)*100/400,ifelse(C>800 & C<=1200,SI <- 200+(C-800)*100/400,
                                                                                                   ifelse(C>1200 & C<=1800,SI <- 300+(C-1200)*100/600,ifelse(C>1800,SI <- 400+(C-1800)*100/600,SI <- 400+(C-1800)*100/600))))))
  check <- ifelse(SI<=0,0,1)
  return(data.frame("NH3"=SI,"NH3 Check"=check))
}

AQI <- function(D)
{
  Sub.Indices <- cbind.data.frame(DATE,PM10.f(D$PM10),PM2.5.f(D$PM2.5),SO2.f(D$SO2),NOx.f(D$NOx),CO.f(D$CO),O3.f(D$Ozone),NH3.f(D$NH3))
  
  Dom <- rep()
  AQI <- rep()
  Level <- rep()
  for (i in 1:NROW(Sub.Indices))
  {
    if((Sub.Indices$PM10.Check[i]==1 | Sub.Indices$PM2.5.Check[i]==1) & sum(Sub.Indices[i,c(3,5,7,9,11,13,15)])>=3)
    {
      AQI[i] <- max(Sub.Indices[i,c(2,4,6,8,10,12,14)])
      Dom[i] <- colnames(Sub.Indices[i,c(2,4,6,8,10,12,14)])[apply(Sub.Indices[i,c(2,4,6,8,10,12,14)],1,which.max)]
      if(is.na(AQI[i]))
        Dom[i] <- NA
      if(AQI[i]<=50)
        Level[i] <- "Good"
      else if(AQI[i]>50 & AQI[i]<=100)
        Level[i] <- "Satisfactory"
      else if(AQI[i]>100 & AQI[i]<=200)
        Level[i] <- "Moderately Polluted"
      else if(AQI[i]>200 & AQI[i]<=300)
        Level[i] <- "Poor"
      else if(AQI[i]>300 & AQI[i]<=400)
        Level[i] <- "Very Poor"
      else if(AQI[i]>400)
        Level[i] <- "Severe"
    }
  }
  return(data.frame(DATE,AQI,Dom,Level))
}
AQI.JP <- cbind(Season,AQI(JP))   
AQI.JU <- cbind(Season,AQI(JU))
AQI.KO <- cbind(Season,AQI(KO))
AQI.UD <- cbind(Season,AQI(UD))


A <- ggplot(AQI.JP,aes(x=AQI.JP$Season,y=AQI.JP$AQI, fill=AQI.JP$Season)) +
  geom_boxplot() +ggtitle("Jaipur AQI")+labs(y="AQI",x="Seasons",fill = "Seasons")+ theme(legend.position = "bottom",axis.text.x = element_blank())
ggsave("JP_AQI.png")
B <- ggplot(AQI.JU,aes(x=AQI.JU$Season,y=AQI.JU$AQI, fill=AQI.JU$Season)) +
  geom_boxplot() +ggtitle("Jodhpur AQI")+labs(y="AQI",x="Seasons",fill = "Seasons")+ theme(legend.position = "bottom",axis.text.x = element_blank())+ guides(guide_legend(title = "Seasons"))
ggsave("JU_AQI.png")
C <- ggplot(AQI.KO,aes(x=AQI.KO$Season,y=AQI.KO$AQI, fill=AQI.KO$Season)) +
  geom_boxplot() +ggtitle("Kota AQI")+labs(y="AQI",x="Seasons",fill = "Seasons")+ theme(legend.position = "bottom",axis.text.x = element_blank())+ guides(guide_legend(title = "Seasons"))
ggsave("KO_AQI.png")
D <- ggplot(AQI.UD,aes(x=AQI.UD$Season,y=AQI.UD$AQI, fill=AQI.UD$Season)) +
  geom_boxplot() +ggtitle("Udaipur AQI")+labs(y="AQI",x="Seasons",fill = "Seasons")+ theme(legend.position = "bottom",axis.text.x = element_blank())+ guides(guide_legend(title = "Seasons"))
ggsave("UD_AQI.png")
ggsave(plot = ggarrange(A,B,C,D,ncol = 4,common.legend = T),"AQI_BP.png",width = 10)

################################
write.csv(AQI.JP,file = "Jaipur AQI.csv")
write.csv(AQI.JU,file = "Jodhpur AQI.csv")
write.csv(AQI.KO,file = "Kota AQI.csv")
write.csv(AQI.UD,file = "Udaipur AQI.csv")

######### Spatial ###############


k <- shapefile("F:/Projects/ISPS/Data/Spatial/gadm36_IND_2.shp")
Raj <- k[k$NAME_1=="Rajasthan",]
Stations <- data.frame(
  lat = c(26.875395, 26.292011, 25.143892, 24.587656),
  long = c(75.816748, 73.037522, 75.821256, 73.697468),
  names = c("Jaipur", "Jodhpur","Kota","Udaipur"),
  stringsAsFactors = FALSE
) 
ggplot(Raj, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") + geom_polygon(data = Raj[Raj$NAME_2=="Jaipur",],fill="cadetblue3")+
  geom_polygon(data = Raj[Raj$NAME_2=="Jodhpur",],fill="violetred2")+geom_polygon(data = Raj[Raj$NAME_2=="Kota",],fill="violet")+
  geom_polygon(data = Raj[Raj$NAME_2=="Udaipur",],fill="yellow")+ 
  geom_point(data = Stations, aes(x = long, y = lat), color = "black", size = 2,inherit.aes = F)+
  geom_label_repel(data=Stations, aes(long, lat, label=Stations$names),inherit.aes = F,size = 3)+
  ggtitle("Air Quality Monitoring Stations")+theme_minimal()
ggsave("Stations.png")

############ AQI Level Plot ###########
Lvl.plot <- function(Q,City=NULL)
{
  k <- as.data.frame(prop.table((table(Q[,c(1,5)])),1)*100)
  k$Level <- factor(k$Level,levels = c("Good","Satisfactory","Moderately Polluted","Poor","Very Poor","Severe"))
  ggplot(k, aes(fill=Level, y=Freq, x=Season)) + 
    geom_bar(position="stack", stat="identity",width = 0.5)+ scale_fill_manual(values = c("darkgreen","green","yellow","darkorange","red","darkred"))+
    ggtitle(label = paste(City))+ ylab("Percentage of days")+ theme(legend.position = "top")+
    theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank())+xlab("none")
}
A <- Lvl.plot(AQI.JP,City = "Jaipur")
B <- Lvl.plot(AQI.JU,City = "Jodhpur")
C <- Lvl.plot(AQI.KO,City = "Kota")
D <- Lvl.plot(AQI.UD,City = "Udaipur")

require(gridExtra)
ggsave(plot = ggarrange(A, B, C, D, ncol = 4,common.legend = T),"Levels.png",width = 10)

########## Correlation ############

JU.cor <- cbind(cor(na.omit(JU[,c(7,20,10,12,17,6,3)]),method = "spearman")[,1],
cor(na.omit(JU[,c(8,20,10,12,17,6,3)]),method = "spearman")[,1]);colnames(JU.cor) <- c("PM10","PM2.5")

JP.cor <- cbind(cor(na.omit(JP[,c(6,17,9,11,16,5,22)]),method = "spearman")[,1]
,cor(na.omit(JP[,c(7,17,9,11,16,5,22)]),method = "spearman")[,1]);colnames(JP.cor) <- c("PM10","PM2.5")

KO.cor <- cbind(cor(na.omit(KO[,c(7,17,10,12,16,6,22)]),method = "spearman")[,1]
,cor(na.omit(KO[,c(8,17,10,12,16,6,22)]),method = "spearman")[,1]);colnames(KO.cor) <- c("PM10","PM2.5")

UD.cor <- cbind(cor(na.omit(UD[,c(9,19,12,14,18,8,4)]),method = "spearman")[,1]
,cor(na.omit(UD[,c(10,19,12,14,18,8,4)]),method = "spearman")[,1]);colnames(UD.cor) <- c("PM10","PM2.5")

write.csv(rbind(JP.cor, JU.cor, KO.cor, UD.cor), "Correlation.csv")


#################### Forecast ##############

range(y)
X1<-read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Monthly Data/UD.csv")
y <- ts(X1[,3],frequency = 12,start = c(2010, 4))
x <- ts(X1[,3],frequency = 12,start = c(2010, 4))
z <- ts(X1[88:112,3],start = c(2017,7),frequency = 12)
autoplot(x)+ scale_x_continuous(breaks = seq(2010,2021,1) )
acf(x,lag.max =12)
pacf(x,lag.max =12)

adf.test(x,alternative="stationary",k=10)                         # Augmented Dickey-Fuller test
ndiffs(x, alpha = 0.01, test = c("kpss"), max.d = 2) # No. of Difference required for stationary series
nsdiffs(x, m = 12, test = c("ocsb"), max.D = 5) 
fit1<-auto.arima(x) # automatic fit the ARIMA model for Given Series i.e. order not Known
fit1
Fitted_x<-fitted(fit1)

head(Fitted_x)
#======================= Model Diagnosis  ====================================
tsdiag(fit1)
Box.test(fit1$fitted,type = "Ljung-Box")

#======================= Forecast Value   ====================================
Forecast<-forecast(fit1,h=12, level = 95)
Forecast$mean
autoplot(Forecast)+ scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1))+ 
  ylab("Concentration")+ geom_hline(yintercept=100, linetype="dashed", color = "red")+
   scale_y_continuous(breaks = seq(from = 0, to = 300, by = 50))+ labs(subtitle = "Udaipur")+
  autolayer(fit1$fitted)+ theme(legend.position = "none")
  
ggsave("UD_Forecast.png",width = 10)


########### Dominant Pollutant Plot ###########

Dom.plot <- function(Q,City=NULL)
{
  k <- as.data.frame(prop.table((table(Q[,c(1,4)])),1)*100)
  #k$Level <- factor(k$Level,levels = c("Good","Satisfactory","Moderately Polluted","Poor","Very Poor","Severe"))
  ggplot(k, aes(fill=Dom, y=Freq, x=Season)) + 
    geom_bar(position="stack", stat="identity",width = 0.5)+ fill_palette("RdYlBu")+
    ggtitle(label = paste(City))+ ylab("Percentage of days")+ theme(legend.position = "top")+
    theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank())+xlab("none")
}
A <- Dom.plot(AQI.JP,City = "Jaipur")
B <- Dom.plot(AQI.JU,City = "Jodhpur")
C <- Dom.plot(AQI.KO,City = "Kota")
D <- Dom.plot(AQI.UD,City = "Udaipur")

require(gridExtra)
ggsave(plot = ggarrange(A, B, C, D, ncol = 4,common.legend = T),"Dom.png",width = 10)

