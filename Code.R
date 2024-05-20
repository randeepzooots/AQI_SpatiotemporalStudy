rm(list = ls())


library(ggplot2)
library(clifro)  #for rose chart


#importing data and removing NAs
JU <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Jodhpur.csv",header = T)
KO <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Kota.csv",header = T)
JP <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Jaipur_PC.csv",header = T)
UD <- read.csv(file = "C:/Users/RD/Desktop/ISPS/Data/Curated/Udaipur.csv",header = T)


#changing to date format
JU$From.Date <- as.Date(JU$From.Date,"%d-%m-%Y")
KO$From.Date <- as.Date(KO$From.Date,"%d-%m-%Y")
JP$From.Date <- as.Date(JP$From.Date,"%d-%m-%Y")
UD$From.Date <- as.Date(UD$From.Date,"%d-%m-%Y")


############# Rose Chart #############
rose <- function(D,S,City=NULL)   # (Dataset, Season, City name)
{
  D <- na.omit(D)
  D$From.Date <- as.Date(D$From.Date,"%d-%m-%Y")
  if(S=="All")
    with(D,windrose(D$WS,D$WD,D$Season,n_col=2,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5,legend_title = "Wind Speed \n m/s"))
  else if(S=="Pre-Monsoon")
  {
    D <- D[D$From.Date>"2018-02-28" & D$From.Date<"2018-06-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5,legend_title = "Wind Speed \n m/s")
  }
  else if(S=="Monsoon")
  {
    D <- D[D$From.Date>"2018-05-31" & D$From.Date<"2018-10-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5,legend_title = "Wind Speed \n m/s")
  }
  else if(S=="Post-Monsoon")
  {
    D <- D[D$From.Date>"2018-09-30" & D$From.Date<"2019-01-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5,legend_title = "Wind Speed \n m/s")
  }
  else if(S=="Winter")
  {
    D <- D[D$From.Date>"2018-12-31" & D$From.Date<"2019-03-01",]
    windrose(D$WS,D$WD,ggtheme = "gray",col_pal = "RdYlBu",n_speeds = 5,legend_title = "Wind Speed \n m/s")
  }
  ggsave(paste(City,"_",S,".png"))
}

rose(JP,"All","Jaipur")
rose(JU,"ALL","Jodhpur")
rose(KO,"All","Kota")
rose(UD,"All","Udaipur")
