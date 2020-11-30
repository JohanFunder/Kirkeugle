str(z_samlet)
#View(z_samlet)

tail(z_45076$FIX.date,2000)
str(vejr.data)

## Saml først hver ugle med vejrdata
# 45067
z_45067.vejr <- cbind.data.frame(z_45067,vejr.data)
# 45068
z_45068.vejr <- cbind.data.frame(z_45068,vejr.data)
# 45072
z_45072.vejr <- cbind.data.frame(z_45072,vejr.data)
# 45076
z_45076.vejr <- cbind.data.frame(z_45076,vejr.data)

## Saml nu alle ugler og alle vejrdata
z_samlet_vejr.1 <- rbind.data.frame(z_45067.vejr,z_45068.vejr,z_45072.vejr,z_45076.vejr)
str(z_samlet_vejr.1)
#View(z_samlet_vejr.1)
head(z_samlet_vejr.1$RTC.date,1000)


#### TIlføj kolonne med GPS-Koordinater for Fræer
z_samlet_vejr.1$LAT <- c("56.863996")
z_samlet_vejr.1$LON <- c("9.935008")
#View(z_samlet_vejr.1)

#### Lav til dataframe
z_samlet_vejr.1 <- data.frame(z_samlet_vejr.1)
z_samlet_vejr.1$original_date <- as.Date(z_samlet_vejr.1$original_date)
z_samlet_vejr.1$LAT <- as.numeric(z_samlet_vejr.1$LAT)
z_samlet_vejr.1$LON <- as.numeric(z_samlet_vejr.1$LON)
z_samlet_vejr.1$original_date_ts <- as.POSIXct(z_samlet_vejr.1$DateTime.1,
                                             origin = "1970-01-01",
                                             tryFormats = c("%m/%d/%Y %H:%M:%OS"))

z_samlet_vejr.1$original_date_num <- as.numeric(z_samlet_vejr.1$original_date_ts)
# as.POSIXct(z_samlet_vejr.1$original_date_num,origin = "1970-01-01")
z_samlet_vejr.1 <- data.frame(z_samlet_vejr.1)
z_samlet_vejr.1


#######******** Assign Sunrise **********
#install.packages("glue")
#install.packages('rlang')
#install.packages("devtools")
library(devtools)
#install_github("motusWTS/motus@master")   ## the lastest stable version
library(motus)
library(StreamMetabolism)

# OR use example sql file included in `motus`
#sql.motus <- tagme(176, update = FALSE, 
#                   dir = system.file("extdata", package = "motus"))
# convert sql file "sql.motus" to a tbl called "tbl.alltags"
library(dplyr)
#tbl.alltags <- tbl(sql.motus, "alltags") 
# convert the tbl "tbl.alltags" to a data.frame called "df.alltags"
#df.alltags <- tbl.alltags %>% 
#  collect() %>% 
#  as.data.frame() 

#df.alltags <- data.frame(df.alltags)

# Subset kun de vigtige kolonner
z_samlet_vejr_brug <- cbind.data.frame(z_samlet_vejr.1$original_date_num,
                                       z_samlet_vejr.1$LAT,z_samlet_vejr.1$LON)
colnames(z_samlet_vejr_brug) <- c('ts','recvDeployLat','recvDeployLon')
z_samlet_vejr_brug
z_samlet_vejr <- NULL
z_samlet_vejr <- cbind.data.frame(z_samlet_vejr.1,z_samlet_vejr_brug) 
z_samlet_vejr <- data.frame(z_samlet_vejr)
head(z_samlet_vejr)



#####*** Indsæt SUNSET
## Prøv først med data eksempel fra motus
# timeToSunriset(df.alltags_brug, lat = "recvDeployLat", lon = "recvDeployLon",
#               ts = "ts", units = "hours")

sunset <- timeToSunriset(z_samlet_vejr, lat = "recvDeployLat", lon = "recvDeployLon",
               ts = "ts", units = "hours")
sunset
####### ********* Samlet data.frame med sunset *****
z_samlet_vejr <- NULL
z_samlet_vejr <- sunset
z_samlet_vejr <- data.frame(z_samlet_vejr)
str(z_samlet_vejr)
# View(z_samlet_vejr)


##### Assign day/night kolonne
library(dplyr)
z_samlet_vejr$day.night <- ifelse(
  ( 
    (z_samlet_vejr$ts > z_samlet_vejr$sunrise)
  ),
  "Day",  # if condition is met, put 1
  "Night"   # else put 0
)

z_samlet_vejr$day.night


#View(z_samlet_vejr)


#### Lav bins med cuts per uge
z_samlet_vejr$cuts <- cut(z_samlet_vejr$original_date,"week")

head(z_samlet_vejr$cuts)

### Udregn mean values
aggr <- aggregate(z_samlet_vejr["dist_dag"], by=z_samlet_vejr["cuts"],FUN=mean, na.rm=TRUE)

### Udregn CI
aggr_sd <- aggregate(z_samlet_vejr["dist_dag"], by=z_samlet_vejr["cuts"],FUN=sd, na.rm=TRUE)
aggr$se <- aggr_sd$dist_dag/length(aggr_sd$dist_dag)

aggr$CI <- aggr$se*1.96

str(aggr)

d <- seq(as.Date("2019-06-15"), as.Date("2020-05-15"), by="1 week")
str(d)


########**** Binned uge intervaller for vejrdata

##*** Temperatur
### Udregn mean values
z_samlet_vejr$Middeltemperatur
aggr_temp <- aggregate(z_samlet_vejr["Middeltemperatur"],
                       by = z_samlet_vejr[c("cuts", "day.night")],
                       FUN=mean, na.rm=TRUE)
aggr_temp

### Udregn CI
se <- function(x) sd(x)/length(x)
aggr_temp_sd <- aggregate(z_samlet_vejr["Middeltemperatur"], 
                          by = z_samlet_vejr[c("cuts", "day.night")],FUN=sd, na.rm=TRUE)
aggr_temp$se <- aggr_temp_sd$Middeltemperatur/length(aggr_temp_sd$Middeltemperatur)

aggr_temp$CI <- aggr_temp$se*1.96
str(aggr_temp)


##*** Vind
### Udregn mean values
z_samlet_vejr$Middelvindhastighed
aggr_vind <- aggregate(z_samlet_vejr["Middelvindhastighed"],
                       by = z_samlet_vejr[c("cuts", "day.night")],
                       FUN=mean, na.rm=TRUE)
aggr_vind

### Udregn CI
se <- function(x) sd(x)/length(x)
aggr_vind_sd <- aggregate(z_samlet_vejr["Middelvindhastighed"], 
                          by = z_samlet_vejr[c("cuts", "day.night")],FUN=sd, na.rm=TRUE)
aggr_vind$se <- aggr_vind_sd$Middelvindhastighed/length(aggr_vind_sd$Middelvindhastighed)

aggr_vind$CI <- aggr_vind$se*1.96
str(aggr_vind)


##*** regn
### Udregn mean values
z_samlet_vejr$Nedbør
aggr_regn <- aggregate(z_samlet_vejr["Nedbør"],
                       by = z_samlet_vejr[c("cuts", "day.night")],
                       FUN=sum, na.rm=TRUE)

# fjern overflødlige kolonner
aggr_regn <- aggr_regn[-c(1, 48, 97), ]

## tilføj kolonne med color
#aggr_regn$color <- c(rep("red4",length(aggr_regn$Nedbør)/2),
#                     rep("navyblue",length(aggr_regn$Nedbør)/2))
aggr_regn


#########***** Samlet plot med to y-akser TEMPERATUR *******################
require(devtools)
# install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(scales)

ggplot() + ylim(0,2800) +
  
  geom_bar(data=aggr, mapping = aes(x=as.Date(cuts),y=dist_dag),
                    stat = "identity",fill = "grey",colour="black",width = 5.5,
           position = position_nudge(x = 1.3)) +

  geom_errorbar(data=aggr, 
                aes(ymin=dist_dag-CI,ymax=dist_dag+CI,x=as.Date(cuts)),
                width=4.1, position = position_nudge(x = 1.3),size=0.7) + 
  
  geom_line(data = aggr_temp, aes(x = as.Date(cuts), 
                          y = Middeltemperatur*100+800,
                          color = day.night),
            size = 0.7,
            inherit.aes = F,
            position = position_nudge(x = 1.3),
            show.legend = T) +
  
  scale_colour_manual(values=c(Day="red4",Night="navyblue"),name="Temp.") +
  
  geom_errorbar(data=aggr_temp, 
                aes(ymin=(Middeltemperatur-CI)*100+800,
                    ymax=(Middeltemperatur+CI)*100+800,
                    x=as.Date(cuts)),
                width=4.1, position = position_nudge(x = 1.3),size=0.7,
                color = c(rep("red4",rep(length(aggr_temp$day.night)/2)),
                          rep("navyblue",rep(length(aggr_temp$day.night)/2)))) + 
  
  scale_x_date(breaks = seq(as.Date("2019-06-15"), as.Date("2020-05-05"), by="1 weeks"),
               labels = date_format(format = "%d. %b"), expand = c(0.01,0.01),
               limits = c(as.Date("2019-06-15"),as.Date("2020-05-02"))) +

   theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        axis.text.x = element_text(angle=90, vjust=0.3, hjust=1),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right") +
  
  xlab("Time - weeks") +
  
  # Add a second axis and specify its features
  scale_y_continuous(name = "Dispersal activity per day (m)", 
                     breaks = c(100,300,500,700,900,1100,1300,1500,1700,1900,2100,
                                2300,2500,2700,2900), 
                     sec.axis = sec_axis(~./100-(800/100),
                                         name = expression("Temperature mean "*~(degree*C)),
                                         breaks = c(-2,0,2,4,6,8,10,12,14,16,18,20,22,
                                                    24,26,28,30)),
                     limits = c(-10,3000))
  


  
#########***** Samlet plot med to y-akser VIND *******################
require(devtools)
# install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(scales)

ggplot() + ylim(0,2800) +
  
  geom_bar(data=aggr, mapping = aes(x=as.Date(cuts),y=dist_dag),
           stat = "identity",fill = "grey",colour="black",width = 5.5,
           position = position_nudge(x = 1.3)) +
  
  geom_errorbar(data=aggr, 
                aes(ymin=dist_dag-CI,ymax=dist_dag+CI,x=as.Date(cuts)),
                width=4.1, position = position_nudge(x = 1.3),size=0.7) + 
  
  geom_line(data = aggr_vind, aes(x = as.Date(cuts), 
                                  y = Middelvindhastighed*250+800,
                                  color = day.night),
            size = 0.7,
            inherit.aes = F,
            position = position_nudge(x = 1.3),
            show.legend = T) +
  
  scale_colour_manual(values=c(Day="red4",Night="navyblue"),name="Wind") +
  
  geom_errorbar(data=aggr_vind, 
                aes(ymin=(Middelvindhastighed-CI)*250+800,
                    ymax=(Middelvindhastighed+CI)*250+800,
                    x=as.Date(cuts)),
                width=4.1, position = position_nudge(x = 1.3),size=0.7,
                color = c(rep("red4",rep(length(aggr_vind$day.night)/2)),
                          rep("navyblue",rep(length(aggr_vind$day.night)/2)))) + 
  
  scale_x_date(breaks = seq(as.Date("2019-06-15"), as.Date("2020-05-05"), by="1 weeks"),
               labels = date_format(format = "%d. %b"), expand = c(0.01,0.01),
               limits = c(as.Date("2019-06-15"),as.Date("2020-05-02"))) +
  
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        axis.text.x = element_text(angle=90, vjust=0.3, hjust=1),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right") +
  
  xlab("Time - weeks") +
  
  # Add a second axis and specify its features
  scale_y_continuous(name = "Dispersal activity per day (m)", 
                     breaks = c(100,300,500,700,900,1100,1300,1500,1700,1900,2100,
                                2300,2500,2700,2900), 
                     sec.axis = sec_axis(~./250-(800/250),
                                         name = expression("Windspeed mean "*~(degree*C)),
                                         breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,
                                                    24,26,28,30)),
                     limits = c(-10,3000))




#########***** Samlet plot med to y-akser REGN *******################
require(devtools)
# install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(scales)
#install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

ggplot() + ylim(0,2800) +
  
  geom_bar(data=aggr, mapping = aes(x=as.Date(cuts),y=dist_dag),
           stat = "identity",fill = "grey",colour="black",width = 5.5,
           position = position_nudge(x = 1.3)) +
  
  geom_errorbar(data=aggr, 
                aes(ymin=dist_dag-CI,ymax=dist_dag+CI,x=as.Date(cuts)),
                width=4.1, position = position_nudge(x = 1.3),size=0.7) + 

  
  ## Barplot regn
  ## Barplot for dags regn
  geom_tile(aes(x = as.Date(aggr_regn$cuts[aggr_regn$day.night == "Day"]),
                y = ((-1*(aggr_regn$Nedbør[aggr_regn$day.night == "Day"])*10)/2)+3500, # y = the center point of each bar
                height = aggr_regn$Nedbør[aggr_regn$day.night == "Day"]*10,
                fill = "red4",),
            alpha = 1/2,
            width = 1.7,
            position = position_nudge(x = 1.3-(1.4)),
            inherit.aes = F,
            color = "black",
            show.legend = TRUE) +
  
  ## Barplot for nats regn
  geom_tile(aes(x = as.Date(aggr_regn$cuts[aggr_regn$day.night == "Night"]),
                y = ((-1*(aggr_regn$Nedbør[aggr_regn$day.night == "Night"])*10)/2)+3500, # y = the center point of each bar
                height = aggr_regn$Nedbør[aggr_regn$day.night == "Night"]*10,
                fill = "navyblue"),
            alpha = 1/2,
            width = 1.7,
            position = position_nudge(x = 1.3+(1.4)),
            inherit.aes = F,
            color = "black",
            show.legend = TRUE) +
  
  # Add patterns
#  scale_pattern_manual(values = c(Day = "stripe", Night = "none"))
  

#  geom_line(data = aggr_regn, aes(x = as.Date(cuts), 
#                                  y = (Nedbør*-1)*10+3000,
#                                  color = day.night),
#            size = 0.75,
#            inherit.aes = F,
#            position = position_nudge(x = 1.3),
#            show.legend = T) +
  
  
  #use scale fill identity to set labels, name and color
    scale_fill_identity("Precipitation", breaks =  c('red4','navyblue'), 
                      labels = c('Day','Night'),
                      guide = "legend") +
  
  
  scale_x_date(breaks = seq(as.Date("2019-06-15"), as.Date("2020-05-05"), by="1 weeks"),
               labels = date_format(format = "%d. %b"), expand = c(0.001,0.001)) +
  
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        axis.text.x = element_text(angle=90, vjust=0.3, hjust=1),
        panel.grid.minor = element_blank(),
        legend.box.just = "top",
        legend.position = 'top') +
  
  xlab("Time - weeks") +
  
  # Add a second axis and specify its features
  scale_y_continuous(name = "Dispersal activity per day (m)", 
                     breaks = c(100,300,500,700,900,1100,1300,1500,1700,1900,2100,
                                2300,2500,2700,2900,3100,3300,3500),
                     labels = c(100,300,500,700,900,1100,1300,1500,1700,1900,2100,
                                2300,2500,2700,2900,"","",""),
                     sec.axis = sec_axis(~ 16 - ./10+(3400/10.25),
                                         name = "Precipitation sum (mm)",
                                         breaks = c(0,20,40,60,80,100,
                                                    120,140,160,180)),
                     limits = c(-10,3500))

###############################################################
# Gammelt plot for regn
ggplot() + ylim(0,2800) +
  
  geom_bar(data=aggr, mapping = aes(x=as.Date(cuts),y=dist_dag),
           stat = "identity",fill = "grey",colour="black",width = 5.5,
           position = position_nudge(x = 1.3)) +
  
  geom_errorbar(data=aggr, 
                aes(ymin=dist_dag-CI,ymax=dist_dag+CI,x=as.Date(cuts)),
                width=4.1, position = position_nudge(x = 1.3),size=0.7) + 
  
  geom_line(mapping = aes(x = as.Date(z_samlet_vejr$original_date), 
                          y = (z_samlet_vejr$Nedbør*-1)*70+2700),
            size = 0.5, color = "navyblue",inherit.aes = F) +
  
  
  scale_x_date(breaks = seq(as.Date("2019-06-15"), as.Date("2020-05-05"), by="1 weeks"),
               labels = date_format(format = "%d. %b"), expand = c(0.001,0.001)) +
  
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        axis.text.x = element_text(angle=90, vjust=0.3, hjust=1),
        panel.grid.minor = element_blank()) + 
  xlab("Time - weeks") +
  
  # Add a second axis and specify its features
  scale_y_continuous(name = "Dispersal activity per day (m)", 
                     breaks = c(100,300,500,700,900,1100,1300,1500,1700,1900,2100,
                                2300,2500,2700), 
                     sec.axis = sec_axis(~ 16 - ./70+22.5,
                                         name = "Precipitation mean (mm)",
                                         breaks = c(0,2,4,6,8,10,12,14,16)),
                     limits = c(-10,2750))






