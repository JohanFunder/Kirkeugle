# Set directory
directory <- "D:/GIS_Kirkeugle/Vejrdata"
################### FOr 45067 ###########################

# First we load the verjdata
# temperatur
temp <- rbind.data.frame(
  read.csv(file=file.path(directory,"/Aars_juni_temp.csv"),
                 header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_juli_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_august_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_september_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_oktober_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_november_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_december_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_januar_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_februar_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_marts_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_april_temp.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_maj_temp.csv"),
           header= TRUE, sep = ',', dec = '.')
)

str(temp)




# vind
vind <- rbind.data.frame(
  read.csv(file=file.path(directory,"/Aars_juni_vind.csv"),
                      header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_juli_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_august_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_september_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_oktober_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_november_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_december_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_januar_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_februar_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_marts_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_april_vind.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Aars_maj_vind.csv"),
           header= TRUE, sep = ',', dec = '.')
)
  
str(vind)

# regn
regn <- rbind.data.frame(
  read.csv(file=file.path(directory,"/Morkeskov_juni_regn.csv"),
                      header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_juli_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_august_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_september_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_oktober_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_november_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_december_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_januar_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_februar_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_marts_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_april_regn.csv"),
           header= TRUE, sep = ',', dec = '.'),
  read.csv(file=file.path(directory,"/Morkeskov_maj_regn.csv"),
           header= TRUE, sep = ',', dec = '.')
)
  
                      
str(regn)
head(regn,100)

##**TEMP************
###### Create a date object from the string (skip this if your column is already in the Date format):
original_date <- as.POSIXct(temp$DateTime, format="%m/%d/%Y %H:%M:%S") 
temp$original_date <- original_date

##### Udvælg ønskede datoer
temp <- temp[temp$original_date >= "2019-06-15" & temp$original_date <= "2020-05-15",]
str(temp)

###### Indføj manglende datoer
library(padr)
temp <- data.frame(pad(temp,by = "original_date"))
str(temp)

#Then format the original date to the new format:
temp$Date <- format(temp$original_date, "%d.%b")   
head(temp,15)
tail(temp,15)

##### Udvælg rigtige intries med 14. dages intervaller
temp$ID <- seq_along(temp[,1])
temp[,1]
# indexer i rigtige levels
dates <- reorder(temp$Date,temp$ID) 
dates
#View(dates)
dat_num <- as.numeric(dates)
dat_num
date <- data.frame(temp$ID,dates,dat_num)
date

# remove dublicates
trim_date <- date[!duplicated(date[c("dat_num","dates")]),]
#View(trim_date)
str(trim_date)
trim_date
Thin_days <- c(trim_date$dat_num[seq(1, length(trim_date$dat_num), 14)])
Thin_days
trim_dates <- subset(trim_date$temp.ID, trim_date$dat_num %in% Thin_days)
trim_dates_names <- subset(trim_date$dates, trim_date$dat_num %in% Thin_days)
trim_dates


## Plot at mod Date
dato <- temp$DateTime
dato
temp_plot <- temp$Middeltemperatur
# View(z_45067$at)

plot.default(dato, temp_plot, xaxt ='n', xlab = '', type = 'p',pch = 19,
             cex = .8, cex.axis=1.5, ylim = c(0,30),las=1,col='grey4')
tck <- axis(side=1, trim_dates, labels=FALSE)
labels <- trim_dates_names
text(tck, par("usr")[3], labels=labels, srt=300,
     xpd=TRUE, adj=c(-0.3,1.2),cex = 1.1)
mtext('Unge 1', side = 3, line = -1.5, outer = FALSE, at = 19,
      adj = NA, padj = NA, cex = 1.5, col = 'grey4', font = NA)

##**VIND*********
###### Create a date object from the string (skip this if your column is already in the Date format):
original_date <- as.POSIXct(vind$DateTime, format="%m/%d/%Y %H:%M:%S") 
vind$original_date <- original_date

##### Udvælg ønskede datoer
vind <- vind[vind$original_date >= "2019-06-15" & vind$original_date <= "2020-05-15",]
str(vind)

###### Indføj manglende datoer
library(padr)
vind <- data.frame(pad(vind,by = "original_date"))
str(vind)

#Then format the original date to the new format:
vind$Date <- format(vind$original_date, "%d.%b")   
head(vind,15)
tail(vind,15)

##### Udvælg rigtige intries med 14. dages intervaller
vind$ID <- seq_along(vind[,1])
vind[,1]
# indexer i rigtige levels
dates <- reorder(vind$Date,vind$ID) 
dates
#View(dates)
dat_num <- as.numeric(dates)
dat_num
date <- data.frame(vind$ID,dates,dat_num)
date

# remove dublicates
trim_date <- date[!duplicated(date[c("dat_num","dates")]),]
#View(trim_date)
str(trim_date)
trim_date
Thin_days <- c(trim_date$dat_num[seq(1, length(trim_date$dat_num), 14)])
Thin_days
trim_dates <- subset(trim_date$vind.ID, trim_date$dat_num %in% Thin_days)
trim_dates_names <- subset(trim_date$dates, trim_date$dat_num %in% Thin_days)
trim_dates


## Plot at mod Date
dato <- vind$DateTime
dato
vind_plot <- vind$Middelvindhastighed
# View(z_45067$at)

plot.default(dato, vind_plot, xaxt ='n', xlab = '', type = 'p',pch = 19,
             cex = .8, cex.axis=1.5, ylim = c(0,30),las=1,col='grey4')
tck <- axis(side=1, trim_dates, labels=FALSE)
labels <- trim_dates_names
text(tck, par("usr")[3], labels=labels, srt=300,
     xpd=TRUE, adj=c(-0.3,1.2),cex = 1.1)
mtext('Unge 1', side = 3, line = -1.5, outer = FALSE, at = 19,
      adj = NA, padj = NA, cex = 1.5, col = 'grey4', font = NA)

##**Regn********
###### Create a date object from the string (skip this if your column is already in the Date format):
original_date <- as.POSIXct(regn$DateTime, format="%m/%d/%Y %H:%M:%S") 
regn$original_date <- original_date

##### Udvælg ønskede datoer
regn <- regn[regn$original_date >= "2019-06-15" & regn$original_date <= "2020-05-15",]
str(regn)

###### Indføj manglende datoer
library(padr)
regn <- data.frame(pad(regn,by = "original_date"))
str(regn)

#Then format the original date to the new format:
regn$Date <- format(regn$original_date, "%d.%b")   
head(regn,15)
tail(regn,15)

##### Udvælg rigtige intries med 14. dages intervaller
regn$ID <- seq_along(regn[,1])
regn[,1]
# indexer i rigtige levels
dates <- reorder(regn$Date,regn$ID) 
dates
#View(dates)
dat_num <- as.numeric(dates)
dat_num
date <- data.frame(regn$ID,dates,dat_num)
date

# remove dublicates
trim_date <- date[!duplicated(date[c("dat_num","dates")]),]
#View(trim_date)
str(trim_date)
trim_date
Thin_days <- c(trim_date$dat_num[seq(1, length(trim_date$dat_num), 14)])
Thin_days
trim_dates <- subset(trim_date$regn.ID, trim_date$dat_num %in% Thin_days)
trim_dates_names <- subset(trim_date$dates, trim_date$dat_num %in% Thin_days)
trim_dates


## Plot at mod Date
dato <- regn$DateTime
dato
regn_plot <- regn$Nedbør
# View(z_45067$at)

plot.default(dato, regn_plot, xaxt ='n', xlab = '', type = 'p',pch = 19,
             cex = .8, cex.axis=1.5, ylim = c(0,30),las=1,col='grey4')
tck <- axis(side=1, trim_dates, labels=FALSE)
labels <- trim_dates_names
text(tck, par("usr")[3], labels=labels, srt=300,
     xpd=TRUE, adj=c(-0.3,1.2),cex = 1.1)
mtext('Unge 1', side = 3, line = -1.5, outer = FALSE, at = 19,
      adj = NA, padj = NA, cex = 1.5, col = 'grey4', font = NA)


#####################----------- flet vejrdata sammen til et data.frame ------####
str(temp)
str(vind)
str(regn)

## Fjern na i original column
temp <- temp[!is.na(temp$original_date), ]
vind <- vind[!is.na(vind$original_date), ]
regn <- regn[!is.na(regn$original_date), ]

#fjern duplicates
temp <- temp[!duplicated(temp$original_date), ]
vind <- vind[!duplicated(vind$original_date), ]
regn <- regn[!duplicated(regn$original_date), ]

vejr.data <- cbind.data.frame(temp,vind,regn)
str(vejr.data)
head(vejr.data,20)







