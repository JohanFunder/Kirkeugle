#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
#install.packages("dygraphs")
library(dygraphs)
library(R.utils)
library(dplyr)
library(ggplot2)

directory <- "D:/GIS_Kirkeugle/csv"
################### FOr 45067 ###########################

# First we load the two data frames 

x <- file.path(directory,"/45067_11_01_20.csv")
x <- read.csv(file=x, header= TRUE, sep = ',', dec = '.')

t <- file.path(directory,"/DIST_45067.csv")
t <- read.csv(file=t, header= TRUE, sep = ',', dec = '.')

# We use the replace() function from r.utils which replaces our NA values
# with the values from the MEAS vector at the positions given by INDEX_FIX

Meas <- rep(NA,length(x$Index_fix))
at <-replace(x = Meas,list = t$INDEX_FIX, values = t$MEAS)

z_45067 <- data.frame(x,at)
names <- rep("45067",length(z_45067$Index_fix))
z_45067 <- cbind(z_45067,names)

################### FOr 45068 ###########################

# First we load the two data frames 

x <- file.path(directory,"/45068_11_01_20.csv")
x <- read.csv(file=x, header= TRUE, sep = ',', dec = '.')

t <- file.path(directory,"/DIST_45068.csv")
t <- read.csv(file=t, header= TRUE, sep = ',', dec = '.')

# We use the replace() function from r.utils which replaces our NA values
# with the values from the MEAS vector at the positions given by INDEX_FIX

Meas <- rep(NA,length(x$Index_fix))
at <-replace(x = Meas,list = t$INDEX_FIX, values = t$MEAS)

z_45068 <- data.frame(x,at)
names <- rep("45068",length(z_45068$Index_fix))
z_45068 <- cbind(z_45068,names)

################### FOr 45072 ###########################

# First we load the two data frames 

x <- file.path(directory,"/45072_11_01_20.csv")
x <- read.csv(file=x, header= TRUE, sep = ',', dec = '.')

t <- file.path(directory,"/DIST_45072.csv")
t <- read.csv(file=t, header= TRUE, sep = ',', dec = '.')

# We use the replace() function from r.utils which replaces our NA values
# with the values from the MEAS vector at the positions given by INDEX_FIX

Meas <- rep(NA,length(x$Index_fix))
at <-replace(x = Meas,list = t$INDEX_FIX, values = t$MEAS)

z_45072 <- data.frame(x,at)
names <- rep("45072",length(z_45072$Index_fix))
z_45072 <- cbind(z_45072,names)

################### FOr 45076 ###########################

# First we load the two data frames 

x <- file.path(directory,"/45076_11_01_20.csv")
x <- read.csv(file=x, header= TRUE, sep = ',', dec = '.')

t <- file.path(directory,"/DIST_45076.csv")
t <- read.csv(file=t, header= TRUE, sep = ',', dec = '.')

# We use the replace() function from r.utils which replaces our NA values
# with the values from the MEAS vector at the positions given by INDEX_FIX

Meas <- rep(NA,length(x$Index_fix))
at <-replace(x = Meas,list = t$INDEX_FIX, values = t$MEAS)

z_45076 <- data.frame(x,at)
names <- rep("45076",length(z_45076$Index_fix))
z_45076 <- cbind(z_45076,names)

###### saml til et data-frame #####

z_samlet <- rbind.data.frame(z_45067,z_45068,z_45072,z_45076)
head(z_samlet)
#View(z_samlet)


########--------- Udtræk gennemsnitlig distance INDTIL spredningsfase -------#####
### For 45067
library('dplyr')
start <- c(1)
end   <- which(z_45067$Date == "10.sep.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45067[((stand_dupl)+start):(end_dupl+end), ]
head(df)

dist_dag <- diff(df$at)
df_dist_dag <- df[-1,]


df_dist_dag$dist_dag <- dist_dag
df_dist_dag$dist_dag
pre_disp_mean_45067 <- mean(df_dist_dag$dist_dag, na.rm = T)

### For 45068
library('dplyr')
start <- c(1)
end   <- which(z_45068$Date == "20.okt.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45068[((stand_dupl)+start):(end_dupl+end), ]
head(df)

 dist_dag <- diff(df$at)
df_dist_dag <- df[-1,]

df_dist_dag$dist_dag <- dist_dag
pre_disp_mean_45068 <- mean(df_dist_dag$dist_dag, na.rm = T)

### For 45072
library('dplyr')
start <- c(1)
end   <- which(z_45072$Date == "20.okt.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45072[((stand_dupl)+start):(end_dupl+end), ]
head(df)

dist_dag <- diff(df$at)

df_dist_dag <- df[-1,]

df_dist_dag$dist_dag <- dist_dag
df_dist_dag
pre_disp_mean_45072 <- mean(df_dist_dag$dist_dag, na.rm = T)

### For 45076
library('dplyr')
start <- which(z_45076$Date == "10.sep.")
end   <- which(z_45076$Date == "20.okt.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45076[((stand_dupl)+start):(end_dupl+end), ]
head(df)

dist_dag <- diff(df$at)
df_dist_dag <- df[-1,]

df_dist_dag$dist_dag <- dist_dag
pre_disp_mean_45076 <- mean(df_dist_dag$dist_dag, na.rm = T)

######### samlet alle pre-dispersal mean
mean(pre_disp_mean_45067, pre_disp_mean_45068,pre_disp_mean_45072, pre_disp_mean_45076)

####### Find max tilbagelagt afstand
max(diff(z_samlet$at),na.rm = T)

head(sort(c(diff(z_45067$at),diff(z_45068$at),diff(z_45072$at),diff(z_45076$at)), decreasing=TRUE), 5)
diff(z_samlet$at)

#######


########--------- Udtræk gennemsnitlig distance UNDER spredningsfase -------#####
### For 45067
library('dplyr')
start <- which(z_45067$Date == "10.sep.")
end   <- which(z_45067$Date == "20.okt.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45067[((stand_dupl)+start):(end_dupl+end), ]
head(df)

dist_dag <- c(diff(df$at))
df_dist_dag <- df[-1,]

dist_dag_45067 <- c(diff(df$at))

df_dist_dag$dist_dag <- dist_dag

disp_mean_45067 <- mean(df_dist_dag$dist_dag, na.rm = T)


### For 45068
library('dplyr')
start <- which(z_45068$Date == "10.sep.")
end   <- which(z_45068$Date == "20.okt.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45068[((stand_dupl)+start):(end_dupl+end), ]
head(df)

dist_dag <- c(diff(df$at))
df_dist_dag <- df[-1,]

dist_dag_45068 <- c(diff(df$at))

df_dist_dag$dist_dag <- dist_dag
disp_mean_45068 <- mean(df_dist_dag$dist_dag, na.rm = T)


### For 45072
library('dplyr')
start <- which(z_45072$Date == "10.sep.")
end   <- which(z_45072$Date == "20.okt.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45072[((stand_dupl)+start):(end_dupl+end), ]
head(df)

dist_dag <- c(diff(df$at))
df_dist_dag <- df[-1,]

dist_dag_45072 <- c(diff(df$at))

df_dist_dag$dist_dag <- dist_dag
disp_mean_45072 <- mean(df_dist_dag$dist_dag, na.rm = T)

### For 45076
library('dplyr')
start <- which(z_45076$Date == "10.sep.")
end   <- which(z_45076$Date == "20.okt.")
stand_dupl <- length(start)
end_dupl <- length(end)

df <- z_45076[((stand_dupl)+start):(end_dupl+end), ]
head(df)

dist_dag <- c(diff(df$at))
df_dist_dag <- df[-1,]

dist_dag_45076 <- c(diff(df$at))

df_dist_dag$dist_dag <- dist_dag
disp_mean_45076 <- mean(df_dist_dag$dist_dag, na.rm = T)

####### Find max tilbagelagt afstand UNDER spredningsfase

head(sort(rbind(dist_dag_45067,dist_dag_45068,dist_dag_45072,dist_dag_45067), decreasing=TRUE), 5)
#######




####################### Lav grafer ######################
par(mfrow=c(2,2))
par(oma=c(4.,5.5,3.5,3.5))
par(mar=c(3.8,2.3,1,1))

########### for 45067 
## Specificer dato
date <- z_45067$Date

# Remove punktum til sidst
date <- sub(".$", "", date)
date
# indexer i rigtige levels
dates <- reorder(date,z_45067$Index_fix)

# Udvælg rigtige intries med 14. dages intervaller
dat_num <- as.numeric(dates)
date <- data.frame(date,dates,dat_num)
Thin_days = c(1,15,29,43,57,71,84,98,112,126,140,154,168,182,196,210,224,238,252,266,280)
trim_date <- subset(date, dat_num %in% Thin_days)
# remove dublicates
trim_date <- trim_date[!duplicated(trim_date[c("dat_num","date")]),]
trim_date <- trim_date$dates
trim_date

#write.csv(date, file = "D:/GIS_Kirkeugle/csv/date.csv")
# lav function for dato med intervaller på n mellemrum
#nth_element <- function(vector, starting_position, n) { 
#  vector[seq(starting_position, length(vector), n)] 
#}
#trim_date <- nth_element(date, 1, 14)
#trim_date

#trim_date <- format(as.Date(trim_date), "%d.%b.")

## Plot at mod fix_date
dato <- z_45067$RTC.date[-c(239)]
dato
dist <- z_45067$at[-c(239)]/1000
dist

plot.default(dato, dist, xaxt ='n', xlab = '', type = 'p',pch = 19,
             cex = .8, cex.axis=1.5, ylim = c(0,100),las=1,col='grey4')
tck <- axis(side=1, trim_date, labels=FALSE)
labels <- trim_date
text(tck, par("usr")[3], labels=labels, srt=300,
     xpd=TRUE, adj=c(-0.3,1.2),cex = 1.1)
mtext('Unge 1', side = 3, line = -1.5, outer = FALSE, at = 13,
      adj = NA, padj = NA, cex = 1.5, col = 'grey4', font = NA)


########### for 45068 
## Specificer dato
date <- z_45068$Date

# Remove punktum til sidst
date <- sub(".$", "", date)
date
# indexer i rigtige levels
dates <- reorder(date,z_45068$Index_fix)

# Udvælg rigtige intries med 14. dages intervaller
dat_num <- as.numeric(dates)
date <- data.frame(date,dates,dat_num)
Thin_days = c(1,15,29,43,57,71,84,98,112,126,140,154,168,182,196,210,224,238,252,266,280)
trim_date <- subset(date, dat_num %in% Thin_days)
# remove dublicates
trim_date <- trim_date[!duplicated(trim_date[c("dat_num","date")]),]
trim_date <- trim_date$dates

## Plot at mod fix_date
dato <- z_45068$RTC.date[-c(98,141,156)]
dato
dist <- z_45068$at[-c(98,141,156)]/1000
dist

plot.default(dato, dist, xaxt ='n', xlab = '', type = 'p',pch = 19,
             cex = .8, cex.axis=1.5, ylim = c(0,100),las=1,col='blue4')
tck <- axis(side=1, trim_date, labels=FALSE)
labels <- trim_date
text(tck, par("usr")[3], labels=labels, srt=300,
     xpd=TRUE, adj=c(-0.3,1.2),cex = 1.1)
mtext('Unge 2', side = 3, line = -1.5, outer = FALSE, at = 13.,
      adj = NA, padj = NA, cex = 1.5, col = 'blue4', font = NA)

########### for 45072 
## Specificer dato
date <- z_45072$Date

# Remove punktum til sidst
date <- sub(".$", "", date)
date
# indexer i rigtige levels
dates <- reorder(date,z_45072$Index_fix)

# Udvælg rigtige intries med 14. dages intervaller
dat_num <- as.numeric(dates)
date <- data.frame(date,dates,dat_num)
Thin_days = c(1,15,29,43,57,71,84,98,112,126,140,154,168,182,196,210,224,238,252,266,280)
trim_date <- subset(date, dat_num %in% Thin_days)
# remove dublicates
trim_date <- trim_date[!duplicated(trim_date[c("dat_num","date")]),]
trim_date <- trim_date$dates

## Plot at mod fix_date
dato <- z_45072$RTC.date[-c(239)]
dato
dist <- z_45072$at[-c(239)]/1000
dist
str(z_45072$at[c(239)])

plot.default(dato, dist, xaxt ='n', xlab = '', type = 'p',pch = 19,
             cex = .8, cex.axis=1.5, ylim = c(0,170),las=1,col='green4')
tck <- axis(side=1, trim_date, labels=FALSE)
labels <- trim_date
text(tck, par("usr")[3], labels=labels, srt=300,
     xpd=TRUE, adj=c(-0.3,1.2),cex = 1.1)
mtext('Unge 3', side = 3, line = -1.5, outer = FALSE, at = 13.,
      adj = NA, padj = NA, cex = 1.5, col = 'green4', font = NA)

########### for 45076 
## Specificer dato
date <- z_45076$Date

# Remove punktum til sidst
date <- sub(".$", "", date)
date
# indexer i rigtige levels
dates <- reorder(date,z_45076$Index_fix)

# Udvælg rigtige intries med 14. dages intervaller
dat_num <- as.numeric(dates)
date <- data.frame(date,dates,dat_num)
Thin_days = c(1,15,29,43,57,71,84,98,112,126,140,154,168,182,196,210,224,238,252,266,280)
trim_date <- subset(date, dat_num %in% Thin_days)
# remove dublicates
trim_date <- trim_date[!duplicated(trim_date[c("dat_num","date")]),]
trim_date <- trim_date$dates

## Plot at mod fix_date
dato <- z_45076$RTC.date[-c(144,260)]
dato
dist <- z_45076$at[-c(144,260)]/1000
dist

plot.default(dato, dist, xaxt ='n', xlab = '', type = 'p',pch = 19,
             cex = .8, cex.axis=1.5, ylim = c(0,100),las=1,col='red4')
tck <- axis(side=1, trim_date, labels=FALSE)
labels <- trim_date
text(tck, par("usr")[3], labels=labels, srt=300,
     xpd=TRUE, adj=c(-0.3,1.2),cex = 1.1)
mtext('Unge 4', side = 3, line = -1.5, outer = FALSE, at = 13.,
      adj = NA, padj = NA, cex = 1.5, col = 'red4', font = NA)


## Indsæt tekst på akser for alle 4 grafer
mtext("Dato for GPS fix", side=1, cex=2, line=1.5, outer=TRUE)   
mtext("Kumuleret bevægelse (km)", side=2, cex=2, line=2, outer=TRUE)



#######################################
### FOrsøg med ggplot
p_fix <- ggplot(data = z_samlet, aes(x = RTC.date, y = at, group = names,
                                    color = names)) + facet_wrap(~names)
p_fix

p_fix + geom_point() + theme_bw()  + 
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  ylim(0, 170000) +
  ggtitle("Escape distance") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Percent disturbed (e.g. flushed)") + ylab("Flying altitude")

###########################################################################################################
# Fit regression line
require(stats)
reg_1 <- lm(MEAS[1:50] ~ Rowid[1:50], data = x)
reg_2 <- lm(MEAS[51:70] ~ Rowid[51:70], data = x)
reg_3 <- lm(MEAS[61:200] ~ Rowid[61:200], data = x)

coeff_1 <- coefficients(reg_1)
coeff_2 <- coefficients(reg_2)
coeff_3 <- coefficients(reg_3)
# equation of the line : 
eq_1 = paste0("y = ", round(coeff_1[2],1), "*x ", round(coeff_1[1],1))
eq_2 = paste0("y = ", round(coeff_2[2],1), "*x ", round(coeff_2[1],1))
eq_3 = paste0("y = ", round(coeff_3[2],1), "*x ", round(coeff_3[1],1))
# plot
plot_1 <- plot(x$MEAS, main="Mobility of 45067 until 04. January")
abline(reg_1, col="blue")
mtext(eq_1, 3, line=-11, at=20, col="blue")
abline(reg_2, col="red")
mtext(eq_2, 3, line=-5, at=60, col="red")
abline(reg_3, col="blue3")
mtext(eq_3, 3, line=-12, at=165, col="blue")

mtext("Number of GPS-fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from last position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
# mtext("Mobility of 45076 until 21. december", side=3, cex=1.5, line=1.5, outer=TRUE)


plot <- plot(x$TIMESTAMP, x$MEAS, srt = 25, adj = 1)

# Shade
dygraph(x[1:3], main="Mobility of 45076 until 21. december") %>%
  dyRangeSelector() %>%
  dySeries(label="Activity 45076", color="black") %>%
  dyShading(from="1", to="30", color="#CCEBD6") %>%
  dyShading(from="30.5", to="41", color="#FFE6E6") %>%
  dyShading(from="41.5", to="160", color="#CCEBD6")



par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

################### FOr 45068 ###########################

#Indlæs fil
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/DIST_45068.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)

mean(x$Aktivitet)
median(x$Aktivitet)

par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

plot(x$Aktivitet,ylim = c(0,6000))
plot(x$MEAS)

# Fit regression line
require(stats)
reg_1 <- lm(MEAS[1:80] ~ Rowid[1:80], data = x)
reg_2 <- lm(MEAS[81:95] ~ Rowid[81:95], data = x)
reg_3 <- lm(MEAS[131:230] ~ Rowid[131:230], data = x)

coeff_1 <- coefficients(reg_1)
coeff_2 <- coefficients(reg_2)
coeff_3 <- coefficients(reg_3)
# equation of the line : 
eq_1 = paste0("y = ", round(coeff_1[2],1), "*x ", round(coeff_1[1],1))
eq_2 = paste0("y = ", round(coeff_2[2],1), "*x ", round(coeff_2[1],1))
eq_3 = paste0("y = ", round(coeff_3[2],1), "*x ", round(coeff_3[1],1))
# plot
plot_2 <- plot(x$MEAS, main="Mobility of 45068 until 04. January")
abline(reg_1, col="blue")
mtext(eq_1, 3, line=-11, at=20, col="blue")
abline(reg_2, col="red")
mtext(eq_2, 3, line=-5, at=60, col="red")
abline(reg_3, col="blue3")
mtext(eq_3, 3, line=-12, at=165, col="blue")

mtext("Number of GPS-fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from last position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
# mtext("Mobility of 45076 until 21. december", side=3, cex=1.5, line=1.5, outer=TRUE)


plot <- plot(x$TIMESTAMP, x$MEAS, srt = 25, adj = 1)

# Shade
dygraph(x[1:3], main="Mobility of 45076 until 21. december") %>%
  dyRangeSelector() %>%
  dySeries(label="Activity 45076", color="black") %>%
  dyShading(from="1", to="30", color="#CCEBD6") %>%
  dyShading(from="30.5", to="41", color="#FFE6E6") %>%
  dyShading(from="41.5", to="160", color="#CCEBD6")



par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

################### For 45072 ###########################

#Indlæs fil
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/DIST_45072.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)

mean(x$Aktivitet)
median(x$Aktivitet)

par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

plot(x$Aktivitet,ylim = c(0,6000))
plot(x$MEAS)

# Fit regression line
require(stats)
reg_1 <- lm(MEAS[1:30] ~ Rowid[1:30], data = x)
reg_2 <- lm(MEAS[35:55] ~ Rowid[35:55], data = x)
reg_3 <- lm(MEAS[61:110] ~ Rowid[61:110], data = x)
reg_4 <- lm(MEAS[111:120] ~ Rowid[111:120], data = x)
reg_5 <- lm(MEAS[121:130] ~ Rowid[121:130], data = x)

coeff_1 <- coefficients(reg_1)
coeff_2 <- coefficients(reg_2)
coeff_3 <- coefficients(reg_3)
coeff_4 <- coefficients(reg_4)
coeff_5 <- coefficients(reg_5)

# equation of the line : 
eq_1 = paste0("y = ", round(coeff_1[2],1), "*x ", round(coeff_1[1],1))
eq_2 = paste0("y = ", round(coeff_2[2],1), "*x ", round(coeff_2[1],1))
eq_3 = paste0("y = ", round(coeff_3[2],1), "*x ", round(coeff_3[1],1))
eq_4 = paste0("y = ", round(coeff_4[2],1), "*x ", round(coeff_4[1],1))
eq_5 = paste0("y = ", round(coeff_5[2],1), "*x ", round(coeff_5[1],1))
# plot
plot_3 <- plot(x$MEAS, main="Mobility of 45072 until 04. January")
abline(reg_1, col="blue")
mtext(eq_1, 3, line=-11, at=20, col="blue")
abline(reg_2, col="red")
mtext(eq_2, 3, line=-5, at=60, col="red")
abline(reg_3, col="blue3")
mtext(eq_3, 3, line=-8, at=78, col="blue")
abline(reg_4, col="red")
mtext(eq_4, 3, line=-4, at=94, col="red")
abline(reg_5, col="blue3")
mtext(eq_5, 3, line=-1, at=100, col="blue")

mtext("Number of GPS-fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from last position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
# mtext("Mobility of 45076 until 21. december", side=3, cex=1.5, line=1.5, outer=TRUE)


plot <- plot(x$TIMESTAMP, x$MEAS, srt = 25, adj = 1)

# Shade
dygraph(x[1:3], main="Mobility of 45076 until 21. december") %>%
  dyRangeSelector() %>%
  dySeries(label="Activity 45076", color="black") %>%
  dyShading(from="1", to="30", color="#CCEBD6") %>%
  dyShading(from="30.5", to="41", color="#FFE6E6") %>%
  dyShading(from="41.5", to="160", color="#CCEBD6")



par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

################### For 45076 ###########################

#Indlæs fil
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/DIST_45076.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)

mean(x$Aktivitet)
median(x$Aktivitet)

par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

plot(x$Aktivitet,ylim = c(0,6000))
plot(x$MEAS)

# Fit regression line
require(stats)
reg_1 <- lm(MEAS[1:30] ~ Rowid[1:30], data = x)
reg_2 <- lm(MEAS[31:40] ~ Rowid[31:40], data = x)
reg_3 <- lm(MEAS[41:200] ~ Rowid[41:200], data = x)

coeff_1 <- coefficients(reg_1)
coeff_2 <- coefficients(reg_2)
coeff_3 <- coefficients(reg_3)
# equation of the line : 
eq_1 = paste0("y = ", round(coeff_1[2],1), "*x ", round(coeff_1[1],1))
eq_2 = paste0("y = ", round(coeff_2[2],1), "*x ", round(coeff_2[1],1))
eq_3 = paste0("y = ", round(coeff_3[2],1), "*x ", round(coeff_3[1],1))
# plot
plot_4 <- plot(x$MEAS, main="Mobility of 45076 until 04. January")
abline(reg_1, col="blue")
mtext(eq_1, 3, line=-12, at=16, col="blue")
abline(reg_2, col="red")
mtext(eq_2, 3, line=-5, at=50, col="red")
abline(reg_3, col="blue3")
mtext(eq_3, 3, line=-8, at=165, col="blue")

mtext("Number of GPS-fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from last position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
# mtext("Mobility of 45076 until 21. december", side=3, cex=1.5, line=1.5, outer=TRUE)

plot <- plot(x$TIMESTAMP, x$MEAS, srt = 25, adj = 1)

# Shade
dygraph(x[1:3], main="Mobility of 45076 until 21. december") %>%
  dyRangeSelector() %>%
  dySeries(label="Activity 45076", color="black") %>%
  dyShading(from="1", to="30", color="#CCEBD6") %>%
  dyShading(from="30.5", to="41", color="#FFE6E6") %>%
  dyShading(from="41.5", to="160", color="#CCEBD6")



par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))



################ Alle fire grafer samlet ######################
par(mfrow=c(2,2))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

