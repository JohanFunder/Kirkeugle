#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
#install.packages("dygraphs")
library(dygraphs)

### Aktiviten intil 04/01-20
################### FOr 45067 ###########################

#Indlæs fil
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/DIST_45067.csv")
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

