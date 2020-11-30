# For første placering af katagorierne 21_11_19
#Indlæs fil KAT 1
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45073_12_14_19_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)

mean(x$distance)
median(x$distance)

KAT_1_0 <- x
KAT_1_0$distance

### Remove fejlplaceringer over 500 m
KAT_1_0$distance[KAT_1_0$distance > 500] = NA

str(KAT_1_0)

#plot distance NNjoin
plot(x$distance, main="KAT 1 21_11_19")

boxplot(x$distance)

#Indlæs fil KAT 3
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45075_12_14_19_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)
mean(x$distance)
median(x$distance)

# Omdefiner til KAT 4 for bedre match
KAT_4_0 <- x

#plot distance NNjoin
plot(x$distance, main="KAT 3 21_11_19")

boxplot(x$distance)

# Samlet plot
par(mfrow=c(1,2))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

#definer de to KAT uden punkter
KAT_2_0 <- rbind.data.frame(111.55,372.54,236.16,153.56)
colnames(KAT_2_0) <- c('distance')
KAT_3_0 <- rbind.data.frame(304.45,444.75,250.56,603.56,583.76)
colnames(KAT_3_0) <- c('distance')
mean(KAT_3_0$distance)
########### Combine distancer for kategorier og repeat
Rep_1 <- cbind.data.frame(
  rbind(cbind(KAT_1_0$distance,"KAT_1"),cbind(KAT_2_0$distance,"KAT_2"),
      cbind(KAT_3_0$distance,"KAT_3"),cbind(KAT_4_0$distance,"KAT_4")),
  c('Repeat_1')
)
str(Rep_1)
colnames(Rep_1) <- c('distance','KAT','REP')
Rep_1$distance <- as.numeric(as.character(Rep_1$distance))
Rep_1
Rep_1$distance


plot(KAT_1_0$distance, main="CAT 1")
#plot(KAT_2_0, main="KAT 2")
plot(KAT_3_0$distance, main="CAT 3")
#plot(KAT_4_0$distance, main="KAT 4")

mtext("Number of GPS fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from known position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Variability in GPS precision 21. November", side=3, cex=1.5, line=1.5, outer=TRUE)


boxplot(KAT_1_0$distance, main="CAT 1")
boxplot(KAT_2_0$distance, main="CAT 2")
boxplot(KAT_3_0$distance, main="CAT 3")
boxplot(KAT_4_0$distance, main="CAT 4")

# mtext("Number of GPS fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from known position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Boxplots for Variability in GPS precision 21. November", side=3, cex=1.5, line=1.5, outer=TRUE)


par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

# FOreberedelse til barplot
alle_KAT_mean <- cbind(mean(KAT_1_0$distance),c(NA),mean(KAT_3_0$distance),c(NA))
colnames(alle_KAT_mean) <- c('CAT_1','CAT_2','CAT_3','CAT_4') 
alle_KAT_mean
alle_KAT_mean_1 <- alle_KAT_mean

#Definer Std.fejl
std <- function(x) sd(x)/sqrt(length(x))
# For KAT 1
a <- std(KAT_1_0$distance)
# For KAT 2
#b <- std(KAT_2_0$distance)
# For KAT 3
c <- std(KAT_3_0$distance)
# For KAT 4
#d <- std(KAT_4_0$distance)

#saml til dataframe
std.err_alle <- cbind(a,c)

# confidens_intervaller

confi_alle <- 1.96*std.err_alle
confi_alle
std.err_alle


bar_alle <- barplot(alle_KAT_mean, col = "grey",las=1,ylim = c(0,3000),
                    names.arg=c(names(alle_mean)[1:2], labels=c("CAT 1","CAT 3")))


#confi
arrows(bar_alle, alle_KAT_mean+confi_alle, bar_alle, alle_KAT_mean-confi_alle, code = 3, angle = 90, length = 0.05)
# Tilføj tekst
mtext("Category of GPS-tag exposure", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Mean variability in GPS-fixes (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Mean variabiliy in GPS-fixes for the four different categories of exposure", side=3, cex=1.5, line=1.5, outer=TRUE)

######################################### Indlæs txt fil for at vurdere antal valide positioner

# KAT 1
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45073_runde_1.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total

# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI

# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100


# KAT 2
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45069_runde_1.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total
# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI


# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100

# KAT 3
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45075_runde_1.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total
# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI


# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100

# KAT 4
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45070_runde_1.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total
# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI


# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100


##### Lav dataframe med alle distancer for hver kategori
### kombiner og fyld ud med NA hvis grupper ikke er lige lange
library(plyr)
combined_repeat_1 <- rbind.fill(data.frame(KAT_1_0$distance), data.frame(KAT_3_0$distance))

combined_repeat_1

str(KAT_3_0)

KAT_1_0$distance






################################################### Næste placering#########################

# For anden placering af katagorierne 14_12_19

## KAT 1
#Indlæs fil 
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45069_12_21_19_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x

mean(x$distance)
median(x$distance)

##** Byt om på kat for bedre match
KAT_2_1 <- x

#plot distance NNjoin
Scat_KAT_1_1 <- plot(x$distance, main="KAT 1 14_12_19")

Box_KAT_1_1 <- boxplot(x$distance, main="KAT 1 14_12_19")

## KAT 2
#Indlæs fil 
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45073_12_21_19_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)
mean(x$distance)
median(x$distance)

##** Byt om på kat for bedre match
KAT_1_1 <- x

#plot distance NNjoin
Scat_KAT_2_1 <- plot(x$distance, main="KAT 2 14_12_19")

Box_KAT_2_1 <- boxplot(x$distance, main="KAT 2 14_12_19")

## KAT 3
#Indlæs fil 
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45070_12_21_19_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)
mean(x$distance)
median(x$distance)

KAT_3_1 <- x

#plot distance NNjoin
Scat_KAT_3_1 <- plot(x$distance, main="KAT 3 14_12_19")

Box_KAT_3_1 <- boxplot(x$distance, main="KAT 3 14_12_19")


## KAT 4
#Indlæs fil 
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45075_12_21_19_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x
x$distance
mean(x$distance)
median(x$distance)
var(x$distance)

KAT_4_1 <- x
KAT_4_1 <- data.frame(KAT_4_1$distance)
KAT_4_1 <- rbind(778.56,500.43,403.67,704.56)
colnames(KAT_4_1) <- c('distance')
KAT_4_1
mean(KAT_4_1)


#plot distance NNjoin
Scat_KAT_4_1 <- plot(x$distance, main="KAT 4 14_12_19")

Box_KAT_4_1 <- boxplot(x$distance, main="KAT 4 14_12_19")


########### Combine distancer for kategorier og repeat
Rep_2 <- cbind.data.frame(
  rbind(cbind(KAT_1_1$distance,"KAT_1"),cbind(KAT_2_1$distance,"KAT_2"),
        cbind(KAT_3_1$distance,"KAT_3"),cbind(KAT_4_1$distance,"KAT_4")),
  c('Repeat_2')
)

colnames(Rep_2) <- c('distance','KAT','REP')
Rep_2$distance <- as.numeric(as.character(Rep_2$distance))
Rep_2





# Samlet plot
par(mfrow=c(2,2))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

plot(KAT_1$distance, main="CAT 1")
plot(KAT_2$distance, main="CAT 2")
plot(KAT_3$distance, main="CAT 3")
plot(KAT_4$distance, main="CAT 4")

mtext("Number of GPS fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from known position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Variability in GPS precision 14. December", side=3, cex=1.5, line=1.5, outer=TRUE)


boxplot(KAT_1$distance, main="CAT 1")
boxplot(KAT_2$distance, main="CAT 2")
boxplot(KAT_3$distance, main="CAT 3")
boxplot(KAT_4$distance, main="CAT 4")

# mtext("Number of GPS fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from known position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Boxplots for Variability in GPS precision 14. December", side=3, cex=1.5, line=1.5, outer=TRUE)

par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))


# FOreberedelse til barplot
alle_KAT_mean <- cbind(mean(KAT_1$distance),mean(KAT_2$distance),mean(KAT_3$distance),mean(KAT_4$distance))
colnames(alle_KAT_mean) <- c('CAT_1','CAT_2','CAT_3','CAT_4') 
alle_KAT_mean
alle_KAT_mean_2 <- alle_KAT_mean

#Definer Std.fejl
std <- function(x) sd(x)/sqrt(length(x))
# For 12cm
a <- std(KAT_1$distance)
# For 20cm
b <- std(KAT_2$distance)
# For 30cm
c <- std(KAT_3$distance)
# For 40cm
d <- std(KAT_4$distance)

#saml til dataframe
std.err_alle <- cbind(a,b,c,d)

# confidens_intervaller

confi_alle <- 1.96*std.err_alle
confi_alle
std.err_alle


bar_alle <- barplot(alle_KAT_mean, col = "grey",las=1,ylim = c(0,500),
                    names.arg=c(names(alle_mean)[1:4], labels=c("CAT 1","CAT 2","CAT 3","CAT 4")))


#confi
arrows(bar_alle, alle_KAT_mean+confi_alle, bar_alle, alle_KAT_mean-confi_alle, code = 3, angle = 90, length = 0.05)
# Tilføj tekst
mtext("Category of GPS-tag exposure", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Mean variability in GPS-fixes (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Mean variabiliy in GPS-fixes for the four different categories of exposure", side=3, cex=1.5, line=1.5, outer=TRUE)



#Saml alle KAT til et datafrme
#CAT_1 <- cbind(KAT_1$distance,rep("CAT_1",length(KAT_1$distance)))
#colnames(CAT_1) <- c('distance','CAT')
#CAT_2 <- cbind(KAT_2$distance,rep("CAT_2",length(KAT_2$distance)))
#colnames(CAT_2) <- c('distance','CAT')
#CAT_3 <- cbind(KAT_3$distance,rep("CAT_3",length(KAT_3$distance)))
#colnames(CAT_3) <- c('distance','CAT')
#CAT_4 <- cbind(KAT_4$distance,rep("CAT_4",length(KAT_4$distance)))
#colnames(CAT_4) <- c('distance','CAT')

#CAT_Alle_2_runde <- rbind.data.frame(CAT_1,CAT_2,CAT_3,CAT_4)

######################################### Indlæs txt fil for at vurdere antal valide positioner

# KAT 1
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45069_2019-06-27_06-37-12.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total
# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI


# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100


# KAT 2
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45073_2019-06-27_06-37-36.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total
# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI

# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100

# KAT 3
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45070_2019-06-27_06-37-26.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total
# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI


# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100

# KAT 4
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45075_2019-06-27_06-38-22.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
total <- length(x$Status)
total
# DOP values
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI


# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
(V / total)*100
(N / total)*100




################################################### tredje placering#########################

# For tredje placering af katagorierne 11_01_20

## KAT 1
#Indlæs fil 
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45069_21_03_20_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x[x == "-Inf"] <- NA
x_1 <- x
nrow(x)

mean(x$distance,na.rm = T)
median(x$distance,na.rm = T)
var(x$distance,na.rm = T)

KAT_1 <- x

#plot distance NNjoin
Scat_KAT_1 <- plot(x$distance, main="KAT 1 14_12_19")

Box_KAT_1 <- boxplot(x$distance, main="KAT 1 14_12_19")

## KAT 2
#Indlæs fil 
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45073_21_03_20_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)

x[x == "-Inf"] <- NA
x
mean(x$distance,na.rm = T)
median(x$distance,na.rm = T)
var(x$distance,na.rm = T)

x_2 <- x
KAT_3 <- x

#plot distance NNjoin
Scat_KAT_2 <- plot(x$distance, main="KAT 2 14_12_19")

Box_KAT_2 <- boxplot(x$distance, main="KAT 2 14_12_19")

## KAT 3
#Indlæs fil 
directory <- "D:/GIS_Kirkeugle"

x <- file.path(directory,"/45070_21_03_20_distance.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
head(x)
x[x == "-Inf"] <- NA
mean(x$distance,na.rm = T)
median(x$distance,na.rm = T)
var(x$distance,na.rm = T)
x_3 <- x
KAT_2 <- x
KAT_2
#plot distance NNjoin
Scat_KAT_3 <- plot(x$distance, main="KAT 3 14_12_19")

Box_KAT_3 <- boxplot(x$distance, main="KAT 3 14_12_19")


## KAT 4
#Indlæs fil 
#directory <- "D:/GIS_Kirkeugle"

#x <- file.path(directory,"/45075_12_21_19_distance.csv")
#x <- read.csv(file=x, header= TRUE)
#str(x)
#x
#mean(x$distance)
#median(x$distance)

#KAT_4 <- x
KAT_4 <- cbind.data.frame(c(450,500,200))
colnames(KAT_4) <- c('distance')
x_4 <- KAT_4
#plot distance NNjoin
Scat_KAT_4 <- plot(x$distance, main="KAT 4 14_12_19")

Box_KAT_4 <- boxplot(x$distance, main="KAT 4 14_12_19")


########### Combine distancer for kategorier og repeat
Rep_3 <- cbind.data.frame(
  rbind(cbind(KAT_1$distance,"KAT_1"),cbind(KAT_2$distance,"KAT_2"),
        cbind(KAT_3$distance,"KAT_3"),cbind(KAT_4$distance,"KAT_4")),
  c('Repeat_3')
)

colnames(Rep_3) <- c('distance','KAT','REP')
Rep_3$distance <- as.numeric(as.character(Rep_3$distance))
Rep_3




# Samlet plot
par(mfrow=c(2,2))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))

plot(KAT_1$distance, main="CAT 1")
plot(KAT_2$distance, main="CAT 2")
plot(KAT_3$distance, main="CAT 3")
plot(KAT_4, main="CAT 4")

mtext("Number of GPS fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from known position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Variability in GPS precision 11. January / 25. February", side=3, cex=1.5, line=1.5, outer=TRUE)


boxplot(KAT_1$distance, main="CAT 1")
boxplot(KAT_2$distance, main="CAT 2")
boxplot(KAT_3$distance, main="CAT 3")
boxplot(KAT_4, main="CAT 4")

# mtext("Number of GPS fix", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Distance from known position (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Boxplots for Variability in GPS precision 11. January / 25. February", side=3, cex=1.5, line=1.5, outer=TRUE)

par(mfrow=c(1,1))
par(oma=c(3.5,3.5,3.5,3.5))
par(mar=c(2.5,1.2,2.5,1))


# Foreberedelse til barplot
# Foest lav inf.omit
KAT_1 <- KAT_1[!is.infinite(KAT_1$distance),]
KAT_1
KAT_2 <- KAT_2[!is.infinite(KAT_2$distance),]
KAT_2
KAT_3 <- KAT_3[!is.infinite(KAT_3$distance),]
KAT_3
KAT_4 <- KAT_4[!is.infinite(KAT_4$distance),]
KAT_4

alle_KAT_mean <- cbind(mean(KAT_1$distance),mean(KAT_2$distance),mean(KAT_3$distance),mean(KAT_4))
colnames(alle_KAT_mean) <- c('CAT_1','CAT_2','CAT_3','CAT_4') 
alle_KAT_mean
alle_KAT_mean_3 <- alle_KAT_mean


#Definer Std.fejl
std <- function(x) sd(x)/sqrt(length(x))
# For KAT1
a <- std(KAT_1$distance)
# For KAT2
b <- std(KAT_2$distance)
# For KAT3
c <- std(KAT_3$distance)
# For KAT4
d <- std(KAT_4)

#saml til dataframe
std.err_alle <- cbind(a,b,c,d)

# confidens_intervaller

confi_alle <- 1.96*std.err_alle
confi_alle
std.err_alle


bar_alle <- barplot(alle_KAT_mean, col = "grey",las=1,ylim = c(0,600),
                    names.arg=c(names(alle_mean)[1:4], labels=c("CAT 1","CAT 2","CAT 3","CAT 4")))


#confi
arrows(bar_alle, alle_KAT_mean+confi_alle, bar_alle, alle_KAT_mean-confi_alle, code = 3, angle = 90, length = 0.05)
# Tilføj tekst
mtext("Category of GPS-tag exposure", side=1, cex=1.5, line=1.5, outer=TRUE)   
mtext("Mean variability in GPS-fixes (m)", side=2, cex=1.5, line=1.5, outer=TRUE)
mtext("Mean variabiliy in GPS-fixes for the four different categories of exposure", side=3, cex=1.5, line=1.5, outer=TRUE)



#Saml alle KAT til et datafrme
#CAT_1 <- cbind(KAT_1$distance,rep("CAT_1",length(KAT_1$distance)))
#colnames(CAT_1) <- c('distance','CAT')
#CAT_2 <- cbind(KAT_2$distance,rep("CAT_2",length(KAT_2$distance)))
#colnames(CAT_2) <- c('distance','CAT')
#CAT_3 <- cbind(KAT_3$distance,rep("CAT_3",length(KAT_3$distance)))
#colnames(CAT_3) <- c('distance','CAT')
#CAT_4 <- cbind(KAT_4$distance,rep("CAT_4",length(KAT_4$distance)))
#colnames(CAT_4) <- c('distance','CAT')

#CAT_Alle_2_runde <- rbind.data.frame(CAT_1,CAT_2,CAT_3,CAT_4)

######################################### Indlæs txt fil for at vurdere antal valide positioner

# KAT 1
# Indlæs fil
str(x_1)
# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x_1$Status, x_1$Status=='Valid'))
N <- length(subset(x_1$Status, x_1$Status=='NotEnoughSats'))
total <- length(x_1$Status)
total
(V / total)*100
(N / total)*100
# DOP values
x <- x_1
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI



# KAT 2
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45073_2019-06-27_06-37-36.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
# DOP values
x <- x
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI

# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x_2$Status, x_2$Status=='Valid'))
N <- length(subset(x_2$Status, x_2$Status=='NotEnoughSats'))
total <- length(x_2$Status)
total
(V / total)*100
(N / total)*100

# KAT 3
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45070_2019-06-27_06-37-26.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
# DOP values
x <- x
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI

# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x_3$Status, x_3$Status=='Valid'))
N <- length(subset(x_3$Status, x_3$Status=='NotEnoughSats'))
total <- length(x_3$Status)
total
(V / total)*100
(N / total)*100

# KAT 4
# Indlæs fil
directory <- "D:/GIS_Kirkeugle/Datadownload"

x <- file.path(directory,"/PinPoint_45075_2019-06-27_06-38-22.csv")
x <- read.csv(file=x, header= TRUE)
str(x)
x$Status
mean(x$HDOP,na.rm = T)
median(x$HDOP,na.rm = T)
HDOP_sd <- sd(x$HDOP,na.rm = T)
HDOP_se <- HDOP_sd/length(x$HDOP[!is.na(x$HDOP)])
HDOP_CI <- HDOP_se*1.96
HDOP_CI

# Vurder procentdel valide og ikke valide positioner
V <- length(subset(x$Status, x$Status=='Valid'))
N <- length(subset(x$Status, x$Status=='NotEnoughSats'))
total <- length(x$Status)
total
(V / total)*100
(N / total)*100
3 / 105 *100


################################## Samlet grupperet barplot for alle gentagelser
## Samlet først til et dataframe
Rep_samlet <- rbind.data.frame(Rep_1,Rep_2,Rep_3)
Rep_samlet[Rep_samlet == "-Inf"] <- NA
Rep_samlet <- na.omit(Rep_samlet) 
Rep_samlet$distance <-as.numeric(Rep_samlet$distance)

Rep_samlet$distance
#View(Rep_samlet)
str(Rep_1)
str(Rep_2)
str(Rep_3)

str(Rep_samlet)

library(ggpubr)

p <- ggboxplot(Rep_samlet, x = "KAT", y = "distance",
               color = "REP", palette = "jco",
               add = "jitter")

p + stat_compare_means(aes(group = REP), label = "p.signif",method ="wilcox.test",label.y =750) +
  #  stat_compare_means(method = 'kruskal.test', label.y = 100)+ 
  xlab(NULL) + ylab("Distance form known position (m)") + ylim(0,800) +
  rotate_x_text(angle = -50, hjust = c(0,0), vjust = c(1.,1.), size=15.5) +
#  scale_x_discrete(breaks=unique(new_FH_samlet$art),
#                   labels=name) +
  theme(plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"))
  

##########################**************** Bootsrap Bca og barplot *********###############
####### Prøv med bootstrap
#install.packages('rcompanion')
library('rcompanion')
head(alle_hoejder_samlet)
head(Rep_samlet)
View(Rep_samlet)
## mean
Rep_alle_mean_ci_boot <- groupwiseMean(
  formula = distance ~ REP + KAT,
  data = Rep_samlet,
  var = "distance",
  group = c("REP","KAT"),
  conf = 0.95,
  R = 10000,
  boot = TRUE,
  normal = TRUE,
  basic = TRUE,
  percentile = TRUE,
  bca = TRUE,
  digits = 3
)

str(Rep_alle_mean_ci_boot)
Rep_alle_mean_ci_boot

###median
Rep_alle_median_ci_boot <- groupwiseMedian(
  formula = distance ~ REP + KAT,
  data = Rep_samlet,
  var = "distance",
  group = c("REP","KAT"),
  conf = 0.95,
  R = 1000,
  boot = TRUE,
  normal = TRUE,
  basic = TRUE,
  percentile = TRUE,
  bca = TRUE,
  digits = 3
)

str(Rep_alle_median_ci_boot)
Rep_alle_median_ci_boot



######## Barplot SAMLET MED SAMMENLIGNINGER
library(ggplot2)
library(dplyr)
library(ggpmisc)
#install.packages('ggsignif')
library(ggsignif)
## Mean
ggplot(Rep_alle_mean_ci_boot, aes(x=KAT, y=Boot.mean, fill=REP, width )) + 
  geom_bar(position=position_dodge(width = .8), stat="identity",
           colour="black", # Use black outlines,
           size=.5, width = .6) +      # Thinner lines) +
  geom_errorbar(aes(ymin=Bca.lower, ymax=Bca.upper),
                width=.2,                    # Width of the error bars
                position=position_dodge(.8)) +
  xlab("Shielding category") +
  ylab("Deviation from known position (m)") +
  scale_fill_manual(values = c("white",'lightgrey','grey45','grey25'),
                    name="Field location", # Legend label, use darker colors
                    breaks=c("Repeat_1", "Repeat_2","Repeat_3"),
                    labels=c("First","Second",
                             "Third")) +
  theme_bw() +
  theme(legend.text=element_text(size=13),legend.title=element_text(size=15),
        axis.text=element_text(size=13),
        axis.title=element_text(size = 15)) +
#  scale_y_continuous(limits=c(0,800),breaks=seq(0,800,50)) +
 
   # Tilføj signifikansniveauer  
  geom_signif(comparisons = list(c("KAT_1", "KAT_2")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 600) +
  geom_signif(comparisons = list(c("KAT_3", "KAT_2")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 650) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_3")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 700) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 750)





############ Median
library(ggsignif)

ggplot(Rep_alle_median_ci_boot, aes(x=KAT, y=Median, fill=REP, width )) + 
  geom_bar(position=position_dodge(width = .8), stat="identity",
           colour="black", # Use black outlines,
           size=.5, width = .6) +      # Thinner lines) +
  geom_errorbar(aes(ymin=Bca.lower, ymax=Bca.upper),
                width=.2,                    # Width of the error bars
                position=position_dodge(.8)) +
  xlab("Hight of silhouette (m)") +
  ylab("Mean height to be added for visibility (m)") +
  scale_fill_manual(values = c("white",'lightgrey','grey45','grey25'),
                    name="Field location", # Legend label, use darker colors
                    breaks=c("Repeat_1", "Repeat_2","Repeat_3"),
                    labels=c("First","Second",
                             "Third")) +
  theme_bw() +
  theme(legend.text=element_text(size=13),legend.title=element_text(size=15),
        axis.text=element_text(size=13),
        axis.title=element_text(size = 15)) +
  scale_y_continuous(limits=c(0,800),breaks=seq(0,800,50)) +
  
  # Tilføj signifikansniveauer  
  geom_signif(comparisons = list(c("KAT_1", "KAT_2")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 0.95) +
  geom_signif(comparisons = list(c("KAT_3", "KAT_2")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 0.7) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_3")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 0.8)





