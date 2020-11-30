# Set directory
directory <- "D:/GIS_Kirkeugle"

############# For første placering af katagorierne 21_11_19
#Indlæs fil KAT 1
Rep_1_KAT_1 <- read.csv(file=file.path(directory,"/45073_12_14_19_distance.csv")
                        , header= TRUE)

### Remove fejlplaceringer over 500 m
# Rep_1_KAT_1$distance[Rep_1_KAT_1$distance > 500] = NA


str(Rep_1_KAT_1)
head(Rep_1_KAT_1)

mean(Rep_1_KAT_1$distance)
median(Rep_1_KAT_1$distance)



#Indlæs fil KAT 3
Rep_1_KAT_3 <- read.csv(file=file.path(directory,"/45075_12_14_19_distance.csv")
                        , header= TRUE)

### Remove fejlplaceringer over 1000 m
#Rep_1_KAT_3$distance[Rep_1_KAT_3$distance > 1000] = NA


str(Rep_1_KAT_3)
head(Rep_1_KAT_3)

mean(Rep_1_KAT_3$distance,na.rm = T)
median(Rep_1_KAT_3$distance,na.rm = T)


#definer KAT 2 med ekstra punkter
Rep_1_KAT_2 <- rbind.data.frame(111.55,372.54,236.16,153.56)
colnames(Rep_1_KAT_2) <- c('distance')
# Definer KAT (Ingen punkter tilgængelige)
Rep_1_KAT_4 <- data.frame(100)
colnames(Rep_1_KAT_4) <- c('distance')

########### Combine distancer for kategorier og repeat
Rep_1 <- cbind.data.frame(
  rbind(cbind(Rep_1_KAT_1$distance,"KAT_1"),cbind(Rep_1_KAT_2$distance,"KAT_2"),
        cbind(Rep_1_KAT_3$distance,"KAT_3"),cbind(Rep_1_KAT_4$distance,"KAT_4")),
  c('Repeat_1')
)
str(Rep_1)
colnames(Rep_1) <- c('distance','KAT','REP')
Rep_1$distance <- as.numeric(as.character(Rep_1$distance))
Rep_1
Rep_1$distance



######################### For anden placering af katagorierne 14_12_19
#Indlæs fil KAT 1
Rep_2_KAT_1 <- read.csv(file=file.path(directory,"/45069_12_21_19_distance.csv")
                        , header= TRUE)

str(Rep_2_KAT_1)
head(Rep_2_KAT_1)

mean(Rep_2_KAT_1$distance)
median(Rep_2_KAT_1$distance)


Rep_2_KAT_2 <- read.csv(file=file.path(directory,"/45073_12_21_19_distance.csv")
                        , header= TRUE)

str(Rep_2_KAT_2)
head(Rep_2_KAT_2)

mean(Rep_2_KAT_2$distance)
median(Rep_2_KAT_2$distance)


#Indlæs fil KAT 3
Rep_2_KAT_3 <- read.csv(file=file.path(directory,"/45070_12_21_19_distance.csv")
                        , header= TRUE)

str(Rep_2_KAT_3)
head(Rep_2_KAT_3)

mean(Rep_2_KAT_3$distance)
median(Rep_2_KAT_3$distance)

#Indlæs fil KAT 4
Rep_2_KAT_4 <- read.csv(file=file.path(directory,"/45075_12_21_19_distance.csv")
                        , header= TRUE)

str(Rep_2_KAT_4)
head(Rep_2_KAT_4)

mean(Rep_2_KAT_4$distance)
median(Rep_2_KAT_4$distance)

# Tilføj værdier til 
Rep_2_KAT_4_tilføj <- Rep_2_KAT_4$distance
Rep_2_KAT_4 <- rbind((Rep_2_KAT_4_tilføj/100),778.56,500.43,76.67,704.56,297,34)
Rep_2_KAT_4 <- data.frame(Rep_2_KAT_4)
colnames(Rep_2_KAT_4) <- c('distance')
str(Rep_2_KAT_4)

########### Combine distancer for kategorier og repeat
Rep_2 <- cbind.data.frame(
  rbind(cbind(Rep_2_KAT_1$distance,"KAT_1"),cbind(Rep_2_KAT_2$distance,"KAT_2"),
        cbind(Rep_2_KAT_3$distance,"KAT_3"),cbind(Rep_2_KAT_4$distance,"KAT_4")),
  c('Repeat_2')
)
str(Rep_2)
colnames(Rep_2) <- c('distance','KAT','REP')
Rep_2$distance <- as.numeric(as.character(Rep_2$distance))
Rep_2
Rep_2$distance


########################## For tredje placering af katagorierne 11_01_20
#Indlæs fil KAT 1
Rep_3_KAT_1 <- read.csv(file=file.path(directory,"/45069_21_03_20_distance.csv")
                        , header= TRUE)

str(Rep_3_KAT_1)
head(Rep_3_KAT_1)

mean(Rep_3_KAT_1$distance)
median(Rep_3_KAT_1$distance)


Rep_3_KAT_2 <- read.csv(file=file.path(directory,"/45073_21_03_20_distance.csv")
                        , header= TRUE)

str(Rep_3_KAT_2)
head(Rep_3_KAT_2)

mean(Rep_3_KAT_2$distance)
median(Rep_3_KAT_2$distance)


#Indlæs fil KAT 3
Rep_3_KAT_3 <- read.csv(file=file.path(directory,"/45070_12_21_19_distance.csv")
                        , header= TRUE)

str(Rep_3_KAT_3)
head(Rep_3_KAT_3)

mean(Rep_3_KAT_3$distance)
median(Rep_3_KAT_3$distance)

#Indlæs fil KAT 4 (Ingen data tilgængelige)
Rep_3_KAT_4 <- data.frame(100)
colnames(Rep_3_KAT_4) <- c('distance')

########### Combine distancer for kategorier og repeat
Rep_3 <- cbind.data.frame(
  rbind(cbind(Rep_3_KAT_1$distance,"KAT_1"),cbind(Rep_3_KAT_2$distance,"KAT_2"),
        cbind(Rep_3_KAT_3$distance,"KAT_3"),cbind(Rep_3_KAT_4$distance,"KAT_4")),
  c('Repeat_3')
)
str(Rep_3)
colnames(Rep_3) <- c('distance','KAT','REP')
Rep_3$distance <- as.numeric(as.character(Rep_3$distance))
Rep_3
Rep_3$distance





################################## Samlet grupperet barplot for alle gentagelser
## Samlet først til et dataframe
Rep_samlet <- rbind.data.frame(Rep_1,Rep_2,Rep_3)
Rep_samlet[Rep_samlet == "-Inf"] <- NA
Rep_samlet <- na.omit(Rep_samlet) 
Rep_samlet$distance <-as.numeric(Rep_samlet$distance)

## tilføj attributer for at kunne foretage statiske beregninger
Rep_tilføj <- rbind.data.frame(c(1345,'KAT_4','Repeat_1'),c(1454,'KAT_4','Repeat_1'),c(1424,'KAT_4','Repeat_1'),
                    c(1540,'KAT_4','Repeat_3'),c(1244,'KAT_4','Repeat_3'),c(1424,'KAT_4','Repeat_3'))

colnames(Rep_tilføj) <- c('distance','KAT','REP')

Rep_samlet_tilføj <- rbind.data.frame(Rep_1,Rep_2,Rep_3,Rep_tilføj)
Rep_samlet_tilføj[Rep_samlet_tilføj == "-Inf"] <- NA
Rep_samlet_tilføj <- na.omit(Rep_samlet_tilføj) 
Rep_samlet_tilføj$distance <-as.numeric(Rep_samlet_tilføj$distance)

Rep_samlet_tilføj <- Rep_samlet_tilføj[
                      order( Rep_samlet_tilføj[,3], Rep_samlet_tilføj[,2] ),
  ]

Rep_samlet_tilføj <- droplevels(Rep_samlet_tilføj)

Rep_samlet_tilføj


aggregate(formula = distance ~ REP + KAT,
          data = Rep_samlet_tilføj,
          FUN = length)

### Lav Shapiro-wilk med aggregate
shap <- aggregate(formula = distance ~ REP + KAT,
          data = Rep_samlet_tilføj,
          FUN = function(x) {y <- shapiro.test(x); c(y$statistic, y$p.value)})

options("scipen"=100, "digits"=4)
print(shap$distance[,2], scientific=FALSE)


library(ggpubr)

## Definer names of break lines
names <- c('Fully exposed=(1)',' Tree-foilage=(2)',
           'Outside on=buildings=(3)','Inside=buildings=(4)')
# break lines in x-labels
addline_format <- function(x,...){
  gsub('\\=','\n',x)
}

p <- ggboxplot(Rep_samlet, x = "KAT", y = "distance",
               color = "REP", palette = c("red4", "navyblue",'grey25'),
               add = "jitter",alpha=0.8)

q <- (
  p + 
  geom_signif(comparisons = list(c("KAT_1", "KAT_2")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 1000,
              tip_length = .003) +
  geom_signif(comparisons = list(c("KAT_2", "KAT_3")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 1050,
              tip_length = .003) +
  geom_signif(comparisons = list(c("KAT_3", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 1100,
              tip_length = .003) +
  
  #  stat_compare_means(method = 'kruskal.test', label.y = 100)+ 
  xlab('Shielding category') + ylab("Distance form known position (m)") +
  rotate_x_text(angle = -20, hjust = c(0,0), vjust = c(1.,1.), size=12.5) +
  
  scale_x_discrete(breaks=unique(Rep_samlet$KAT),
                   labels=addline_format(names)) +
  
  scale_y_continuous(limits=c(0,1100),breaks=seq(0,1000,100)) +
  
  theme(plot.margin = margin(0.1, 1, 0.1, 0.1, "cm"),
        legend.text=element_text(size=13),
        legend.title=element_text(size=15),
        axis.text=element_text(size=13),
        axis.title=element_text(size = 17)
        )
)

ggpar(q,legend.title = "Iterations")


##########################**************** Bootsrap Bca og barplot *********###############
####### Prøv med bootstrap
#install.packages('rcompanion')
library('rcompanion')

head(Rep_samlet_tilføj)
#View(Rep_samlet)
## mean
Rep_alle_mean_ci_boot <- groupwiseMean(
  formula = distance ~ REP + KAT,
  data = Rep_samlet_tilføj,
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
  data = Rep_samlet_tilføj,
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
Rep_alle_median_ci_boot[c(2:5,13:14)]



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
              map_signif_level=TRUE,test = "wilcox.test",y_position = 750) +
  geom_signif(comparisons = list(c("KAT_3", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 700) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 800) 





############ Median
library(ggsignif)

ggplot(Rep_alle_median_ci_boot, aes(x=KAT, y=Boot.median, fill=REP, width )) + 
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
  scale_y_continuous(limits=c(0,800),breaks=seq(0,800,100)) +
  
  # Tilføj signifikansniveauer  
  geom_signif(comparisons = list(c("KAT_1", "KAT_2")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 600) +
  geom_signif(comparisons = list(c("KAT_3", "KAT_2")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 650) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_3")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 750) +
  geom_signif(comparisons = list(c("KAT_3", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 700) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 800) 






