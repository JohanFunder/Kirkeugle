# Set directory
directory <- "D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner/Angle/csv"
################### FOr 45067 ###########################

# First we load the csv the angles and distances
# temperatur
angle_home <- rbind(
  data.frame(c(read.csv(file=file.path(directory,"/Angle_home_45067.csv"),
                        header= T, sep = ',', dec = '.'),id="45067")),
  data.frame(c(read.csv(file=file.path(directory,"/angle_home_45068.csv"),
                        header= T, sep = ',', dec = '.'),id="45068")),
  data.frame(c(read.csv(file=file.path(directory,"/angle_home_45069.csv"),
                        header= T, sep = ',', dec = '.'),id="45069")),
  data.frame(c(read.csv(file=file.path(directory,"/angle_home_45072.csv"),
                        header= T, sep = ',', dec = '.'),id="45072")),
  data.frame(c(read.csv(file=file.path(directory,"/angle_home_45074.csv"),
                        header= T, sep = ',', dec = '.'),id="45074")),
  data.frame(c(read.csv(file=file.path(directory,"/angle_home_45076.csv"),
                        header= T, sep = ',', dec = '.'),id="45076")))

head(angle_home)
str(angle_home)
#View(angle_home)
# set cuts for heading against true North
cut <- c(-180,-135,-90,-45,0,45,90,135,180)


angle_home$cuts <- cut(angle_home$NEAR_ANGLE,cut)
angle_home

# Remove rows with -1 values
angle_home <- angle_home[angle_home$NEAR_Y != -1.00000, ]


## aggregate mean
angle_home_aggr <- aggregate(angle_home["NEAR_DIST"], 
                             by=c(angle_home["cuts"],angle_home["id"]),
                             FUN = mean, na.rm=TRUE)
## aggregate N
angle_home_aggr$N <- aggregate(angle_home["NEAR_DIST"], 
                               by=c(angle_home["cuts"],angle_home["id"]),
                             FUN = function(x) length(unique(x)))
angle_home_aggr

## Udregn occurences per group (id og cuts)
library(dplyr)
dup <- angle_home %>% group_by(id, cuts) %>% mutate(count = n())
View(dup)
### Indsæt duplicater med "0.5,1 og 2" for occurences med mindre end 3 observationer

dup_1 <- rbind(dup,
               dup %>% 
                 filter(count<4) %>% 
                 mutate(count=10000000,
                        NEAR_DIST=0.5))

dup_2 <- rbind(dup_1,
               dup_1 %>% 
                 filter(count<10000000) %>% 
                 mutate(count=20000000,
                        NEAR_DIST=1.))

dup_3 <- rbind(dup_2,
               dup_2 %>% 
                 filter(count<20000000) %>% 
                 mutate(count=30000000,
                        NEAR_DIST=1.5))

angle_home_dup <- dup_3

# tæl igen antal observationer
tæl <- aggregate(angle_home_dup["NEAR_DIST"], 
                               by=c(angle_home_dup["cuts"],angle_home_dup["id"]),
                               FUN = function(x) length(unique(x)))
View(tæl)


##########################**************** Bootsrap Bca og barplot *********###############
####### Prøv med bootstrap
#install.packages('rcompanion')
library('rcompanion')

min(angle_home$NEAR_DIST)
max(angle_home$NEAR_DIST)

## mean
angle_home_boot_mean <- groupwiseMean(
  formula = NEAR_DIST ~ id + cuts,
  data = angle_home_dup,
  var = "NEAR_DIST",
  group = c("id","cuts"),
  conf = 0.95,
  R = 10000,
  boot = TRUE,
  normal = TRUE,
  basic = TRUE,
  percentile = TRUE,
  bca = TRUE,
  digits = 3
)


angle_home_boot_mean
str(angle_home_boot_mean)
#View(angle_home_boot_mean)

# sort by cuts
angle_home_boot_mean[order(angle_home_boot_mean$cuts, decreasing = F),]

# Tilføj heading

# Create replacements data frame
#install.packages('DataCombine')
require('DataCombine')
Replaces <- data.frame(from = c("-180,-135","-135,-90","-90,-45","-45,0",
                                "0,45","45,90","90,135","135,180"),
                       to = c("South-west","South","South-east","East",
                              "North-east","North","North-west","West"))

# Replace patterns and return full data frame
angle_home_boot_mean_head <- FindReplace(data = angle_home_boot_mean,
                                         Var = "cuts", replaceData = Replaces,
                                         from = "from", to = "to", exact = FALSE)
str(angle_home_boot_mean_head)
angle_home_boot_mean_head$heading <- angle_home_boot_mean_head$cuts
levels(angle_home_boot_mean_head$heading)
sort(angle_home_boot_mean_head$cuts,decreasing = F)

newdata <- angle_home_boot_mean_head[order(angle_home_boot_mean_head$cuts),]
str(newdata)

# Remove patterns ( and ]
angle_home_boot_mean_head$cuts <- gsub("[()]", "", angle_home_boot_mean_head$cuts)

angle_home_boot_mean_head$cuts <- gsub("[]]", "", angle_home_boot_mean_head$cuts)

# Sort levels
angle_home_boot_mean_head$cuts <- factor(angle_home_boot_mean_head$cuts,
                                         levels = c("North","North-east","East","South-east",
                                                    "South","South-west","West","North-west"))

angle_home_boot_mean_head[c(1:4,15:17)]

angle_home_boot_mean_head$cuts <- factor(angle_home_boot_mean_head$cuts)
levels(angle_home_boot_mean_head$cuts)

###median
angle_home_boot_median <- groupwiseMedian(
  formula = NEAR_DIST ~ id + cuts,
  data = angle_home_dup,
  var = "NEAR_DIST",
  group = c("id","cuts"),
  conf = 0.95,
  R = 10000,
  boot = TRUE,
  normal = TRUE,
  basic = TRUE,
  percentile = TRUE,
  bca = TRUE,
  digits = 3
)

str(angle_boot_median)
#View(Rep_alle_median_ci_boot)

# Tilføj heading
angle_home_boot_median$heading <- c("North","North-east","East","South-east","South",
                               "South-west","West","North-west")

# Sort levels
angle_home_boot_median$heading <- factor(angle_home_boot_median$heading,
                                    levels = c("North","North-east","East","South-east",
                                               "South","South-west","West","North-west"))

angle_boot_median[c(2:5,13:15)]


######## Barplot SAMLET MED SAMMENLIGNINGER
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('ggpmisc')
library(ggplot2)
library(dplyr)
library(ggpmisc)
#install.packages('ggsignif')
library(ggsignif)

##### Mean
# Lav name vector
#heading <- c("North-west","South","East","North-east",
#             "North","South-west","South-east","North-west")

ggplot(angle_home_boot_mean_head, aes(x=cuts, y=Boot.mean, fill=id)) + 
  geom_bar(position=position_dodge(width = .8), stat="identity",
           colour="black", # Use black outlines,
           size=.5, width = .6) +      # Thinner lines) +
  geom_errorbar(aes(ymin=Bca.lower, ymax=Bca.upper),
                width=.2,                    # Width of the error bars
                position=position_dodge(.8)) +
  xlab("Heading") +
  ylab("Mean distance from nest site (m)") +
  scale_fill_manual(values = c("white",'lightgrey','grey55','grey45','grey35'
                               ,'grey25'),
                    name="Individuals", # Legend label, use darker colors
                    breaks=c("45067", "45068","45069","45072","45074","45076"),
                    labels=c("45067", "45068","45069","45072","45074","45076")) + 
  
#  scale_x_discrete(breaks=unique(angle_home_boot_mean_head$cuts),
#                   labels=addline_format(heading)) +
  
  theme_bw() +
  theme(legend.text=element_text(size=13),legend.title=element_text(size=15),
        axis.text=element_text(size=13),
        axis.title=element_text(size = 15)) 
#  scale_y_continuous(limits=c(0,800),breaks=seq(0,800,50)) 

# Tilføj signifikansniveauer  
geom_signif(comparisons = list(c("North", "East")), size = 0.7,
            map_signif_level=TRUE,test = "wilcox.test",y_position = 4000) +
  geom_signif(comparisons = list(c("East", "South")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 4400) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_3")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 750) +
  geom_signif(comparisons = list(c("KAT_3", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 700) +
  geom_signif(comparisons = list(c("KAT_1", "KAT_4")), size = 0.7,
              map_signif_level=TRUE,test = "wilcox.test",y_position = 800) 





############ Median
library(ggsignif)

ggplot(angle_boot_median, aes(x=heading, y=Boot.median, fill=id)) + 
  geom_bar(position=position_dodge(width = .8), stat="identity",
           colour="black", # Use black outlines,
           size=.5, width = .6) +      # Thinner lines) +
  geom_errorbar(aes(ymin=Bca.lower, ymax=Bca.upper),
                width=.2,                    # Width of the error bars
                position=position_dodge(.8)) +
  xlab("Heading") +
  ylab("Mean distance per movement (m)") +
  scale_fill_manual(values = c("white",'lightgrey','grey45','grey25'),
                    name="Individuals", # Legend label, use darker colors
                    breaks=c("45067", "45068","45072","45076"),
                    labels=c("First","Second",
                             "Third","Fourth")) +
  
  scale_x_discrete(breaks=unique(angle_aggr$heading),
                   labels=addline_format(angle_aggr$heading)) +
  
  theme_bw() +
  theme(legend.text=element_text(size=13),legend.title=element_text(size=15),
        axis.text=element_text(size=13),
        axis.title=element_text(size = 15)) 
#  scale_y_continuous(limits=c(0,800),breaks=seq(0,800,50)) 

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



