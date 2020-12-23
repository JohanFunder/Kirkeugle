# Set directory
directory <- "D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner/Angle/csv"
################### FOr 45067 ###########################

# First we load the csv the angles and distances
# temperatur
angle <- rbind(
  data.frame(c(read.csv(file=file.path(directory,"/angle_45067.csv"),
           header= T, sep = ',', dec = '.'),id="45067")),
           data.frame(c(read.csv(file=file.path(directory,"/angle_45068.csv"),
           header= T, sep = ',', dec = '.'),id="45068")),
           data.frame(c(read.csv(file=file.path(directory,"/angle_45072.csv"),
           header= T, sep = ',', dec = '.'),id="45072")),
           data.frame(c(read.csv(file=file.path(directory,"/angle_45076.csv"),
           header= T, sep = ',', dec = '.'),id="45076")))
  
angle

angle$cuts <- cut(angle$Angle,(360/45))

angle

### Udregn mean values
angle_aggr <- aggregate(angle["DIST"], by=angle["cuts"],FUN=mean, na.rm=TRUE)

## Påsæt headings i ord
angle_aggr$heading <- c("North","North-east","East","South-east","South",
                        "South-west","West","North-west")


### Udregn CI
angle_DIST_sd <- aggregate(angle["DIST"], by=angle["cuts"],FUN=sd, na.rm=TRUE)
angle_DIST_se <- angle_DIST_sd$DIST/length(angle_DIST_sd)

angle_aggr$CI <- angle_DIST_se*1.96
angle_aggr


####### Plot angle #######
library(ggplot2)
library(ggpubr)

# break lines in x-labels
addline_format <- function(x,...){
  gsub('\\-','\n',x)
}

## Plot for DIST 
ggplot()  + ylim(-2300,4000) +
  
  geom_bar(data=angle_aggr, mapping = aes(x=heading,y=DIST),
           stat = "identity",fill = "lightgrey",colour="black",width = 0.9,
           position = position_nudge(x = .0)) +
  
  geom_errorbar(data=angle_aggr, 
                aes(ymin=DIST-CI,ymax=DIST+CI,x=heading),
                width=0.2, position = position_nudge(x = .0),size=0.5) +
  
  scale_x_discrete(breaks=unique(angle_aggr$heading),
                   labels=addline_format(angle_aggr$heading)) +
  
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
#        axis.text.x = element_text(angle=-45, vjust=0.3, hjust=1),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right")






## Plot for KUMMULERET 
ggplot()  + ylim(0,250) +
  
  geom_bar(data=angle, mapping = aes(x=cuts),
           stat = "count",fill = "lightgrey",colour="black",width = 0.9,
           position = position_nudge(x = .0)) +

  
  scale_x_discrete(breaks=unique(angle_aggr$cuts),
                   labels=addline_format(angle_aggr$heading)) +
  
  
  
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        #        axis.text.x = element_text(angle=-45, vjust=0.3, hjust=1),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right") +
  
  xlab("Heading") +
  ylab("Number of registred movements")




##########################**************** Bootsrap Bca og barplot *********###############
####### Prøv med bootstrap
#install.packages('rcompanion')
library('rcompanion')

head(angle)
#View(Rep_samlet)
## mean
angle_boot_mean <- groupwiseMean(
  formula = DIST ~ id + cuts,
  data = angle,
  var = "DIST",
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

str(angle_boot_mean)
angle_boot_mean

# Tilføj heading
angle_boot_mean$heading <- c("North","North-east","East","South-east","South",
                        "South-west","West","North-west")
# Sort levels
angle_boot_mean$heading <- factor(angle_boot_mean$heading,
                    levels = c("North","North-east","East","South-east","South",
                               "South-west","West","North-west"))

angle_boot_mean[c(2:4,15:17)]

###median
angle_boot_median <- groupwiseMedian(
  formula = DIST ~ id + cuts,
  data = angle,
  var = "DIST",
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
angle_boot_median$heading <- c("North","North-east","East","South-east","South",
                             "South-west","West","North-west")

# Sort levels
angle_boot_median$heading <- factor(angle_boot_median$heading,
                                  levels = c("North","North-east","East","South-east","South",
                                             "South-west","West","North-west"))

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
## Mean
ggplot(angle_boot_mean, aes(x=heading, y=Boot.mean, fill=id)) + 
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






