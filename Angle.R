# Set directory
directory <- "D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner/Angle/csv"
################### FOr 45067 ###########################

# First we load the csv the angles and distances
# temperatur
angle <- rbind.data.frame(
  read.csv(file=file.path(directory,"/angle_45067.csv"),
           header= T, sep = ',', dec = '.'))
  
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
ggplot()  + ylim(0,80) +
  
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

