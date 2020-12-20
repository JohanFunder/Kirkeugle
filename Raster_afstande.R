#install.packages('raster')
install.packages('sf')
library(sf)
library(raster)
library(plotly)
library(lattice)
library(plot3D)
library(rgl)
require(rgdal)

# Indlæs shp pointdata for de forskellige individer
points_45067 <- readOGR(dsn="D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner", layer="45067_sidste_positioner") # read .shp
points_45068 <- readOGR(dsn="D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner", layer="45068_sidste_positioner") # read .shp
points_45069 <- readOGR(dsn="D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner", layer="45069_sidste_positioner") # read .shp
points_45072 <- readOGR(dsn="D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner", layer="45072_sidste_positioner") # read .shp
points_45074 <- readOGR(dsn="D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner", layer="45074_sidste_positioner") # read .shp
points_45076 <- readOGR(dsn="D:/GIS_Kirkeugle/shp_filer_ugledata/Sidste_positioner", layer="45076_sidste_positioner") # read .shp



# Lav samlet dataframe for points
points_samlet <- NULL
points_samlet <- rbind.data.frame(c(data.frame(points_45067,id='points_45067')),
                                  c(data.frame(points_45068,id='points_45068')),
                                  c(data.frame(points_45069,id='points_45069')),
                                  c(data.frame(points_45072,id='points_45072')),
                                  c(data.frame(points_45074,id='points_45074')),
                                  c(data.frame(points_45076,id='points_45076')))

# Fjern nul-geometrirækker
#points_samlet <- points_samlet[points_samlet$coords.x1 != 0, ]

# Define id levels
library('data.table')
setattr(points_samlet$id,"levels",c('points_45067','points_45068',
                                    'points_45069','points_45072',
                                    'points_45074','points_45076'))

###### Plot data
library(ggplot2)
ggplot() + coord_fixed(ratio = 1,expand = TRUE, clip = "on") +
  
  geom_point(data = points_samlet, aes(x = coords.x1, y = coords.x2, color = id),
                                       size=2.5, alpha=0.6) +
  
  geom_path(data = points_samlet, aes(x = coords.x1, y = coords.x2, color = id),
            size=.5, alpha=0.6) +
  
  
  scale_colour_manual(values=c(points_45067="red4",points_45068="navyblue",
                               points_45069="grey50",points_45072="green4",
                               points_45074="grey30",points_45076="black"),
                      name="Activity") +
  
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        axis.text.x = element_text(face = "bold",angle=-45, vjust=0.3, hjust=0),
        panel.grid.minor = element_blank(),
        
        legend.position = "right") +
  
   scale_y_continuous(name = "Latitude", 
                     breaks = c(6270000,6280000,6290000,6300000,6310000),
                     limits = c(6279600,6310000)) +
  
  scale_x_continuous(name = "Longitude", 
                    breaks = c(540000,550000,560000,570000,580000),
                    limits = c(545000,570400))



