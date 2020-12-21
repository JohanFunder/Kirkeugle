#install.packages('raster')
#install.packages('sf')
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
points_samlet <- rbind.data.frame(c(data.frame(points_45067,id='45067')),
                                  c(data.frame(points_45068,id='45068')),
                                  c(data.frame(points_45069,id='45069')),
                                  c(data.frame(points_45072,id='45072')),
                                  c(data.frame(points_45074,id='45074')),
                                  c(data.frame(points_45076,id='45076')))

# Fjern nul-geometrirækker
#points_samlet <- points_samlet[points_samlet$coords.x1 != 0, ]

# Define id levels
library('data.table')
setattr(points_samlet$id,"levels",c('45067','45068',
                                    '45069','45072',
                                    '45074','45076'))

###### Plot data
library(ggplot2)
ggplot() + coord_fixed(ratio = 1,expand = TRUE, clip = "on") +
  
  geom_point(data = points_samlet, aes(x = coords.x1, y = coords.x2, color = id),
                                       size=2.5, alpha=0.6) +
  
  geom_path(data = points_samlet, aes(x = coords.x1, y = coords.x2, color = id),
            size=.5, alpha=0.6) +
  
  
  scale_colour_manual(values=c("45067"="black","45068"="navyblue",
                               "45069"="grey50","45072"="green4",
                               "45074"="grey30","45076"="red4"),
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


###### Plot data (updateret version)
library(ggplot2)
ggplot() + coord_fixed(ratio = 1,expand = TRUE, clip = "on") +
  
  geom_point(data = points_samlet, aes(x = coords.x1, y = coords.x2, color = id),
             size=2.5, alpha=0.6) +
  
  geom_path(data = points_samlet, aes(x = coords.x1, y = coords.x2, color = id),
            size=.5, alpha=0.6) +
  
  
  scale_colour_manual(values=c("45067"="black","45068"="navyblue",
                               "45069"="grey50","45072"="green4",
                               "45074"="grey30","45076"="red4"),
                      name="GPS-tag number") +
  guides(color = guide_legend(override.aes = list(alpha = 1,size = 1.6))) +
  
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(2,"line"),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.justification = "top") +
  
  scale_y_continuous(name = "Latitude", 
                     breaks = c(6270000,6280000,6290000,6300000,6310000),
                     limits = c(6279600,6310000)) +
  
  scale_x_continuous(name = "Longitude", 
                     breaks = c(540000,550000,560000,570000,580000),
                     limits = c(545000,570400))

# Set the size of the point in the legend to 2 mm
grid::grid.gedit("key-[-0-9]-1-1", size = unit(4, "mm"))

