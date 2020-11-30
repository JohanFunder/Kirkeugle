str(z_samlet_vejr)
#View(z_samlet_vejr)

# trim entries væk hvor value mindre end forrige row entri
nrow(z_samlet_vejr)
z_samlet_vejr <- z_samlet_vejr[z_samlet_vejr$dist_dag != 0,]



### Lav ggplot over kumuleret bevægelse for alle ugler samlet
library(ggplot2)
library(scales)

ggplot() +
  
  scale_y_continuous(breaks = seq(0, 260, by = 20)) +
  
  
  geom_line(data = z_samlet_vejr[!is.na(z_samlet_vejr$at),], aes(x = as.Date(original_date_ts), 
                                  y = (at/1000),
                                  color = names),
            size = 1.1,
            inherit.aes = F,
            position = position_nudge(x = 1.3),
            show.legend = T,na.rm = T) + 
  
  scale_colour_manual(values=c("45067"="black","45068"="navyblue","45072"="green4","45076"="red4"),
                      name="GPS-tag number") +
  
  scale_x_date(breaks = seq(as.Date("2019-06-15"), as.Date("2020-05-05"), by="1 weeks"),
               labels = date_format(format = "%d. %b"), expand = c(0.01,0.01),
               limits = c(as.Date("2019-06-15"),as.Date("2020-05-02"))) +
  
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(face="bold"),
        axis.text.y.right = element_text(face="bold",color='navyblue'),
        axis.title.y.right = element_text(color = "navyblue"),
        axis.text.x = element_text(angle=90, vjust=0.3, hjust=1),
        panel.grid.minor = element_blank(),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.box.just = "left") +
  
  xlab("Time - weeks") +
  ylab("Dispersal cumulated distance (km)")
  
 