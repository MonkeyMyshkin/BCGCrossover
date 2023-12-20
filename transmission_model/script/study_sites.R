#visualise study sites

#Creating Figure 3 requires a shapefile with first-level administrative divisions for Ethiopia, obtained from
#https://earthworks.stanford.edu/catalog/stanford-gw295bx8934
#save the shapefile in a folder named 'gis' and place it the Data folder "./Data/gis"

Ethiopia1<-read_sf(dsn="./Data/gis", layer="ETH_adm1", stringsAsFactors = FALSE)

#Enter coordinates of study areas and convert to sf object
study_site_coords<-data.frame(city=c("Addis Ababa", "Gondar", "Hawassa", "Mekelle","Kombolcha"), 
                              latitude=c(8.9806, 12.6030, 7.0504, 13.4936, 11.0849), 
                              longitude=c(38.7578, 37.4521, 38.4955, 39.4657, 39.7292))
study_site<-st_as_sf(study_site_coords, coords=c("longitude", "latitude"), crs=4326, agr="constant")

#Plot map of Ethiopia with locations of study sites
ethiopia<-ggplot()+
  geom_sf(data=Ethiopia1, 
          fill = "#f6e8c3", colour="#525252", size=0.1)+
  geom_sf(data=study_site,
          mapping = aes(colour=city), size=8, 
          pch=19,
          show.legend=FALSE)+
  scale_color_manual(values=brewer.pal(5, "Dark2")[c(2, 1, 3:5)], #assign colours
                     breaks=c("Addis Ababa", "Gondar", "Mekelle", "Kombolcha", "Hawassa"))+  #specify order of locations in legend
  geom_sf_text(data=study_site, #%>%   #add names of study sites  to map 
               aes(label=city),
               size=12, nudge_y=0.65, family="sans")+ 
  theme_void()+
  labs(x = NULL, 
       y = NULL,
       colour=NULL,
       fill=NULL,
       title = NULL, 
       subtitle = NULL, 
       caption = NULL)+
  theme(text=element_text(size=32,  family="sans"))

#save figure 
save(ethiopia, file=paste('./transmission_model/RData_files/ethiopia.RData'))