###
### Mapping Census Data in R
###

####Libraries

require(acs) #Package for pulling ACS Data
#http://dusp.mit.edu/sites/dusp.mit.edu/files/attachments/publications/working_with_acs_R_v_2.0.pdf


require(leaflet) #Map Rendering
require(tigris) #download and subset shapefile form Census
require(dplyr)  #Data manipulation
require(stringr) #string manipulation


### Here we use the "acs" package to pull data from the Census API.
### This package makes it easier to pull data and understand the variables

#Census API Key, I included mine so you can try it out
#but you can get one of your own at their website

api.key<-Sys.getenv("CENSUS_API_KEY")

#Register Your API key with the package
api.key.install(key=api.key)

#Create Geography Object you Want to Pull
my.geo <- geo.make(state="KS",county=173, tract="*")

#Pull Data
yearbuilt_data<-acs.fetch(geography = my.geo,endyear = 2017,table.number="B25035")

#Create an object that we will later merge to our spatial data
#NOTE: we need to pad the county FIPS code with 0's so it has width = 3
yearbuilt_df<- data.frame( GEOID = paste0(yearbuilt_data@geography$state,
                                       str_pad(yearbuilt_data@geography$county,
                                               width=3,
                                               side="left",
                                               pad="0"),
                                       yearbuilt_data@geography$tract
                                       ),
                        yearbuilt = as.numeric(yearbuilt_data@estimate),
                        row.names=NULL)


############# Getting Shapefile and Merging Data
#We use tigris package to pull and subset 
#a shape file from Census, you can always do
#all this on your own with only a little more work
#this is just the easiest way


#download and create shapefile for Arlington, Fairfax and Alexandria
ks.shape<-tracts(state="KS",county=c(173))

#quick plot to see what it looks like
plot(ks.shape)

#create a temporary copy so that we can add data to it 
#without overwriting the original


##Merge out income data onto the spatial dataset
ks.shape@data<-ks.shape@data %>% left_join(yearbuilt_df,by="GEOID") %>% filter(yearbuilt>=0)

ks.shape@data %>% head()

#### Create Leaflet Map


### Create Color Pallete for Choropleth
pal<-colorNumeric(palette = "Purples", 
                   domain = NULL)

#Create "on-click" popup information for out map
# it simply will display the tract number and the
#median income
tract_popup <- paste0("<strong>Tract: </strong>", 
                      ks.shape@data$TRACTCE,
                      "<br><strong>",
                      "Value", 
                      ": </strong>",
                      ks.shape@data$yearbuilt)



#finally create map; view and zoom were set by trial and error
leaflet() %>% setView(lng=-97.3832762, 
                         lat=37.6836838, 
                         zoom = 10) %>% 
              #Add Map Tiles (the background map)
              addProviderTiles("CartoDB.Positron") %>% 
              #Add our Census Tracts and color them
              #using our palette
              #Also inclue pop-ups we created above
              addPolygons(data = ks.shape,weight=1,color="#000000",
                             fillColor=~pal((ks.shape@data$yearbuilt)),
                             fillOpacity = .6,
                             popup=tract_popup) %>%
               #finally include 
              addLegend("bottomleft",
                          pal=pal,
                          values =ks.shape@data$yearbuilt,
                          title="Median Year Built")



  


