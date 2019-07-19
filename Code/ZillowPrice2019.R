#install.packages("leaflet")
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(tidytext)
library(readr)
library(forcats)  #used for reordering
library(htmltools) #used for HTML 

### Load Data
propertiesTrain <- read_csv("properties_2016.csv")
transactionsTrain <- read_csv("train_2016_v2.csv")


### Glimpse of the "transaction"
## Distibution of transaction dates in "transaction"
View(transactionsTrain)
#since I just focus on Year_Month analysis, I need to extract the year and month information
#it's just on 2016, now create another new column, "month" to order the months chronologically
transactionsTrainNew <- transactionsTrain %>% 
  mutate(
  Year_Month = paste(month.abb[as.numeric(month(transactionsTrain$transactiondate))],
                     year(transactionsTrain$transactiondate), sep = " "),
  Month = factor(month.abb[as.numeric(month(transactionsTrain$transactiondate))])
  #YearMonth =as.numeric(paste(year(transactionsTrain$transactiondate),month(transactionsTrain$transactiondate),sep = ""))
)
transactionsTrainNew %>%
  group_by(Month) %>%
  ggplot(aes(x=Month))+ # add `` here to solve problem
  geom_bar(stat = "count", fill="#FF6666")+
  scale_x_discrete(limits = month.abb)+
  #geom_vline(aes(xintercept=which(transactionsTrain$transactiondate=="2016-10-01")))+
  annotate("text",x="Oct",y=6000, label="Prediction Area")+
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  ggtitle("The sum of transaction in every month on 2016")+
  xlab("2016_Month")+
  ylab("Total of Transactions")
#donnot know how to add a vertical lines
  
##Distribution of the difference of logerror in "transaction"
transactionsTrainNew %>%
  ggplot(aes(x=logerror)) +
  geom_histogram(bins = 500,fill="red")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  theme_bw()+
  coord_cartesian(x=c(-0.5,0.5))+ # the range of x axis
  ggtitle("The count of logerror on 2016")+
  xlab("Logerror")+
  ylab("Count")
#We could conclude that most logerror is relatively close to zero which means their 
#prediction is good. A positive logerror means Zestimate is overestimated while a
#negative one is underestimated.

##Distribution of the difference of absolute logerror'
transactionsTrainNew %>%
  mutate(abs_logerror=abs(logerror)) %>%
  ggplot(aes(x=abs_logerror))+
  geom_histogram(bins = 500,fill="red")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  theme_bw()+
  coord_cartesian(x=c(0,0.5))+
  ggtitle("The count of Absolute logerror on 2016")+
  xlab("Absolute Logerror")+
  ylab("Count")

##The trend of logerror changed over time
#It's the trend of the mean of logerror
transactionsTrainNew %>%
  group_by(Month) %>% 
  summarise(mean_logerror=mean(logerror)) %>%
  ggplot(aes(x=Month,y=mean_logerror,group=1))+
  scale_x_discrete(limits = month.abb)+
  geom_line(size=2,color="red")+
  geom_point(size=4,color="red")+
  theme_bw()+
  ggtitle("The count of mean logerror on 2016")+
  xlab("Absolute Logerror")+
  ylab("Count")
#It's the trend of the mean of absolute logerror
transactionsTrainNew %>%
  mutate(abs_logerror=abs(logerror)) %>%
  group_by(Month) %>%  
  summarise(mean_absolute_logerror = mean(abs_logerror)) %>%
  ggplot(aes(x=Month,y=mean_absolute_logerror,group=1))+
  scale_x_discrete(limits = month.abb)+
  geom_line(size=2,color="red")+
  geom_point(size=4,color="red")+
  theme_bw()+
  ggtitle("The count of the mean od Absolute logerror on 2016")+
  xlab("Absolute Logerror")+
  ylab("Count")
#since "group_by(Month)" divides all obversations into 12 groups, "geom_line"
#can not connect all points together. Using "group=1" to change the default value.

### Glimpse of the "propertiesTrain"
str(propertiesTrain)

##deal with some Boolean columns in "propertiesTrain"
unique(propertiesTrain$fireplaceflag)
unique(propertiesTrain$taxdelinquencyflag)
#I need to change those two columns with numeric type, 1 or 0
propertiesTrainNew <- propertiesTrain %>%
  mutate(fireplaceflag = ifelse(fireplaceflag==TRUE,1,0),
         taxdelinquencyflag = ifelse(taxdelinquencyflag=="Y",1,0))


##Show the percentage of missing value in different features 
require(data.table)
require(graphics)
#define the function to calculate the percentage
definefunction <- function(x){
  (sum(is.na(x))/length(x))
}

#
data.table(
  pmiss = as.numeric(format(round(100*sapply(propertiesTrainNew, definefunction),2),nsmall = 2)),
  column=names(propertiesTrainNew)
) %>%
  ggplot(aes(x=reorder(column,-pmiss),y=pmiss))+
  geom_bar(stat = "identity",fill="orange")+
  geom_text(aes(label=pmiss),vjust=1.5,colour="black")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 50,hjust = 2))+
  labs(
    title = "The Percent of Missing value Grouped by Features",
    x = "Feature",
    y = "The Percent of Missing Value"
  )+
  coord_flip()
#
#reorder() can only deal with numeric verctor, the below code, I add paste0() which change
#"pmiss" variable to a charactere one.
data.table(
  pmiss =  paste0(format(round(100*sapply(propertiesTrainNew, definefunction),2),nsmall = 2)
                  ,"%"),
  column=names(propertiesTrainNew)
) %>%
  mutate(column=as.factor(column) %>%
           fct_reorder(pmiss)) %>%  
  #fct_reorder() can reorder "column" based on "pmiss"
  ggplot(aes(x=fct_rev(fct_infreq(column)),y=pmiss))+
  #fct_rev(fct_infreq()) can change the order of "column"
  geom_bar(stat = "identity",fill="red")+
  geom_text(aes(label=pmiss),vjust=1.5,colour="black")+
  theme(axis.text.x = element_text(angle = 50,hjust = 2))+
  labs(
    title = "The Percent of Missing value Grouped by Features",
    x = "Feature",
    y = "The Percent of Missing Value"
  )+
  coord_flip()+
  theme_bw()

#if there are so many missing values, it means this column could not provide useful information
#in the final model. Therefore, deleting those columns
propertiesTrainNew_goodfeatures <- propertiesTrainNew %>%
  summarise_each(funs(sum(is.na(.))/n())) %>% #n(): the number of obversations in the current group
  gather(key = "feature",value = "pmiss") %>% #key,value: names of new key and value
  filter(pmiss < .75)
#summarise_each is another alternative approach to summarise()
#summarise_each(funs(X),x), X means different functions defined by yourselves or others.
#x means column
goodfeatures <- as.matrix(propertiesTrainNew_goodfeatures %>%
  select(feature))


###Combine "propertiesTrainNew" and "transactionsTrainNew" together by left-join
TransactProperties <-transactionsTrainNew %>%
  mutate(abs_logerror=abs(logerror)) %>%  #add a new column, the absolute value of logerror
  left_join(propertiesTrainNew,by="parcelid")

##Show the difference in error between the building-quality types
TransactProperties %>%
  ggplot(aes(x=as.factor(buildingqualitytypeid), y=logerror)) +
  geom_jitter(alpha=0.5,color="lightgrey") +
  geom_boxplot(color="blue",outlier.colour = NA)

##Correlation with absolute logerror in "propertiesTrainNew"
#I just focus on numeric ones

#since there aere 63 columns in "TransactProperties", we firstly need to select 33 good features,
#and then choose numeric ones from those 33 features to make a correlation plot
TransactPropertiesAllGoodFeature <- TransactProperties %>%
  select(c(goodfeatures,"abs_logerror","logerror","Year_Month"))
#Keep numeric columns from the above data frame
index <- unlist(lapply(TransactPropertiesAllGoodFeature,is.numeric))
TransactPropertiesNumericFeature <- TransactPropertiesAllGoodFeature[,index]


###Correlation plot based on "absolute logerror"
##Focus on the correlation between the number of bathroom, bedroom, garage, room, unit and absolute logerror
TransactPropertiesNumericFeatureChosen_num <- TransactPropertiesNumericFeature %>% 
  select(one_of(c("abs_logerror","bathroomcnt","bedroomcnt","calculatedbathnbr",
                  "fullbathcnt","garagecarcnt","roomcnt","unitcnt")))
corrplot(cor(TransactPropertiesNumericFeatureChosen_num, use="complete.obs"),type="lower")

##Focus on the correlation between "area" features and absolute logerror
TransactPropertiesNumericFeatureChosen_area <- TransactPropertiesNumericFeature %>%
  select(one_of(c("abs_logerror","calculatedfinishedsquarefeet","finishedsquarefeet12",
                  "garagetotalsqft","lotsizesquarefeet")))
corrplot(cor(TransactPropertiesNumericFeatureChosen_area, use="complete.obs"),type="lower")

##Focus on the correlation between "tax" features and absolute logerror
TransactPropertiesNumericFeatureChosen_tax <- TransactPropertiesNumericFeature %>%
  select(one_of(c("abs_logerror","structuretaxvaluedollarcnt","taxvaluedollarcnt"
                  ,"landtaxvaluedollarcnt","taxamount")))
corrplot(cor(TransactPropertiesNumericFeatureChosen_tax, use="complete.obs"),type="lower")

###Correlation plot based on "logerror" (not same as before three plots)
#the number of rooms
TransactPropertiesNumericFeatureChosen_num1 <- TransactPropertiesNumericFeature %>% 
  select(one_of(c("logerror","bathroomcnt","bedroomcnt","calculatedbathnbr",
                  "fullbathcnt","garagecarcnt","roomcnt","unitcnt")))
corrplot(cor(TransactPropertiesNumericFeatureChosen_num1, use="complete.obs"),type="lower")
#"area" features
TransactPropertiesNumericFeatureChosen_area1 <- TransactPropertiesNumericFeature %>%
  select(one_of(c("logerror","calculatedfinishedsquarefeet","finishedsquarefeet12",
                  "garagetotalsqft","lotsizesquarefeet")))
corrplot(cor(TransactPropertiesNumericFeatureChosen_area1, use="complete.obs"),type="lower")
#"tax" features
TransactPropertiesNumericFeatureChosen_tax1 <- TransactPropertiesNumericFeature %>%
  select(one_of(c("logerror","structuretaxvaluedollarcnt","taxvaluedollarcnt"
                  ,"landtaxvaluedollarcnt","taxamount")))
corrplot(cor(TransactPropertiesNumericFeatureChosen_tax1, use="complete.obs"),type="lower")
##Conclusion: the prediction is very good

###The year of houses built
TransactPropertiesNumericFeature %>%
  ggplot(aes(x=yearbuilt)) +
  geom_line(stat = "density",color="dark green") +
  labs(
    title = "The year of houses built",
    x = "Year",
    y = "Density"
  )+
  theme_bw()
#The total sum of density is equal to 1.
#Most houses were built after 1950.

###The trend of absolute logerror changed with the year of house built
TransactPropertiesNumericFeature %>%
  group_by(yearbuilt) %>%
  summarize(mean_abs_logerror= mean(abs(logerror),n())) %>%
  ggplot(aes(x=yearbuilt,y=mean_abs_logerror)) +
  geom_smooth(color="grey40") +
  geom_point(color="red") +
  coord_cartesian(y=c(0,0.3))+
  theme_bw()
##Conclusion: The prediction after 1950 is good and improved a lot after 1950.

###Where Zestimate predict well?
##Divide the percentiles into three parts, best predictions(top 10%), worst predictions(worst 10%),
#typical predictions(50% around the median)
transactionsTrainNew1 <- transactionsTrainNew %>% 
  mutate(abs_logerror = abs(logerror))
transactionsTrainNew_Percentile <- transactionsTrainNew1 %>%
  mutate(percentile = cut(abs_logerror, quantile(abs_logerror, 
                                                probs = c(0, .1,.25,.75,.9,1),
                                                names=FALSE), include.lowest = TRUE, labels = FALSE ))
#divide into five classes
#create three subset data frame
TransactProperties_percentile1 <- transactionsTrainNew_Percentile %>%
  filter(percentile == 1) %>%
  mutate(type = "best_predicton") %>%
  left_join(propertiesTrainNew,by="parcelid")
#percentile 1
TransactProperties_percentile2 <- transactionsTrainNew_Percentile %>%
  filter(percentile == 5) %>%
  mutate(type = "worst_prediction") %>%
  left_join(propertiesTrainNew,by="parcelid")
#percentile 2
TransactProperties_percentile3 <- transactionsTrainNew_Percentile %>%
  filter(percentile == 3) %>%
  mutate(type = "typical_prediction") %>%
  left_join(propertiesTrainNew,by="parcelid")
#percentile 3
unique(TransactProperties_percentile1$percentile)  #double check the result

TransactProperties_Allpercentile <- bind_rows(TransactProperties_percentile1,
                                              TransactProperties_percentile2,
                                              TransactProperties_percentile3) %>%
  mutate(type = factor(type, levels = c("best_predicton",
                                               "worst_prediction",
                                               "typical_prediction")),
         ClassOfType = as.numeric(type)
         )
##Focus on the relationship between all-finished-area 
#Based on different prediction class
TransactProperties_Allpercentile %>%
  ggplot(aes(x=finishedsquarefeet15, fill=type, color=type)) +
  geom_line(stat = "density", size = 1.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The distribution of three prediction class based on total-finished-area",
    x = "Total Finished Area",
    y = "Density"
  ) +
  coord_cartesian(xlim=c(700,7.5e3))
#Based on absolute logerror
TransactProperties_Allpercentile %>%
  ggplot(aes(x=finishedsquarefeet15,y=abs_logerror)) +
  geom_smooth(color = "red") + # show the overlapping points and then find a trend by using defaulting function LS
  theme_bw() +
  labs(
    title = "The distribution of Absolute Logerror based on total-finished-area",
    x = "Total Finished Area",
    y = "Absolute Logerror"
  ) +
  coord_cartesian(xlim=c(600,7.5e3),ylim=c(0.1,0.2))

##Focus on the relationship between finished-lived-area
#Based on different prediction class
TransactProperties_Allpercentile %>%
  ggplot(aes(x=finishedsquarefeet12, fill=type, color=type)) +
  geom_line(stat = "density", size = 1.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The distribution of three prediction class based on finished-lived-area",
    x = "Finished lived Area",
    y = "Density"
  ) +
  coord_cartesian(xlim=c(0,1e4))

##Focus on the number of rooms
TransactProperties_Allpercentile %>%
  ggplot(aes(x=roomcnt, fill=type, color=type)) +
  geom_line(stat = "density", size = 1.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The distribution of three prediction class based on the number of rooms",
    x = "The Number of Room",
    y = "Density"
  ) +
  coord_cartesian(xlim=c(0,10))

##Focus on the number of units
TransactProperties_Allpercentile %>%
  ggplot(aes(x=unitcnt, fill=type, color=type)) +
  geom_line(stat = "density", size = 1.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The distribution of three prediction class based on the number of units",
    x = "The Number of Unit",
    y = "Density"
  ) +
  coord_cartesian(xlim=c(1,4))

##Focus on different year
TransactProperties_Allpercentile %>%
  ggplot(aes(x=yearbuilt, fill=type, color=type)) +
  geom_line(stat = "density", size = 1.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The distribution of three prediction class based on different year",
    x = "Different Year",
    y = "Density"
  ) 

##Focus on total sum of tax
#Based on different prediction class
TransactProperties_Allpercentile %>%
  ggplot(aes(x=taxvaluedollarcnt, fill=type, color=type)) +
  geom_line(stat = "density", size = 1.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The distribution of three prediction class based on total-sum-tax",
    x = "Total Sum of Tax",
    y = "Density"
  ) +
  coord_cartesian(xlim=c(0,1e6))
#Based on absolute logerror
TransactProperties_Allpercentile %>%
  ggplot(aes(x=taxvaluedollarcnt,y=abs_logerror)) +
  geom_smooth(color = "red") + # show the overlapping points and then find a trend by using defaulting function LS
  theme_bw() +
  labs(
    title = "The distribution of Absolute Logerror based on total-sum-tax",
    x = "Total Sum of Tax",
    y = "Absolute Logerror"
  ) +
  coord_cartesian(xlim=c(0,1e6),ylim=c(0.05,0.2))

##Focus on the assessed value of the house
TransactProperties_Allpercentile %>%
  ggplot(aes(x=structuretaxvaluedollarcnt, fill=type, color=type)) +
  geom_line(stat = "density", size = 1.2) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The distribution of three prediction class based on the Assessed value of the House",
    x = "The Assessed Value of the House",
    y = "Density"
  ) +
  coord_cartesian(xlim=c(0,1e6))


###Where does Zestimate Overpredict or Underpredict???
##As I mentioned before, if logerror < 0, it is underpredicting, otherwise.
TransactProperties_Allpercentile_OverUnder <- TransactProperties_Allpercentile %>%
  mutate(OverUnder = ifelse(logerror < 0, "Underprediction", "Overprediction"))

##Focus on area
TransactProperties_Allpercentile_OverUnder %>%
  ggplot(aes(x=latitude,y=abs_logerror,color=OverUnder)) +
  geom_smooth() +
  theme_bw() +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "The distribution of Overprediction/Underprediction based on different latitude",
    x = "latitude",
    y = "Absolute Logerror"
  )

TransactProperties_Allpercentile_OverUnder %>%
  ggplot(aes(x=longitude,y=abs_logerror,color=OverUnder)) +
  geom_smooth() +
  theme_bw() +
  scale_color_brewer(palette="Set1") +
  labs(
    title = "The distribution of Overprediction/Underprediction based on different longitude",
    x = "longitude",
    y = "Absolute Logerror"
  )

###Mapping
Latitude <- range(TransactProperties_Allpercentile_OverUnder$latitude/1e06,na.rm = TRUE)
Longitude <- range(TransactProperties_Allpercentile_OverUnder$longitude/1e06,na.rm = TRUE)  
#I know the range of mapping
 TransactProperties_Mapping <- propertiesTrainNew%>%
   sample_n(20000) %>%
   select(parcelid,longitude,latitude) %>%
   mutate(Longitude=longitude/1e6, Latitude=latitude/1e6) %>%
   select(parcelid,Latitude,Longitude) %>%
   left_join(transactionsTrainNew, by="parcelid")
#Where are those properties???
leaflet(TransactProperties_Mapping) %>%
   addTiles() %>% #Add default OpenStreetMap map tiles
   fitBounds(Longitude[1],Latitude[1],Longitude[2],Latitude[2]) %>% # the bound of map
   addCircleMarkers(stroke = FALSE) %>% #add circle markers to the map
   addMiniMap()
 
##Mapping 
TransactProperties_Mapping_Abslogerror <- transactionsTrainNew1 %>%
  sample_n(2000) %>%
  left_join(propertiesTrainNew, by="parcelid") %>%
  select(parcelid,longitude,latitude,abs_logerror) %>%
  mutate(lon=longitude/1e6, lat=latitude/1e6) %>%
  select(parcelid,lon,lat,abs_logerror)
#define the color of markers
qpal <- colorQuantile("YlOrRd", TransactProperties_Mapping_Abslogerror$abs_logerror,n=7)
#mapping
leaflet(TransactProperties_Mapping_Abslogerror) %>%
  addTiles() %>%
  #fitBounds(lon[1],lat[1],lon[2],lat[2]) %>%
  addCircleMarkers(stroke = FALSE, color = ~qpal(abs_logerror),fillOpacity = 1) %>%
  addLegend("bottomright",pal = qpal, values = ~abs_logerror,title = "Absolute Logerror",
            opacity = 1) %>%
  addMiniMap()

##Geographic Features about some specific information related to every house
#[row,column]
TransactProperties_Mapping1 <- propertiesTrainNew%>%
  sample_n(20000) %>%
  select(parcelid,longitude,latitude,bedroomcnt,bathroomcnt,finishedsquarefeet15) %>%
  mutate(Lon=longitude/1e6, Lat=latitude/1e6) %>%
  select(parcelid,Lat,Lon,bedroomcnt,bathroomcnt,finishedsquarefeet15) %>%
  left_join(transactionsTrainNew, by="parcelid") 

myLabel <- paste(sep = "<br>",
        paste("Bedrooms:",TransactProperties_Mapping1$bedroomcnt),
        paste("Bathrooms:",TransactProperties_Mapping1$bathroomcnt),
        paste("Total area:",TransactProperties_Mapping1$finishedsquarefeet15))


leaflet(TransactProperties_Mapping1) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = ~Lat,
    lng = ~Lon,
    label = ~HTML(myLabel),
    clusterOptions = markerClusterOptions()
  )
#Ask for professor McGibney why every label is not unique

###Modeling
##like I mentioned before, there are some good features, "TransactPropertiesAllGoodFeature"
glimpse(TransactPropertiesAllGoodFeature)
#there are some factor columns, "fips","propertycountylandusecode", 
#"propertyzoningdesc", "rawcensustractandblock",
unique(TransactPropertiesAllGoodFeature$fips)  #keep it
#I can use one-hot-encoding to change this factor
unique(TransactPropertiesAllGoodFeature$propertycountylandusecode)  #delete it, one-hot-encoding won't work
unique(TransactPropertiesAllGoodFeature$propertyzoningdesc) # delete it
unique(TransactPropertiesAllGoodFeature$rawcensustractandblock) #delete it
#those columns include so many unique information, I decide to delelte it
unique(TransactPropertiesAllGoodFeature$assessmentyear) #delete it, just one unique obcersation
#drop up useless chacter columns
drops <- c("propertyzoningdesc", "rawcensustractandblock","propertycountylandusecode","assessmentyear")
TransactPropertiesAllGoodFeature_finshed <- TransactPropertiesAllGoodFeature[,
                                                                             !(names(TransactPropertiesAllGoodFeature) %in% drops)]
###First Model
###random forest
install.packages("randomForest")
library(randomForest)
library(rsample)
#the original dataset, TransactPropertiesAllGoodFeature_finshed
#create training and validation data
set.seed(123456)
valid_split <- initial_split(TransactPropertiesAllGoodFeature_finshed,.8)
dtrain <- analysis(valid_split) #training dataset
validation <- assessment(valid_split) #validation data
x_test <- validation[setdiff(names(validation),"logerror")]
y_test <- validation$logerror

rf_oob_comp <- randomForest(
  formula=logerror ~.,
  data = dtrain,
  xtest=x_test,
  ytest=y_test
)



###this one works
###Second Model
###Modeling
### There are four columns which are factor, and I should keep them in factor, but use 
#one-hot-encoding or dummy variable to deal with them. they all present geographic information
#people will take account in geographic information when buying houses

#deal with the type of features in "TransactPropertiesAllGoodFeature_finshed"
#ensure that they all are numeric 
#"fips" keep it
#"regionidcity" delete it, large, useless information
#"regionidcountry" keep it
#"regionidneighborhood" delete it, large, useless informaion
#"regionidzip" delete it, large, useless information
TransactPropertiesAllGoodFeature_finshed$regionidneighborhood <- as.factor(TransactPropertiesAllGoodFeature_finshed$regionidneighborhood)
TransactPropertiesAllGoodFeature_finshed$regionidzip <- as.factor(TransactPropertiesAllGoodFeature_finshed$regionidzip)
glimpse(TransactPropertiesAllGoodFeature_finshed)
#using onehotencoding or dummy variables
library(data.table)
library(mltools)
Drops1 <- c("regionidneighborhood","regionidzip","regionidcity")
TransactPropertiesAllGoodFeature_finshed <- TransactPropertiesAllGoodFeature_finshed[,!names(TransactPropertiesAllGoodFeature_finshed) %in% Drops1]
glimpse(TransactPropertiesAllGoodFeature_finshed)
d1 <- TransactPropertiesAllGoodFeature_finshed %>%
  mutate(value=1) %>%
  spread(fips,value,fill = 0)
d2 <- d1 %>%
  mutate(value=1) %>%
  spread(regionidcounty,value,fill = 0)
#rename the columns
TransactPropertiesAllGoodFeature_finshed_model <- setnames(d2,old = c("06037","06059","06111","1286","2061","3101"),
                                                           new = c("FIP06037","FIP06059","FIP06111",
                                                                   "COUNTRY1286","COUNTRY2061","COUNTRY3101")
                                                           )
#split the train and test dataset
require(caTools)
set.seed(123456)
sample <- sample.split(TransactPropertiesAllGoodFeature_finshed_model,SplitRatio = 0.75)

train <- subset(TransactPropertiesAllGoodFeature_finshed_model,sample==TRUE)
test <- subset(TransactPropertiesAllGoodFeature_finshed_model,sample==FALSE)
#xgboost model
target <- train$logerror
library(data.table)
drops <- c("parcelid","logerror","Year_Month","abs_logerror")
dTrain <- train[,!names(train) %in% drops]
glimpse(dTrain)
feature_names <- names(dTrain)
dTrain <- xgb.DMatrix(data = as.matrix(dTrain),label=target,missing = NA)
dtest <- xgb.DMatrix(data = as.matrix(test[,feature_names]),missing = NA)
#Set XGBoost parameters
param <- list(booster="gbtree",
              subsample=.7,
              max_depth=2,
              colsample_bytree=.7,
              eta=.05,
              min_child_weight=100)
amm_mae <- function(preds,dTrain) {
  labels <- xgboost::getinfo(dTrain,"label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mae(elab,epreds)
  return(list(metric="amm_mae", value=err))
}
xgboost_modeling <- xgb.train(params = param,
                              data = dTrain,
                              nrounds = 30,
                              verbose = 1,
                              print_every_n = 5,
                              feval = amm_mae)
preds <- predict(xgboost_modeling, dtest)

results <- data.table(parcelid=test$parcelid,
                      Predicted_logerror=preds)



###I decide to use "properties_2016" as training dataset while "properties_2017" as test dataset
#then I can compare the predicted logerrorr with the actual one in "properties_2017"

#firstly load the data
#XGB_train <- TransactPropertiesAllGoodFeature_finshed_model
#glimpse(XGB_train)
#propertiesTest2017 <- read_csv("properties_2017.csv")
#transactionsTest2017 <- read_csv("train_2017.csv")
#deal with test dataset
#transactionsTest2017New <- transactionsTest2017 %>% 
 # mutate(
   # Year_Month = paste(month.abb[as.numeric(month(transactionsTest2017$transactiondate))],
                     #  year(transactionsTest2017$transactiondate), sep = " "),
   # Month = factor(month.abb[as.numeric(month(transactionsTest2017$transactiondate))])
 # )

#propertiesTest2017New <- propertiesTest2017 %>%
  #mutate(fireplaceflag = ifelse(fireplaceflag==TRUE,1,0),
         #taxdelinquencyflag = ifelse(taxdelinquencyflag=="Y",1,0))

#TransactProperties2017 <-transactionsTest2017New %>%
  #mutate(abs_logerror=abs(logerror)) %>%  #add a new column, the absolute value of logerror
  #left_join(propertiesTest2017New,by="parcelid")

#TransactPropertiesAllGoodFeature2017 <- TransactProperties2017 %>%
  #select(c(goodfeatures,"abs_logerror","logerror","Year_Month"))

#target1 <- names(TransactPropertiesAllGoodFeature_finshed)
#TransactPropertiesAllGoodFeature2017_finished <- TransactPropertiesAllGoodFeature2017[,target1]
#fix charcter columns
#I notice that in the "test" dataset, the unique observations in "fips" and "regionidcountry" are more than
#those columns in the "train" dataset, I decide to delete those useless rows
TransactPropertiesAllGoodFeature2017_finished1 <- TransactPropertiesAllGoodFeature2017_finished %>%
  filter(fips ==c("06059", "06111", "06037"),
         regionidcounty == c("1286","2061","3101"))

d3 <- TransactPropertiesAllGoodFeature2017_finished1 %>%
  mutate(value=1) %>%
  spread(fips,value,fill = 0)
d4 <- d3 %>%
  mutate(value=1) %>%
  spread(regionidcounty,value,fill = 0) 
#rename the column
TransactPropertiesAllGoodFeature2017_finshed_model <- setnames(d4,old = c("06037","06059","06111","1286","2061","3101"),
                                                           new = c("FIP06037","FIP06059","FIP06111",
                                                                   "COUNTRY1286","COUNTRY2061","COUNTRY3101")
)
XGB_test <- TransactPropertiesAllGoodFeature2017_finshed_model
###XGBOOST
#xgboost model
target1 <- XGB_train$logerror
drops <- c("parcelid","logerror","Year_Month","abs_logerror")
XGB_Train <- XGB_train[,!names(XGB_train) %in% drops]
feature_names1 <- names(XGB_Train)
XGB_Train <- xgb.DMatrix(data = as.matrix(XGB_Train),label=target1,missing = NA)
XGB_Test <- xgb.DMatrix(data = as.matrix(XGB_test[,feature_names1]),missing = NA)
#Set XGBoost parameters
param <- list(booster="gbtree",
              subsample=.7,
              max_depth=2,
              colsample_bytree=.7,
              eta=.05,
              min_child_weight=100)

amm_mae <- function(preds,XGB_Train) {
  labels <- xgboost::getinfo(XGB_Train,"label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mae(elab,epreds)
  return(list(metric="amm_mae", value=err))
}
xgboost_modeling <- xgb.train(params = param,
                              data = XGB_Train,
                              nrounds = 30,
                              verbose = 1,
                              print_every_n = 5,
                              feval = amm_mae)
preds <- predict(xgboost_modeling, XGB_Test)

results_finalmodel <- data.table(parcelid=XGB_test$parcelid,
                      Predicted_logerror=preds)

#calculate the RMSE between the predicted logerror and the actual one in the test dataset
RMSE_Comparsion <- results_finalmodel %>%
  left_join(transactionsTest2017,by="parcelid")

RMSE <- sqrt(mean(RMSE_Comparsion$Predicted_logerror - RMSE_Comparsion$logerror)^2)
#RMSE is equal to 0.09894484. It is relatively acceptable, but I may improve my model by tuning the parameter.
