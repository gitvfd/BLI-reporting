library(ggplot2)
library(dplyr)
library(reshape2)
library(rgdal)

#to save as pdf use ggsave(ratings, file="ratings.pdf", width=4, height=4) ggsave(filename = default_name(plot), plot = last_plot(), device = default_device(filename), path = NULL, scale = 1, width = par("din")[1], height = par("din")[2], units = c("in","cm", "mm"), dpi = 300, limitsize = TRUE, ...)


#Observe--- the data quote="" enables to ingnore all quote in the text file
#dataSample <- read.csv2("/Users/vfduclos/Dropbox/BLI_Milano/indexes-20141215.csv",header=T,quote="")

#Import the needed data 
BLIdata <- read.csv2("/Users/vfduclos/Dropbox/BLI_Milano/indexes-20141215.csv",header=T,quote="",na.strings="",colClasses=c("NULL",NA,NA,NA,NA,NA,NA,"NULL","NULL","NULL","NULL","NULL"))

############################################ PREPARE DATA  ############################################
#Split weights
BLIdata$Housing <- as.integer(substr(BLIdata$Weights,1,1))
BLIdata$Income <- as.integer(substr(BLIdata$Weights,2,2))
BLIdata$Jobs <- as.integer(substr(BLIdata$Weights,3,3))
BLIdata$Community <- as.integer(substr(BLIdata$Weights,4,4))
BLIdata$Education <- as.integer(substr(BLIdata$Weights,5,5))
BLIdata$Environment <- as.integer(substr(BLIdata$Weights,6,6))
BLIdata$Governance <- as.integer(substr(BLIdata$Weights,7,7))
BLIdata$Health <- as.integer(substr(BLIdata$Weights,8,8))
BLIdata$LifeSatisfaction <- as.integer(substr(BLIdata$Weights,9,9))
BLIdata$Safety <- as.integer(substr(BLIdata$Weights,10,10))
BLIdata$WorkLifeBalance <- as.integer(substr(BLIdata$Weights,11,11))

#Clean the comments section
BLIdata$comment[is.na(BLIdata$comment)] <- " "
BLIdata[BLIdata=="null"]<-" "
BLIdata <- na.omit(BLIdata)

#get sum of weights for normalisation
BLIdata$Total <- BLIdata$Housing + BLIdata$Income + BLIdata$Jobs + BLIdata$Community + BLIdata$Education+ BLIdata$Environment + BLIdata$Governance + BLIdata$Health + BLIdata$LifeSatisfaction+ BLIdata$Safety+ BLIdata$WorkLifeBalance


#weights normalisation
BLIdata$HousingNorm <- BLIdata$Housing / BLIdata$Total
BLIdata$IncomeNorm <- BLIdata$Income / BLIdata$Total
BLIdata$JobsNorm <- BLIdata$Jobs / BLIdata$Total
BLIdata$CommunityNorm <- BLIdata$Community / BLIdata$Total
BLIdata$EducationNorm <- BLIdata$Education / BLIdata$Total
BLIdata$EnvironmentNorm <- BLIdata$Environment / BLIdata$Total
BLIdata$GovernanceNorm <- BLIdata$Governance / BLIdata$Total
BLIdata$HealthNorm <- BLIdata$Health/ BLIdata$Total
BLIdata$LifeSatisfactionNorm <- BLIdata$LifeSatisfaction / BLIdata$Total
BLIdata$SafetyNorm <- BLIdata$Safety / BLIdata$Total
BLIdata$WorkLifeBalanceNorm <- BLIdata$WorkLifeBalance / BLIdata$Total

#Calculate sum of normalized indicators to make sure it is equal to 1
BLIdata$TotalNorm <- BLIdata$HousingNorm + BLIdata$IncomeNorm + BLIdata$JobsNorm + BLIdata$CommunityNorm + BLIdata$EducationNorm+ BLIdata$EnvironmentNorm + BLIdata$GovernanceNorm + BLIdata$HealthNorm + BLIdata$LifeSatisfactionNorm+ BLIdata$SafetyNorm+ BLIdata$WorkLifeBalanceNorm

BLIdata <- na.omit(BLIdata) #Remove all NAs


#write.table(BLIdata, "/Users/vfduclos/Dropbox/How-was-life/Archive/finalData/countryAvailability.tsv", sep="\t", row.names=F)


############################################ PLOT AVERAGE WEIGHTS  ############################################

#set color scales
cols3 <- c("Housing"="#3DA594","Income"="#2CA3E0","Jobs"="#237FBD","Community"="#CE485D","Education"="#7EA943","Environment"="#30A457","Civic Engagement"="#DCA922","Health"="#7C3A73","Life Satisfaction"="#E26237","Safety"="#606060","Work-Life Balance"="#962828")

#Get the data
BLIdataWeight <- BLIdata[, c("HousingNorm", "IncomeNorm","JobsNorm","CommunityNorm","EducationNorm","EnvironmentNorm","GovernanceNorm","HealthNorm","LifeSatisfactionNorm","SafetyNorm","WorkLifeBalanceNorm")]

#format the data
BLIdataWeight <-BLIdataWeight %>% summarise_each(funs(mean))   
BLIdataWeight <- melt(BLIdataWeight)

BLIdataWeight$variable <- c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance")

#plot
averageWeights <- ggplot(BLIdataWeight,aes(x=reorder(variable,value),y=100*value))+  geom_bar(aes(colour=variable),size=.6,width=.005,stat="identity") + geom_point(aes(colour=variable),size=8) + scale_colour_manual(values=cols3,labels = c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance"),guide=FALSE) + geom_text(data=BLIdataWeight,aes(label=round(100*value,2)),size=7,color="#7685A0",vjust=-1.3) +ggtitle("Average preferences") +theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="top", legend.text = element_text(size = 14), legend.title=element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_text(size=16, colour="#7685A0", face="bold", vjust=1), axis.text.y = element_blank(),axis.ticks.y = element_blank())  + coord_fixed(ratio=0.5)
print(averageWeights)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/averageWeights.pdf",width=20, height=11)


#Alternative display
BLIdataAverageWeights <- data.frame(matrix(c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance",mean(BLIdata$HousingNorm,na.rm=T),mean(BLIdata$IncomeNorm,na.rm=T),mean(BLIdata$JobsNorm,na.rm=T),mean(BLIdata$CommunityNorm,na.rm=T),mean(BLIdata$EducationNorm,na.rm=T),mean(BLIdata$EnvironmentNorm,na.rm=T),mean(BLIdata$GovernanceNorm,na.rm=T),mean(BLIdata$HealthNorm,na.rm=T),mean(BLIdata$LifeSatisfactionNorm,na.rm=T),mean(BLIdata$SafetyNorm,na.rm=T),mean(BLIdata$WorkLifeBalanceNorm,na.rm=T)),nrow=11,ncol=2))

colnames(BLIdataAverageWeights)<- c("Topic","Weight")

cols0 <-c("Housing"="#3DA594","Income"="#2CA3E0","Jobs"="#237FBD","Community"="#CE485D","Education"="#7EA943","Environment"="#30A457","Civic Engagement"="#DCA922","Health"="#7C3A73","Life Satisfaction"="#E26237","Safety"="#606060","Work-Life Balance"="#962828")

BLIdataAverageWeights$Weight <- 100* (as.numeric(as.character(BLIdataAverageWeights$Weight)))

averageWeights_alt <- ggplot(data=BLIdataAverageWeights,aes(x=reorder(Topic,Weight),y=Weight))+geom_bar(aes(fill=Topic),stat="identity",size=.6,width=.8)+scale_fill_manual(values=cols0) +ggtitle("Average preferences") + xlab('') + ylab('in percentage') + guides(fill=FALSE) + geom_text(data=BLIdataAverageWeights,aes(label=round(Weight,2)),size=7,color="white",vjust=1.2)  + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="top", legend.text = element_text(size = 14), legend.title=element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_text(size=16, colour="#7685A0", face="bold", vjust=1), axis.text.y = element_text(size=14)) + coord_fixed(ratio=0.5)
print(averageWeights_alt)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/averageWeights_alt.pdf",width=20, height=11)



############################################  PLOT GENDER REPARTITION  ############################################

#Get the data
BLIdataBullEye <-BLIdata%>%  group_by(gender) %>% summarise(count=n())   
BLIdataGenderCount<-BLIdataBullEye
GenderTitle<- BLIdataGenderCount$count[1]+BLIdataGenderCount$count[2]
GenderTitle<-paste(GenderTitle," men and women") 

BLIdataGenderCount$count[1]<-paste(BLIdataGenderCount$count[1]," women")
BLIdataGenderCount$count[2]<-paste(BLIdataGenderCount$count[2]," men")  



BLIdataBullEye$count[2]<-BLIdataBullEye$count[2]+BLIdataBullEye$count[1]
BLIdataBullEye$count <-apply(BLIdataBullEye[,2],2,function(x){sqrt(x/pi)})
BLIdataBullEye$count[2]<-BLIdataBullEye$count[2]-BLIdataBullEye$count[1]

BLIdataBullEye<-transform(BLIdataBullEye, count = as.numeric(count))

 
#plot
genderBullseye <- ggplot(BLIdataBullEye, aes(x = factor(1),y=count, fill=count)) +  geom_bar(width=1,stat="identity")
# The bullseye chart
genderBullseye  <- genderBullseye  + coord_polar(theta = "y")+ coord_polar()+ggtitle(GenderTitle) +theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),axis.ticks.x = element_blank(), axis.text.x = element_text(size=16, colour="#7685A0", face="bold", vjust=1), axis.ticks.y = element_blank(),axis.text.y = element_blank()) + scale_fill_continuous(low="cadetblue1",high="coral1",guide=FALSE) 
genderBullseye  <-genderBullseye + annotate("text", x = 1, y = 1, label = BLIdataGenderCount$count[1], size = 8, colour = "#7685A0",fontface="bold") + annotate("text", x = 1, y = 125, label = BLIdataGenderCount$count[2], size = 8, colour = "#7685A0",fontface="bold")
print(genderBullseye)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/genderBullseye.pdf",width=10, height=10)


############################################  PLOT AGE DISTRIBUTION  ############################################


BLIdataAge <-BLIdata%>%  group_by(age) %>% summarise(count=n())  

ageDistribution <- ggplot(BLIdata,aes(x=age))+  geom_bar(size=.6,width=.005,fill="coral1") + geom_point(data=BLIdataAge,aes(x = age,y=count),colour="coral1",size=7) +scale_x_discrete(limits=c("<15","15-24","25-34","35-44","45-54","55-64",">65")) +ggtitle("Visitors by age") +theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="top", legend.text = element_text(size = 14), legend.title=element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_text(size=16, colour="#7685A0", face="bold", vjust=1), axis.text.y = element_text(size=14)) 
print(ageDistribution)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/ageDistribution.PDF",width=15, height=7.5)



############################################ PLOT MAP PREFERENCES  ############################################

#https://uchicagoconsulting.wordpress.com/tag/r-ggplot2-maps-visualization/
#http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/
#http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
#http://docs.ggplot2.org/current/coord_map.html
#http://spatial.ly/2013/12/introduction-spatial-data-ggplot2/
#http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot

#Data extraction
BLIdataCountry <- BLIdata[, c("country", "HousingNorm", "IncomeNorm","JobsNorm","CommunityNorm","EducationNorm","EnvironmentNorm","GovernanceNorm","HealthNorm","LifeSatisfactionNorm","SafetyNorm","WorkLifeBalanceNorm")]


#data preparation
BLIdataCountry <-BLIdataCountry%>%  group_by(country) %>% summarise_each(funs(mean,n=n()))  
#BLIdataCountry$MAXtest<- apply(BLIdataCountry[,c("HousingNorm_mean", "IncomeNorm_mean","JobsNorm_mean","CommunityNorm_mean","EducationNorm_mean","EnvironmentNorm_mean","GovernanceNorm_mean","HealthNorm_mean","LifeSatisfactionNorm_mean","SafetyNorm_mean","WorkLifeBalanceNorm_mean")], 1, function(x) which(x == max(x)))# data being your data.frame 

BLIdataCountry$MAX<- apply(BLIdataCountry[,c("HousingNorm_mean", "IncomeNorm_mean","JobsNorm_mean","CommunityNorm_mean","EducationNorm_mean","EnvironmentNorm_mean","GovernanceNorm_mean","HealthNorm_mean","LifeSatisfactionNorm_mean","SafetyNorm_mean","WorkLifeBalanceNorm_mean")], 1,                            
function(x) {
  if(which(x == max(x))==1)
    "#3DA594"
  else if(which(x == max(x))==2)
    "#2CA3E0"
  else if(which(x == max(x))==3)
    "#237FBD"
  else if(which(x == max(x))==4)
    "#CE485D"
  else if(which(x == max(x))==5)
    "#7EA943"
  else if(which(x == max(x))==6)
    "#30A457"
  else if(which(x == max(x))==7)
    "#DCA922"
  else if(which(x == max(x))==8)
    "#7C3A73"
  else if(which(x == max(x))==9)
    "#E26237"
  else if(which(x == max(x))==10)
    "#606060"
  else if(which(x == max(x))==11)
    "#962828"
  else
     "0"
})# data being your data.frame 

#import capital latitude and longitude
geo <- read.csv2("/Users/vfduclos/Dropbox/BLI_Milano/geo1.csv",sep=",",header=T,quote="",na.strings="")

geo$longitude<-as.numeric(levels(geo$longitude))[geo$longitude]
geo$latitude<-as.numeric(levels(geo$latitude))[geo$latitude]

#join original data and geo coordinates
BLIdataCountry<-full_join(BLIdataCountry, geo,by=c("country"="ISO3"))
BLIdataCountry <- na.omit(BLIdataCountry)
BLIdataCountry<-data.frame(BLIdataCountry)


ogrInfo("/Users/vfduclos/Dropbox/OECD_Official_World_Map_Shape_File/", "National_Boundaries")
world <- readOGR("/Users/vfduclos/Dropbox/OECD_Official_World_Map_Shape_File/", "National_Boundaries")
summary(world)  
#plot(world, col = "white")  

worldRobinson <- spTransform(world, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#summary(worldRobinson)  
#plot(worldRobinson, col = "grey",border="white") 


proj4string(worldRobinson)
class(BLIdataCountry)
coordinates(BLIdataCountry) <-~ longitude+latitude
class(BLIdataCountry)

# does it have a projection/coordinate system assigned?
proj4string(BLIdataCountry) 

# we know that the coordinate system is NAD83 so we can manually
# tell R what the coordinate system is
proj4string(BLIdataCountry)<-CRS("+proj=longlat +datum=NAD83")

# now we can use the spTransform function to project. We will project
# the mapdata and for coordinate reference system (CRS) we will
# assign the projection from counties

BLIdataCountry<-spTransform(BLIdataCountry, CRS(proj4string(worldRobinson)))

# double check that they match
identical(proj4string(BLIdataCountry),proj4string(worldRobinson))

# ggplot can't deal with a SpatialPointsDataFrame so we can convert back to a data.frame
BLIdataCountry<-data.frame(BLIdataCountry)

# we're not dealing with lat/long but with x/y
# this is not necessary but for clarity change variable names
names(BLIdataCountry)[names(BLIdataCountry)=="longitude"]<-"x"
names(BLIdataCountry)[names(BLIdataCountry)=="latitude"]<-"y"

# now create the map http://sape.inf.usi.ch/quick-reference/ggplot2/themes


#Main topic map
cols <- c("#3DA594"="#3DA594","#2CA3E0"="#2CA3E0","#237FBD"="#237FBD","#CE485D"="#CE485D","#7EA943"="#7EA943","#30A457"="#30A457","#DCA922"="#DCA922","#7C3A73"="#7C3A73","#E26237"="#E26237","#606060"="#606060","#962828"="#962828")

# now create the map http://sape.inf.usi.ch/quick-reference/ggplot2/themes
topPreferencesMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=HousingNorm_n, color=MAX ), alpha=0.7)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                            axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Number of users",range = c(2, 18)) + scale_colour_manual(name="Favorite topic", values=cols, breaks = c("#3DA594","#2CA3E0","#237FBD","#CE485D","#7EA943","#30A457","#DCA922","#7C3A73","#E26237","#606060","#962828"),labels = c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance")) +ggtitle("Favorite topics across world countries")  +coord_equal(ratio=1) 
print(topPreferencesMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map0_topPreferencesMap.PDF",scale=3)


#housing map
housingMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=HousingNorm_mean), color="#3DA594", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Housing average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(housingMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map1_housingMap.PDF",scale=3)

#income map
incomeMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=IncomeNorm_mean), color="#2CA3E0", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Income average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(incomeMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map2_incomeMap.PDF",scale=3)

#job map
jobMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=JobsNorm_mean), color="#237FBD", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Jobs average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(jobMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map3_jobMap.PDF",scale=3)

#Community map
communityMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=CommunityNorm_mean), color="#CE485D", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Community average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(communityMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map4_communityMap.PDF",scale=3)

#Education map
educationMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=EducationNorm_mean), color="#7EA943", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Education average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(educationMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map5_educationMap.PDF",scale=3)

#Environment map
environmentMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=EnvironmentNorm_mean), color="#30A457", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Environment average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(environmentMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map6_environmentMap.PDF",scale=3)

#Civic Engagement map
civicEngMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=GovernanceNorm_mean), color="#DCA922", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Civic Engagement average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(civicEngMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map7_civicEngMap.PDF",scale=3)

#Health map
healthMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=HealthNorm_mean), color="#7C3A73", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Health average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(healthMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map8_healthMap.PDF",scale=3)

#Life satisfaction map
lifeSatisfactionMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=LifeSatisfactionNorm_mean), color="#E26237", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Life Satisfaction average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(lifeSatisfactionMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map9_lifeSatisfactionMap.PDF",scale=3)

#Safety map
safetyMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=SafetyNorm_mean), color="#606060", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Safety average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(safetyMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map10_safetyMap.PDF",scale=3)

#Work-life balance map
workLifeBalanceMap <- ggplot() +geom_polygon(data=worldRobinson, aes(x=long, y=lat, group=group), fill="#CBD5E2",colour=element_blank())+  geom_point(data=BLIdataCountry, aes(x=x, y=y, size=WorkLifeBalanceNorm_mean), color="#962828", alpha=0.6)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank(),                                                                                                               axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="bottom",legend.text = element_text(size = 14), legend.title=element_text(size = 16,colour="#7685A0")) + scale_size(name="Average weight",range = c(1, 10))  +ggtitle("Work-Life Balance average importance across countries")   +   coord_equal(ratio=1) # square plot to avoid the distortioncoord_equal(ratio=1)
#print(workLifeBalanceMap)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/Map11_workLifeBalanceMap.PDF",scale=3)




############################################ YEARLY PARALLEL COORDINATES  ############################################ 

#format year and change all 1970 data to 2011
BLIdata$Year<-as.numeric(format(as.Date(BLIdata$datetime,"%d-%m-%Y"), "%Y"))
BLIdata$Year[ BLIdata$Year == 1970 ] <- 2011

#prepare data
BLIdataParallelCoord <- BLIdata[, c("Year", "HousingNorm", "IncomeNorm","JobsNorm","CommunityNorm","EducationNorm","EnvironmentNorm","GovernanceNorm","HealthNorm","LifeSatisfactionNorm","SafetyNorm","WorkLifeBalanceNorm")]
BLIdataParallelCoord <-BLIdataParallelCoord%>%  group_by(Year) %>% summarise_each(funs(mean))  
BLIdataParallelCoord <- melt(BLIdataParallelCoord,id="Year")


#color scale
cols2 <- c("HousingNorm"="#3DA594","IncomeNorm"="#2CA3E0","JobsNorm"="#237FBD","CommunityNorm"="#CE485D","EducationNorm"="#7EA943","EnvironmentNorm"="#30A457","GovernanceNorm"="#DCA922","HealthNorm"="#7C3A73","LifeSatisfactionNorm"="#E26237","SafetyNorm"="#606060","WorkLifeBalanceNorm"="#962828")

#plot
yearlyComparison <- ggplot(BLIdataParallelCoord, aes(group = variable, color=variable)) + geom_line(aes(Year, value),size=2,alpha=0.7) + scale_colour_manual(values=cols2,labels = c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance")) +ggtitle("Evolution of preferences per year")  + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="top", legend.text = element_text(size = 14), legend.title=element_blank(),panel.grid.major.x = element_line(colour="#7685A0", size = 0.7, linetype = 2),axis.ticks.x = element_blank(), axis.text.x = element_text(size=16, colour="#7685A0", face="bold", vjust=1), axis.text.y = element_text(size=14)) + coord_fixed(ratio=25)  
print(yearlyComparison)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/yearlyComparison.PDF",width=20, height=10)



############################################ SEX PARALLEL COORDINATES  ############################################ 

#prepare data
BLIdataParallelCoord <- BLIdata[, c("gender", "HousingNorm", "IncomeNorm","JobsNorm","CommunityNorm","EducationNorm","EnvironmentNorm","GovernanceNorm","HealthNorm","LifeSatisfactionNorm","SafetyNorm","WorkLifeBalanceNorm")]
BLIdataParallelCoord <-BLIdataParallelCoord%>%  group_by(gender) %>% summarise_each(funs(mean))  
BLIdataParallelCoord <- melt(BLIdataParallelCoord,id="gender")


#plot
sexComparison <- ggplot(BLIdataParallelCoord, aes(group = variable, color=variable)) + geom_line(aes(gender, value),size=2,alpha=0.7) + scale_colour_manual(values=cols2,labels = c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance")) +ggtitle("Preferences by sex")  + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="top", legend.text = element_text(size = 14), legend.title=element_blank(),panel.grid.major.x = element_line(colour="#7685A0", size = 0.7, linetype = 2),axis.ticks.x = element_blank(), axis.text.x = element_text(size=16, colour="#7685A0", face="bold", vjust=1), axis.text.y = element_text(size=14)) + coord_fixed(ratio=25)  
print(sexComparison)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/sexComparison.PDF",width=20, height=10)





############################################ AGE PARALLEL COORDINATES  ############################################ 

#prepare data
BLIdataParallelCoord <- BLIdata[, c("age", "HousingNorm", "IncomeNorm","JobsNorm","CommunityNorm","EducationNorm","EnvironmentNorm","GovernanceNorm","HealthNorm","LifeSatisfactionNorm","SafetyNorm","WorkLifeBalanceNorm")]
BLIdataParallelCoord <-BLIdataParallelCoord%>%  group_by(age) %>% summarise_each(funs(mean))  
BLIdataParallelCoord <- melt(BLIdataParallelCoord,id="age")


#plot
ageComparison <- ggplot(BLIdataParallelCoord, aes(group = variable, color=variable)) + geom_line(aes(age, value),size=2,alpha=0.7) + scale_colour_manual(values=cols2,labels = c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance")) + scale_x_discrete(limits=c("<15","15-24","25-34","35-44","45-54","55-64",">65")) + ggtitle("Preferences by age range")  + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="top", legend.text = element_text(size = 14), legend.title=element_blank(),panel.grid.major.x = element_line(colour="#7685A0", size = 0.7, linetype = 2),axis.ticks.x = element_blank(), axis.text.x = element_text(size=16, colour="#7685A0", face="bold", vjust=1), axis.text.y = element_text(size=14))   + coord_fixed(ratio=30)  
print(ageComparison)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/ageComparison.PDF",width=20, height=10)


############################################ COUNTRY COMPARISON  ############################################ 

country1<-"FRA"
country2<-"MEX"

BLIdataCouComp <- filter(BLIdata,(country==country1)|(country==country2) )
BLIdataCouComp <- BLIdataCouComp[, c("country", "HousingNorm", "IncomeNorm","JobsNorm","CommunityNorm","EducationNorm","EnvironmentNorm","GovernanceNorm","HealthNorm","LifeSatisfactionNorm","SafetyNorm","WorkLifeBalanceNorm")]

BLIdataCouComp <-BLIdataCouComp%>%  group_by(country) %>% summarise_each(funs(mean)) 

colnames(BLIdataCouComp) <- c("country","Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance")

BLIdataCouComp$CountryOrder <- c(1,2)
BLIdataCouComp <- melt(BLIdataCouComp,id=c("country","CountryOrder"))

BLIdataCouComp.temp1 <- filter(BLIdataCouComp,(country==country1) )
BLIdataCouComp.temp2 <- filter(BLIdataCouComp,(country==country2) )

BLIdataCouComp.temp1$rank <- min_rank(BLIdataCouComp.temp1$value)
BLIdataCouComp.temp2$rank <- min_rank(BLIdataCouComp.temp2$value)
BLIdataCouComp <- rbind(BLIdataCouComp.temp1,BLIdataCouComp.temp2)

#BLIdataCouComp <- data.frame(BLIdataCouComp)

countryComparison <- ggplot(BLIdataCouComp,aes(rank,CountryOrder,group=variable))+   geom_point(aes(colour=variable,size=value)) + geom_line(aes(colour=variable),linetype = 2)  + scale_size(name="Topics weight",range = c(4, 10))+ scale_colour_manual(values=cols3,guide=FALSE)  + annotate("text", x = 0, y = 1, label = country1, size = 8, colour = "#7685A0",fontface="bold") + annotate("text", x = 0, y = 2, label = country2, size = 8, colour = "#7685A0",fontface="bold") + theme(axis.title.x=element_blank(),axis.title.y=element_blank(), plot.title = element_text(size=24,lineheight=1,colour="#7685A0", face="bold", vjust=1),panel.background = element_rect(fill="grey97"),legend.position="top", legend.text = element_text(size = 14), legend.title=element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank())+ggtitle("Comparison of country preferences")+ geom_text(data = filter(BLIdataCouComp,(country==country2) ),aes(x=rank,y=CountryOrder + 0.1, label=variable),colour="#7685A0", size = 5,angle=0) + coord_fixed(ratio=2)   
print(countryComparison)
ggsave("/Users/vfduclos/Dropbox/BLI_Milano/graphics/countryComparison.PDF",width=20, height=8)













#######   FILTER DATA   ########
BLIdataFilter <- subset(BLIdata, country=="EST") 


WeightsAvgFilter <- data.frame(matrix(c("Housing","Income","Jobs","Community","Education","Environment","Civic Engagement","Health","Life Satisfaction","Safety","Work-Life Balance",mean(BLIdataFilter$HousingNorm,na.rm=T),mean(BLIdataFilter$IncomeNorm,na.rm=T),mean(BLIdataFilter$JobsNorm,na.rm=T),mean(BLIdataFilter$CommunityNorm,na.rm=T),mean(BLIdataFilter$EducationNorm,na.rm=T),mean(BLIdataFilter$EnvironmentNorm,na.rm=T),mean(BLIdataFilter$GovernanceNorm,na.rm=T),mean(BLIdataFilter$HealthNorm,na.rm=T),mean(BLIdataFilter$LifeSatisfactionNorm,na.rm=T),mean(BLIdataFilter$SafetyNorm,na.rm=T),mean(BLIdataFilter$WorkLifeBalanceNorm,na.rm=T)),nrow=11,ncol=2))
colnames(WeightsAvgFilter)<- c("Topic","Weight")

WeightsAvgFilter$Color<- c("#DCA922","#CE485D","#7EA943","#30A457","#7C3A73","#3DA594","#2CA3E0","#237FBD","#E26237","#606060","#962828")

WeightsAvgFilter$Weight <- 100* (as.numeric(as.character(WeightsAvgFilter$Weight)))

ggplot(data=WeightsAvgFilter,aes(x=reorder(Topic,Weight),y=Weight))+geom_bar(aes(fill=Topic),stat="identity",size=.3,width=.8)+scale_fill_manual(values=WeightsAvgFilter$Color) +ggtitle("Average user weights") + guides(fill=FALSE) + geom_text(data=WeightsAvgFilter,aes(label=round(Weight,2)))

ggplot(data=WeightsAvgFilter,aes(x=reorder(Topic,Weight),y=Weight))+geom_bar(aes(fill=Topic),stat="identity",size=.6,width=.8)+scale_fill_manual(values=WeightsAvgFilter$Color) +ggtitle("Average user weights") + xlab('') + ylab('in percentage') + guides(fill=FALSE) + geom_text(data=WeightsAvgFilter,aes(label=round(Weight,2)),size=7,color="white",vjust=1.2) + theme(plot.title=element_text(size=rel(3),colour="#343434"),axis.text=element_text(size=rel(1),colour="#343434"),axis.title=element_text(size=rel(1.2),colour="#343434",face="italic"),panel.background=element_rect(fill="#D9E5ED"),panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank())

