#-------------------------------------------------------------------------#
# This script calculates the neighbourhood matrices of each vine 
# through the poly2nb function in spdep
# Written by Rodelyn Jaksons
#-------------------------------------------------------------------------#


rm(list=ls())

#packages
library(sf)
library(rgeos)
library(raster)
library(spdep)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(here)
library(scales)


#------------------------------------------------------------------------------------------------------------------------------

#read in data 

#------------------------------------------------------------------------------------------------------------------------------

# taking all data sets from workspace
# Full_data <- lapply(list.files(path = here("workspace"), pattern = "data.csv", full.names = TRUE),read.csv)
# names(Full_data) <- c("all_arch_data", "all_fruit_data","all_shoot_data") #assigning them names
# 
# #taking the data frames and placing them in indiviual data set in the global environment
# lapply(names(Full_data), function(x) assign(x, Full_data[[x]], envir = .GlobalEnv))

# changed to use standard import script
redo <- FALSE
source(here("code/data_import/import_all_data.R"))

#Fruit data
all_fruit_data <- all_fruit_data %>% 
	                   mutate(Row = case_when(Quadrant %in% 1:6 ~ 1,
						                      Quadrant %in% 7:12 ~ 2,
						                      Quadrant %in% 13:18 ~ 3,
						                      Quadrant %in% 19:24 ~ 4,
						                      Quadrant %in% 25:30 ~ 5,
						                      Quadrant %in% 31:36 ~ 6),
		                      Col = case_when(Quadrant %in% seq(1,31,by=6) ~ 1,
		   				                      Quadrant %in% seq(2,32,by=6) ~ 2,
		   				                      Quadrant %in% seq(3,33,by=6) ~ 3,
		   				                      Quadrant %in% seq(4,34,by=6) ~ 4,
		   				                      Quadrant %in% seq(5,35,by=6) ~ 5,
		   				                      Quadrant %in% seq(6,36,by=6) ~ 6))
#summarise DM by shoot
ggplot(all_fruit_data, aes(DryMatter,group=VineUUID)) + geom_density()
ggplot(all_fruit_data, aes(DryMatter,group=ParentOriginID)) + geom_density()

#give ID
all_fruit_data <- all_fruit_data %>% 
	mutate(ID =paste(SegmentEndY,SegmentEndX,CaneUUID,ParentOriginID,VineUUID,sep="_"))


all_arch_data <- all_arch_data %>% 
	           mutate(Row = case_when(Quadrant %in% 1:6 ~ 1,
						              Quadrant %in% 7:12 ~ 2,
						              Quadrant %in% 13:18 ~ 3,
						              Quadrant %in% 19:24 ~ 4,
						              Quadrant %in% 25:30 ~ 5,
						              Quadrant %in% 31:36 ~ 6),
		              Col = case_when(Quadrant %in% seq(1,31,by=6) ~ 1,
		   				              Quadrant %in% seq(2,32,by=6) ~ 2,
		   				              Quadrant %in% seq(3,33,by=6) ~ 3,
		   				              Quadrant %in% seq(4,34,by=6) ~ 4,
		   				              Quadrant %in% seq(5,35,by=6) ~ 5,
		   				              Quadrant %in% seq(6,36,by=6) ~ 6))
#days since flowering
all_fruit_data <- all_fruit_data %>%
	mutate(FloweringDate = lubridate::ymd(FloweringDate),
		   DaysSinceFlowering =as.numeric(FloweringDate - lubridate::dmy('27-10-2019')) )

all_fruit_data <- left_join(all_fruit_data,all_shoot_data)

all_fruit_data[which(is.na(lubridate::ymd(all_fruit_data$FloweringDate))),]
#------------------------------------------------------------------------------------------------------------------------------

#Visualisations of initial tree structure

#------------------------------------------------------------------------------------------------------------------------------



all_arch_data %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineTreatmentNoNumber %in% c("Conventional","Strung") ) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter),col="lightgrey") +
	all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung")) %>% 
	group_by(SegmentEndY, SegmentEndX, CaneUUID,VineUUID,ParentOriginID) %>%
	summarise(DryMatter = mean(DryMatter, na.rm = T)) %>%
    geom_point(data=., aes(SegmentEndY, SegmentEndX,col=ParentOriginID),
			shape = 21, alpha=0.8) +
	geom_line(data=	all_fruit_data %>% filter(.,!is.na(DryMatter) & VineTreatmentNoNumber %in% c("Conventional","Strung")) %>% 
			  	group_by(SegmentEndY, SegmentEndX, CaneUUID), aes(SegmentEndY, SegmentEndX, group = paste(ParentOriginID,CaneUUID,VineUUID)),alpha=0.8) +
	facet_wrap(~VineUUID, scales="free")+
	scale_size_continuous(breaks = pretty_breaks(10)) +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) 

#------------------------------------------------------------------------------------------------------------------------------

#Calculating neighbourhoods

#------------------------------------------------------------------------------------------------------------------------------

#Neighbourhood matrix calculation function, this function is intended to be done cane wise
NB_mat <- function(Data){
	#takes number of observations in the data set
	n = length(Data$SegmentEndY)
	#this bit deals with lone shoots and when fruit runs along x-axis not y-axis
	if(n>1){ 
	if(!sum(diff(Data$SegmentEndY))==0) {x <- as.numeric(factor(rank(Data$SegmentEndY)))} else {x <- as.numeric(factor(rank(Data$SegmentEndX)))}
	
    #creating a separate data frame o calculate neighbors of a shoot
	ParentOriginID <- as.character(unique(Data$ParentOriginID))
	DF <- data.frame(x=x, SegmentEndY = Data$SegmentEndY,SegmentEndX = Data$SegmentEndX, 
					 CaneUUID = Data$CaneUUID,ParentOriginID = Data$ParentOriginID,VineUUID = Data$VineUUID,ID=Data$ID)
	Ymax=max(DF$x)
	mat <- lapply(DF$x, function(x) if(x==1) {c(x+1)}	else {if(x==Ymax) c(x-1) else c(x-1,  x+1)})
	names(mat) <- DF$ID
	
	DF2 <- data.frame(x=rep(DF$x,unlist(lapply(mat, length))),X_neigbour=unlist(mat))
	DF2$from_id <- rep(names(mat),unlist(lapply(mat, length)))
	DF2$to_id <- as.character(DF[match(DF2$X_neigbour,DF$x),]$ID)
	DF2$NB_SegmentEndX <- DF[match(DF2$X_neigbour,DF$x),]$SegmentEndX
	DF2$NB_SegmentEndY <- DF[match(DF2$X_neigbour,DF$x),]$SegmentEndY
	DF2 <- left_join(DF2,DF[match(DF2$X_neigbour,DF$x),])
    DF2[,c("CaneUUID", "ParentOriginID","ID")] <- sapply(DF2[,c("CaneUUID", "ParentOriginID","ID")],as.character)
    DX <- data.frame(DF2[which.min(abs(DF2$SegmentEndY-0)),])
    DX$to_id <- "to_Parent"
    DF2 <- unique(rbind(DF2,DX) %>% as.data.frame())
	} else  {
    	x <- as.numeric(factor(rank(Data$SegmentEndY)))
    	DF <- data.frame(x=x, X_neigbour=NA, from_id =Data$ID, to_id=NA,
    					 SegmentEndY = Data$SegmentEndY,SegmentEndX = Data$SegmentEndX, 
    					 CaneUUID = Data$CaneUUID,ParentOriginID = Data$ParentOriginID,VineUUID = Data$VineUUID,ID=Data$ID,
    					 NB_SegmentEndX=NA, NB_SegmentEndY=NA)
    
    	DF2 <- rbind(DF,DF)
    	DF2[2,"to_id"] <- "to_Parent"
    	DF2[,c("CaneUUID", "ParentOriginID","ID")] <- sapply(DF2[,c("CaneUUID", "ParentOriginID","ID")],as.character)
    }
	
	return(DF2)
}

#subsetting vines for analysis, only interested in conventional and strung
Data_toanalyse <- all_fruit_data %>% filter(.,!is.na(ParentOriginID) & VineTreatmentNoNumber %in% c("Conventional","Strung"))
#multiple observations by shoot (because shoot produces more than one fruit) so we take only the first of each shoot ID
Data_toanalyse <- Data_toanalyse %>% group_by(ID) %>% slice(1) %>% as.data.frame()

#calculating neighbourhood matrices and storing in NB_mat_df analysed cane wise
NB_mat_df <- data.frame()
for(i in 1:length(unique(Data_toanalyse$CaneUUID))){
	#take a cane
	DF_subset <- Data_toanalyse[Data_toanalyse$CaneUUID %in% unique(Data_toanalyse$CaneUUID)[i], ] %>% as.data.frame()
	#calculate neighbourhood matrix by cane
	DF_mat <- NB_mat(DF_subset) 
	#give a 'y' coordinated
	DF_mat$y <- i
	#bind it to NB_mat_df
	NB_mat_df <- unique(rbind(NB_mat_df, DF_mat) %>% as.data.frame())
}

NB_mat_df2 <- left_join(NB_mat_df, data.frame(VineUUID=as.numeric(names(tapply(NB_mat_df$y,NB_mat_df$VineUUID, function(x)length(unique(x))))),
		                nCane=tapply(NB_mat_df$y,NB_mat_df$VineUUID, function(x)length(unique(x)))))

NB_mat_df2 <- NB_mat_df2 %>% group_by(VineUUID) %>%
	mutate(y = as.numeric(as.character(factor(y, labels = seq(1, unique(nCane)*2, by=2))))) %>% as.data.frame()
#finding the first of each cane to link to parent node/vine trunk
NB_mat_df2 <- rbind(NB_mat_df2,NB_mat_df2 %>% group_by(VineUUID) %>% 
	      mutate(y = mean(y),
		   x=-10) %>% filter(.,to_id %in% "to_Parent") %>%as.data.frame())

#check that structure makes sense
ggplot(NB_mat_df2, aes(x=x, y=y, group=paste(ParentOriginID,CaneUUID)) ) +
	geom_point(aes(fill=factor(y)),alpha=0.5,shape=21) +  
	geom_line(alpha=0.5) +
	facet_wrap(~VineUUID, scales="free_y") + 
	theme_classic() +
	theme(legend.position="none", 
		  axis.text.x = element_blank(),axis.ticks.x =  element_blank(), 
		  axis.text.y = element_blank(),axis.ticks.y =  element_blank()) + xlab("") + ylab("") 

#join neighbourhood matrix with fruit data and calculate meanDM by shoot
NB_mat_df2 <- left_join(NB_mat_df2, all_fruit_data) %>% 
	group_by(ID) %>% mutate(meanDM = mean(DryMatter,na.rm=T),
							meanFW = mean(FreshWeight,na.rm=T)) %>% slice(1) %>%
	as.data.frame() 




#insert missing y_grid values in dataset and bind together 
pts <- data.frame(expand.grid(VineUUID = c(1,2,4,6,8,9),x=1:max(NB_mat_df2$x), 
							  y= seq(2,max(NB_mat_df2$y),by=2),meanDM=NA,CaneUUID=NA,
							  meanFW=NA,FruitPerShoot=NA,ShootTypeCoarse=NA,
							  WoodType=NA,LeafLoss=NA,DaysSinceFlowering=NA))
#pts$ID <- paste(pts$x,pts$y_grid, sep="_")
df_sub2 <- rbind(NB_mat_df2[NB_mat_df2$x>0,c("VineUUID","x","y","meanDM","CaneUUID",
											   "meanFW","FruitPerShoot","ShootTypeCoarse",
											   "WoodType","LeafLoss","DaysSinceFlowering")],pts)

#want to put into one polygon so we can analyse together so we will shift the x_axis
df_sub2 %>% group_by(VineUUID) %>%
	summarise(shift_x = max(x) + 1,
			  shift_y = max(y) + 1)

df_sub2 <- df_sub2 %>% 
	mutate(new_x = case_when(VineUUID %in% 1 ~ x ,
		                     VineUUID %in% 2 ~ x + 40,
							 VineUUID %in% 4 ~ x + 40*2,
							 VineUUID %in% 6 ~ x + 40*3,
							 VineUUID %in% 8 ~ x + 40*4,
							 VineUUID %in% 9 ~ x + 40*5))

#give ID var
df_sub2$ID <- paste(df_sub2$new_x,df_sub2$y,df_sub2$meanDM,df_sub2$VineUUID,sep="_")
#plot to check
ggplot(df_sub2, aes(x=new_x, y= y,fill=meanDM)) + geom_tile(col="black") + 
	 facet_wrap(~VineUUID, scales="free") + scale_fill_continuous(na.value = "white")

tapply(df_sub2$new_x, df_sub2$VineUUID, range)
# create raster-to-polygon
r <- rasterFromXYZ(unique(df_sub2[df_sub2$new_x>0, c("new_x", "y","meanDM")]))
#check
plot(r)
#create polygon
rSB <- rasterToPolygons(r)
plot(rSB) #check
rSB@data$new_x <- coordinates(rSB)[,1]
rSB@data$y <- coordinates(rSB)[,2]
rSB@data <- rSB@data %>% 
	mutate(VineUUID = case_when(new_x < 40 ~ 1, 
								new_x > 40 & new_x < 80 ~ 2,
								new_x > 80 & new_x < 120 ~ 4,
								new_x > 120 & new_x < 160 ~ 6,
								new_x > 160 & new_x < 200 ~ 8,
								new_x > 200 & new_x < 240 ~ 9))

rSB@data$ID <- paste(rSB@data$new_x, rSB@data$y,rSB@data$meanDM, rSB@data$VineUUID,sep="_")
rSB@data <- left_join(rSB@data,unique(df_sub2))

plot(rSB,col=rSB@data$VineUUID)

#view(rSB@data)
#view(df_sub2[,c("meanDM","new_x","y","VineUUID","ID","CaneUUID")])

#take coordinates of polygons and turn into simple features type
s1 <- sf::st_as_sf(data.frame(x=coordinates(rSB)[,1],y=coordinates(rSB)[,2]),coords=1:2)
coords <- sf::st_coordinates(s1) #get coordinates
xx <- poly2nb(st_as_sf(rSB)) #calculate neighbourhoods
#plot polygon and show neighbours given by red line
plot(st_geometry(st_as_sf(rSB)))
plot(xx, coords, add=TRUE, col="red",pch=26,cex=0.5)

#save shapefile
# only write out if files dont already exist
if(length(list.files(here("workspace"), pattern = "_shapefile.")) < 1) {
	rgdal::writeOGR(rSB,here("workspace/"),layer="vine_architecture_shapefile", driver="ESRI Shapefile")
}

write.csv(data.frame(field_names = colnames(rSB@data)),file=here('workspace/fieldnames.csv'),row.names = F)
