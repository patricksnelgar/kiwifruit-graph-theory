#-------------------------------------------------------------------------#
# Analysis using R-INLA
# Written by Rodelyn Jaksons
#-------------------------------------------------------------------------#


rm(list=ls())

#packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(here)
library(scales)
library(sf)
library(rgdal)
library(spdep)
library(INLA)

# download INLA from here: warning, 160mB +
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#------------------------------------------------------------------------------------------------------------------------------

#read in data 

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#vine architecture data set
vines_df <- readOGR(here("workspace/vine_architecture_shapefile.shp"))
vines_df_names <- read.csv(here('workspace/fieldnames.csv'),header=T)
colnames(vines_df@data) <- as.character(vines_df_names[,1])
vines_df$idarea <- 1:nrow(vines_df@data)

# calculate Neighbourhood matrix
vines_nb <- poly2nb(vines_df, queen = FALSE)
vines_nbList <- nb2listw(vines_nb,style="B",zero.policy = T)
vines_nb_mat <- nb2mat(vines_nb, style = "B",zero.policy = T)


if(!exists(here("workspace/map.adj"))) {
	nb2INLA(here("workspace/map.adj"), vines_nb)
}

g <- inla.read.graph(filename = here("workspace/map.adj"))


#plot polygon and show neighbours given by red line
plot(st_geometry(st_as_sf(vines_df)))
plot(vines_nb, data.frame(x=coordinates(vines_df)[,1],y=coordinates(vines_df)[,2]), add=TRUE, col="red",pch=26,cex=0.5)
plot(vines_df, col = vines_df@data$VineUUID)

#making nested random effects for cane in vine
vines_df@data$cane_in_vine <- paste(vines_df@data$CaneUUID,vines_df@data$VineUUID, sep="_")

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#Models

#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------

#Drymatter
#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------

vines_df@data$LeafLoss <- factor(vines_df@data$LeafLoss)

#no spatial effects - random effects on obs only
m1.f <- meanDM ~  WoodType  +ShootTypeCoarse + f(LeafLoss, model='rw1') + f(ID)
m1.dm <- inla(m1.f, 
			  data=vines_df@data, 
			  family="gaussian",
			  control.compute = list(dic = TRUE),
			  control.fixed=list(prec=0.01),
			  num.threads = detectCores()) 

#no patial effects, but nested random effects
m2.f <- meanDM ~  WoodType  +ShootTypeCoarse + f(LeafLoss, model='rw1') + f(cane_in_vine) + f(VineUUID)
m2.dm <- inla(m2.f, 
			  data=vines_df@data, 
			  family="gaussian",
			  control.compute = list(dic = TRUE),
			  control.fixed=list(prec=0.01), 
			  num.threads = detectCores())


#spatial priors
prior <- list(
	prec = list(
		prior = "pc.prec",
		param = c(0.5 / 0.31, 0.01)),
	phi = list(
		prior = "pc",
		param = c(0.5, 2 / 3)))


#bym model
m3.f <- meanDM ~  WoodType  +ShootTypeCoarse + f(LeafLoss, model='rw1')+f(cane_in_vine) + 
	f(VineUUID) + f(idarea, model = "besag", graph =g,scale.model = T )
m3.dm <- inla(m3.f,
			  data = vines_df@data,
			  control.compute = list(dic = TRUE),
			  control.predictor = list(compute = TRUE),
			  control.fixed=list(prec=0.01),
			  num.threads = detectCores())

#bym2 model

m4.f <- meanDM ~  WoodType  +ShootTypeCoarse + f(LeafLoss, model='rw1')+f(cane_in_vine) + 
	f(VineUUID) + f(idarea, model = "bym2", graph =g,hyper=prior, scale.model = T )
m4.dm <- inla(m4.f,
			  data = vines_df@data,
			  control.compute = list(dic = TRUE),
			  control.predictor = list(compute = TRUE),
			  control.fixed=list(prec=0.1),
			  num.threads = detectCores())

m4.dm$summary.fixed

#DIC for model selection, based on this Model 4 is best
DIC_dm <- data.frame(Model = c("No spatial effects", "Nested random effects only",
							"Nested random effects and BYM","Nested random effects and BYM"),
				  DIC = c(m1.dm$dic$dic,m2.dm$dic$dic,m3.dm$dic$dic,m4.dm$dic$dic))
##
### Model results
##

#plots for best model
vines_df@data$fitted <- m4.dm$summary.fitted.values$mean

#probability that DM is above 16.1
vines_df@data$DM_threshold <- sapply(m4.dm$marginals.fitted.values,
									 FUN = function(marg){1-inla.pmarginal(q = 16.1, marginal = marg)})

#spatial residuals, prob that spatial residual is greater than zero
vines_df@data$resid_spatial <- sapply(m4.dm$marginals.random$idarea[1883:c(1882*2)],
								 FUN = function(marg){1-inla.pmarginal(q = 0, marginal = marg)})

res_plot <- left_join(vines_df@data,vines_df@data %>% group_by(VineUUID) %>% 
					  	summarise(y = mean(y),
					  			  new_x=-10,
					  			  CaneUUID = "Parent") %>%as.data.frame())
res_plot$vine_type <- ifelse(res_plot$VineUUID %in%c(1,4,9), "conventional","strung" )

res_plot <- res_plot %>% group_by(VineUUID) %>%
	mutate(x = as.numeric(factor(new_x))) %>%  as.data.frame()


#Spatial residual
gridExtra::grid.arrange(
	ggplot(res_plot %>% filter(vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=resid_spatial)) + 
		geom_tile() + 
		facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + 
		ggtitle("Vinetype:Conventional"),
	
	ggplot(res_plot %>% filter(!vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=resid_spatial)) + 
		geom_tile() + 
		facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + 
		ggtitle("Vinetype:Strung"),ncol=1)

#fitted values
gridExtra::grid.arrange(
	ggplot(res_plot %>% filter(vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=fitted)) + 
		geom_tile() + 
		facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + 
		ggtitle("Vinetype:Conventional"),
	
	ggplot(res_plot %>% filter(!vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=fitted)) + 
		geom_tile() + 
		facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + 
		ggtitle("Vinetype:Strung"),ncol=1)

#threshold
gridExtra::grid.arrange(
	ggplot(res_plot %>% filter(vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=DM_threshold)) + 
		geom_tile() + 
		facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + 
		ggtitle("Vinetype:Conventional"),
	
	ggplot(res_plot %>% filter(!vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=DM_threshold)) + 
		geom_tile() + 
		facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + 
		ggtitle("Vinetype:Strung"),ncol=1)

#other effects
m2.dm$summary.random$VineUUID %>% 
	ggplot(., aes(x=ID, y = mean)) +
	geom_point() +
	geom_linerange(aes(ymin=`0.025quant`, ymax= `0.975quant`)) + 
	theme_bw() + 
	geom_hline(yintercept = 0, linetype='dashed')

m2.dm$summary.random$LeafLoss%>% 
	ggplot(., aes(x=ID, y = mean)) + 
	geom_point() + 
	geom_line() + 
	geom_linerange(aes(ymin=`0.025quant`, ymax= `0.975quant`)) + 
	theme_bw() + 
	geom_hline(yintercept = 0, linetype='dashed')


m2.dm$summary.fixed%>%
	mutate(type = row.names(m2.dm$summary.fixed)) %>%
	filter(row_number() %in% 2:5) %>%
	ggplot(., aes(x=type, y = mean)) + 
	geom_point() + 
	geom_line() +
	geom_linerange(aes(ymin=`0.025quant`, ymax= `0.975quant`)) +
	theme_bw() + 
	geom_hline(yintercept = 0, linetype='dashed')

#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------

#Fresh weight
#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------


#no spatial effects - random effects on obs only
m1.f <- meanFW ~ DaysSinceFlowering  +ShootTypeCoarse  + f(ID)
m1.fw <- inla(m1.f, 
			  data=vines_df@data, 
			  family="gaussian",
			  control.compute = list(dic = TRUE),
			  control.fixed=list(prec=0.1),
			  num.threads = detectCores())

#no patial effects, but nested random effects
m2.f <- meanFW ~ DaysSinceFlowering  +ShootTypeCoarse + + f(cane_in_vine) + f(VineUUID)
m2.fw <- inla(m2.f, 
			  data=vines_df@data, 
			  family="gaussian",
			  control.compute = list(dic = TRUE),
			  control.fixed=list(prec=0.1),
			  num.threads = detectCores())

#bym model
m3.f <- meanFW ~ DaysSinceFlowering  +ShootTypeCoarse + +f(cane_in_vine) + 
	f(VineUUID) + f(idarea, model = "besag", graph =g,scale.model = T )
m3.fw <- inla(m3.f,
			  data = vines_df@data,
			  control.compute = list(dic = TRUE),
			  control.predictor = list(compute = TRUE),
			  control.fixed=list(prec=0.1),
			  num.threads = detectCores())


#bym2 model

m4.f <- meanFW ~ DaysSinceFlowering  +ShootTypeCoarse + f(cane_in_vine) + 
	f(VineUUID) + f(idarea, model = "bym2", graph =g,scale.model = T,hyper=prior )
m4.fw <- inla(m4.f,
			  data = vines_df@data,
			  control.compute = list(dic = TRUE),
			  control.predictor = list(compute = TRUE),
			  control.fixed=list(prec=0.1),
			  num.threads = detectCores())


summary(m4.dm)

#DIC for model selection, based on this Model 2 is best, so evidence for a spatial effect
DIC_fw <- data.frame(Model = c("No spatial effects", "Nested random effects only",
							   "Nested random effects and BYM","Nested random effects and BYM2"),
					 DIC = c(m1.fw$dic$dic,m2.fw$dic$dic,m3.fw$dic$dic,m4.fw$dic$dic))
##

m1.dm$summary.fixed
m2.dm$summary.fixed
m3.dm$summary.fixed
m4.dm$summary.fixed
summary(m4.dm)

#-------------------------------------------------
#Results
#-------------------------------------------------

#fitted values
vines_df@data$fitted.fw <- m2.fw$summary.fitted.values$mean

#probabilyt that fresh weight is between 84 - 240 grams
vines_df@data$fw_threshold <- sapply(m2.fw$marginals.fitted.values,
									 FUN = function(marg){(inla.pmarginal(q = 240, marginal = marg))-(inla.pmarginal(q = 84, marginal = marg))}) 

res_plot.fw <- left_join(vines_df@data,vines_df@data %>% group_by(VineUUID) %>% 
					  	summarise(y = mean(y),
					  			  new_x=-10,
					  			  CaneUUID = "Parent") %>%as.data.frame())
res_plot.fw$vine_type <- ifelse(res_plot.fw$VineUUID %in%c(1,4,9), "conventional","strung" )

res_plot.fw <- res_plot.fw %>% group_by(VineUUID) %>%
	mutate(x = as.numeric(factor(new_x))) %>%  as.data.frame()


#fitted freshweight
gridExtra::grid.arrange(
	ggplot(res_plot.fw %>% filter(vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=fitted.fw)) + 
		geom_tile() + facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + ggtitle("Vinetype:Conventional"),
	
	ggplot(res_plot.fw %>% filter(!vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=fitted.fw)) + 
		geom_tile() + facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + ggtitle("Vinetype:Strung"),ncol=1)

#freshweight thresholds
gridExtra::grid.arrange(
	ggplot(res_plot.fw %>% filter(vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=fw_threshold)) + 
		geom_tile() + facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + ggtitle("Vinetype:Conventional"),
	
	ggplot(res_plot.fw %>% filter(!vine_type=="conventional"), aes(x=x, y=y , group=CaneUUID,fill=fw_threshold)) + 
		geom_tile() + facet_grid(~VineUUID,scales="free") + 
		theme_classic() + 
		theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
			  legend.position = "bottom") + scale_fill_viridis_c(direction = -1) + ggtitle("Vinetype:Strung"),ncol=1)


#other random effects
m2.dm$summary.random$VineUUID %>% 
	ggplot(., aes(x=ID, y = mean)) + geom_point() +geom_linerange(aes(ymin=`0.025quant`, ymax= `0.975quant`))
