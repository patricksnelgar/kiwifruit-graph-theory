# load libraries
library(lsmeans)


# Setting up df for facet_grid

vine_order <- data.frame(vine_label = paste("Vine", c(1:4, 6, 5, 9, 8, 7)), 
						 VineRow = c(rep(1:3, each = 3)),
						 VineTreatment = c("1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur"))

vine_labels <- paste("Vine", c(1:9))
names(vine_labels) <- c(1:9)

column_labels <- c("Conventional", "Strung", "Spur")
names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")

# setting order for shoot types in legends etc

all_fruit_data$ShootTypeCoarse = factor(all_fruit_data$ShootTypeCoarse, levels = c("long","medium","short"))
all_fruit_data$ShootTypeRefined = factor(all_fruit_data$ShootTypeRefined, levels = c("very long","long","long pruned", "long stubbed", "medium", "medium pruned", "short", "very short", "stub"))

all_shoot_data$ShootTypeCoarse = factor(all_shoot_data$ShootTypeCoarse, levels = c("long","medium","short"))
all_shoot_data$ShootTypeRefined = factor(all_shoot_data$ShootTypeRefined, levels = c("very long","long","long pruned", "long stubbed", "medium", "medium pruned", "short", "very short", "stub"))


# 2D plot by ShootTypeRefined

ShootTypeRMap <-
  all_arch_data %>%
    left_join(select(all_shoot_data, ShootUUID, WoodType:ShootTypeCoarse), by = "ShootUUID") %>%
  	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
  		ggplot() +
  		geom_segment(aes(x = SegmentStartY, 
  		                 y = SegmentStartX, 
  		                 xend = SegmentEndY, 
  		                 yend =  SegmentEndX, 
  		                 size = SegmentDiameter), colour = "lightgrey") +
  		scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
  		geom_point(aes(x = SegmentEndY, y = SegmentEndX, colour = ShootTypeRefined),
  		           alpha = 0.7,
  		           size=3,
  		           shape=19,
  				   na.rm=TRUE) +
		scale_color_manual(breaks = c("stub","very short","short", "medium pruned", "medium", "long stubbed", "long pruned", "long", "very long"), values = c("#ff26a8","#ff002b","#ff9900", "#04ccde", "#003cb5", "#e5ff21", "#9ed61a", "#00c25e", "#12db00")) +
  		labs(x = NULL, y = NULL) +
      	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
  	  	geom_text(aes(-2200, -2300, label = vine_label), 
  		 	  size = 4,
  		 	  data = vine_order) + 
  		guides(size = FALSE) +
  		ggtitle("Shoot Type Refined by Vine") +
  		theme_base_graph() 
ShootTypeRMap

ggsave("Shoot Type Refined by Vine.png", path = here("output"),  dpi = 2000)


# 2D plot by ShootTypeCoarse

ShootTypeCMap <-
	all_arch_data %>%
	left_join(select(all_shoot_data, ShootUUID, WoodType:ShootTypeCoarse), by = "ShootUUID") %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, 
					 y = SegmentStartX, 
					 xend = SegmentEndY, 
					 yend =  SegmentEndX, 
					 size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_point(aes(x = SegmentEndY, y = SegmentEndX, colour = ShootTypeCoarse),
			   alpha = 0.7,
			   size=3,
			   shape=19,
			   na.rm=TRUE) +
	scale_color_manual(breaks = c("short", "medium", "long"), values = c("#d1495b", "#edae49", "#66a182")) +
	labs(x = NULL, y = NULL) +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	geom_text(aes(-2200, -2300, label = vine_label), 
			  size = 4,
			  data = vine_order) + 
	guides(size = FALSE) +
	ggtitle("Shoot Type Coarse by Vine") +
	theme_base_graph() 
ShootTypeCMap

ggsave("Shoot Type Coarse by Vine.png", path = here("output"),  dpi = 2000)


# 2D plot by ShootLeafArea

ShootLeafAreaMap <-
	all_arch_data %>%
	left_join(select(all_shoot_data, ShootUUID, WoodType:ShootLeafArea), by = "ShootUUID") %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, 
					 y = SegmentStartX, 
					 xend = SegmentEndY, 
					 yend =  SegmentEndX, 
					 size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_point(aes(x = SegmentEndY, y = SegmentEndX, colour = ShootLeafArea),
			   alpha = 1,
			   size=3,
			   shape=19,
			   na.rm=TRUE) +
	scale_color_viridis(option = "C", na.value = NA) +
	labs(x = NULL, y = NULL) +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	geom_text(aes(-2200, -2300, label = vine_label), 
			  size = 4,
			  data = vine_order) + 
	guides(size = FALSE) +
	ggtitle("Shoot Leaf Area maps") +
	theme_base_graph() 
ShootLeafAreaMap

ggsave("Shoot Leaf Area by Vine.png", path = here("output"),  dpi = 2000)

# 2D plot by LogShootLeafArea

LogShootLeafAreaMap <-
	all_arch_data %>%
	left_join(select(all_shoot_data, ShootUUID, WoodType:ShootLeafArea), by = "ShootUUID") %>%
	mutate(LogShootLeafArea = log10(ShootLeafArea)) %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, 
					 y = SegmentStartX, 
					 xend = SegmentEndY, 
					 yend =  SegmentEndX, 
					 size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_point(aes(x = SegmentEndY, y = SegmentEndX, colour = LogShootLeafArea),
			   alpha = 1,
			   size=3,
			   shape=19,
			   na.rm=TRUE) +
	scale_color_viridis(option = "C", na.value = NA) +
	labs(x = NULL, y = NULL) +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	geom_text(aes(-2200, -2300, label = vine_label), 
			  size = 4,
			  data = vine_order) + 
	guides(size = FALSE) +
	ggtitle("Log Shoot Leaf Area maps") +
	theme_base_graph() 
LogShootLeafAreaMap

ggsave("Log Shoot Leaf Area by Vine.png", path = here("output"),  dpi = 2000)



# 2D plot by ShootLength

ShootLengthMap <-
	all_arch_data %>%
	left_join(select(all_shoot_data, ShootUUID, ShootLength, ShootDiameter, WoodType:ShootLeafArea), by = "ShootUUID") %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, 
					 y = SegmentStartX, 
					 xend = SegmentEndY, 
					 yend =  SegmentEndX, 
					 size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_point(aes(x = SegmentEndY, y = SegmentEndX, colour = ShootLength),
			   alpha = 1,
			   size=3,
			   shape=19,
			   na.rm=TRUE) +
	scale_color_viridis(option = "C", na.value = NA) +
	labs(x = NULL, y = NULL) +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	geom_text(aes(-2200, -2300, label = vine_label), 
			  size = 4,
			  data = vine_order) + 
	guides(size = FALSE) +
	ggtitle("Shoot length maps") +
	theme_base_graph() 
ShootLengthMap

ggsave("Shoot Length map by Vine.png", path = here("output"),  dpi = 2000)


# 2D plot by ShootDiameter

ShootDiameterMap <-
	all_arch_data %>%
	left_join(select(all_shoot_data, ShootUUID, ShootLength, ShootDiameter, WoodType:ShootLeafArea), by = "ShootUUID") %>%
	filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
	ggplot() +
	geom_segment(aes(x = SegmentStartY, 
					 y = SegmentStartX, 
					 xend = SegmentEndY, 
					 yend =  SegmentEndX, 
					 size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_point(aes(x = SegmentEndY, y = SegmentEndX, colour = ShootDiameter),
			   alpha = 1,
			   size=3,
			   shape=19,
			   na.rm=TRUE) +
	scale_color_viridis(option = "C", na.value = NA) +
	labs(x = NULL, y = NULL) +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	geom_text(aes(-2200, -2300, label = vine_label), 
			  size = 4,
			  data = vine_order) + 
	guides(size = FALSE) +
	ggtitle("Shoot Diameter maps") +
	theme_base_graph() 
ShootDiameterMap

ggsave("Shoot Diameter map by Vine.png", path = here("output"),  dpi = 2000)



# Scatter plots

FWvsDM <- 
  all_fruit_data %>%
	filter(!is.na(ShootTypeCoarse)) %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	 ggplot(aes(x = FreshWeight, y = DryMatter)) +
		geom_point(aes(col= ShootTypeCoarse), size=0.1) +
		geom_smooth(method = "lm", aes(group=ShootTypeCoarse, col=ShootTypeCoarse)) +
		facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
		geom_text(aes(175, 14.5, label = vine_label), 
			  size = 4,
			  data = vine_order) + 
		ggtitle("Fresh Weight vs Dry Matter by vine & shoot type") +
		labs(col='Shoot Type', size=20, x= "Fresh Weight (g)", y = "Dry Matter (%)") +
		guides(color=guide_legend(override.aes=list(fill="white"))) +
		theme(
			plot.margin = margin(0.2,3,0.2,0.2, "cm"),
			panel.background = element_rect(fill = 'white', colour = 'black'), 
			panel.grid = element_line(color = "gray 90"), 
			panel.border = element_rect(color = "black", fill=NA), 
			legend.justification = c(1, 1), 
			legend.position = c(1.14,1),
			legend.text=element_text(size=rel(0.9)),  
			strip.background.y=element_blank(), 
			strip.text.y = element_blank(), 
			strip.background.x = element_rect(colour = "black", fill="gray 80"), 
			strip.text.x = element_text(size = 11))
FWvsDM
ggsave("FW vs DM by Vine.png", path = here("output"),  dpi = 2000)



DiamSummary <- all_fruit_data%>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	summarySE(measurevar="SegmentDiameter", na.rm=TRUE, groupvars=c("FloweringDate"))
DiamSummary

DiamFT <- 
	DiamSummary %>%
	ggplot(aes(x = FloweringDate, y = SegmentDiameter)) +
	geom_point() 
DiamFT


ShootTypeSummary <- all_fruit_data%>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	summarySE(measurevar="FloweringDate", na.rm=TRUE, groupvars=c("ShootTypeCoarse"))
ShootTypeSummary

ShootTypeFT <- 
	ShootTypeSummary %>%
	ggplot(aes(x = ShootTypeCoarse, y = FloweringDate)) +
	geom_point() 
ShootTypeFT
	

# Histograms__________________________________________________________________________




DMHisto <-
all_fruit_data %>%
filter(!is.na(ShootTypeCoarse)) %>%
filter(!is.na(DryMatter)) %>%
	ggplot(aes(x = DryMatter)) + 
	#geom_histogram(aes(y =..density.., fill = ShootTypeCoarse, color=ShootTypeCoarse), position="identity", alpha=0.2		, breaks = seq(12, 24, by = 0.3)) +
	geom_density(alpha=0.2, aes(fill=ShootTypeCoarse))+
	ggtitle("Dry Matter distribution by vine") +
	labs(x= "Dry Matter (%)") +
	
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	#geom_text(aes(10, 0.1, label = vine_label), 
	#		  size = 4,
	#		  data = vine_order) + 
	theme(
		plot.margin = margin(0.2,2, 0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.border = element_rect(color = "black", fill=NA), 
		legend.position = "right",
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11)) +
	scale_colour_manual(values=c("grey20", "grey20", "grey20"))+
	scale_fill_manual(values=c("#4256FF", "#FF41A3", "#00D10D"))
DMHisto
	
ggsave("Dry Matter distribution by vine.png", path = here("output"),  dpi = 2000)
	

FWHisto<-
all_fruit_data %>%
	filter(!is.na(ShootTypeCoarse)) %>%
	filter(!is.na(FreshWeight)) %>%
	ggplot(aes(x = FreshWeight)) + 
	#xlim(30, 225)+
	#geom_histogram(aes(y =..density.., fill = ShootTypeCoarse, color=ShootTypeCoarse), alpha=0.2, position="identity", breaks = seq(30, 225, by = 5)) +
	geom_density(alpha=0.2, aes(fill=ShootTypeCoarse))+
	ggtitle("Fresh Weight distribution by vine") +
	labs(x= "Fresh Weight (g)") +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme(
		plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(1, 1), 
		legend.position = "right",
		legend.text=element_text(size=rel(0.9)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11)) +
	scale_colour_manual(values=c("grey20", "grey20", "grey20"))+
	scale_fill_manual(values=c("#4256FF", "#FF41A3", "#00D10D"))
FWHisto

ggsave("Fresh Weight distribution by vine.png", path = here("output"),  dpi = 2000)




# Flowering % and air temperature histogram - all vines together

FlwrTempDF <- read_csv(here("input/flowering_weather.csv")) %>%
	mutate(FlTempDate = dmy(Date), MaxTemp=MaxTemp/0.3333, MinTemp=MinTemp/0.3333, MeanTemp=MeanTemp/0.3333) %>%
	filter(FlTempDate >= as.Date("2019-10-30"), FlTempDate <= as.Date("2019-11-11"))

coeff <- 0.3333

FlowerTempHisto <- 
	all_fruit_data %>%
	filter(!is.na(FloweringDate)) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	mutate(FloweringDateBar = FloweringDate) %>%
	ggplot(aes(x = FloweringDateBar)) + 
	geom_bar(aes(y = ((..count..)/sum(..count..))*100), color = "black", fill="darkgoldenrod1", position = position_dodge(width = 1), width=2, ) +
	geom_line(data = FlwrTempDF, aes(x = FlTempDate, y = MeanTemp), inherit.aes = FALSE, col="red1", size = 1) +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%b", expand = c(0.01,0.01)) +
	scale_y_continuous(breaks = seq(0,80, by = 15), limits = c(0,80), expand = c(0,0),
		sec.axis = sec_axis(~.*coeff, name="Mean daily air temperature (°C)", (breaks = seq(0,25, by = 5)))) +
		labs(x= "Date (2019)", y="Newly opened flowers (%)") +
		theme(
		plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.grid.minor = element_blank(),
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(1, 1), 
		legend.position = c(1.14,1),
		legend.text=element_text(size=rel(0.9)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11))
FlowerTempHisto	
ggsave("Flower timing and air temp.png", path = here("output"), dpi = 2000)
ggsave("Flower timing and air temp.jpg", path = here("output"), dpi = 500)



# Flowering Histogram - just bars

FlowerHisto <- 
	all_fruit_data %>%
	filter(!is.na(FloweringDate)) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	mutate(FloweringDateBar = FloweringDate) %>%
	ggplot(aes(x = FloweringDateBar)) + 
	geom_bar(aes(y = ((..count..)/sum(..count..))*100), color = "black", fill="darkgoldenrod1", position = position_dodge(width = 1), width=2, ) +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%b", expand = c(0.01,0.01)) +
	scale_y_continuous(breaks = seq(0,80, by = 15), limits = c(0,80), expand = c(0,0)) +
	labs(x= "Date (2019)", y="Newly opened flowers (%)") +
	theme(
		plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.grid.minor = element_blank(),
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(1, 1), 
		legend.position = c(1.14,1),
		legend.text=element_text(size=rel(0.9)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11))
FlowerHisto	

ggplotly(FlowerHisto)
ggsave("Flower timing combined.png", path = here("output"), dpi = 2000)
ggsave("Flower timing combined.jpg", path = here("output"), dpi = 500)




# Mean air temp line plot

FlwrTemp2DF <- read_csv(here("input/flowering_weather.csv")) %>%
	mutate(FlTempDate = dmy(Date)) 
	  # filter(FlTempDate >= as.Date("2019-10-31"), FlTempDate <= as.Date("2019-11-10"))
		   	

AirTemp <- 
	FlwrTemp2DF %>%
	ggplot(aes(x = FlTempDate)) + 
	geom_line(aes(x = FlTempDate, y = MaxTemp, col="Max"), size = 1) +
	geom_line(aes(x = FlTempDate, y = MeanTemp, col="Mean"), size = 1) +
	geom_line(aes(x = FlTempDate, y = MinTemp, col="Min"), size = 1) +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%b", limits= c(as.Date("2019-10-30"), as.Date("2019-11-11")), expand = c(-0.00,-0.00)) +
	scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by = 5)) +
	labs(x= "Date (2019)", y="Air temperature (°C)") +
	scale_color_manual(name = "", values = c("Max" = "red1", "Mean" = "black", "Min" = "blue1")) +
		theme(
		plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.grid.minor = element_blank(),
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(0, 0), 
		legend.position = c(0.75,0.03),
		legend.text=element_text(size=rel(0.8)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		legend.key = element_rect(colour = NA, fill = NA),
		legend.background = element_rect(linetype = 1, size = 0.5, colour = "black"),
		legend.margin = margin(-1, 3, 0, 3),
		legend.title = element_blank(),
		strip.text.x = element_text(size = 11))
AirTemp	
ggsave("Flower timing and air temp.png", path = here("output"), dpi = 2000)
ggsave("Flower timing and air temp.jpg", path = here("output"), dpi = 500)



# Panel air temp data and flowering histogram

plot_grid(AirTemp, FlowerHisto, 
		  labels = c("a)", "b)"), label_size = 12, ncol = 1, nrow = 2, scale = 0.98)
ggsave("Flower timing and air temp panelled.png", path = here("output"), height = 6, width = 4.5, dpi = 2000)
ggsave("Flower timing and air temp panelled.jpg", path = here("output"), height = 6, width = 4.5, dpi = 500)







 # Flower timing by vine facet grid


FlowerHisto <- 
all_fruit_data %>%
filter(!is.na(FloweringDate)) %>%
	ggplot(aes(x = FloweringDate)) + 
	geom_histogram(aes(y =..density.., fill = VineTreatment, color=VineTreatment), binwidth=2)+
	ggtitle("Flowering Date distribution by vine") +
	labs(x= "Flowering Date") +
		facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme(
		plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(1, 1), 
		legend.position = c(1.14,1),
		legend.text=element_text(size=rel(0.9)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11)) +
	scale_colour_manual(values=c("grey20", "grey20", "grey20"))+
	scale_fill_manual(values=c("#4256FF", "#FF41A3", "#00D10D"))
	FlowerHisto	
	
	ggsave("Flowering date distribution by vine.png", path = here("output"),  dpi = 2000)
	




SSCHisto<-
all_fruit_data %>%
filter(!is.na(SolubleSolidsContent)) %>%
	ggplot(aes(x = SolubleSolidsContent)) + 
	geom_histogram(aes(y =..density.., fill = VineTreatment, color=VineTreatment), breaks = seq(5, 20, by = 0.5)) +
	ggtitle("Soluble Solids Content distribution by vine") +
	labs(x= "Soluble Solids Content (°Brix)") +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme(
		plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(1, 1), 
		legend.position = c(1.14,1),
		legend.text=element_text(size=rel(0.9)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11)) +
	scale_colour_manual(values=c("grey20", "grey20", "grey20"))+
	scale_fill_manual(values=c("#4256FF", "#FF41A3", "#00D10D"))
SSCHisto
	
	ggsave("Soluble Solids Content distribution by vine.png", path = here("output"),  dpi = 2000)

	
	
HueHisto<-
all_fruit_data %>%
filter(!is.na(AverageHueAngle)) %>%	
	ggplot(aes(x = AverageHueAngle)) + 
	geom_histogram(aes(y =..density.., fill = VineTreatment, color=VineTreatment), breaks = seq(95, 115, by = 0.5)) +
	ggtitle("Average Hue angle distribution by vine") +
	labs(x= "Average Hue angle") +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme(
		plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(1, 1), 
		legend.position = c(1.14,1),
		legend.text=element_text(size=rel(0.9)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11)) +
	scale_colour_manual(values=c("grey20", "grey20", "grey20"))+
	scale_fill_manual(values=c("#4256FF", "#FF41A3", "#00D10D"))
HueHisto
	
ggsave("Hue angle distribution by vine.png", path = here("output"),  dpi = 2000)
	
	
FirmHisto<-
all_fruit_data %>%
filter(!is.na(AverageFirmness)) %>%	
	ggplot(aes(x = AverageFirmness)) + 
	#xlim(30, 225)+
	geom_histogram(aes(y =..density.., fill = ShootTypeCoarse, color=ShootTypeCoarse),alpha=0.3, position="identity", breaks = seq(0, 10, by = 0.2)) +
	geom_density(alpha=0.2, aes(fill=ShootTypeCoarse))+
		ggtitle("Average Firmness distribution by vine") +
		labs(x= "Average firmness (kgf)") +
		facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
		theme(
			plot.margin = margin(0.2,2.5,0.2,0.2, "cm"),
			panel.background = element_rect(fill = 'white', colour = 'black'), 
			panel.grid = element_line(color = "gray 90"), 
			panel.border = element_rect(color = "black", fill=NA), 
			legend.justification = c(1, 1), 
			legend.position = c(1.14,1),
			legend.text=element_text(size=rel(0.9)),  
			strip.background.y=element_blank(), 
			strip.text.y = element_blank(), 
			strip.background.x = element_rect(colour = "black", fill="gray 80"), 
			strip.text.x = element_text(size = 11)) +
		scale_colour_manual(values=c("grey20", "grey20", "grey20"))+
		scale_fill_manual(values=c("#4256FF", "#FF41A3", "#00D10D"))
	FirmHisto
	
	ggsave("Average Firmness distribution by vine.png", path = here("output"),  dpi = 2000)

		
HeightHisto<-
		all_fruit_data %>%
		filter(!is.na(DistanceFromCanopyWire)) %>%	
		ggplot(aes(x = DistanceFromCanopyWire)) + 
		#xlim(30, 225)+
		geom_histogram(aes(y =..density.., fill = ShootTypeCoarse, color=ShootTypeCoarse), breaks = seq(-50, 50, by = 10)) +
		ggtitle("Relative fruit height distribution by vine") +
		labs(x= "Relative fruit height (cm)") +
		facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
		theme(
			plot.margin = margin(0.2,2.5,0.2,0.2, "cm"),
			panel.background = element_rect(fill = 'white', colour = 'black'), 
			panel.grid = element_line(color = "gray 90"), 
			panel.border = element_rect(color = "black", fill=NA), 
			legend.justification = c(1, 1), 
			legend.position = c(1.14,1),
			legend.text=element_text(size=rel(0.9)),  
			strip.background.y=element_blank(), 
			strip.text.y = element_blank(), 
			strip.background.x = element_rect(colour = "black", fill="gray 80"), 
			strip.text.x = element_text(size = 11)) +
		scale_colour_manual(values=c("grey20", "grey20", "grey20"))
	#	scale_fill_manual(values=c("#ff0000", "#1100ff", "#00ff00"))
HeightHisto

ggsave("Average Height distribution by vine.png", path = here("output"),  dpi = 2000)
	



ShootHisto<-
	all_fruit_data %>%
	filter(!is.na(DistanceFromCanopyWire)) %>%	
	ggplot(aes(x = DistanceFromCanopyWire)) + 
	#xlim(30, 225)+
	geom_bar(aes(y =..density.., fill = ShootTypeCoarse, color=ShootTypeCoarse), breaks = seq(-50, 50, by = 10)) +
	ggtitle("Relative fruit height distribution by vine") +
	labs(x= "Shoot Type height (cm)") +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme(
		plot.margin = margin(0.2,2.5,0.2,0.2, "cm"),
		panel.background = element_rect(fill = 'white', colour = 'black'), 
		panel.grid = element_line(color = "gray 90"), 
		panel.border = element_rect(color = "black", fill=NA), 
		legend.justification = c(1, 1), 
		legend.position = c(1.14,1),
		legend.text=element_text(size=rel(0.9)),  
		strip.background.y=element_blank(), 
		strip.text.y = element_blank(), 
		strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		strip.text.x = element_text(size = 11)) +
	scale_colour_manual(values=c("grey20", "grey20", "grey20"))
#	scale_fill_manual(values=c("#ff0000", "#1100ff", "#00ff00"))
ShootHisto

ggsave("Shoot type distribution by vine.png", path = here("output"),  dpi = 2000)




	
			

# FW and DM Box and Violin plots___________________________________________________________
	
	as.factor(all_fruit_data$QuadrantFromLeader)
	as.factor(all_fruit_data$QuadrantFromTrunk)
	as.factor(all_fruit_data$FloweringDate)
	
QfLBox <- 
	all_fruit_data %>%
	ggplot(aes(group=QuadrantFromLeader, y=DryMatter)) + 
	xlim(0.5, 3.5) +
	geom_boxplot() +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme_base_graph() +
	geom_text(aes(125, 13, label = vine_label), 
		  size = 4,
		  data = vine_order)+
	theme_bw() 
QfLBox



ShootTypeRefBox <- 
	all_fruit_data %>%
	filter(ShootTypeRefined!="NA") %>%
	ggplot(aes(y=DryMatter, x = ShootTypeRefined)) +
	stat_summary(
		geom = "point",
		fun = "mean",
		col = "black",
		shape = 21,
		fill = "identity") +
	scale_fill_brewer(palette="Dark2") +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme(axis.text.x = element_text(angle = 90),
		  plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		  panel.background = element_rect(fill = 'white', colour = 'black'), 
		  panel.grid = element_line(color = "gray 90"), 
		  panel.grid.minor = element_blank(),
		  panel.border = element_rect(color = "black", fill=NA), 
		  legend.justification = c(1, 1), 
		  legend.position = c(1.14,1),
		  legend.text=element_text(size=rel(0.9)),  
		  strip.background.y=element_blank(), 
		  strip.text.y = element_blank(), 
		  strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		  strip.text.x = element_text(size = 11))

ShootTypeRefBox





FlwrTimeDMViolin <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	ggplot(aes(x=FloweringDate, y=DryMatter, group=FloweringDate)) + 
	geom_violin() +
	theme_bw() +
#	facet_grid(cols = vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")

FlwrTimeDMViolin





FlwrTimeDMViolin <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	ggplot(aes(x=FloweringDate, y=DryMatter, group=FloweringDate)) + 
	geom_violin(size=1) +
	labs(x ="Flower opening date", y = "Dry matter content (%)") +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%m", expand = c(0.05,0.05)) +
	scale_y_continuous(limits = c(14.5,22)) +
	theme_patrick() +
#	facet_grid(cols = vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	#	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")
	stat_summary(fun=mean, geom="line", aes(group=1), color="red")  + 
	stat_summary(fun=mean, geom="point", color="red")

FlwrTimeDMViolin
ggsave("DM flower timing violin combined.png", path = here("output"),  dpi = 2000)
ggsave("DM flower timing violin combined.jpg", path = here("output"),  dpi = 500)






FlwrTimeDMBox <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	ggplot(aes(x=FloweringDate, y=DryMatter, group=FloweringDate)) + 
	geom_boxplot() +
	labs(x ="Flower opening date", y = "Dry matter content (%)") +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%m", expand = c(0.05,0.05)) +
	scale_y_continuous(limits = c(14.5,22)) +
	theme_patrick() +
	#	facet_grid(cols = vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	#	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")
	stat_summary(fun=mean, geom="line", aes(group=1), color="red")  + 
	stat_summary(fun=mean, geom="point", color="red")

FlwrTimeDMBox
ggsave("DM flower timing box combined.png", path = here("output"),  dpi = 2000)
ggsave("DM flower timing box combined.jpg", path = here("output"),  dpi = 500)



FlwrTimeFWViolin <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	ggplot(aes(x=FloweringDate, y=FreshWeight, group=FloweringDate)) + 
	geom_violin(size=1) +
	labs(x ="Flower opening date", y = "Fresh weight (g)") +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%m", expand = c(0.01,0.01)) +
	theme_patrick() +
	facet_grid(cols = vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	#	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")
	stat_summary(fun=mean, geom="line", aes(group=1), color="red")  + 
	stat_summary(fun=mean, geom="point", color="red")

FlwrTimeFWViolin
ggsave("FW flower timing violin.png", path = here("output"),  dpi = 2000)
ggsave("FW flower timing violin.jpg", path = here("output"),  dpi = 500)




FlwrTimeFWBox <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	ggplot(aes(x=FloweringDate, y=FreshWeight, group=FloweringDate)) + 
	geom_boxplot() +
	theme_patrick() +
	facet_grid(cols = vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")

ggplotly(FlwrTimeFWBox)


FlwrTimeSeedCountBox <- 
	all_fruit_data %>%
	ggplot(aes(x=FloweringDate, y=AverageSeedCount, group=FloweringDate)) + 
	geom_violin() +
	theme_bw() +
	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")

FlwrTimeSeedCountBox



# DM model testing


LMDMxFT <- 
	lm(DryMatter ~ FloweringDays * VineTreatmentNoNumber, na.action=na.omit, data=all_fruit_data)
summary(LMDMxFT)
lsmeans(LMDMxFT, pairwise~FloweringDays, adjust="tukey")


ANOVADMFT <- 
	aov(DryMatter ~ as.factor(FloweringDays), na.action=na.omit, data=all_fruit_data)


ANOVADMFTxTrt <- 
	aov(DryMatter ~ as.factor(FloweringDays) * as.factor(VineTreatmentNoNumber), na.action=na.omit, data=all_fruit_data)
summary(ANOVADMxFT)
interaction.plot(FloweringDays,VineTreatmentNoNumber,DryMatter,type="b",col=c(3:17),leg.bty="o",leg.bg="beige", lwd=2, pch=c(18,24), xlab="Diet", ylab="Weight lost",main="Interaction plot")
TukeyHSD(ANOVADMxFT)


ANOVABlockDMFTxTrt <- 
	aov(DryMatter ~ as.factor(VineRow) + as.factor(FloweringDays) * as.factor(VineTreatmentNoNumber), na.action=na.omit, data=all_fruit_data)


model.set <- list(ANOVADMFT, ANOVADMFTxTrt, ANOVABlockDMFTxTrt)
model.names <- c("ANOVADMFT", "ANOVADMFTxTrt", "ANOVABlockDMFTxTrt")

aictab(model.set, modnames = model.names)
hist(ANOVABlockDMFTxTrt$residuals)
qqPlot(ANOVABlockDMFTxTrt$residuals,id = FALSE)





# LM and ANOVA for FloweringDays * VineTreatmentNoNumber on DryMatter

all_fruit_data_filtered <-
	all_fruit_data%>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) 

modDM <-
	lm(DryMatter ~ VineRow + FloweringDays * VineTreatmentNoNumber, na.action=na.omit, data=all_fruit_data_filtered)
Anova(modDM, type = "III")

hist(modDM$residuals)
qqPlot(modDM$residuals,id = FALSE)


DMFDays <- HSD.test(modDM, "FloweringDays", main = "DM FT main effect", unbalanced = TRUE)
DMFDays

DMTreat <- HSD.test(modDM, "VineTreatmentNoNumber",  unbalanced = TRUE)
DMTreat

DMBlock <- HSD.test(modDM, "VineRow")
DMBlock


DMTX <- with(all_fruit_data_filtered, interaction(FloweringDays, VineTreatmentNoNumber))

modDMTX <- aov(DryMatter ~ DMTX, data = all_fruit_data_filtered)
DMTXTest <- HSD.test(modDMTX, "TX", unbalanced = TRUE)
DMTXTest

interaction.plot(as.numeric(all_fruit_data_filtered$FloweringDays), all_fruit_data_filtered$VineTreatmentNoNumber, all_fruit_data_filtered$DryMatter, type = ("l"))

DMgrp <- data.frame(FloweringDate = as.Date(c("2019-10-31", "2019-11-02", "2019-11-04","2019-11-06", "2019-11-08",  "2019-11-10")), grp = c("a", "ab", "ab", "bc", "c", "d"))


# DM over flowering dates graphing



DMSummary <- all_fruit_data%>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	summarySE(measurevar="DryMatter", groupvars=c("FloweringDate"))
DMSummary <- left_join(DMSummary, DMgrp)
DMSummary


FlwrTimeDMMeanStdDev <- 
	DMSummary %>%
	ggplot(aes(x=FloweringDate, y=DryMatter, group=FloweringDate, label = grp)) +
	stat_summary(fun=mean, geom="line", aes(group=1), color="deepskyblue1", size=1) +  
	geom_point(stat='identity', size=2) +
	geom_text(nudge_y = 0.3) +
	geom_errorbar(aes(ymin=DryMatter-se, ymax=DryMatter+se), width=.5) +
	labs(x ="Flower opening date", y = "Dry matter content (%)") +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%b", expand = c(0.05,0.05)) +
	scale_y_continuous(limits = c(16.75,19.25), breaks=(seq(15,20, by=0.5))) +
	theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		  panel.background = element_rect(fill = 'white', colour = 'black'), 
		  panel.grid = element_line(color = "gray 90"), 
		  panel.grid.minor = element_blank(),
		  panel.border = element_rect(color = "black", fill=NA), 
		  legend.justification = c(1, 1), 
		  legend.position = c(1.14,1),
		  legend.text=element_text(size=rel(0.9)),  
		  strip.background.y=element_blank(), 
		  strip.text.y = element_blank(), 
		  strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		  strip.text.x = element_text(size = 11))

FlwrTimeDMMeanStdDev
ggsave("DM flower timing MSD.png", path = here("output"),  dpi = 2000)
ggsave("DM flower timing MSD.jpg", path = here("output"),  dpi = 500)






# LM and ANOVA for FloweringDays * VineTreatmentNoNumber on FreshWeight


all_fruit_data_filtered <-
	all_fruit_data%>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) 

modFW <-
	lm(FreshWeight ~ VineRow + FloweringDays * VineTreatmentNoNumber, na.action=na.omit, data=all_fruit_data_filtered)
Anova(modFW, type = "III")

hist(modFW$residuals)
qqPlot(modFW$residuals,id = FALSE)

FWFDays <- HSD.test(modFW, "FloweringDays",  unbalanced = TRUE)
FWFDays

FWTreat <- HSD.test(modFW, "VineTreatmentNoNumber")
FWTreat

FWBlock <- HSD.test(modFW, "VineRow")
FWBlock

FWTX <- with(all_fruit_data_filtered, interaction(FloweringDays, VineTreatmentNoNumber))

modFWTX <- aov(FreshWeight ~ FWTX, data = all_fruit_data_filtered)
FWTXTest <- HSD.test(modFWTX, "FWTX", unbalanced = TRUE)
FWTXTest

interaction.plot(as.numeric(all_fruit_data_filtered$FloweringDays), all_fruit_data_filtered$VineTreatmentNoNumber, all_fruit_data_filtered$FreshWeight, type = ("l"))

FWgrp <- data.frame(FloweringDate = as.Date(c("2019-10-31", "2019-11-02", "2019-11-04","2019-11-06", "2019-11-08",  "2019-11-10")), grp = c("b", "a", "ab", "c", "d", "d"))


# FW over flower timing line plot____________________________________

FWSummary <- all_fruit_data%>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	summarySE(measurevar="FreshWeight", groupvars=c("FloweringDate"))
FWSummary <- left_join(FWSummary, FWgrp)
FWSummary

FlwrTimeFWMeanStdDev <- 
	FWSummary %>%
	ggplot(aes(x=FloweringDate, y=FreshWeight, group=FloweringDate, label = grp)) +
	stat_summary(fun=mean, geom="line", aes(group=1), color="deepskyblue1", size=1) +  
	geom_point(stat='identity', size=2) +
	geom_text(nudge_y = 7) +
	geom_errorbar(aes(ymin=FreshWeight-se, ymax=FreshWeight+se), width=.5,position=position_dodge(.9)) +
	labs(x ="Flower opening date", y = "Fresh weight (g)") +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%b", expand = c(0.05,0.05)) +
	scale_y_continuous(limits = c(90,140), breaks=(seq(90,140, by=10))) +
	theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		  panel.background = element_rect(fill = 'white', colour = 'black'), 
		  panel.grid = element_line(color = "gray 90"), 
		  panel.grid.minor = element_blank(),
		  panel.border = element_rect(color = "black", fill=NA), 
		  legend.justification = c(1, 1), 
		  legend.position = c(1.14,1),
		  legend.text=element_text(size=rel(0.9)),  
		  strip.background.y=element_blank(), 
		  strip.text.y = element_blank(), 
		  strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		  strip.text.x = element_text(size = 11)) 

FlwrTimeFWMeanStdDev
ggsave("FW flower timing MSD.png", path = here("output"),  dpi = 2000)
ggsave("FW flower timing MSD.jpg", path = here("output"),  dpi = 500)




# LM and ANOVA for FloweringDays * VineTreatmentNoNumber on SeedCount


all_fruit_data_seed_filtered <-
	all_fruit_data%>%
	filter(!is.na(AverageSeedCount)) %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) 

modSC <-
	lm(AverageSeedCount ~ VineRow + FloweringDays * VineTreatmentNoNumber, na.action=na.omit, data=all_fruit_data_seed_filtered)
Anova(modSC, type = "III")

hist(modSC$residuals)
qqPlot(modSC$residuals,id = FALSE)

SCFDays <- HSD.test(modSC, "FloweringDays",  unbalanced = TRUE)
SCFDays

SCTreat <- HSD.test(modSC, "VineTreatmentNoNumber", unbalanced = TRUE)
SCTreat

SCBlock <- HSD.test(modSC, "VineRow", unbalanced = TRUE)
SCBlock

SCTX <- with(all_fruit_data_seed_filtered, interaction(FloweringDays, VineTreatmentNoNumber))

modSCTX <- aov(AverageSeedCount ~ SCTX, data = all_fruit_data_seed_filtered)
SCTXTest <- HSD.test(modSCTX, "SCTX", unbalanced = TRUE)
SCTXTest

interaction.plot(as.numeric(all_fruit_data_seed_filtered$FloweringDays), all_fruit_data_seed_filtered$VineTreatmentNoNumber, all_fruit_data_seed_filtered$AverageSeedCount, type = ("l"))

SCgrp <- data.frame(FloweringDate = as.Date(c("2019-10-31", "2019-11-02", "2019-11-04","2019-11-06", "2019-11-08",  "2019-11-10")), grp = c("a", "a", "a", "b", "c", "bc"))




# SC over flower timing line plot____________________________________

SCSummary <- all_fruit_data_seed_filtered %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	summarySE(measurevar="AverageSeedCount", na.rm=TRUE, groupvars=c("FloweringDate"))
SCSummary <- left_join(SCSummary, SCgrp)
SCSummary

FlwrTimeSCMeanStdDev <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(FloweringDate >= as.Date("2019-10-31"), FloweringDate <= as.Date("2019-11-10")) %>%
	ggplot(aes(x=FloweringDate, y=AverageSeedCount, group=FloweringDate, na.rm=TRUE)) +
	stat_summary(na.rm=TRUE, fun=mean, geom="line", aes(group=1), color="deepskyblue1", size=1) + 
	geom_point(data=SCSummary, mapping=aes(x = FloweringDate, y = AverageSeedCount), col="black", size = 2) +
	geom_text(data=SCSummary, aes(label= grp), nudge_y = 30) +
	geom_errorbar(data=SCSummary, aes(ymin=AverageSeedCount-se, ymax=AverageSeedCount+se), width=.5,position=position_dodge(.9))  +

	labs(x ="Flower opening date", y = "Seed count") +
	scale_x_date(breaks = date_breaks("2 day"),date_labels = "%d-%b", expand = c(0.05,0.05)) +
	scale_y_continuous(breaks=(seq(200,500, by=50)), expand= c(0.1,0.1)) +
	theme(plot.margin = margin(0.2,0.2,0.2,0.2, "cm"),
		  panel.background = element_rect(fill = 'white', colour = 'black'), 
		  panel.grid = element_line(color = "gray 90"), 
		  panel.grid.minor = element_blank(),
		  panel.border = element_rect(color = "black", fill=NA), 
		  legend.justification = c(1, 1), 
		  legend.position = c(1.14,1),
		  legend.text=element_text(size=rel(0.9)),  
		  strip.background.y=element_blank(), 
		  strip.text.y = element_blank(), 
		  strip.background.x = element_rect(colour = "black", fill="gray 80"), 
		  strip.text.x = element_text(size = 11)) 


FlwrTimeSCMeanStdDev
ggsave("SC flower timing MSD.png", path = here("output"),  dpi = 2000)
ggsave("SC flower timing MSD.jpg", path = here("output"),  dpi = 500)



plot_grid(FlwrTimeSCMeanStdDev, FlwrTimeFWMeanStdDev, FlwrTimeDMMeanStdDev,
		  labels = c("a)", "b)", "c)"), label_size = 12, ncol = 1, nrow = 3, scale = 0.98)
ggsave("Flower timing 3 panelled.png", path = here("output"), height = 9, width = 4.5, dpi = 2000)
ggsave("Flower timing 3 panelled.jpg", path = here("output"), height = 9, width = 4.5, dpi = 500)



plot_grid(AirTemp, FlowerHisto, FlwrTimeSCMeanStdDev, FlwrTimeFWMeanStdDev, FlwrTimeDMMeanStdDev,
		  labels = c("a)", "b)", "c)", "d)", "e)"), label_size = 12, ncol = 1, nrow = 5, scale = 0.96)
ggsave("Flower timing 5 panelled.png", path = here("output"), height = 15, width = 5, dpi = 2000)
ggsave("Flower timing 5 panelled.jpg", path = here("output"), height = 15, width = 5, dpi = 500)

# Carpel and seed counts vs FW and DM

# CarpelCount vs FW

LMFWCarpel <- 
		lm(FreshWeight ~ CarpelCount, rm.na= TRUE, all_fruit_data, subset=(ShootTypeCoarse=="long"))

summary(LMFWCarpel)


FWbyCarpels <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(SegmentDiameter<30) %>%
	
	ggplot(aes(x=CarpelCount, y = FreshWeight)) +
	geom_point() + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
	theme_patrick() +
	labs(fill ="Shoot Type", color= "Shoot Type")

FWbyCarpels

# CarpelCount vs SeedCount


LMSCCarpel <- 
	lm(AverageSeedCount ~ CarpelCount, all_fruit_data)
summary(LMSCCarpel)


SCbyCarpels <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(SegmentDiameter<30) %>%
	
	ggplot(aes(x=CarpelCount, y = AverageSeedCount)) +
	geom_point() + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
	theme_patrick() +
	labs(fill ="Shoot Type", color= "Shoot Type")


SCbyCarpels



# Cane and Wood Types plots

# Wood Types
FWbyShootType <- 
	all_fruit_data %>%
	filter(!is.na(WoodType)) %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	ggplot(aes(y = FreshWeight, x=WoodType)) +
	geom_violin(size=1) +
	stat_summary(
		geom = "point",
		fun = "mean",
		col = "black",
		size = 3,
		shape = 8,
		fill = "red"
	) 
FWbyShootType

facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) 


# Parent internode diameter

FWbyCaneDiameter <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(SegmentDiameter<30) %>%
	
	ggplot(aes(x=SegmentDiameter, y = FreshWeight)) +
	geom_point(shape = 15, size = 0.001) + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
	theme_patrick() +
	labs(fill ="Shoot Type", color= "Shoot Type")
	
	
FWbyCaneDiameter


DMbyCaneDiameter <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(SegmentDiameter<30) %>%
		ggplot(aes(x=SegmentDiameter, y = DryMatter)) +
	geom_point(shape = 15, size = 0.001) + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
	theme_patrick()

DMbyCaneDiameter


# Shoot basal diameter
all_fruit_data_2 <- all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(SegmentDiameter<30)
LM1 <-	lm(FreshWeight~SegmentDiameter, data = all_fruit_data_2)
summary(LM1)



FWbyShootDiameter <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(SegmentDiameter<30) %>%
	
	ggplot(aes(x=ShootDiameter, y = FreshWeight)) +
	geom_point(shape = 15, size = 0.001) + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
	theme_patrick()

FWbyShootDiameter


DMbyShootDiameter <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(SegmentDiameter<30) %>%
	
	ggplot(aes(x=ShootDiameter, y = DryMatter)) +
	geom_point(shape = 15, size = 0.001) + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
	theme_patrick()


DMbyShootDiameter



# Shoot length

FWbyShootLength <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(ShootLength<501) %>%

	ggplot(aes(x=ShootLength, y = FreshWeight)) +
	geom_point(shape = 15, size = 0.001) + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
		theme_patrick()

FWbyShootLength


DMbyShootLength <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	filter(ShootLength<501) %>%
	
	
	ggplot(aes(x=ShootLength, y = DryMatter)) +
	geom_point(shape = 15, size = 0.001) + 
	geom_smooth(method = "lm", se = TRUE, aes(color = ShootTypeCoarse, fill =ShootTypeCoarse)) +
	theme_patrick()

DMbyShootLength


FWDMWoodShoot <-
	plot_grid(FWbyCaneDiameter + theme(legend.position="none"), FWbyShootDiameter + theme(legend.position="none"), FWbyShootLength + theme(legend.position="none"), DMbyCaneDiameter + theme(legend.position="none"), DMbyShootDiameter + theme(legend.position="none"), DMbyShootLength + theme(legend.position="none"),
		  labels = c("a)", "b)", "c)", "d)", "e)", "f)"), label_size = 11, ncol = 3, nrow = 2, scale = 0.98)
FWDMWoodShoot

legend <- get_legend(
	FWbyCaneDiameter + theme(legend.box.margin = margin(0, 5, 0, 5)) +
		theme(legend.position="bottom")
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(FWDMWoodShoot, legend, ncol = 1, nrow = 2, rel_heights = c(3,0.15))
ggsave("Fruit Quality vs Wood and Shoot metrics.png", path = here("output"), height = 9, width = 16, dpi = 1000)

