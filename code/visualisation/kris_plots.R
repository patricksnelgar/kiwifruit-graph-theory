library("ggpubr")


# Setting up df for facet_grid

vine_order <- data.frame(vine_label = paste("Vine", c(1:4, 6, 5, 9, 8, 7)), 
						 VineRow = c(rep(1:3, each = 3)),
						 VineTreatment = c("1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur"))

vine_labels <- paste("Vine", c(1:9))
names(vine_labels) <- c(1:9)

column_labels <- c("Conventional", "Strung", "Spur")
names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")

# setting order for shoot types in legends etc

all_fruit_data$ShootTypeCoarse = factor(all_fruit_data$ShootTypeCoarse, levels = c("short","medium","long"))
all_fruit_data$ShootTypeRefined = factor(all_fruit_data$ShootTypeRefined, levels = c("stub","very short","short", "medium pruned", "medium", "long stubbed", "long pruned", "long", "very long"))

all_shoot_data$ShootTypeCoarse = factor(all_shoot_data$ShootTypeCoarse, levels = c("short","medium","long"))
all_shoot_data$ShootTypeRefined = factor(all_shoot_data$ShootTypeRefined, levels = c("stub","very short","short", "medium pruned", "medium", "long stubbed", "long pruned", "long", "very long"))


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
  		           size=3.5,
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
			   size=3.5,
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
			   size=6,
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
			   size=6,
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
			   size=6,
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
			   size=6,
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
	
# Histograms

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




	
			

	# Box plots
	
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
	ggplot(aes(y=DryMatter, group = ShootTypeRefined)) + 
	#facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) 
ggplotly(ShootTypeRefBox)



FlwrTimeDMBox <- 
	all_fruit_data %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	ggplot(aes(x=FloweringDate, y=DryMatter, group=FloweringDate)) + 
	geom_boxplot() +
	theme_bw() +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")

ggplotly(FlwrTimeDMBox)



FlwrTimeFWBox <- 
	all_fruit_data %>%
	ggplot(aes(x=FloweringDate, y=FreshWeight, group=FloweringDate)) + 
	geom_boxplot() +
	theme_bw() +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")
	
ggplotly(FlwrTimeFWBox)


FlwrTimeFWBox <- 
	all_fruit_data %>%
	ggplot(aes(x=FloweringDate, y=FreshWeight, group=FloweringDate)) + 
	geom_boxplot() +
	theme_bw() +
	#facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	stat_summary(fun=mean, geom="point", size=1, color="red", fill="red")

ggplotly(FlwrTimeFWBox)


