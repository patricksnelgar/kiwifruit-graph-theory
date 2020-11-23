# Data exploration and plotting

vine_order <- data.frame(vine_label = paste("Vine", c(1:4, 6, 5, 9, 8, 7)), 
						 VineRow = c(rep(1:3, each = 3)),
						 VineTreatment = c("1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur"))

vine_labels <- paste("Vine", c(1:9))
names(vine_labels) <- c(1:9)

column_labels <- c("Conventional", "Strung", "Spur")
names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")

#Scatter plots

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

DMHisto <- all_fruit_data %>%
ggplot(aes(x = DryMatter)) + 
	xlim(12, 24)+
	geom_histogram(aes(y =..density.., fill = VineTreatment, color=VineTreatment), breaks = seq(12, 24, by = 0.3)) +
	ggtitle("Dry Matter distribution by vine") +
	labs(x= "Dry Matter (%)") +
	facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
	theme(
	plot.margin = margin(0.2,0.2, 0.2,0.2, "cm"),
	panel.background = element_rect(fill = 'white', colour = 'black'), 
	panel.grid = element_line(color = "gray 90"), 
	panel.border = element_rect(color = "black", fill=NA), 
	legend.position = "none",
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
	ggplot(aes(x = FreshWeight)) + 
	xlim(30, 225)+
	geom_histogram(aes(y =..density.., fill = VineTreatment, color=VineTreatment), breaks = seq(30, 225, by = 5)) +
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
FWHisto

FlowerHisto <- 
all_fruit_data %>%
	ggplot(aes(group =FloweringTiming)) + 
	geom_histogram(aes(y =..density..),
				   colour = "black", 
				   fill = "white") +
	stat_function(fun = dnorm, args = list(mean = mean(all_fruit_data$DryMatter), sd = sd(all_fruit_data$DryMatter))) +
	facet_wrap(~VineUUID)
	FlowerHisto	

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


