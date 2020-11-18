# Data exploration and plotting

vine_order_labels <- paste("Vine", c(1:4, 6, 5, 9, 8, 7))
vine_order <- data.frame(VineUUID = c(1:9), order = c(1:4, 6, 5, 9, 8, 7), 
                           VineRow = c(rep(1:3, each = 3)),
                           VineTreatment = c("Conventional", "Strung", "Spur", "Conventional", "Strung", "Spur",  "Conventional", "Strung", "Spur"))

vine.labs <- c(1:4, 6, 5, 9, 8, 7)
names(vine.labs) <- c(1:4, 6, 5, 9, 8, 7)

temp <- 
  all_fruit_data %>%
	filter(!is.na(ShootTypeCoarse)) %>%
	filter(FreshWeight>50 & DryMatter>12) %>%
	left_join(select(vine_order, VineUUID, order), by = "VineUUID") %>%
	
	
	  ggplot(aes(x=FreshWeight, y=DryMatter)) +
	 	geom_point(aes(col= VineTreatmentNoNumber), size=0.5) +
		geom_smooth(method = "lm", aes(group=VineTreatmentNoNumber, col=VineTreatmentNoNumber)) +
		facet_wrap(~order, labeller = labeller(VineUUID=vine.labs)) +
	   	ggtitle("Fresh Weight vs Dry Matter by vine & shoot type") +
		xlab("Fresh Weight (g)") +
		ylab("Dry Matter (%)") +
		labs(col='Shoot Type', size=20) +
		theme(legend.text=element_text(size=rel(0.9))) +
		theme(plot.margin = margin(5, 150, 10, 20)) +
		guides(col = guide_legend(override.aes = list(size = 5)))



ggplotly(temp)
ggsave("FW vs DM by Vine.png", path = here("output"),  dpi = 500)
	


all_fruit_data %>%
ggplot(aes(x = DryMatter)) + 
	geom_histogram(aes(y =..density..),
				   breaks = seq(10, 25, by = 0.5), 
				   colour = "black", 
				   fill = "white") +
	stat_function(fun = dnorm, args = list(mean = mean(all_fruit_data$DryMatter), sd = sd(all_fruit_data$DryMatter))) +
	facet_wrap(~VineUUID)


all_fruit_data %>%
	ggplot(aes(x = FreshWeight)) + 
	geom_histogram(aes(y =..density..),
				   breaks = seq(50, 250, by = 5), 
				   colour = "black", 
				   fill = "white") +
	stat_function(fun = dnorm, args = list(mean = mean(all_fruit_data$FreshWeight), sd = sd(all_fruit_data$FreshWeight))) +
facet_wrap(~VineUUID)
  