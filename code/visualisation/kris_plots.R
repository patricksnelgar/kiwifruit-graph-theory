# Data exploration and plotting

vine_order <- data.frame(vine_label = paste("Vine", c(1:4, 6, 5, 9, 8, 7)), 
						 VineRow = c(rep(1:3, each = 3)),
						 VineTreatment = c("1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur"))

vine_labels <- paste("Vine", c(1:9))
names(vine_labels) <- c(1:9)

column_labels <- c("Conventional", "Strung", "Spur")
names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")

temp <- 
  all_fruit_data %>%
	filter(!is.na(ShootTypeCoarse)) %>%
	filter(FreshWeight>50 & DryMatter>12) %>%

	
	 ggplot(aes(x = FreshWeight, y = DryMatter)) +
		geom_point(aes(col= VineTreatmentNoNumber), size=0.5) +
		geom_smooth(method = "lm", aes(group=VineTreatmentNoNumber, col=VineTreatmentNoNumber)) +
		facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
		ggtitle("Fresh Weight vs Dry Matter by vine & shoot type") +
		labs(col='Shoot Type', size=20) +
		#guides(size = FALSE) +
		#theme(legend.text=element_text(size=rel(0.9))) +
		theme(plot.margin = margin(5, 160, 10, 20)) +
		theme_base_graph() +
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
				   breaks = seq(0, 350, by = 5), 
				   colour = "black", 
				   fill = "white") +
	stat_function(fun = dnorm, args = list(mean = mean(all_fruit_data$FreshWeight), sd = sd(all_fruit_data$FreshWeight))) +
facet_wrap(~VineUUID)
  