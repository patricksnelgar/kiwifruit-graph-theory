	
	#### Basic DM by Vine ####
	vine_labels <- paste("Vine", c(1:9))
	names(vine_labels) <- c(1:9)
	
	all_arch_data %>%
		filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
			ggplot() +
				geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend = SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
				scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
				geom_jitter(aes(SegmentEndY, SegmentEndX, fill = dm_bins),
							data = filter(all_fruit_data, !is.na(dm_bins) & DryMatter > 12),
							alpha = 0.8,
							size = 4,
							width = 10,
							height = 70,
							shape = 21) +
				scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
				labs(x = NULL, y = NULL) +
				facet_wrap(~VineUUID, labeller = labeller(VineUUID = vine_labels)) +
				guides(size = FALSE) +
				ggtitle("Dry Matter by Vine") +
				theme_base_graph() 
	
	ggsave(here("output/Vine 1 - 9 dry matter map.jpg"), width = 20, height = 15)
	
	#### Dry matter (binned) by treatment ####
	
	column_labels <- c("Conventional", "Strung", "Spur")
	names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")
	
	vine_metrics <- all_fruit_data %>%
						group_by(VineUUID) %>%
						summarise(fruit_count = n())
	
	vine_order <- data.frame(vine_label = paste("Vine", c(1:4, 6, 5, 9, 8, 7)), 
							 VineRow = c(rep(1:3, each = 3)),
							 VineTreatment = c("1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur"),
							 cropload = c(vine_metrics$fruit_count[c(1:4, 6, 5, 9, 8, 7)]))
	
	all_arch_data %>%
		filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
			ggplot() +
				geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
				scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
				geom_jitter(aes(SegmentEndY, SegmentEndX, fill = dm_bins),
							data = filter(all_fruit_data, !is.na(dm_bins)),
							alpha = 0.8,
							size = 4,
							width = 10,
							height = 70,
							shape = 21) +
				scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
				labs(x = NULL, y = NULL) +
				facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
				geom_text(aes(-2200, -2300, label = vine_label), 
						  size = 8,
						  data = vine_order) + 
				geom_text(aes(2200, -2300, label = paste("crop:", cropload)),
						  size = 6,
						  data = vine_order) +
				guides(size = FALSE) +
				ggtitle("Dry Matter by Vine & treatment") +
				theme_base_graph() 
	
	ggsave(here("output/Vine 1 - 9 dry matter by treatment map.jpg"), width = 20, height = 15)
	
	#### Flower Timing by treatment ####
	
	all_arch_data %>%
		filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
		ggplot() +
			geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
			scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
			geom_jitter(aes(SegmentEndY, SegmentEndX, fill = factor(FloweringDate)),
						data = filter(all_fruit_data, !is.na(FloweringDate)),
						alpha = 0.8,
						size = 4,
						width = 10,
						height = 70,
						shape = 21) +
			scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
			labs(x = NULL, y = NULL) +
			facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
			geom_text(aes(-2200, -2300, label = vine_label), 
					  size = 8,
					  data = vine_order) + 
			guides(size = FALSE) +
			ggtitle("Flowering date by Vine & treatment") +
			theme_base_graph()
	
	ggsave(here("output/Vine 1 - 9 flowering date by treatment map.jpg"), width = 20, height = 15)
	
	#### Fresh weight (binned) by treatment ####
	
	all_arch_data %>%
		filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
		ggplot() +
			geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
			scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
			geom_jitter(aes(SegmentEndY, SegmentEndX, fill = fw_bins),
						data = filter(all_fruit_data, !is.na(fw_bins)),
						alpha = 0.8,
						size = 4,
						width = 10,
						height = 70,
						shape = 21) +
			scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
			labs(x = NULL, y = NULL) +
			facet_grid(vars(VineRow), vars(VineTreatment), labeller = labeller(VineTreatment = column_labels)) +
			geom_text(aes(-2200, -2300, label = vine_label), 
					  size = 8,
					  data = vine_order) + 
			guides(size = FALSE) +
			ggtitle("Fresh weight by Vine & treatment") +
			theme_base_graph()
	
	ggsave(here("output/Vine 1 - 9 fresh weight by treatment map.png"), width = 20, height = 15)
