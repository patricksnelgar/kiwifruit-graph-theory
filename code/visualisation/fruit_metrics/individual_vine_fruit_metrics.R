# Generate graphs for each metric for each vine,
# combine the plots and output.


for(vine_id in 1:9){
	
	base_plot <- all_arch_data %>%
		filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY) & 
			   	VineUUID == vine_id) %>%
		ggplot() +
			geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
			scale_size_continuous(breaks = pretty_breaks(10)) +
			labs(x = NULL, y = NULL) +
			guides(size = FALSE) +
			theme_base_graph() 
	
	dm_graph <- 
		base_plot +
		geom_jitter(aes(SegmentEndY, SegmentEndX, fill = dm_bins),
					data = filter(all_fruit_data, !is.na(dm_bins) & VineUUID == vine_id),
					alpha = 0.8,
					size = 4,
					width = 10,
					height = 70,
					shape = 21) +
		scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1")
	
	fw_graph <- 
		base_plot +
		geom_jitter(aes(SegmentEndY, SegmentEndX, fill = fw_bins),
					data = filter(all_fruit_data, !is.na(fw_bins) & VineUUID == vine_id),
					alpha = 0.8,
					size = 4,
					width = 10,
					height = 70,
					shape = 21) +
		scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1")
	
	flowering_graph <- 
		base_plot +
		geom_jitter(aes(SegmentEndY, SegmentEndX, fill = factor(FloweringDate)),
					data = filter(all_fruit_data, !is.na(FloweringDate) & VineUUID == vine_id),
					alpha = 0.8,
					size = 4,
					width = 10,
					height = 70,
					shape = 21) +
		scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1")
		
	
	hue_graph <- 
		base_plot +
		geom_jitter(aes(SegmentEndY, SegmentEndX, fill = hue_bins),
					data = filter(all_fruit_data, !is.na(hue_bins) & VineUUID == vine_id),
					alpha = 0.8,
					size = 4,
					width = 10,
					height = 70,
					shape = 21) +
		scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1")
	ssc_graph <- 
		base_plot +
		geom_jitter(aes(SegmentEndY, SegmentEndX, fill = ssc_bins),
					data = filter(all_fruit_data, !is.na(ssc_bins) & VineUUID == vine_id),
					alpha = 0.8,
					size = 4,
					width = 10,
					height = 70,
					shape = 21) +
		scale_fill_brewer(name = "Soluable solids", type = "div", palette = "RdYlBu", direction = "1")
	
	firmness_graph <- 
		base_plot +
		geom_jitter(aes(SegmentEndY, SegmentEndX, fill = firm_bins),
					data = filter(all_fruit_data, !is.na(firm_bins) & VineUUID == vine_id),
					alpha = 0.8,
					size = 4,
					width = 10,
					height = 70,
					shape = 21) +
		scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1")
	
	plot_grid(dm_graph, fw_graph, flowering_graph, hue_graph, ssc_graph, firmness_graph,
			  labels = c(paste("Vine", vine_id)),
			  hjust = -0.3,
			  vjust = 2,
			  label_size = 24) 
	
	ggsave(here(paste0("output/Vine ", vine_id, "/vine", vine_id, "_all_metrics.jpg")), width = 30, height = 16)
	
}

# Environment cleanup
rm(list = ls(pattern = "_graph"))
