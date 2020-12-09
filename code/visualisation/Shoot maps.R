
theme_patrick <- function() {
	theme_bw() %+replace%
		theme(plot.title = element_text(hjust = 0.5, 
										size = 20,
										margin = margin(b = 5))
		)
}

theme_base_graph <- function() {
	theme_bw() %+replace%
		theme(plot.title = element_text(size = 18, hjust = 0.5),
			  axis.ticks = element_blank(),
			  axis.text = element_blank(),
			  strip.text.x = element_text(size = 12),
			  strip.text.y = element_blank())
}


vine_labels <- paste("Vine", c(1:9))
names(vine_labels) <- c(1:9)
column_labels <- c("Conventional", "Strung", "Spur")
names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")
# this file plots the network diagrams for each vine.

for(vine_id in 1:9) {
	
	temp_arch <- 
		all_arch_data %>%
		filter(VineUUID == vine_id)
	
	# need a list of all the possible node IDs
	vine_nodes <- 
		temp_arch %$%
		c(ParentNodeID, NodeID) %>%
		unique() %>%
		as.data.frame(.) %>%
		rename(label = 1) %>%
		arrange(label)
	
	# add architecture information for each node
	vine_nodes %<>%
		left_join(select(temp_arch, NodeID:ParentOriginID), by = c("label" = "NodeID")) %>%
		mutate(NodeType = factor(if_else(!is.na(ShootUUID), "Shoot", 
										 if_else(!is.na(OriginUUID), "Origin", "Junction")),
								 levels = c("Shoot", "Origin", "Junction")),
			   NodeLabel = if_else(!is.na(ShootUUID), ShootUUID, OriginUUID))
	
	
	# join previous node dataset and
	# edge dataset into a ggraph object then plot
	
	
	
	NodePlot<-
		temp_arch %>%
		filter(!is.na(SegmentStartX) & !is.na(SegmentStartY) & !is.na(SegmentEndX) & !is.na(SegmentEndY)) %>%
		ggplot() +
		geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend = SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
		scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
		labs(x = NULL, y = NULL) +
		geom_text_repel(aes(x = SegmentEndY, y = SegmentEndX,label=ShootUUID),size= 6) +
		guides(size = FALSE) +
		ggtitle(paste("Vine", vine_id, "node architecture")) +
		theme_base_graph() 
	NodePlot
	
	ggsave(here(paste0("output/Vine ", vine_id, "/vine", vine_id, "_shoot_nodes.png")), width = 30, height = 20)
}
