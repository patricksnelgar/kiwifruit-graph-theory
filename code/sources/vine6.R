#### data import ####
vine6_data <- read_csv("input/architecture/kiwimac_data_vine6.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine6_fruit_data <- read_csv("input/fruit_data/fruit_data_vine6.csv") %>%
	mutate(shoot_id = paste(Vine, ShootID, sep = "-"))

#### data gathering ####

vine6_sources <- vine6_data %>%
	distinct(from) %>%
	rename(label = from)

vine6_targets <- vine6_data %>%
	distinct(to) %>%
	rename(label = to)

vine6_nodes <- full_join(vine6_sources, vine6_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine6_data, to, length, to_shoot_id, x, y, quadrant, diameter, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine6_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine6_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier) %>%
	select(label:to_shoot_id, target_type, origin_target_id, x_pos, y_pos, diameter, to_origin_id) %>%
	filter(!is.na(label))

vine6_links <- vine6_data %>%
	select(from, to) %>%
	full_join(select(vine6_nodes, label, x_pos, y_pos), by = c("from" = "label")) %>%
	rename(xstart = x_pos, ystart = y_pos) %>%
	full_join(vine6_nodes, by = c("to" = "label")) %>%
	rename(xend = x_pos, yend = y_pos) 

vine6_fruit_data %<>%
	left_join(., select(vine6_links, to_shoot_id, xend, yend), by = c("shoot_id" = "to_shoot_id")) 

vine6_fruit_data %<>%
	mutate(taste_bin = cut(DryMatter, c(0, 16.1, 17.4, 17.9, Inf), include.lowest = TRUE, labels = c("Under MTS", "M band", "T band", "Y band")),
		   dm_bins = cut(DryMatter, seq(0, 30, by = 1), include.lowest = TRUE, labels = paste((0:29), "to", (1:30))))

#### DryMatter heatmap plot ####
vine6_basic_dm <- ggplot(filter(vine6_links, !is.na(to))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend), colour = "lightgrey")+
	geom_point(aes(x = yend, y = xend, colour = target_type), 
			   data = filter(vine6_links, !is.na(to) & target_type == "Shoot"),
			   shape = 19, 
			   size = 4) + 
	scale_colour_manual(name = "target_type", 
						labels = c("Shoot"),
						values = c("Shoot" = "dimgrey")) +
	geom_jitter(aes(yend, xend, fill = dm_bins), 
				data = filter(vine6_fruit_data, !is.na(dm_bins)), 
				alpha = 0.8, 
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(type = "div", palette = "RdYlBu", direction = "-1") +
	guides(size = FALSE) +
	ggtitle("Kiwimac vine6 - basic Dry Matter heatmap") +
	labs(x = NULL, y = NULL) +
	theme_bw() + 
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

ggsave("./output/vine 6/basic Dry Matter heatmap.jpg", width = 16, height = 12)


#### ggraph stuff ####
vine6_graph <- tbl_graph(vine6_nodes, vine6_data) %>%
	activate(nodes) %>%
	mutate(cost_to_origin = node_distance_from(origin_target_id, weights = length)) %>%
	mutate(target_label = ifelse(!is.na(to_shoot_id), to_shoot_id, to_origin_id))


ggraph(vine6_graph, layout = "manual", x = vine6_nodes$y_pos, y = vine6_nodes$x_pos) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(fill = target_type), shape = 21, size = 5) + 
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 6") +
	geom_text(x = 0, y = 1550, label = "N", size = 14) +
	geom_text(x = 0, y = -1550, label = "S", size = 14) +
	# geom_vline(xintercept = c(-1000, -500, 0, 500, 1000)) +
	# geom_hline(yintercept = c(-2000, -1000, 0, 1000, 2000)) +
	# geom_text(aes(x = x, y = y, label = label), data = quadrant_labels, size = 14) +
	 theme_graph()


ggsave("output/vine 6/x-and-y_layout.png", width = 20, height = 20)


ggraph(vine6_graph, layout = "tree") + 
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 6) +
	geom_node_text(aes(label = target_label, colour = target_type), repel = TRUE, size = 6) +
	ggtitle("Kiwimac - Vine 6 architecture") + 
	theme_graph() +
	theme(text = element_text(size = 14), title = element_text(size = 18))

ggsave("output/vine 6/tree_architecture.png", width = 49, height = 20)