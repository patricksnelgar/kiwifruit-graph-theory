#### data import ####
vine7_data <- read_csv("input/architecture/kiwimac_data_vine7.csv") %>%
	mutate(to_shoot_id = ifelse(!is.na(to_shoot_id), paste(vine_id, to_shoot_id, sep = "-"), NA),
		   cane_id = ifelse(!is.na(cane_id), paste(vine_id, cane_id, sep = "-"), NA),
		   to_origin_id = ifelse(!is.na(to_origin_id), paste(vine_id, to_origin_id, sep = "-"), NA),
		   base_origin_id = ifelse(!is.na(base_origin_id), paste(vine_id, base_origin_id, sep = "-"), NA))

vine7_fruit_data <- read_csv("input/fruit_data/fruit_data_vine7.csv") %>%
	mutate(shoot_id = paste(Vine, ShootID, sep = "-"))

#### data gathering ####

vine7_sources <- vine7_data %>%
	distinct(from) %>%
	rename(label = from)

vine7_targets <- vine7_data %>%
	distinct(to) %>%
	rename(label = to)

vine7_nodes <- full_join(vine7_sources, vine7_targets, by = "label") %>%
	arrange(label) %>%
	full_join(select(vine7_data, to, length, to_shoot_id, x, y, quadrant, diameter, cane_id, to_origin_id, base_origin_id, notes), by = c("label" = "to"))

vine7_nodes %<>% 
	mutate(target_type = ifelse(!is.na(to_shoot_id), "Shoot", ifelse(!is.na(to_origin_id), "Origin","Junction"))) %>%
	mutate(target_type = factor(target_type, levels = c("Shoot", "Origin", "Junction"))) %>%
	group_by(base_origin_id) %>%
	mutate(origin_target_id = first(label)) %>%
	ungroup()

vine7_nodes %<>%
	full_join(quadrant_info, by = c("quadrant" = "quadrant")) %>%
	mutate(x_pos = (x + x_offset) * x_multiplier, y_pos = (y + y_offset) * y_multiplier) %>%
	select(label:to_shoot_id, target_type, origin_target_id, x_pos, y_pos, diameter, to_origin_id) %>%
	filter(!is.na(label))

vine7_links <- vine7_data %>%
	select(from, to) %>%
	full_join(select(vine7_nodes, label, x_pos, y_pos), by = c("from" = "label")) %>%
	rename(xstart = x_pos, ystart = y_pos) %>%
	full_join(vine7_nodes, by = c("to" = "label")) %>%
	rename(xend = x_pos, yend = y_pos) 

vine7_fruit_data %<>%
	left_join(., select(vine7_links, to_shoot_id, xend, yend), by = c("shoot_id" = "to_shoot_id")) 

vine7_fruit_data %<>%
	mutate(taste_bin = cut(DryMatter, c(0, 16.1, 17.4, 17.9, Inf), include.lowest = TRUE, labels = c("Under MTS", "M band", "T band", "Y band")),
		   dm_bins = cut(DryMatter, seq(0, 30, by = 1), include.lowest = TRUE, labels = paste((0:29), "to", (1:30))))
