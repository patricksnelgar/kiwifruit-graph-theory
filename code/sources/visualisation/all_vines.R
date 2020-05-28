
all_fruit_data <- bind_rows(vine1_fruit_data, vine2_fruit_data, vine3_fruit_data,
							vine4_fruit_data, vine5_fruit_data, vine6_fruit_data, 
							vine7_fruit_data, vine8_fruit_data, vine9_fruit_data) %>%
	mutate(VineTreatmentOrder = factor(Vine, levels = c(1, 2, 3, 4, 6, 5, 9, 8, 7)),
		   VineTreatment = ifelse(Vine %in% c(1, 4, 9), "1 Conv", 
		   					   ifelse(Vine %in% c(2, 6, 8), "2 Stru",
		   					   	   ifelse(Vine %in% c(3, 5, 7), "3 Spur", NA))),
		   VineRow = ifelse(Vine %in% c(1:3), 1,
		   				 ifelse(Vine %in% c(4:6), 2,
		   				 	   ifelse(Vine %in% c(7:9), 3, NA)))) %>%
	mutate(dm_bins = cut(DryMatter, c(0,14:23,30), include.lowest = TRUE, labels = c("< 14", paste(14:22, "to", 15:23), "> 23")),
		   fw_bins = cut(FreshWeight, c(0, 74, 84, 95, 108, 118, 128, 138, 151, 180, 210, 240), 
		   			  include.lowest = TRUE, 
		   			  right = FALSE,
		   			  labels = c("< 74g", paste("count", c(42, 39, 36, 33, 30, 27, 25, 22, 18, 16)))),
		   hue_bins = cut(Hue1, seq(0, 120, 5), labels = paste(seq(0, 115, 5), "to", seq(5, 120, 5))),
		   ssc_bins = cut(SSC1, seq(5, 20, 1.5), include.lowest = TRUE, labels = paste(seq(5, 18.5, 1.5), "to", seq(6.5, 20, 1.5))),
		   firm_bins = cut(Firm1, seq(0, 11, 1), include.lowest = TRUE, labels = paste(0:10, "to", 1:11)))

all_fruit_data %<>%
	left_join(flowering_dates, by = c("FloweringColour" = "FloweringColour"))


all_arch_data <- bind_rows(mutate(vine1_links, Vine = 1),
						   mutate(vine2_links, Vine = 2),
						   mutate(vine3_links, Vine = 3),
						   mutate(vine4_links, Vine = 4),
						   mutate(vine5_links, Vine = 5),
						   mutate(vine6_links, Vine = 6),
						   mutate(vine7_links, Vine = 7),
						   mutate(vine8_links, Vine = 8),
						   mutate(vine9_links, Vine = 9)) %>%
	mutate(VineTreatmentOrder = factor(Vine, levels = c(1, 2, 3, 4, 6, 5, 9, 8, 7)),
		   VineTreatment = ifelse(Vine %in% c(1, 4, 9), "1 Conv", 
		   					   ifelse(Vine %in% c(2, 6, 8), "2 Stru",
		   					   	   ifelse(Vine %in% c(3, 5, 7), "3 Spur", NA))),
		   VineRow = ifelse(Vine %in% c(1:3), 1,
		   				 ifelse(Vine %in% c(4:6), 2,
		   				 	   ifelse(Vine %in% c(7:9), 3, NA))))



#### Basic DM by vine ####
vine_labels <- paste("Vine", c(1:9))
names(vine_labels) <- c(1:9)

ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & DryMatter > 12),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	facet_wrap(~Vine, labeller = labeller(Vine = vine_labels)) +
	guides(size = FALSE) +
	ggtitle("Dry Matter by Vine") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank(),
		  strip.text.x = element_text(size = 12))

ggsave("output/vine 1 - 9 dry matter map.jpg", width = 20, height = 15)

#### Dry matter (binned) by treatment ####

column_labels <- c("Conventional", "Strung", "Spur")
names(column_labels) <- c("1 Conv", "2 Stru", "3 Spur")

vine_metrics <- all_fruit_data %>%
					group_by(Vine) %>%
					summarise(fruit_count = n())

vine_order <- data.frame(vine_label = paste("Vine", c(1:4, 6, 5, 9, 8, 7)), 
						 VineRow = c(rep(1:3, each = 3)),
						 VineTreatment = c("1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur", "1 Conv", "2 Stru", "3 Spur"),
						 cropload = c(vine_metrics$fruit_count[c(1:4, 6, 5, 9, 8, 7)]))

ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend)) ) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
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
	ggtitle("Dry Matter by vine & treatment") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank(),
		  strip.text.x = element_text(size = 12),
		  strip.text.y = element_blank())

ggsave("output/vine 1 - 9 dry matter by treatment map.jpg", width = 20, height = 15)

#### Flower Timing by treatment ####

ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend)) ) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
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
	ggtitle("Flowering date by vine & treatment") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank(),
		  strip.text.x = element_text(size = 12),
		  strip.text.y = element_blank())

ggsave("output/vine 1 - 9 flowering date by treatment map.jpg", width = 20, height = 15)

#### Fresh weight (binned) by treatment ####

ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend)) ) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
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
	ggtitle("Fresh weight by vine & treatment") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank(),
		  strip.text.x = element_text(size = 12),
		  strip.text.y = element_blank())

ggsave("output/vine 1 - 9 fresh weight by treatment map.jpg", width = 20, height = 15)


#### Vine 1 - all metrics ####

vine1_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 1)) +
				geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
				scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
				geom_jitter(aes(yend, xend, fill = dm_bins),
							data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 1),
							alpha = 0.8,
							size = 4,
							width = 10,
							height = 70,
							shape = 21) +
				scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
				labs(x = NULL, y = NULL) +
				guides(size = FALSE) +
				ggtitle("Dry matter") +
				theme_bw() +
				theme(plot.title = element_text(size = 22, hjust = 0.5),
					  axis.ticks = element_blank(),
					  axis.text = element_blank())

vine1_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 1)) +
				geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
				scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
				geom_jitter(aes(yend, xend, fill = fw_bins),
							data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 1),
							alpha = 0.8,
							size = 4,
							width = 10,
							height = 70,
							shape = 21) +
				scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
				labs(x = NULL, y = NULL) +
				guides(size = FALSE) +
				ggtitle("Fresh weight") +
				theme_bw() +
				theme(plot.title = element_text(size = 22, hjust = 0.5),
					  axis.ticks = element_blank(),
					  axis.text = element_blank())

vine1_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 1)) +
					geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
					scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
					geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
								data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 1),
								alpha = 0.8,
								size = 4,
								width = 10,
								height = 70,
								shape = 21) +
					scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
					labs(x = NULL, y = NULL) +
					guides(size = FALSE) +
					ggtitle("Flowering") +
					theme_bw() +
					theme(plot.title = element_text(size = 22, hjust = 0.5),
						  axis.ticks = element_blank(),
						  axis.text = element_blank())

vine1_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 1)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 1),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine1_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 1)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 1),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine1_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 1)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 1),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine1_dm, vine1_fw, vine1_flowering, vine1_hue, vine1_ssc, vine1_firm,
				   labels = c("Vine 1"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 1/vine1_all_metrics.jpg", width = 30, height = 16)	

#### vine 2 - all metrics ####

vine2_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 2)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 2),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine2_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 2)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 2),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine2_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 2)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 2),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine2_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 2)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 2),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine2_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 2)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 2),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine2_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 2)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 2),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine2_dm, vine2_fw, vine2_flowering, vine2_hue, vine2_ssc, vine2_firm,
				   labels = c("Vine 2"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 2/vine2_all_metrics.jpg", width = 30, height = 16)	
#### Vine 3 - all metrics ####

vine3_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 3)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 3),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine3_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 3)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 3),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine3_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 3)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 3),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine3_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 3)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 3),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine3_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 3)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 3),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine3_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 3)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 3),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine3_dm, vine3_fw, vine3_flowering, vine3_hue, vine3_ssc, vine3_firm,
				   labels = c("Vine 3"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 3/vine3_all_metrics.jpg", width = 30, height = 16)	
#### vine 4 - all metrics ####

vine4_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 4)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 4),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine4_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 4)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 4),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine4_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 4)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 4),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine4_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 4)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 4),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine4_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 4)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 4),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine4_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 4)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 4),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine4_dm, vine4_fw, vine4_flowering, vine4_hue, vine4_ssc, vine4_firm,
				   labels = c("Vine 4"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 4/vine4_all_metrics.jpg", width = 30, height = 16)	
#### vine 5 - all metrics ####

vine5_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 5)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 5),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine5_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 5)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 5),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine5_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 5)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 5),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine5_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 5)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 5),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine5_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 5)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 5),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine5_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 5)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 5),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine5_dm, vine5_fw, vine5_flowering, vine5_hue, vine5_ssc, vine5_firm,
				   labels = c("Vine 5"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 5/vine5_all_metrics.jpg", width = 30, height = 16)	
#### vine 6 - all metrics ####

vine6_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 6)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 6),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine6_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 6)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 6),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine6_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 6)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 6),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine6_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 6)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 6),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine6_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 6)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 6),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine6_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 6)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 6),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine6_dm, vine6_fw, vine6_flowering, vine6_hue, vine6_ssc, vine6_firm,
				   labels = c("Vine 6"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 6/vine6_all_metrics.jpg", width = 30, height = 16)	
#### vine 7 - all metrics ####

vine7_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 7)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 7),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine7_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 7)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 7),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine7_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 7)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 7),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine7_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 7)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 7),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine7_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 7)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 7),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine7_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 7)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 7),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine7_dm, vine7_fw, vine7_flowering, vine7_hue, vine7_ssc, vine7_firm,
				   labels = c("Vine 7"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 7/vine7_all_metrics.jpg", width = 30, height = 16)
#### vine 8 - all metrics ####

vine8_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 8)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 8),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine8_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 8)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 8),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine8_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 8)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 8),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine8_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 8)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 8),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine8_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 8)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 8),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine8_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 8)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 8),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine8_dm, vine8_fw, vine8_flowering, vine8_hue, vine8_ssc, vine8_firm,
				   labels = c("Vine 8"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 8/vine8_all_metrics.jpg", width = 30, height = 16)	
#### vine 9 - all metrics ####

vine9_dm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 9)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & Vine == 9),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Dry matter (%)", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Dry matter") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine9_fw <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 9)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & Vine == 9),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Fresh weight", type = "div", palette = "RdYlBu", direction = "-1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Fresh weight") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine9_flowering <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 9)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & Vine == 9),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Flowering date", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Flowering") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine9_hue <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 9)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & Vine == 9),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Hue", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Hue") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine9_ssc <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 9)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & Vine == 9),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "SSC (brix)", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("SSC") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

vine9_firm <- ggplot(filter(all_arch_data, !is.na(xend) & !is.na(yend) & Vine == 9)) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend, size = diameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(yend, xend, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & Vine == 9),
				alpha = 0.8,
				size = 4,
				width = 10,
				height = 70,
				shape = 21) +
	scale_fill_brewer(name = "Firmness", type = "div", palette = "RdYlBu", direction = "1") +
	labs(x = NULL, y = NULL) +
	guides(size = FALSE) +
	ggtitle("Firmness") +
	theme_bw() +
	theme(plot.title = element_text(size = 22, hjust = 0.5),
		  axis.ticks = element_blank(),
		  axis.text = element_blank())

cowplot::plot_grid(vine9_dm, vine9_fw, vine9_flowering, vine9_hue, vine9_ssc, vine9_firm,
				   labels = c("Vine 9"),
				   hjust = -0.1,
				   label_size = 24)

ggsave("output/vine 9/vine9_all_metrics.jpg", width = 30, height = 16)	