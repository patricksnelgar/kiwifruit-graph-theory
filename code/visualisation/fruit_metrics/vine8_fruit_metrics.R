#### Vine 8 - all metrics ####

vine8_dm <- ggplot(filter(all_arch_data, !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineUUID == 8)) +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(SegmentEndY, SegmentEndX, fill = dm_bins),
				data = filter(all_fruit_data, !is.na(dm_bins) & VineUUID == 8),
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

vine8_fw <- ggplot(filter(all_arch_data, !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineUUID == 8)) +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(SegmentEndY, SegmentEndX, fill = fw_bins),
				data = filter(all_fruit_data, !is.na(fw_bins) & VineUUID == 8),
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

vine8_flowering <- ggplot(filter(all_arch_data, !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineUUID == 8)) +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(SegmentEndY, SegmentEndX, fill = factor(FloweringDate)),
				data = filter(all_fruit_data, !is.na(FloweringDate) & VineUUID == 8),
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

vine8_hue <- ggplot(filter(all_arch_data, !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineUUID == 8)) +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(SegmentEndY, SegmentEndX, fill = hue_bins),
				data = filter(all_fruit_data, !is.na(hue_bins) & VineUUID == 8),
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

vine8_ssc <- ggplot(filter(all_arch_data, !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineUUID == 8)) +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(SegmentEndY, SegmentEndX, fill = ssc_bins),
				data = filter(all_fruit_data, !is.na(ssc_bins) & VineUUID == 8),
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

vine8_firm <- ggplot(filter(all_arch_data, !is.na(SegmentEndX) & !is.na(SegmentEndY) & VineUUID == 8)) +
	geom_segment(aes(x = SegmentStartY, y = SegmentStartX, xend =  SegmentEndY, yend =  SegmentEndX, size = SegmentDiameter), colour = "lightgrey") +
	scale_size_continuous(name = "Segment diameter", breaks = pretty_breaks(10)) +
	geom_jitter(aes(SegmentEndY, SegmentEndX, fill = firm_bins),
				data = filter(all_fruit_data, !is.na(firm_bins) & VineUUID == 8),
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

plot_grid(vine8_dm, vine8_fw, vine8_flowering, vine8_hue, vine8_ssc, vine8_firm,
		  labels = c("Vine 8"),
		  hjust = -0.1,
		  label_size = 24)

ggsave("output/Vine 8/vine8_all_metrics.jpg", width = 30, height = 16)

rm(list = ls(pattern = "vine8"))