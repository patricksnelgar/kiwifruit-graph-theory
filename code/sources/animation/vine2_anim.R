vine2_links <- vine2_data %>%
					select(from, to) %>%
					full_join(select(vine2_nodes, label, x_pos, y_pos), by = c("from" = "label")) %>%
					rename(xstart = x_pos, ystart = y_pos) %>%
					full_join(vine2_nodes, by = c("to" = "label")) %>%
					rename(xend = x_pos, yend = y_pos) 

fruit_plots <- vine2_links %>% full_join(fruit_data, by = c("to_shoot_id" = "to_shoot_id"))

manual_plot <- ggplot(filter(vine2_links, !is.na(to))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend), colour = "brown")+
	geom_point(aes(x = yend, y = xend, colour = target_type), size = 1) + 
	geom_point(aes(xend, yend), colour = "lightgrey", alpha = 0.5, size = 1)+
	scale_color_discrete(labels = c("Shoot", "Origin", "Junction")) 

fruit_summary <- fruit_data %>%
					group_by(to_shoot_id) %>%
					summarise(dm = format(mean(dm), digits = 3), 
							  fw = format(mean(fw), digits = 4),
							  num_fruit = mean(num_fruit))

inter_manual <- ggplotly(manual_plot)

inter_manual$x$data[[1]]$text <- ""
inter_manual$x$data[[2]]$text <- paste("No. fruit:", fruit_summary$num_fruit, "\nAvg DM:", 
									   fruit_summary$dm, "%\nAvg FW:",
									   fruit_summary$fw, "g", sep = "")
inter_manual$x$data[[3]]$text <- filter(vine2_links, !is.na(to_origin_id))$to_origin_id
inter_manual$x$data[[4]]$text <- ""

inter_manual
