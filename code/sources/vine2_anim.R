vine2_links <- vine2_data %>%
					select(from, to) %>%
					full_join(select(vine2_nodes, label, x_pos, y_pos), by = c("from" = "label")) %>%
					rename(xstart = x_pos, ystart = y_pos) %>%
					full_join(vine2_nodes, by = c("to" = "label")) %>%
					rename(xend = x_pos, yend = y_pos) 

vine2_links %<>%
	mutate(fruit_count = as.integer(runif(length(to_shoot_id), min = 0, max = 6))) %>%
	mutate(dm = runif(length(fruit_count), 15, 19),
		   fruit_count = ifelse(!is.na(to_shoot_id), as.integer(fruit_count), NA),
		   dm = ifelse(!is.na(to_shoot_id), dm, NA))

manual_plot <- ggplot(filter(vine2_links, !is.na(to))) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend), colour = "brown")+
	geom_point(aes(x = yend, y = xend, colour = target_type), size = 3) + 
	scale_color_discrete(labels = c("Shoot", "Origin", "Junction")) +
	theme_blank() 


inter_manual <- ggplotly(manual_plot)

inter_manual$x$data[[1]]$text <- ""
inter_manual$x$data[[2]]$text <- paste("No. fruit:", vine2_links$fruit_count, "\nAvg. DM:", format(vine2_links$dm, digits = 3), "%")
inter_manual$x$data[[3]]$text <- filter(vine2_links, !is.na(to_origin_id))$to_origin_id
inter_manual$x$data[[4]]$text <- ""

inter_manual
