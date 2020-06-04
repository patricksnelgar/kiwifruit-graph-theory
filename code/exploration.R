
edges <- data.frame(from = c(1,2,3,4,5,2), to = c(2,3,4,5,6,7))

nodes <- data.frame(label = c(1,2,3,4,5,6,7), x = c(1,1,1,1,1,1,2), y = c(1,2,3,4,5,6,2))


anim_data <- tbl_graph(edges = edges, nodes = nodes)

graph <- ggraph(anim_data, 'manual', x = x, y = y) +
			geom_edge_link(colour = "red") + 
			geom_node_point(aes(group = label), colour = "blue") + 
			xlim(c(0, 2)) + 
			ylim(c(0, 7)) +
			transition_reveal(along = label) 

#'manual', x = c(1,1,1,1,1,1,2), y = c(1,2,3,4,5,6,2)
#'


#https://paldhous.github.io/ucb/2018/dataviz/week14.html

#https://stackoverflow.com/questions/56206985/smooth-transitions-between-different-layouts-of-the-same-network-in-ggraph

#https://imagemagick.org/script/download.php#windows


for(i in 1:7) {
	file_name <- paste("part_", i, ".png", sep = "")
	anim_data %N>%
		filter(label <= i) %>%
			ggraph('manual', x = x, y = y) +
				geom_node_point(colour = "blue") +
				geom_edge_link(colour = "brown") + 
				geom_node_text(aes(label = label), nudge_x = 0.05) +
				xlim(c(-2.5, 2.5)) + 
				ylim(c(-6, 6)) +
				theme_graph()
		
	
		ggsave(file_name, width = 4, height = 3)
		
}

system('magick -delay 75 part*.png output/test_tree.gif')

ggraph(anim_data, 'manual', x = x, y = y) +
	geom_node_point(colour = "blue") +
	geom_edge_link(colour = "brown") + 
	geom_node_text(aes(label = label), nudge_x = 0.05) +
	xlim(c(-2.5, 2.5)) + 
	ylim(c(-6, 6)) +
	theme_graph()


d3 <- data.frame(from = as.numeric(edges$from)-1, to = as.numeric(edges$to)-1)

forceNetwork(d3, nodes, "from", "to", NodeID = "label", Group = 1)


ggplotly(graph)




ggraph(vine2_graph, layout = "manual", x = y_pos, y = x_pos) +
	geom_edge_link(colour = "brown") +
	geom_node_point(aes(colour = target_type), size = 5) + 
	geom_node_text(aes(label = target_label), colour = "black", repel = TRUE) +
	ggtitle("2D layout - Vine 2") +
	#geom_text(x = 0, y = 1750, label = "N", size = 14) +
	#geom_text(x = 0, y = -1700, label = "S", size = 14) +
	theme_graph()


ggplotly(vine2)




vine2_subset <- vine2_graph %N>%
	filter(label < 100) %>%
	mutate(fruit_count = ifelse(!is.na(to_shoot_id), as.integer(runif(length(label), 1, 6)), NA))

origin_labels <- vine2_subset %N>%
	filter(!is.na(to_origin_id)) %>%
	as_tibble() %>%
	select(to_origin_id)

shoot_labels <- vine2_subset %N>%
	filter(!is.na(fruit_count)) %>%
	as_tibble() %>%
	select(fruit_count)

junction_labels <- vine2_subset %N>%
	filter(is.na(fruit_count) & is.na(to_origin_id)) %>%
	select(label) %>%
	as_tibble()

gp <- ggraph(vine2_subset, 'manual', x = y_pos, y = x_pos) +
	geom_node_point(aes(colour = target_type), size = 3) +
	geom_edge_link(colour = "brown") +
	theme_graph()

vine2_nodes_subset <- vine2_subset %N>%
	as.data.frame()
vine2_data_subset <- vine2_subset %E>%
	as.data.frame()

vine2_links <- vine2_data_subset %>%
	select(from, to) %>%
	full_join(select(vine2_nodes_subset, label, x_pos, y_pos), by = c("from" = "label")) %>%
	rename(xstart = x_pos, ystart = y_pos) %>%
	full_join(vine2_nodes_subset, by = c("to" = "label")) %>%
	rename(xend = x_pos, yend = y_pos) 

inter_gp <- ggplotly(gp)

inter_gp %>%
	add_segments(., data = vine2_links, x = ~xstart, y = ~ystart, xend = ~xend, yend = ~yend, inherit = FALSE, colour = "brown")

# for(i in 1:nrow(vine2_links)){
# 	inter_gp <- add_segments(inter_gp,
# 					   data = vine2_links,
# 					   inherit = FALSE,
# 					   x = ~xstart[i], 
# 					   y = ~ystart[i], 
# 					   xend = ~xend[i], 
# 					   yend = ~yend[i],
# 					   hoverinfo = 'none')
# }



#' customise the 'onHover' text attribute for each dataset
#' data[[1]] = target_type 'shoot'
#' data[[2]] = target_type 'origin'
#' data[[3]] = target_type 'junction'
#' 
#' 
inter_gp$x$data[[1]]$text <- paste("No. fruit:", shoot_labels$fruit_count, "\nAvg. DM: 16.3%")
inter_gp$x$data[[2]]$text <- origin_labels$to_origin_id
inter_gp$x$data[[3]]$text <- junction_labels$label

inter_gp




#' custom js events docs
#' https://plotly-r.com/js-event-handlers.html
#' this <ADDS> text on hover 
#' 
#' Can abstract the custom events to a js file instead of inline mess

inter_gp %>%
	onRender("
    function(el) { 
      el.on('plotly_hover', function(d) { 
      	var x = d.points[0].x;
      	var y = d.points[0].y
        var ann = {
        	text: 'test',
        	showarrow: false,
        	x: x -0.2,
        	y: y
        };
        Plotly.relayout(el.id, {annotations: [ann]});
      });
    }
  ")

manual_plot <- ggplot(vine2_links) +
	geom_segment(aes(x = ystart, y = xstart, xend = yend, yend = xend), colour = "brown")+
	geom_point(aes(x = yend, y = xend, colour = target_type), size = 3) + 
	theme_blank() 


inter_manual <- ggplotly(manual_plot)

inter_manual$x$data[[2]]$text <- paste("No. fruit:", shoot_labels$fruit_count, "\nAvg. DM: 16.3%")
inter_manual$x$data[[3]]$text <- origin_labels$to_origin_id
inter_manual$x$data[[4]]$text <- ""

inter_manual

