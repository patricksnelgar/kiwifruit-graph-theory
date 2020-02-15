
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


