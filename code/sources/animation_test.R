
edges <- data.frame(from = c(1,2,3,4,5,2), to = c(2,3,4,5,6,7))

nodes <- data.frame(label = c(1,2,3,4,5,6,7))


anim_data <- tbl_graph(edges = edges, nodes = nodes)

graph <- ggraph(anim_data, 'manual', x = c(1,1,1,1,1,1,2), y = c(1,2,3,4,5,6,2)) +
			geom_edge_link(aes(frame = label), colour = "red", data = nodes) + 
			geom_node_point(aes(frame = label), colour = "blue", data = nodes) + 
			view_step()


gg <- ggplot()+
		geom_point(aes(x = c(1,1,1,1,1,1,2), y = c(1,2,3,4,5,6,2), frame = c(1,2,3,4,5,6,7)))


gganimate(gg)		
