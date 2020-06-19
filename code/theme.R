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
