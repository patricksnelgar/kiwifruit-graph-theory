
gp <- 
	all_arch_data %>%
	filter(VineUUID == 4) %>%
	ggplot() +
		geom_segment(aes(SegmentStartX, SegmentStartY, xend = SegmentEndX, yend = SegmentEndY), colour = "brown") +
		geom_point(aes(SegmentEndX, SegmentEndY), size = 3) + 
		theme_base_graph()

inter_gp <- ggplotly(gp)

fruit_data <- 
	all_fruit_data %>%
	filter(VineUUID == 4)

shoot_data <- 
	all_shoot_data %>%
	filter(VineUUID == 4)

arch_data <- 
	all_arch_data %>%
	filter(VineUUID == 4) %>%
	left_join(shoot_data, by = "ShootUUID")

#' customise the 'onHover' text attribute for each dataset
#' data[[1]] = target_type 'shoot'
#' data[[2]] = target_type 'origin'
#' data[[3]] = target_type 'junction'
#' 
#' 
inter_gp$x$data[[2]]$text <- paste("ID:", arch_data$ShootUUID, "No. fruit:", arch_data$NumFruit)


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

