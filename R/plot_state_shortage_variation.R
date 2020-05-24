#' This function plots shortages by year.
#'
#' @export

plot_shortages <- function(write_to_bp = "./doc/figures/shortage_bp.pdf", write_to_ln = "./doc/figures/shortage_ln.pdf") {

ls <- readd("analysis_sample_ls")
state_tbl <- ls$state_tbl 

short_boxplot <- ggplot(state_tbl, aes(x = state, y = avg_shortage)) +
	geom_boxplot() + 
	coord_flip() +
	theme_pubr() +
	theme(
  panel.background = element_rect(fill = "#d3d3d3", colour = "black", size = 1, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
  axis.text.y = element_text(size = 9),
  axis.title.x = element_text(size = 11)
  ) + 
ylab("Shortage") +
xlab("")


biggest_states <- c("Uttar Pradesh", "Maharashtra", "Bihar", "West Bengal", "Andhra Pradesh")

big_states <- state_tbl %>% 
	filter(state %in% biggest_states) %>%
	group_by(state) %>%
	mutate(period_avg_shortage = mean(avg_shortage))

five_biggest_p <- ggplot(big_states, aes(x = year, y = avg_shortage)) +
	geom_line(aes(y = period_avg_shortage), linetype = "dotted", size = 0.8) +
	geom_line(size = 0.8) +
	facet_grid(rows= vars(state)) +
	theme_pubr() +
	theme(
  panel.background = element_rect(fill = "#d3d3d3", colour = "black", size = 1, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
  axis.text.x = element_text(size = 9),
  axis.title.x = element_text(size = 11)
  ) +
 ylab("Shortage") +
 xlab("Year")

ggsave(plot = ggarrange(five_biggest_p, short_boxplot), here("doc/figures/shortage_p_dl.pdf"))

}
