#' publR ggplot2 theme: A easy customizable theme for publishing ggplot2 figures  
#' @param font_family The font family used in all text of the figure
#' @param axis_line_width Size of axis lines and border around legend
#' @param font_size_labels Size of fonts in axis labels
#' @param font_size_axis Size of fonts in axis text
#' @param text_color Color of text in figure. The same color will be applied to all text
#' @export
publr_theme <- function(font_family =  "sans", axis_line_width = 0.2, font_size_labels = 12, font_size_axis = 10, text_color = "black"){
        
        ggplot2::theme_bw() +
                theme(axis.title = ggplot2::element_text(family = font_family, size = font_size_labels, color = text_color),
                      axis.line = ggplot2::element_line(size = axis_line_width),
                      axis.text = ggplot2::element_text(family = font_family, size = font_size_axis, color = text_color),
                      axis.ticks = ggplot2::element_line(size = axis_line_width),
                      legend.text = ggplot2::element_text(family = font_family, size = font_size_axis, color = text_color),
                      legend.title = ggplot2::element_text(family = font_family, size = font_size_axis, color = text_color),
                      panel.background = ggplot2::element_blank(), 
                      panel.border = ggplot2::element_blank(), 
                    #  panel.spacing = ggplot2::element_blank(),
                    #  panel.spacing.x = ggplot2::element_blank(), 
                    #  panel.spacing.y = ggplot2::element_blank(),
                      panel.grid = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(), 
                      panel.grid.major.x = ggplot2::element_blank(),
                      panel.grid.major.y = ggplot2::element_blank(),
                      panel.grid.minor.x = ggplot2::element_blank(),
                      panel.grid.minor.y = ggplot2::element_blank(),
                      legend.background = ggplot2::element_rect(color = text_color, size = axis_line_width))
}


