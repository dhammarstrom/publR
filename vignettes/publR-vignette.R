## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----publr_color---------------------------------------------------------
library(ggplot2); library(publR)
publr_color(display = TRUE)


## ----publr_color select colors-------------------------------------------

publr_color("cxxs19")


## ----publr_color select subset colors------------------------------------

publr_color("cxxs19")[c(1,3)]


## ----publr_color in plot-------------------------------------------------

data(iris)

ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) + geom_point(size = 4, shape = 21) +
        scale_fill_manual(values=publr_color("cxxd05")[c(3,7,11)])



## ----publr_theme---------------------------------------------------------

data(iris)

ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) + geom_point(size = 4, shape = 21) +
        scale_fill_manual(values=publr_color("cxxd05")[c(3,7,11)]) +
        publr_theme(font_family = "mono", axis_line_width = 0.5, font_size_labels = 10, font_size_axis = 8,
                    text_color = "black")



## ------------------------------------------------------------------------


p <- cor.test(iris$Sepal.Width, iris$Petal.Length)$p.value

ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) + geom_point(size = 4, shape = 21) +
        scale_fill_manual(values=publr_color("cxxd05")[c(3,7,11)]) +
        publr_theme(font_family = "mono", axis_line_width = 0.5, font_size_labels = 10, font_size_axis = 8,
                    text_color = "black") +
        annotate("text", x= 4, y = 7.5, label = pval(p, plot = TRUE), parse = TRUE, size = 2)


