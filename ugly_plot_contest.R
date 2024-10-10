# load the built-in 'iris' data set
data(iris)

# load ggplot2 
library(ggplot2)

# create an ugly plot using the 'iris' data set 
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, shape = Species)) +
  geom_point(size = 8, alpha = 0.2) +  # absurdly large and transparent points
  geom_text(aes(label = paste(Sepal.Length, Sepal.Width, sep = ", ")), 
            size = 4, vjust = 1, hjust = 1, color = "black", angle = 200) +  # added labels to each point
  theme(
    panel.background = element_rect(fill = "lightblue"),  # unnecessarily distracting background color
    plot.background = element_rect(fill = "lightgreen"),  # distracting and bright background color
    axis.title = element_text(size = 5, color = "gold"),  # tiny and weird font size/color
    axis.text = element_text(size = 47, color = "darkred", face = "italic"),  # over sized axis values
    legend.position = "bottom",  # awkward legend position
    legend.background = element_rect(fill = "red"),  # pointless background color
    panel.grid.major = element_line(color = "lightpink", size = 0.5),  # off-color grid lines
    panel.grid.minor = element_line(color = "orange", linetype = "dotted", size = 2),  # unnecessarily large minor grid lines
    plot.title = element_text(size = 20, color = "purple", family = "Comic Sans MS", face = "bold"),  # odd title font and color
    plot.subtitle = element_text(size = 15, color = "blue", family = "Courier", face = "italic"),  # odd subtitle font and color
    strip.text = element_text(size = 5, color = "gold", family = "Comic Sans MS", face = "italic")  # illegible facet label font and color
  ) +
  labs(
    title = "iris PLOT",  # non-descriptive
    subtitle = "You ugly, you yo daddy's son...",  # unhelpful description
    x = "variable 1 length (units?)",  # confusing x-axis label
    y = "variable 2 width (units?)"  # confusing y-axis label
  ) +
  coord_flip() + # pointlessly flipped coordinates
  facet_wrap(~ Species, scales = "free_y")  # adds faceting, giving each plot its own y-axis scale
