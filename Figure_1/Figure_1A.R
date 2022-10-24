###############################################################################
# Code to visuals Figure 1A (the PK of SP* with illustrate the selection      #
# windows of the  quintuple mutant, and the prophylactic period               #
# confer by AQ)                                                               #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# Load the data
Data_2 <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_1A.csv", header = T)

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Plot the results
constant <- 2.5
PLOT_A1 <- ggplot(data = Data_2, aes(x = Time, y = C)) +
  geom_line(size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "Concentration (mg/l)") +
  scale_x_continuous(name = "Time (days)", lim = c(0, 45)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.75, 0.1, 0.1, 0.1), "cm")) +
  annotate("rect", xmin = 21, xmax = 35, ymin = -4,  ymax = 84, alpha = .5, fill = "grey") +
  annotate("rect", xmin = 0, xmax = 17, ymin = -4, ymax = 84, alpha = .2, fill = "yellow") +
  geom_line(size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = 21, y = -Inf, xend = 21, yend = 86), linetype = "dashed", color = "#860086", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = 35, y = -Inf, xend = 35, yend = 86), linetype = "dashed", color = "#FF7F00", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = 42, y = -Inf, xend = 42, yend = 86), linetype = "dashed", color = "#1F78B4", size = 3 / ggplot2::.pt / constant) +
  annotate("text", x = 21, y = 87, label = "5", color = "#860086", size = 16 / ggplot2::.pt / constant, fontface = 2) +
  annotate( "text", x = 35, y = 87, label = "4", color = "#FF7F00", size = 16 / ggplot2::.pt / constant, fontface = 2) +
  annotate("text", x = 42, y = 87, label = "0", color = "#1F78B4", size = 16 / ggplot2::.pt / constant, fontface = 2) +
  annotate("text", x = 28, y = 40, label = "Selection\nwindows", size = 16 / ggplot2::.pt / constant, fontface = 2) + 
  coord_cartesian(ylim = c(0, 80), clip = "off")
