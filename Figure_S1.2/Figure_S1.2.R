###############################################################################
# Code to visuals Figure S1.2 (Comparison between sulfadoxine concentrations  #
# predicted by De Kock et al.18 and those from our model)                     #
#                                                                             #
#                                                                             #
# Input: Data of drug concentration of SP* across time                        #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Data <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_S1.2.csv", header = T)

# Plot
constant <- 3
PLOT_A <- ggplot(data = Data[Data$Weight <= 15, ]) + geom_point(aes(x = Time, y = Concentration), size = 3 / ggplot2::.pt /constant) +
  geom_point(aes(x = Time, y = Concentration), size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "Predicted concentration (ug/ml)", breaks = seq(0, 200, 40), limits = c(0, 200)) +
  scale_x_continuous(name = "Time after dose (days)", breaks = seq(0, 50, 10), limits = c(0, 60)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant),
        axis.title.y = element_text(size =  18 / constant),
        plot.title = element_text(size = 18 / constant, hjust = 0.5)) +
  ggtitle("< Age < 2 years") +
  theme(plot.margin = unit(c(0.75, 0.1, 0.1, 0.1), "cm"))

PLOT_B <- ggplot(data = Data[Data$Weight <= 20 & Data$Weight >= 7, ]) +
  geom_point(aes(x = Time, y = Concentration), size = 3 / ggplot2::.pt /constant) +
  scale_y_continuous(name = "Predicted concentration (ug/ml)", breaks = seq(0, 200, 40), limits = c(0, 200)) +
  scale_x_continuous(name = "Time after dose (days)", breaks = seq(0, 50, 10), limits = c(0, 60)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant),
        axis.title.y = element_text(size =  18 / constant),
        plot.title = element_text(size = 18 / constant, hjust = 0.5)) +
  ggtitle("2 years < Age < 5 years") +
  theme(plot.margin = unit(c(0.75, 0.1, 0.1, 0.1), "cm"))

PLOT_C <- ggplot(data = Data[Data$Weight >= 12, ]) +
  geom_point(aes(x = Time, y = Concentration), size = 3 / ggplot2::.pt /constant) +
  scale_y_continuous(name = "Predicted concentration (ug/ml)", breaks = seq(0, 200, 40), limits = c(0, 200)) +
  scale_x_continuous(name = "Time after dose (days)", breaks = seq(0, 50, 10), limits = c(0, 60)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant),
        axis.title.y = element_text(size =  18 / constant),
        plot.title = element_text(size = 18 / constant, hjust = 0.5)) +
  ggtitle("Age > 5 years") +
  theme(plot.margin = unit(c(0.75, 0.1, 0.1, 0.1), "cm"))

#Merge all plot
PLOT <- plot_grid(PLOT_A, PLOT_B, PLOT_C, ncol = 3, nrow = 1, rel_widths = c(1, 1), scale = 1, labels = c("", "", ""), label_size = 16 / 2, align = "h")

# Save
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S1.2.pdf", plot = PLOT, width = 12, height = 8, device = "pdf", units = "cm", dpi = 300)