###############################################################################
# Code to visuals Figure S1.4 & S.1.5 (Comparison between the maximum DEAQ    #
# concentrations, and 7 days concentration predicted by Ali et al.2 and our   #
# model stratified by child weight confer by AQ)                              #
#                                                                             #
#                                                                             #
# Author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Data <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_S1.4_&_S1.5.csv", header = T)

# Figure S1.4
constant <- 2.1
PA <- ggplot(data = Data, aes(x = Weight, y = Cmax_2, color=Prediction)) +
  geom_point(size = 2.05 / constant) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15 / constant, face = "bold", color = "black"),
        axis.text.y = element_text(size = 15 / constant, face = "bold", color = "black"),
        axis.title.x = element_text(size = 16 / constant, face = "bold"),
        axis.title.y = element_text(size = 16 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  ylab("Maximum concentration [ng/ml]") +
  scale_x_continuous(breaks = seq(5, 80, 5)) +
  scale_y_continuous(breaks = c(250, 500, 750, 1000), limits = c(100, 1000)) +
  scale_color_manual(name = "Predictions from:", values = c("#2C85B2", "#E5B17E"), breaks = c("Original", "Our"), labels = c("Model of Ali et al.", "Our model")) +
  xlab("Individual weight [kg]") +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(legend.key.width = unit(1.0, "cm")) +
  theme(plot.margin = unit(c(5, 5, 5.5, 5.5), "pt"))

# Save
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S1.5.pdf", plot = PA, width = 12, height = 5.5, device = "pdf", units = "cm", dpi = 300)

# Figure S1.4
PB <- ggplot(data = Data, aes(x = Weight, y = C_7_2,color=Prediction)) +
  geom_point(size = 2.05 / constant) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15 / constant, face = "bold", color = "black"),
        axis.text.y = element_text(size = 15 / constant, face = "bold", color = "black"),
        axis.title.x = element_text(size = 16 / constant, face = "bold"),
        axis.title.y = element_text(size = 16 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  ylab("Day 7 concentration [ng/ml]") +
  ylim(0, 175) +
  scale_x_continuous(breaks = seq(5, 80, 5)) +
  scale_color_manual(name = "Predictions from:", values = c("#2C85B2", "#E5B17E"), breaks = c("Original", "Our"), labels = c("Model of Ali et al.", "Our model")) +
  xlab("Individual weight [kg]") +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(legend.key.width = unit(1.0, "cm")) +
  theme(plot.margin = unit(c(5, 5, 5.5, 5.5), "pt"))

# Save
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S1.4.pdf", plot = PB, width = 12, height = 5, device = "pdf", units = "cm", dpi = 300)
