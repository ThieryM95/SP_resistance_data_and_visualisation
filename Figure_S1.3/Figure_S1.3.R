##############################################################
# Code to visualize Figure 1B (the relationship between the  #
# prophylactic period and the EC50)                          #
#                                                            #
#                                                            #
# author: thiery.masserey@swisstph.ch                        #
##############################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# load the results
Scenario_liste <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_S1.3.csv",header = T)

# Make EIR intro Factor
Scenario_liste$EIR <- as.factor(Scenario_liste$EIR)

# Plot
constant <- 2.5
PLOT_B1 <- ggplot(data = Scenario_liste, aes(x = IC50_SP, y =  Indicator_40_b, color = EIR)) +
  geom_line(size = 3 / ggplot2::.pt / constant) +
  scale_x_continuous(name = "EC50 (mg/l)", lim = c(0, 80)) +
  scale_y_continuous(name = "Prophylactic period (days)", lim = c(0, 45)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.75, 0.1, 0.1, 0.1), "cm")) +
  geom_segment(aes(y = 21, x = -Inf, yend = 21, xend = 24), linetype = "dashed", color = "#860086", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(y = 35, x = -Inf, yend = 35, xend = 2.4), linetype = "dashed", color = "#FF7F00", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(y = -Inf, x = 24, yend = 21, xend = 24), linetype = "dashed", color = "#860086", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(y = -Inf, x = 2.4, yend = 35, xend = 2.4), linetype = "dashed", color = "#FF7F00", size = 3 / ggplot2::.pt / constant) +
  scale_color_manual(name = "EIR:", values = c("#D7D7D7", "#A0A0A0", "#5A5A5A", "#0A0A0A"), labels = c("5", "50", "100", "150"), breaks = c("5", "50", "100", "150")) +
  theme(legend.position = "none")

ggsave("C:/Users/massth/Desktop/SMC/Figure_final/Figure_S1.3.pdf", plot = PLOT_B1, width = 7, height = 7, device = "pdf", units = "cm", dpi = 300)
