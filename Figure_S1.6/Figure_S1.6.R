###############################################################################
# Code to visuals Figure S1.6 (Protective efficacy decay of SMC with SP+AQ    #
# estimated by Zongo et al.16 and from our model)                             #
#                                                                             #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load data
DATA <- read.csv("C:/Users/massth/Desktop/SMC/CODE_WF_PLOT/SP_resistance_data_and_visualisation/Figure_S1.6/Data_figure_S1.6.csv", header = T)

# Plot
constant <- 2.5
PLOT <- ggplot(data = DATA) +
  geom_line(aes(x = Time, y = Efficacy, color = Results), size = 1 / constant, alpha = 1) +
  geom_ribbon(aes(x = Time, ymin = Efficacy_L, ymax = Efficacy_U, fill = Results),
  alpha = 0.25) +
  theme_bw() +
  xlim(5, 60) +
  ylim(0, 1) +
  labs(title = "", x = "Time since last SMC treatment (days)", y = "Protective efficacy") +
  scale_color_manual(name = "Setting:",
                     values = c("#FF7F00", "#1F78B4", "#FF50FF"),
                     labels = c("100% of quadruple mutant", "100% of senstive genotype", "Zongo et al."),
                     breaks = c("Quadruple", "Senstive", "Zongo")) +
    scale_fill_manual(name = "Setting:",
                      values = c("#FF7F00", "#1F78B4", "#FF50FF"),
                      labels = c("100% of quadruple mutant", "100% of senstive genotype", "Zongo et al."),
                      breaks = c("Quadruple", "Senstive", "Zongo")) +
    theme(axis.text.x = element_text(size = 16 / constant),
          axis.text.y = element_text(size = 16 / constant),
          axis.title.x = element_text(size = 18 / constant, face = "bold"),
          axis.title.y = element_text(size = 18 / constant, face = "bold"),
          plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
    theme(legend.text = element_text(size = 18 / constant)) +
    theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
    theme(legend.key.size = unit(0.9 / constant, "cm"))

# Save
ggsave("C:/Users/massth/Desktop/SMC/CODE_WF_PLOT/SP_resistance_data_and_visualisation/Figure_S1.6/Figure_S1.6.pdf", plot = PLOT, width = 10, height = 6, device = "pdf", units = "cm", dpi = 300)
