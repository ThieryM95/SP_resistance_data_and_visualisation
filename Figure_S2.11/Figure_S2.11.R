###############################################################################
# Code to visuals Figure S2.9 (The impact of equitable and in-equitable       #
# SMC delivery on the spread of the quintuple mutant)                         #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Prediction_table_final_100 <- read.csv("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Data_figure_S2.9.csv", header = T)

# Change format of variable
Prediction_table_final_20$Age <- as.factor(Prediction_table_final_20$Age)
Prediction_table_final_20$Number_round <- as.factor(Prediction_table_final_20$Number_round)
Prediction_table_final_20$Seasonality <- as.factor(Prediction_table_final_20$Seasonality)
Prediction_table_final_20$Coverage_reduction <- as.factor(Prediction_table_final_20$Coverage_reduction)
Prediction_table_final_20$Coverage <- as.factor(Prediction_table_final_20$Coverage)
Prediction_table_final_20$Coverage <- factor(Prediction_table_final_20$Coverage, levels = c("0.95", "0.85", "0.75"))
Prediction_table_final_20$Access <- as.factor(Prediction_table_final_20$Access)

# Label factors
S.labs <- c("Moderate seasonality setting", "High seasonality setting")
names(S.labs) <- c("sesonality2", "sesonality3")

Cov.labs <- c("Coverage = 75%", "Coverage = 85%", "Coverage = 95%")
names(Cov.labs) <- c("0.75", "0.85", "0.95")

C.labs <- c("Coverage reduction\n at each round = 0%", "Coverage reduction\n at each round = 10%")
names(C.labs) <- c("0", "0.1")

T.labs <- c("Low access to treatment", "High access to treatment")
names(T.labs) <- c("0.1", "0.4")

# Plot
PLOT_S7 <- ggplot(data = Prediction_table_final_100) +
  geom_point(aes(x = EIR, y = Time_0.1, color = Equity), size = 3 / constant, alpha = 1) +
  facet_nested(. ~ Coverage, labeller = labeller(Seasonality = S.labs, Coverage = Cov.labs, Coverage_reduction = C.labs, Access = T.labs)) +
  theme_bw() +
  ylim(0, 50) +
  xlim(0, 150) +
  labs(title = "", x = "EIR (inoculations per person per year)", y = "T50 (years)") +
  theme_bw() +
  scale_color_manual(name = "SMC delivery:",
                     values = c("#2C85B2", "#FF50FF"),
                     labels = c("Equitable ", "In-equitable"),
                     breaks = c("High", "Poor")) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.1, "cm")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) + 
  guides(fill = FALSE) + theme(legend.position = "top") + 
  theme(legend.spacing.x = unit(0.5, "cm")) + 
  guides(colour = guide_legend(override.aes = list(size = 2)))

# Save
ggsave("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S2.9.pdf", plot = PLOT_S7, width = 15, height = 7, device = "pdf", units = "cm", dpi = 300)
