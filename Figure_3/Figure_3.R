###############################################################################
# Code to visuals Figure 3 and figure S2.8 (Impact of SMC deployment          #
# strategies on the spread of the quintuple mutant)                           #
#                                                                             #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Prediction_table_final_2 <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_3_&_S2.9&_S2.10.csv", header = T)

# Transform continuous variables into factors
Prediction_table_final_2$Age <- as.factor(Prediction_table_final_2$Age)
Prediction_table_final_2$Number_round <- as.factor(Prediction_table_final_2$Number_round)
Prediction_table_final_2$Seasonality <- as.factor(Prediction_table_final_2$Seasonality)
Prediction_table_final_2$Coverage_reduction <- as.factor(Prediction_table_final_2$Coverage_reduction)
Prediction_table_final_2$Coverage <- as.factor(Prediction_table_final_2$Coverage)
Prediction_table_final_2$Coverage <- factor(Prediction_table_final_2$Coverage, levels = c("0.95", "0.85", "0.75"))
Prediction_table_final_2$Access <- as.factor(Prediction_table_final_2$Access)

# Label the different factors levels
S.labs <- c("Moderate seasonality setting", "High seasonality setting")
names(S.labs) <- c("sesonality2", "sesonality3")

Cov.labs <- c("Coverage = 75%", "Coverage = 85%", "Coverage = 95%")
names(Cov.labs) <- c("0.75", "0.85", "0.95")

C.labs <- c("Coverage reduction\n at each round = 0%", "Coverage reduction\n at each round = 10%")
names(C.labs) <- c("0", "0.1")

T.labs <- c("Low access to treatment", "High access to treatment")
names(T.labs) <- c("0.1", "0.4")

# Figure with EIR (Figure 3)
##############################

# Plot
Prediction_table_final_3 <- Prediction_table_final_2[Prediction_table_final_2$Coverage==0.95 & Prediction_table_final_2$Coverage_reduction==0.1 & Prediction_table_final_2$Access==0.4, ]
constant <- 2.5
PLOT_3 <- ggplot(data = Prediction_table_final_3) +
  geom_point(aes(x = EIR, y = Time_0.1, color = Number_round, shape = Age), size = 4 / constant, alpha = 1, stroke = 2/ constant) +
  facet_nested(~ Seasonality, labeller = labeller(Seasonality = S.labs)) +
  theme_bw() +
  ylim(0, 150) +
  xlim(0, 150) +
  labs(title = "", x = "EIR (inoculations per person per year)", y = "Time needed for the quintuple genotype to spread\n from a frequency in inoculations of 1% to 50% (years)") +
  theme_bw() +
  scale_color_manual(name = "Number of rounds:",
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("Usual deployment", "Usual deployment + 1 after", "Usual deployment + 1 before"),
                     breaks = c("4", "4.5", "5.4")) +
  scale_shape_manual(values = c(0, 17),
                     name = "Targeted age group:",
                     breaks = c("5", "10"),
                     labels = c("Under 5 years", "Under 10 years")) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5,face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(1/constant, "cm")) +
  theme(plot.margin = unit(c(0, 0.05, 0.05, 0), "cm")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text( size = 18 / constant, color = "black", face = "bold")) + 
  guides(fill = FALSE) + 
  #theme(legend.position = "top", legend.box = "vertical") + 
  theme(legend.spacing.x = unit(0.25/constant, "cm"),
        legend.spacing.y = unit(0.25/constant, "cm"), # legend.spacing.y = unit(-0.25, "cm"),
        legend.margin = margin(0, 0, 0, 0)) + 
  guides(colour = guide_legend(override.aes = list(size = 3/constant)))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

PLOT_3

ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_3.pdf", plot = , width = 15, height = 7, device = "pdf", units = "cm", dpi = 300)

PLOT_3 <- ggplot(data = Prediction_table_final_3) +
  geom_point(aes(x = EIR, y = Time_0.1, color = Number_round, shape = Age), size = 4 / constant, alpha = 1, stroke = 2/ constant) +
  facet_nested(~ Seasonality, labeller = labeller(Seasonality = S.labs)) +
  theme_bw() +
  ylim(0, 150) +
  xlim(0, 150) +
  labs(title = "", x = "EIR (inoculations per person per year)", y = "T50 (years)") +
  theme_bw() +
  scale_color_manual(name = "Number of rounds:",
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("Usual deployment", "Usual deployment + 1 after", "Usual deployment + 1 before"),
                     breaks = c("4", "4.5", "5.4")) +
  scale_shape_manual(values = c(0, 17),
                     name = "Targeted age group:",
                     breaks = c("5", "10"),
                     labels = c("Under 5 years", "Under 10 years")) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5,face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(1, "cm")) +
  theme(plot.margin = unit(c(0, 0.05, 0.05, 0), "cm")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text( size = 18 / constant, color = "black", face = "bold")) + 
  guides(fill = FALSE) + 
  #theme(legend.position = "top", legend.box = "vertical") + 
  theme(legend.spacing.x = unit(0.25, "cm"),
        legend.spacing.y = unit(0.25, "cm"), # legend.spacing.y = unit(-0.25, "cm"),
        legend.margin = margin(0, 0, 0, 0)) + 
  guides(colour = guide_legend(override.aes = list(size = 3)))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
PLOT_3


