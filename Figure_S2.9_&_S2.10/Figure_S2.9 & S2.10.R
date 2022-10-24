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
Prediction_table_final_2 <- read.csv("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Data_figure_3_&_S2.9&_S2.10.csv", header = T)

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
Prediction_table_final_3 <- Prediction_table_final_2
constant <- 2.5
PLOT_3 <- ggplot(data = Prediction_table_final_3) +
  geom_point(aes(x = EIR, y = Time_0.1, color = Number_round, shape = Age), size = 3 / constant, alpha = 1) +
  facet_nested(Seasonality + Access ~ Coverage + Coverage_reduction, labeller = labeller(Seasonality = S.labs, Coverage = Cov.labs, Coverage_reduction = C.labs, Access = T.labs)) +
  theme_bw() +
  ylim(0, 150) +
  xlim(0, 150) +
  labs(title = "", x = "EIR (inoculations per person per year)", y = "Time needed for the quintuple genotype to spread from a frequency in inoculations of 1% to 50% (years)") +
  theme_bw() +
  scale_color_manual(name = "Number of rounds:",
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("Usual deployment", "Usual deployment + 1 after", "Usual deployment + 1 before"),
                     breaks = c("4", "4.5", "5.4")) +
  scale_shape_manual(values = c(15, 17),
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
  theme(legend.position = "top", legend.box = "vertical") + 
  theme(legend.spacing.x = unit(0.25, "cm"),
        legend.spacing.y = unit(-0.25, "cm"),
        legend.margin = margin(0, 0, 0, 0)) + 
  guides(colour = guide_legend(override.aes = list(size = 2)))+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

# Save
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S2.9.pdf", plot = PLOT_3, width = 18, height = 19, device = "pdf", units = "cm", dpi = 300)

# Figure with prevalence Figure S2.8
####################################

# Select the data
Prediction_table_final_3 <- Prediction_table_final_2[Prediction_table_final_2$Access == 0.4, ]

# Define label values
Label_xx <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)
break_x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)

# Plot
constant <- 2.5
PLOT_1 <- ggplot(data = Prediction_table_final_3) +
  geom_point(aes(x = Prevalance * 100, y = Time_0.1, color = Number_round, shape = Age), size = 3 / constant, alpha = 1) +
  facet_nested(Seasonality ~ Coverage + Coverage_reduction, labeller = labeller(Seasonality = S.labs, Coverage = Cov.labs, Coverage_reduction = C.labs, Access = T.labs)) +
  theme_bw() +
  ylim(0, 100) +
  labs(title = "", y = "T50 (years)") +
  theme_bw() +
  scale_x_continuous(name = "Mean annual parasite prevalence in 2 to 10 years old (%)", breaks = break_x, labels = Label_xx) +
  scale_color_manual(name = "Number of rounds:",
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("Usual deployment", "Usual deployment + 1 after", "Usual deployment + 1 before"),
                     breaks = c("4", "4.5", "5.4")) +
  scale_shape_manual(values = c(15, 17),
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
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"), strip.text.y = element_text(size = 18 / constant, color = "black",face = "bold")) + 
  guides(fill = FALSE) + 
  theme(legend.position = "top", legend.box = "vertical", legend.margin = margin(0, 0, 0, 0)) + 
  theme(legend.spacing.x = unit(0.5, "cm"), legend.spacing.y = unit(-0.25, "cm")) + 
  guides(colour = guide_legend(override.aes = list(size = 2)))

# Extract the legend
legend_PLOT_1 <- get_legend(PLOT_1)

# Plot
PLOT_1 <- ggplot(data = Prediction_table_final_3) +
  geom_point(aes(x = Prevalance * 100, y = Time_0.1, color = Number_round, shape = Age), size = 3 / constant, alpha = 1) +
  facet_nested(Seasonality ~ Coverage + Coverage_reduction, labeller = labeller(Seasonality = S.labs, Coverage = Cov.labs, Coverage_reduction = C.labs, Access = T.labs)) +
  theme_bw() +
  ylim(0, 100) +
  labs(title = "", y = "T50 (years)") +
  theme_bw() +
  scale_x_continuous(name = "Mean annual parasite prevalence in 2 to 10 years old (%)",
                     breaks = break_x,
                     labels = Label_xx) +
  scale_color_manual(name = "Number of rounds:", 
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("Usual deployment", "Usual deployment + 1 after", "Usual deployment + 1 before"),
                     breaks = c("4", "4.5", "5.4")) +
  scale_shape_manual(values = c(15, 17), name = "Targeted age group:", breaks = c("5", "10"), labels = c("Under 5 years", "Under 10 years")) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.9 / constant, "cm")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) + 
  guides(fill = FALSE) + 
  theme(legend.position = "top",
        legend.box = "vertical",
        legend.margin = margin(0, 0, 0, 0)) + 
  theme(legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(-0.25, "cm")) + 
  guides(colour = guide_legend(override.aes = list(size = 2))) +
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

# Select the data
Prediction_table_final_3 <- Prediction_table_final_2[Prediction_table_final_2$Access == 0.1, ]

# Plot
PLOT_2 <- ggplot(data = Prediction_table_final_3) +
  geom_point(aes(x = Prevalance * 100, y = Time_0.1, color = Number_round, shape = Age), size = 3 / constant, alpha = 1) +
  facet_nested(Seasonality ~ Coverage + Coverage_reduction, labeller = labeller(Seasonality = S.labs, Coverage = Cov.labs,  Coverage_reduction = C.labs, Access = T.labs)) +
  theme_bw() +
  ylim(0, 100) +
  labs(title = "", y = "T50 (years)") +
  theme_bw() +
  scale_x_continuous(name = "Mean annual parasite prevalence in 2 to 10 years old (%)", breaks = break_x, labels = Label_xx) +
  scale_color_manual(name = "Number of rounds:",
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("Usual deployment", "Usual deployment + 1 after", "Usual deployment + 1 before"),
                     breaks = c("4", "4.5", "5.4")) +
  scale_shape_manual(values = c(15, 17),
                     name = "Targeted age group:",
                     breaks = c("5", "10"),
                     labels = c("Under 5 years", "Under 10 years")) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(  size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  guides(fill = FALSE) +
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))

# Merge the Plots
PLOT_S6 <- plot_grid( legend_PLOT_1, PLOT_1, PLOT_2, ncol = 1, nrow = 3, rel_heights  = c(0.2, 1, 1), scale = 1, labels = c("", "A", "B"), label_size = 18 / constant, label_fontface = 2)

# Save
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S2.10.pdf", plot = PLOT_S6, width = 18, height = 22, device = "pdf", units = "cm", dpi = 300)
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S2.10A.pdf", plot = PLOT_1, width = 18, height = 10, device = "pdf", units = "cm", dpi = 300)
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S2.10B.pdf", plot = PLOT_2, width = 18, height = 10, device = "pdf", units = "cm", dpi = 300)
ggsave("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Figure_S2.10Legend.pdf", plot = legend_PLOT_1, width = 18, height = 2, device = "pdf", units = "cm", dpi = 300)
