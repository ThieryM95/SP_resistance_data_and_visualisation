##################################################################################
# Code to visualize Figure 2 (the effect of each factor during the global        #
# sensitivity analyses of the spread of parasites resistant to SP)               #
#                                                                                #
# Input: table of the estimated median selection coefficients estimated          #
#        during the global sensitivity analyses over the parameter ranges        #
#                                                                                #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Quantil_final_final <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_2_&_S2.6.csv",header = T)

# Transform constrained variable into factor
Quantil_final_final$Age <- factor(Quantil_final_final$Age, levels = c("5", "10"))
Quantil_final_final$Coverage_reduction <- factor(Quantil_final_final$Coverage_reduction, levels = c("0", "0.1"))
Quantil_final_final$Seasonality <- factor(Quantil_final_final$Seasonality, levels = c("sesonality2", "sesonality3"))
Quantil_final_final$Number_round <- factor(Quantil_final_final$Number_round, levels = c("4", "4.5", "5.4"))

# Create labels for each constrain factor
A.labs <- c("Age < 5 years", "Age < 10 years")
names(A.labs) <- c("5", "10")
C.labs <- c("Coverage reduction\n at each round = 0%", "Coverage reduction\n at each round = 10%")
names(C.labs) <- c("0", "0.1")
N.labs <- c( "Number of SMC\nrounds = 4", "Number of SMC\nrounds = 4 + 1 after", "Number of SMC\nrounds = 4 + 1 before")
names(N.labs) <- c("4", "4.5", "5.4")
N_3.labs <- c("Number of SMC\nrounds = 3", "Number of SMC\nrounds = 3 + 1 after", "Number of SMC\nrounds = 3 + 1 before")
names(N_3.labs) <- c("4", "4.5", "5.4")
S.labs <- c("Moderate seasonality setting", "High seasonality setting")
names(S.labs) <- c("sesonality2", "sesonality3")

# Define the break for the y axis
break_y <- c(1, 10, 20, 30, 39)
Label_yy <- c("Min", "", "", "", "Max")


# Select all settings with low seasonality
Quantil_final_2 <- Quantil_final_final[Quantil_final_final$Seasonality == "sesonality2",]

# Plot
constant <- 2.5
P2 <- ggplot(data = Quantil_final_2) +
  geom_line(aes(x = Range, y = Selection_coefficient, color = Factor, linetype = Age), size = 3 / constant, alpha = 0.75) +
  facet_nested(Number_round ~ Seasonality * Coverage_reduction, labeller = labeller(Number_round = N.labs, Coverage_reduction = C.labs, Seasonality = S.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread of the quintuple mutant", lim = c(0, 0.06)) +
  scale_x_continuous(name = "", breaks = break_y, labels = Label_yy) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold", hjust = -0.2),
    axis.title.y = element_text(size = 18 / constant, face = "bold"),
    plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:", values = c("#2C85B2", "#E5B17E", "#BFB2FF"), labels = c("SMC coverage", "EIR", "Treatment access", "half_life_long"), breaks = c("Coverage", "eir", "Access", "half_life_long")) +
  scale_linetype_manual(values = c("solid", "dotted"), name = "Age", breaks = c("5", "10"), labels = c("5", "10")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0,  0.05, 0.05, 0), "cm"))

# Select all settings with high seasonality
Quantil_final_3 <- Quantil_final_final[Quantil_final_final$Seasonality == "sesonality3",]

# Plot
P3 <- ggplot(data = Quantil_final_3) +
  geom_line(aes(x = Range, y = Selection_coefficient, color = Factor, linetype = Age), size = 3 / constant, alpha = 0.75) +
  facet_nested(Number_round ~ Seasonality * Coverage_reduction, labeller = labeller(Number_round = N_3.labs, Coverage_reduction = C.labs, Seasonality = S.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "", lim = c(0, 0.06)) +
  scale_x_continuous(name = "", breaks = break_y, labels = Label_yy) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold", hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(legend.key.width = unit(1.0, "cm")) +
  scale_color_manual(name = "Factors:",
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("SMC coverage", "EIR", "Treatment access", "half_life_long"),
                     breaks = c("Coverage", "eir", "Access", "half_life_long")) +
  scale_linetype_manual(values = c("solid", "dotted"), name = "Targeted age group:", breaks = c("5", "10"), labels = c("Under 5 years", "Under 10 years")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0, 0.05, 0.05, 0), "cm")) +   theme(legend.position = "none") #+ theme(legend.position="top") + theme(legend.spacing.x = unit(0.5, "cm"))


# Merge all the plot
PLOT <- plot_grid(P2, P3, ncol = 2, nrow = 1, rel_widths = c(1, 1), scale = 1)
PLOT_2 <- ggdraw(add_sub(PLOT, "Factor values", vpadding = grid::unit(0, "lines"), y = 6, x = 0.5, vjust = 5, size = 18 / constant, fontface = "bold"))

# Extract the legend
P3 <- ggplot(data = Quantil_final_3) +
  geom_line(aes(x = Range, y = Selection_coefficient, color = Factor, linetype = Age), size = 3 / constant, alpha = 0.75) +
  facet_nested(Number_round ~ Seasonality * Coverage_reduction, labeller = labeller(Number_round = N_3.labs, Coverage_reduction = C.labs, Seasonality = S.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "", lim = c(0, 0.06)) +
  scale_x_continuous(name = "", breaks = break_y, labels = Label_yy) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold", hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5,face = "bold")) +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(legend.key.width = unit(1.0, "cm")) +
  scale_color_manual(name = "Factors:",
                     values = c("#2C85B2", "#E5B17E", "#BFB2FF"),
                     labels = c("SMC coverage", "EIR", "Access to treatment", "half_life_long"),
                     breaks = c("Coverage", "eir", "Access", "half_life_long")) +
  scale_linetype_manual(values = c("solid", "dotted"),
                        name = "Targeted age group:",
                        breaks = c("5", "10"),
                        labels = c("Under 5 years", "Under 10 years")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0, 0.05, 0.05, 0), "cm")) + 
  theme(legend.position = "top", legend.box = "vertical", legend.margin = margin(0, 0, 0, 0)) + 
  theme(legend.spacing.x = unit(0.25, "cm"), legend.spacing.y = unit(-0.25, "cm"))


legend_P3 <- get_legend(P3)


# Merge all the plot with the legend
PLOT_3 <-plot_grid(legend_P3, PLOT_2, ncol = 1, nrow = 2, rel_heights = c(0.25, 3))

# Save the plot
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/Figure_S2.6.pdf", plot = PLOT_3, width = 16, height = 12, device = "pdf", units = "cm", dpi = 300)
