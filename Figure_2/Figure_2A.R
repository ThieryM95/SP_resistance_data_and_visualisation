######################################################################################
# Code to visualize figure S2.5( the first order indices of each factor during the   #
# constrained global sensitivity analysis  of the spread of SP-resistant             #
# parasites)                                                                         #
#                                                                                    #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                              #
######################################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
data <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_S2.5.csv",header = T)

# Transform the constrained variable into factor
data$AGE <- factor(data$AGE, levels = c("5", "10"))
data$Coverage_reduction <- factor(data$Coverage_reduction, levels = c("0", "0.1"))
data$Seasonality <- factor(data$Seasonality, levels = c("sesonality2", "sesonality3"))
data$Number_round <- factor(data$Number_round, levels = c("4", "4.5", "5.4"))

# Create labels for each constrain factor
A.labs <- c("Age < 5 year", "Age < 10 year")
names(A.labs) <- c("5", "10")
C.labs <- c("Coverage reduction\n at each round = 0%", "Coverage reduction\n at each round = 10%")
names(C.labs) <- c("0", "0.1")
N.labs <- c("Number of SMC\nrounds = 4", "Number of SMC\nrounds = 4 + 1 after", "Number of SMC\nrounds = 4 + 1 before")
names(N.labs) <- c("4", "4.5", "5.4")
N_3.labs <- c("Number of SMC\nrounds = 3", "Number of SMC\nrounds = 3 + 1 after", "Number of SMC\nrounds = 3 + 1 before")
names(N_3.labs) <- c("4", "4.5", "5.4")
S.labs <- c("Moderate seasonality setting", "High seasonality setting")
names(S.labs) <- c("sesonality2", "sesonality3")

# Define the break for the y axis
break_y <- c(0, 0.25, 0.5, 0.75, 1)
Label_yy <- c(0, 0.25, 0.5, 0.75, 1)

# Order the level of each factors
data_2 <- data
data_2$Factor <- factor(data_2$Factor, levels = c("Coverage", "Access", "eir"))

# Select all settings with moderate seasonality
data_22 <- data_2[data_2$Seasonality == "sesonality2",]
data_22 <- data_22[data_22$Number_round==4 & data_22$Coverage_reduction==0,]




# Plot
constant <- 2.5
PA <- ggplot(data_22, aes(x = AGE, y = First, fill = Factor)) + 
  geom_col(color = "black", width = 0.6) +
  #facet_nested(Number_round ~ Seasonality * Coverage_reduction, labeller = labeller(Number_round = N.labs, Coverage_reduction = C.labs, Seasonality = S.labs)) +
  scale_fill_manual(values = c("#2C85B2", "#E5B17E","#BFB2FF"), 
                    name = "Factors:", 
                    breaks = c("Coverage", "eir", "Access"), 
                    labels = c("SMC coverage", "EIR", "Access to treatment")) +
  theme(axis.title = element_text(face = "bold")) +
  ylab("First-order indices") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold", hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  ggtitle(label = "") +
  theme(legend.text = element_text(size = 18 / constant)) +
  theme(legend.title = element_text(size = 18 / constant, face = "bold")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  scale_y_continuous(breaks = break_y, labels = Label_yy) +
  scale_x_discrete(breaks = c(5, 10), labels = c("< 5 years", "< 10 years")) +
  theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm")) #+ 
 # theme(legend.position = "top", legend.box = "horizontal", legend.margin = margin(0, 0, 0, 0)) + 
#theme(legend.spacing.x = unit(0.25, "cm"), legend.spacing.y = unit(-0.25, "cm"))


PA2<-plot_grid(PA, NULL, ncol= 2, nrow = 1, scale=c(1,1), rel_widths = c(1, 0.5))
ggsave("C:/Users/massth/Desktop/SMC/CODE_WF_PLOT/SP_resistance_data_and_visualisation/Figure_2/Figure_2A.pdf", plot = PA2, width = 15, height = 7, device = "pdf", units = "cm", dpi = 300)
