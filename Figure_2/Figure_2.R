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
library("cowplot")

# Load the data
Quantil_final_final <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_2_&_S2.6.csv",header = T)

# Transform constrained variable into factor
Quantil_final_final$Age <- factor(Quantil_final_final$Age, levels = c("5", "10"))
Quantil_final_final$Coverage_reduction <- factor(Quantil_final_final$Coverage_reduction, levels = c("0", "0.1"))
Quantil_final_final$Seasonality <- factor(Quantil_final_final$Seasonality, levels = c("sesonality2", "sesonality3"))
Quantil_final_final$Number_round <- factor(Quantil_final_final$Number_round, levels = c("4", "4.5", "5.4"))
Quantil_final_final$Factor <- factor(Quantil_final_final$Factor, levels = c("eir", "Coverage", "Access"))



# Create labels for each constrain factor
F.labs <- c("Access to treatment\n (%)", "SMC Coverage\n (%)", "EIR\n(inoculations per person per year)")
names(F.labs) <- c("Access", "Coverage", "eir")


# Define the break for the y axis
break_y <- c(1, 10, 20, 30, 39)
Label_yy <- c("Min", "", "", "", "Max")

# Select all settings with low seasonality
Quantil_final_2 <- Quantil_final_final[Quantil_final_final$Seasonality == "sesonality2",]
Quantil_final_2 <- Quantil_final_2[Quantil_final_2$Number_round==4 & Quantil_final_2$Coverage_reduction==0 & Quantil_final_2$Age==5,]
Quantil_final_2$Range[Quantil_final_2$Factor=="eir"] <- seq(5,500, length=40)
Quantil_final_2$Range[Quantil_final_2$Factor=="Access"] <- seq(10,80, length=40)
Quantil_final_2$Range[Quantil_final_2$Factor=="Coverage"] <- seq(70,100, length=40)


# Plot
constant <- 2.5
P1 <- ggplot(data = Quantil_final_2[Quantil_final_2$Factor=="eir",]) +
  geom_line(aes(x = Range, y = Selection_coefficient, color = Factor), size = 3 / constant, alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = Lower_quartile, ymax = Upper_quartile, fill = Factor), alpha = 0.1) +
  facet_wrap(~Factor, scales = "free_x", nrow = 1, strip.position = "bottom" ,labeller = labeller(Factor = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Rate of spread of the quintuple mutant", lim = c(0, 0.03)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold", hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:", values = c("#2C85B2", "#E5B17E", "#BFB2FF"), labels = c("SMC coverage", "EIR", "Treatment access", "half_life_long"), breaks = c("Coverage", "eir", "Access", "half_life_long")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm"))+
  theme(strip.background = element_blank(),  strip.placement = "outside")

P2 <- ggplot(data = Quantil_final_2[Quantil_final_2$Factor=="Coverage",]) +
  geom_line(aes(x = Range, y = Selection_coefficient, color = Factor), size = 3 / constant, alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = Lower_quartile, ymax = Upper_quartile, fill = Factor), alpha = 0.1) +
  facet_wrap(~Factor, scales = "free_x", nrow = 1, strip.position = "bottom" ,labeller = labeller(Factor = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "", lim = c(0, 0.03)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold", hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:", values = c("#2C85B2", "#E5B17E", "#BFB2FF"), labels = c("SMC coverage", "EIR", "Treatment access", "half_life_long"), breaks = c("Coverage", "eir", "Access", "half_life_long")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm"))+
  theme(strip.background = element_blank(),  strip.placement = "outside")

P3 <- ggplot(data = Quantil_final_2[Quantil_final_2$Factor=="Access",]) +
  geom_line(aes(x = Range, y = Selection_coefficient, color = Factor), size = 3 / constant, alpha = 0.75) +
  geom_ribbon(aes(x = Range, ymin = Lower_quartile, ymax = Upper_quartile, fill = Factor), alpha = 0.1) +
  facet_wrap(~Factor, scales = "free_x", nrow = 1, strip.position = "bottom" ,labeller = labeller(Factor = F.labs))  +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "", lim = c(0, 0.03)) +
  scale_x_continuous(name = "") +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold", hjust = -0.2),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  theme(legend.position = "none") +
  scale_color_manual(name = "Factors:", values = c("#2C85B2", "#E5B17E", "#BFB2FF"), labels = c("SMC coverage", "EIR", "Treatment access", "half_life_long"), breaks = c("Coverage", "eir", "Access", "half_life_long")) +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0.1,  0.1, 0.1, 0.1), "cm"))+
  theme(strip.background = element_blank(),  strip.placement = "outside")

# Combine the plot

PLOT_S1 <- plot_grid(P1, P2, P3 ,ncol= 3, nrow = 1, scale = 1, labels = c("A", "B", "C"), label_size = 18 / constant, label_fontface = 2)


# Save the plot
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_2.pdf", plot = PLOT_S1, width = 16, height = 7, device = "pdf", units = "cm", dpi = 300)
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_2A.pdf", plot = P1, width = 16/3, height = 7, device = "pdf", units = "cm", dpi = 300)
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_2B.pdf", plot = P2, width = 16/3, height = 7, device = "pdf", units = "cm", dpi = 300)
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_2C.pdf", plot = P3, width = 16/3, height = 7, device = "pdf", units = "cm", dpi = 300)



COVERAGE_DATA<-Quantil_final_2[Quantil_final_2$Factor=="eir",]
COVERAGE_DATA_1<-COVERAGE_DATA$Selection_coefficient[COVERAGE_DATA$Range==5]
COVERAGE_DATA_4<-COVERAGE_DATA$Selection_coefficient[COVERAGE_DATA$Range>=50 & COVERAGE_DATA$Range<=60]
mean((COVERAGE_DATA_4[1]-COVERAGE_DATA_1[1])/COVERAGE_DATA_1[1]*100)

# 1
COVERAGE_DATA<-Quantil_final_2[Quantil_final_2$Factor=="Coverage",]
COVERAGE_DATA_1<-COVERAGE_DATA$Selection_coefficient[COVERAGE_DATA$Range==70]
COVERAGE_DATA_4<-COVERAGE_DATA$Selection_coefficient[COVERAGE_DATA$Range==80]
mean((COVERAGE_DATA_4-COVERAGE_DATA_1)/COVERAGE_DATA_1*100)

# 2
COVERAGE_DATA<-Quantil_final_2[Quantil_final_2$Factor=="Access",]
COVERAGE_DATA_1<-COVERAGE_DATA$Selection_coefficient[COVERAGE_DATA$Range==10]
COVERAGE_DATA_4<-COVERAGE_DATA$Selection_coefficient[COVERAGE_DATA$Range>=29 & COVERAGE_DATA$Range<=30]
mean((COVERAGE_DATA_4-COVERAGE_DATA_1)/COVERAGE_DATA_1*100)
