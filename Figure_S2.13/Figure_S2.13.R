##################################################################################
# Code to visuals Figure 4 (Relation between the protective effectiveness of SMC #
# and the prophylactic period conferred by SP+AQ                                 #
#                                                                                #
#                                                                                #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                          #
##################################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load data
Scenario_liste_11 <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_4_&_S2.13.csv", header = T)

# Transform variable in factor
Scenario_liste_11$Access<-as.factor(Scenario_liste_11$Access)

# Define different level of coverage
C.labs <- c("Coverage = 75%", "Coverage = 80%", "Coverage = 85%", "Coverage = 90%", "Coverage = 95%","Coverage = 100%")
names(C.labs) <- c("0.75", "0.8", "0.85", "0.9", "0.95","1")

# Plot
constant<-2.5
pd <- position_dodge(0.5)
PLOT<-ggplot(data = Scenario_liste_11) +
  geom_boxplot (aes(x = as.factor(PP), y = PE_1, color = Access), size = 1/constant,position = pd, alpha=0.5) +
  facet_wrap( ~ Coverage, labeller = labeller(Coverage = C.labs)) +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Protective effectivness (%)", lim = c(0, 1)) +
  scale_x_discrete(name = "Prophylactic period (days)") +
  scale_colour_manual(    
    values = c("#de7065ff", "#a65c85ff", "#403891ff", "#0c2a50ff"),
    name = "Level of access to treatment:",
    breaks = c("0.05", "0.1", "0.2", "0.4"),
    labels = c("10%", "25%", "45%", "70%")) +
  theme(axis.text.x = element_text(size = 16/constant),
        axis.text.y = element_text(size = 16/constant),
        axis.title.x = element_text(size = 18/constant, face = "bold"),
        axis.title.y = element_text(size = 18/constant, face = "bold"),
        plot.title = element_text(size = 18/constant, hjust = 0.5, face = "bold")) +
  theme(legend.text = element_text(size = 18/constant)) +
  theme(legend.title = element_text(size = 18/constant, face = "bold")) +
  theme(legend.key.size = unit(0.9, "cm")) +
  theme(legend.position="top", legend.box="vertical")  +
  ggtitle(label = "") +
  theme(strip.text.x = element_text(size = 18/constant, color = "black", face = "bold"),
        strip.text.y = element_text(size = 18/constant, color = "black", face = "bold")) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) #+ theme(legend.position="top") + theme(legend.spacing.x = unit(0.5, "cm"))  

# Save
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/Figure_2.13.pdf",
       plot = PLOT, width = 14, height = 10, device="pdf", units = "cm", dpi = 300)
