##################################################################################
# Code to visuals Figure 5 (Relation between the protective effectiveness of SMC #
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
Scenario_liste<-read.csv(file = "C:/Users/massth/Desktop/SMC/CODE_WF_PLOT/SP_resistance_data_and_visualisation/Figure_5/Data_Figure_S2.14.csv",  header=T)


# Transform variable in factor
Scenario_liste$eir<-as.factor(Scenario_liste$eir)
Scenario_liste$Access<-as.factor(Scenario_liste$Access)


# Define different level of coverage
C.labs <- c("Coverage = 75%", "Coverage = 80%", "Coverage = 85%", "Coverage = 90%", "Coverage = 95%","Coverage = 100%")
names(C.labs) <- c("0.75", "0.8", "0.85", "0.9", "0.95","1")

Scenario_liste<-Scenario_liste[Scenario_liste$IC50_SP_R!=0,]

# Plot
constant<-2.5
pd <- position_dodge(0.5)
PLOT<-ggplot(data = Scenario_liste) +
  geom_boxplot (aes(x = as.factor(IC50_SP_R), y = time_0.1, color = eir), size = 1.5/constant,position = pd, alpha=0.5, outlier.size = 1/constant) +
  facet_wrap( ~ Coverage, labeller = labeller(Coverage = C.labs), scales='free') +
  theme_bw() +
  ggtitle("") +
  scale_y_continuous(name = "Time needed for the most resistant genotype to spread\n from a frequency in inoculations of 1% to 50% (years)", lim = c(0, 200)) +
  scale_x_discrete(name = "Prophylactic period of quintuple mutant (days)") +
  scale_colour_manual(
    values = c("#de7065ff", "#a65c85ff", "#403891ff", "#0c2a50ff"),
    name = "EIR:",
    breaks = c("25", "50", "100", "150"),
    labels = c("25", "50", "100", "150")) +
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

PLOT

# Save
ggsave("C:/Users/massth/Desktop/SMC/CODE_WF_PLOT/SP_resistance_data_and_visualisation/Figure_5/Figure_S2.14.pdf",
       plot = PLOT, width = 14, height = 10, device="pdf", units = "cm", dpi = 300)
