################################################################################
# Code to visuals Figure 1C (Example of the number of patent infections in     #
# SMC children caused by the quadruple (orange line) or quintuple (purple line)# 
# mutants                                                                      #
#                                                                              #
#                                                                              #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                        #
################################################################################

# Load the data
Output_data_4 <-read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_1B&_S2.2_&_S2.3.csv", header = T)

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Define the Number of survey per year
Number_survey_years <- 365 / 5

# Estimate when SMC was deployed
time_MDA <- Output_data_4$Survey[Output_data_4$nMDAs_2 >= 1] / Number_survey_years

# Plot
PLOT_C1 <- ggplot(Output_data_4) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_S2), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_R2), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
  scale_y_continuous(name = "Number of patent infections\nin children by genotype", lim = c(0, 5000), expand = c(0, 0)) +
  scale_x_continuous(name = "Time (days)", 
                     lim = c(0.5, 1.1), 
                     breaks = c(5 / 12, 6 / 12, 7 / 12, 8 / 12, 9 / 12, 10 / 12, 11 / 12, 12 / 12, 13 / 12), 
                     labels = c("", "Jun.", "Jul.", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.", "Jan."), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) + 
  theme(plot.margin = unit(c(0.75, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(
    xintercept = time_MDA - 30,
    linetype = "dashed",
    color = "grey",
    size = 3 / ggplot2::.pt / constant)

# Merge plot A, B and C together
PLOT1 <- plot_grid(PLOT_A1, PLOT_C1, ncol = 2, nrow = 1, scale = 1, labels = c("A", "B"), label_size = 18 / constant, label_fontface = 2)

# Save Figure 1
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_1.pdf", plot = PLOT1, width = 10, height = 5, device = "pdf", units = "cm", dpi = 300)

ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_1A.pdf", plot = PLOT_A1, width = 5, height = 5, device = "pdf", units = "cm", dpi = 300)
ggsave("C:/Users/massth/Desktop/SMC/Figure_final/potential new/Figure_1B.pdf", plot = PLOT_C1, width = 5, height = 5, device = "pdf", units = "cm", dpi = 300)




