###############################################################################
# Code to visuals Figure S2.4 (Number of patent infection and inoculations    #
# carrying each genotype)                                                     #
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
Output_data_4 <- read.csv("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Data_figure_1B&_S2.2_&_S2.3.csv", header = T)


# Plot
PLOT_S2A <- ggplot(Output_data_4) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_S2 + nPatentByGenotype_S4 + nPatentByGenotype_S1 + nPatentByGenotype_S3), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_R2 + nPatentByGenotype_R4 + nPatentByGenotype_R1 + nPatentByGenotype_R3), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
  scale_y_continuous(name = "Number of patent\ninfections by genotype", lim = c(0, 30000), expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years) since the first\n year of SMC implementation", lim = c(0.5, 3.5), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = time_MDA - 30, linetype = "dashed", color = "grey", size = 3 / ggplot2::.pt / constant) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_S2 + nPatentByGenotype_S4 + nPatentByGenotype_S1 + nPatentByGenotype_S3), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_R2 + nPatentByGenotype_R4 + nPatentByGenotype_R1 + nPatentByGenotype_R3), size = 3 / ggplot2::.pt / constant, color = "#FF7F00")


PLOT_S2B <- ggplot(Output_data_4) + geom_line(
    aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_S2 + nPatentByGenotype_S3), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_R2 + nPatentByGenotype_R3), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
  scale_y_continuous(name = "Number of patent\ninfections by genotype", lim = c(0, 10000), expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years) since the first\n year of SMC implementation", lim = c(0.5, 3.5), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = time_MDA - 30, linetype = "dashed", color = "grey", size = 3 / ggplot2::.pt / constant) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_S2 + nPatentByGenotype_S3),
    size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_R2 + nPatentByGenotype_R3),
    size = 3 / ggplot2::.pt / constant,
    color = "#FF7F00")

PLOT_S2C <- ggplot(Output_data_4) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_S4), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_R4), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
  scale_y_continuous(name = "Number of patent\ninfections by genotype", lim = c(0, 30000), expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years) since the first\n year of SMC implementation", lim = c(0.5, 3.5), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = time_MDA - 30, color = "grey", linetype = "dashed", size = 3 / ggplot2::.pt / constant) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_S4), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = nPatentByGenotype_R4), size = 3 / ggplot2::.pt / constant, color = "#FF7F00")

PLOT_S2D <- ggplot(Output_data_4) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_S2 + innoculationsPerAgeGroup_S3 + innoculationsPerAgeGroup_S1 + innoculationsPerAgeGroup_S4), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_R2 + innoculationsPerAgeGroup_R3 + innoculationsPerAgeGroup_R1 + innoculationsPerAgeGroup_R4), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
  scale_y_continuous(name = "Number of inoculations\ncarrying each genotype", lim = c(0, 1000000), expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years) since the first\n year of SMC implementation", lim = c(0.5, 3.5), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = time_MDA - 30, linetype = "dashed", color = "grey", size = 3 / ggplot2::.pt / constant) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_S2 + innoculationsPerAgeGroup_S3 + innoculationsPerAgeGroup_S1 + innoculationsPerAgeGroup_S4), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_R2 + innoculationsPerAgeGroup_R3 + innoculationsPerAgeGroup_R1 + innoculationsPerAgeGroup_R4), size = 3 / ggplot2::.pt / constant, color = "#FF7F00")



PLOT_S2E <- ggplot(Output_data_4) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_S2 + innoculationsPerAgeGroup_S3), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_R2 + innoculationsPerAgeGroup_R3), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
  scale_y_continuous(name = "Number of inoculations\ncarrying each genotype", lim = c(0, 150000), expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years) since the first\n year of SMC implementation", lim = c(0.5, 3.5), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = time_MDA - 30, linetype = "dashed", color = "grey", size = 3 / ggplot2::.pt / constant) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_S2 + innoculationsPerAgeGroup_S3), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_R2 + innoculationsPerAgeGroup_R3), size = 3 / ggplot2::.pt / constant, color = "#FF7F00")


PLOT_S2F <- ggplot(Output_data_4) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_S4), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_R4), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
  scale_y_continuous(name = "Number of inoculations\ncarrying each genotype", lim = c(0, 1000000), expand = c(0, 0)) +
  scale_x_continuous(name = "Time (years) since the first\n year of SMC implementation", lim = c(0.5, 3.5), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = time_MDA - 30, linetype = "dashed", color = "grey", size = 3 / ggplot2::.pt / constant) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_S4), size = 3 / ggplot2::.pt / constant, color = "#860086") +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_R4), size = 3 / ggplot2::.pt / constant, color = "#FF7F00")

# Merge all plot
PLOTS2 <- plot_grid(PLOT_S2A, PLOT_S2B, PLOT_S2C, PLOT_S2D, PLOT_S2E, PLOT_S2F, ncol = 3, nrow = 2, scale = 1, labels = c("A", "B", "C", "D", "E", "F"), label_size = 18 / constant, label_fontface = 2)

# Save
ggsave( "/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S2.3.pdf", plot = PLOTS2, width = 15, height = 10, device = "pdf", units = "cm", dpi = 300)
