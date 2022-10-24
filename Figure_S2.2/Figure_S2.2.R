###############################################################################
# Code to visuals Figure S2.2 (frequency of each genotype in inoculation)     #
#                                                                             #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Plot
Output_data_4 <- read.csv("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Data_figure_1C&_S2.2_&_S2.3.csv", header = T)

# Define the number of survey per year
Number_survey_years <- 365 / 5

# Plot
PLOT_S10 <- ggplot(Output_data_4) +
        geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_S4 / (innoculationsPerAgeGroup_S4 + innoculationsPerAgeGroup_R4)), size = 3 / ggplot2::.pt / constant, color = "#860086") +
        geom_line(aes(x = Survey / Number_survey_years - 30, y = innoculationsPerAgeGroup_R4 / (innoculationsPerAgeGroup_S4 + innoculationsPerAgeGroup_R4)), size = 3 / ggplot2::.pt / constant, color = "#FF7F00") +
        scale_y_continuous(name = "The frequency of\neach  genotype in inoculations", lim = c(0, 1), expand = c(0, 0)) +
        scale_x_continuous(name = "Time (years)", lim = c(0, 6), expand = c(0, 0)) +
        theme_bw() +
        theme(axis.text.x = element_text(size =  16 / constant),
              axis.text.y = element_text(size =  16 / constant),
              axis.title.x = element_text(size =  18 / constant, face = "bold"),
              axis.title.y = element_text(size =  18 / constant, face = "bold")) +
        theme(plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm")) +
        theme(legend.position = "none") +
        expand_limits(x = 0, y = 0) +
        geom_vline(xintercept = time_MDA - 30,
                   linetype = "dashed",
                   color = "grey",
                   size = 3 / ggplot2::.pt / constant)

# Save
ggsave("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S2.2.pdf", plot = PLOT_S10, width = 7, height = 7, device = "pdf", units = "cm", dpi = 300)


