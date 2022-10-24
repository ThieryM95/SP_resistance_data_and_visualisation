###############################################################################
# Code to visualize figure S1.1 (seasonality pattern)                         #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load data
Output_data <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_S1.1.csv", header = T)
Output_data_2 <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_2_figure_S1.1.csv", header = T)

# Estimate number of survey per year
Number_survey_years <- 365 / 5

# Estimate when MDA is deployed
time_MDA <- Output_data_2$Survey[Output_data_2$MDA >= 1] / Number_survey_years
constant<-2.5
# Plot
PLOT_AS1 <- ggplot(Output_data_2) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = EIR), size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "EIR \n(inoculations per person per month)", lim = c(0, 4), expand = c(0, 0)) +
  scale_x_continuous(name = "Time",
                     lim = c(1 / 12, 13 / 12),
                     breaks = c(1 / 12, 2 / 12, 3 / 12, 4 / 12, 5 / 12, 6 / 12, 7 / 12, 8 / 12, 9 / 12, 10 / 12, 11 / 12, 12 / 12, 13 / 12),
                     labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.", "Jan."),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = time_MDA - 30, linetype = "dashed", color = "#2C85B2", size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = time_MDA[1] - (6 / Number_survey_years) - 30, linetype = "dashed", color = "#FF50FF", size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = time_MDA[3] + (6 / Number_survey_years) - 30, linetype = "dashed", color = "#FF50FF", size = 3 / ggplot2::.pt / constant)


# Plot
time_MDA <- Output_data$Survey[Output_data$MDA >= 1] / Number_survey_years
PLOT_BS1 <- ggplot(Output_data) +
  geom_line(aes(x = Survey / Number_survey_years - 30, y = EIR), size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "EIR \n(inoculations per person per month)", lim = c(0, 4), expand = c(0, 0)) +
  scale_x_continuous(name = "Time",
                     lim = c(1 / 12, 13 / 12),
                     breaks = c(1 / 12, 2 / 12, 3 / 12, 4 / 12, 5 / 12, 6 / 12, 7 / 12, 8 / 12,  9 / 12, 10 / 12, 11 / 12, 12 / 12, 13 / 12),
                     labels = c("Jan.", "Feb.", "Mar.", "Apr.",  "May", "Jun.", "Jul.", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.", "Jan."),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0) +
  geom_vline( xintercept = time_MDA - 30, linetype = "dashed", color = "#2C85B2", size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = time_MDA[1] - (6 / Number_survey_years) - 30, linetype = "dashed", color = "#FF50FF", size = 3 / ggplot2::.pt / constant) +
  geom_vline(xintercept = time_MDA[4] + (6 / Number_survey_years) - 30, linetype = "dashed", color = "#FF50FF", size = 3 / ggplot2::.pt / constant)

# Merge the plot
PLOT_S1 <- plot_grid(PLOT_AS1, PLOT_BS1, ncol = 1, nrow = 2, scale = 1, labels = c("A", "B"), label_size = 18 / constant, label_fontface = 2)

# Save
ggsave("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S1.1.pdf", plot = PLOT_S1, width = 10, height = 10, device = "pdf", units = "cm", dpi = 300)
