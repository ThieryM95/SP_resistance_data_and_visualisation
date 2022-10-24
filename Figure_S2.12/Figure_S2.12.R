###############################################################################
# Code to visuals Figure S2.10 (An illustration of the estimation of the SMC  #
# protective effectiveness)                                                   #
#                                                                             #
#                                                                             #
# author: Thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Output_data <- read.csv("C:\\Users\\massth\\Desktop\\SMC\\Figure_final\\Data_figure_S2.12.csv", header = T)

# Define time steps
time_step <- 5

# Define number of survey per year
Number_survey_years <- 365 / time_step

# Estimate when MDA was deployed
Time_MDA <- Output_data$Survey[Output_data$MDAs_2 >= 1]

#Plot
constant <- 2.5
PLOT_10 <- ggplot(data = Output_data, aes(x = Survey / Number_survey_years - 29, y = Uncomp_2 / Host_2 * 1000)) +
  geom_line(size = 3 / ggplot2::.pt / constant) +
  scale_y_continuous(name = "Number of clinical cases\n (per 1000 children under five years of age)", lim = c(0, 100), expand = c(0, 0)) +
  scale_x_continuous(name = "Time", 
                     lim = c(1 / 12, 25 / 12),
                     expand = c(0, 0), breaks = (1:25) / 12,
                     labels = c("Jan.", "", "Mar.", "", "May", "", "Jul.", "", "Sept.", "", "Nov.", "", "Jan.", "", "Mar.", "", "May", "", "Jul.", "", "Sept.", "", "Nov.", "", "Jan.")) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.75, 0.5, 0.2, 0.1), "cm")) +
  annotate("rect", xmin = 21, xmax = 35, ymin = -4, ymax = 84, alpha = .5, fill = "grey") +
  annotate("rect", xmin = 0, xmax = 17, ymin = -4, ymax = 84, alpha = .2, fill = "yellow") +
  geom_line(size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = Time_MDA[1] / Number_survey_years - 30, y = -Inf, xend = Time_MDA[1] / Number_survey_years - 30, yend = Inf), linetype = "dashed", color = "#FF50FF", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = (Time_MDA[1] + 30 * 4 / time_step) / Number_survey_years - 30, y = -Inf, xend = (Time_MDA[1] + 30 * 4 / time_step) / Number_survey_years - 30, yend = Inf), linetype = "dashed", color = "#FF50FF", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = Time_MDA[1] / Number_survey_years - 29, y = -Inf, xend = Time_MDA[1] / Number_survey_years - 29, yend = Inf), linetype = "dashed", color = "#1F78B4" , size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = (Time_MDA[1] + 30 * 4 / time_step) / Number_survey_years - 29, y = -Inf, xend = (Time_MDA[1] + 30 * 4 / time_step) / Number_survey_years - 29, yend = Inf), linetype = "dashed", color = "#1F78B4", size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = (Time_MDA[5] / Number_survey_years - 31), y = 90 , xend = ((Time_MDA[5] + 30 * 4 / time_step) / Number_survey_years - 31), yend = 90), arrow = arrow(length = unit(0.5 / constant, "cm"), ends = 'both'), color = "#FF50FF" , size = 3 / ggplot2::.pt / constant) +
  geom_segment(aes(x = (Time_MDA[5] / Number_survey_years - 30), y = 90 , xend = ((Time_MDA[5] + 30 * 4 / time_step) / Number_survey_years - 30), yend = 90), arrow = arrow(length = unit(0.5 / constant, "cm"), ends = 'both'), color = "#1F78B4", size = 3 / ggplot2::.pt / constant)

# Save
ggsave("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S2.12.pdf", plot = PLOT_10, width = 11, height = 6, device = "pdf", units = "cm", dpi = 300)
