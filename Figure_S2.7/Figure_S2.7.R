###############################################################################
# Code to visuals Figure S2.6 (the impact of access to first-line treatment   #
# on the number of patent infections in SMC children and adults)              # 
#                                                                             #
# Input: Data of drug concentration of SP* across time                        #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load the data
Output_data_3 <- read.csv("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Data_figure_S2.7.csv", header = T)
Output_data_4 <- read.csv("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Data_2_figure_S2.7.csv", header = T)

# Plot
PLOT_S4 <- ggplot() + geom_line(data = Output_data_4, aes(x = Survey / Number_survey_years - 31, y = nPatent_2), size = 3 / ggplot2::.pt / constant, color = "#2C85B2") +
  geom_line( data = Output_data_4, aes(x = Survey / Number_survey_years - 31, y = nPatent_3 + nPatent_4), size = 3 / ggplot2::.pt / constant, color = "#FF50FF") +
  geom_line(data = Output_data_3, aes(x = Survey / Number_survey_years - 31, y = nPatent_2), size = 3 / ggplot2::.pt / constant, color = "#2C85B2", linetype = "dashed") +
  geom_line(data = Output_data_3, aes(x = Survey / Number_survey_years - 31, y = nPatent_3 + nPatent_4), size = 3 / ggplot2::.pt / constant, color = "#FF50FF", linetype = "dashed") +
  scale_y_continuous(name = "Number of patent infections",  lim = c(0, 60000), expand = c(0, 0)) + 
  scale_x_continuous(name = "Time",
                     lim = c(0.5, 1.1),
                     breaks = c(1 / 12, 2 / 12, 3 / 12, 4 / 12, 5 / 12, 6 / 12, 7 / 12, 8 / 12, 9 / 12, 10 / 12, 11 / 12, 12 / 12, 13 / 12),
                     labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.", "Jan."), 
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size =  16 / constant),
        axis.text.y = element_text(size =  16 / constant),
        axis.title.x = element_text(size =  18 / constant, face = "bold"),
        axis.title.y = element_text(size =  18 / constant, face = "bold")) +
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm")) +
  theme(legend.position = "none") +
  expand_limits(x = 0, y = 0)

# Save
ggsave("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S2.6.pdf", plot = PLOT_S4, width = 7, height = 7, device = "pdf", units = "cm", dpi = 300)

