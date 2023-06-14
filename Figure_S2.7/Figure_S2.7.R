###############################################################################
# Code to visuals Figure S2.6 (the impact of access to first-line treatment   #
# on the number of patent infections in SMC children and adults)              # 
#                                                                             #
# Input: Data of drug concentration of SP* across time                        #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################


# Load the data
Output_data <- read.csv("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Data_figure_S2.7.csv", header = T)


# Define number of survey per year
Number_survey_years<-73

# Define label for coverage
C.labs <- c("Coverage = 75%", "Coverage = 80%", "Coverage = 85%", "Coverage = 90%", "Coverage = 95%","Coverage = 100%")
names(C.labs) <- c("0.75", "0.8", "0.85", "0.9", "0.95","1")

# Define lable for populaiton targeted
T.labs <- c("SMC targets children under 5 years", "SMC targets children target under 10 years")
names(T.labs) <- c("5", "10")


# Plot
PLOT_S4<-ggplot()+
  geom_line(data=Output_data[Output_data$Age==5,],aes(x=Survey/Number_survey_years-31, y=nPatent_2, linetype=Access), size=3/ggplot2::.pt/constant, color=SteppedSequential5Steps[17])+
  geom_line(data=Output_data[Output_data$Age==5,],aes(x=Survey/Number_survey_years-31, y=nPatent_3+nPatent_4, linetype=Access),size=3/ggplot2::.pt/constant, color=Green2Magenta16Steps[12])+
  facet_grid( Age~ Coverage, labeller = labeller(Age =T.labs, Coverage = C.labs)) +
  scale_y_continuous(name = "Number of patent infections", lim=c(0,60000),expand = c(0, 0)) +
  scale_x_continuous(name = "Time", lim=c(0.5,1.1), breaks = c(1/12,2/12,3/12,4/12,5/12,6/12,7/12,8/12,9/12,10/12,11/12,12/12, 13/12), labels=c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sept.","Oct.","Nov.","Dec.","Jan."),expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size =  16/constant),
        axis.text.y = element_text(size =  16/constant),
        axis.title.x = element_text(size =  18/constant, face = "bold"),
        axis.title.y = element_text(size =  18/constant, face = "bold"))+
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text( size = 18 / constant, color = "black", face = "bold")) + 
  expand_limits(x = 0, y = 0)

PLOT_S4B<-ggplot()+
  geom_line(data=Output_data[Output_data$Age==10],aes(x=Survey/Number_survey_years-31, y=nPatent_2+nPatent_3, linetype=Access), size=3/ggplot2::.pt/constant, color=SteppedSequential5Steps[17])+
  geom_line(data=Output_data[Output_data$Age==10],aes(x=Survey/Number_survey_years-31, y=nPatent_4, linetype=Access),size=3/ggplot2::.pt/constant, color=Green2Magenta16Steps[12])+
  facet_grid( Age~ Coverage, labeller = labeller(Age =T.labs, Coverage = C.labs)) +
  scale_y_continuous(name = "Number of patent infections", lim=c(0,60000),expand = c(0, 0)) +
  scale_x_continuous(name = "Time", lim=c(0.5,1.1), breaks = c(1/12,2/12,3/12,4/12,5/12,6/12,7/12,8/12,9/12,10/12,11/12,12/12, 13/12), labels=c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sept.","Oct.","Nov.","Dec.","Jan."),expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size =  16/constant),
        axis.text.y = element_text(size =  16/constant),
        axis.title.x = element_text(size =  18/constant, face = "bold"),
        axis.title.y = element_text(size =  18/constant, face = "bold"))+
  theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"))+
  theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
        strip.text.y = element_text( size = 18 / constant, color = "black", face = "bold")) + 
  expand_limits(x = 0, y = 0)

PLOT1 <- plot_grid(PLOT_S4, PLOT_S4B, ncol = 1, nrow = 2, scale = 1)


# Save Figure 1
ggsave("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S2.6.pdf", plot = PLOT1, width = 15, height = 15, device = "pdf", units = "cm", dpi = 300)
