###############################################################################
# Code to visualse S2.4  (fit of the GP)                                      #
#                                                                             #
# author: thiery Masserey (thiery.masserey@swisstph.ch)                       #
###############################################################################

# Load package
library("rlang")
library("ggh4x")
library("ggplot2")
library("plyr")

# Load data
Precision_final <- read.csv("C:/Users/massth/Desktop/SMC/Figure_final/Data_figure_S2.4.csv", header = T)

# Define constrained variable as a factor
Precision_final$Age <- factor(Precision_final$Age, levels = c("5", "10"))
Precision_final$Coverage_reduction <- factor(Precision_final$Coverage_reduction, levels = c("0", "0.1"))
Precision_final$seasonality <- factor(Precision_final$seasonality, levels = c("sesonality2", "sesonality3"))
Precision_final$Number_round[Precision_final$Number_round == 4 & Precision_final$seasonality == "sesonality3"] <- 3
Precision_final$Number_round[Precision_final$Number_round == 4.5 & Precision_final$seasonality == "sesonality3"] <- 3.5
Precision_final$Number_round[Precision_final$Number_round == 5.4 & Precision_final$seasonality == "sesonality3"] <- 5.3
Precision_final$Number_round <- factor(Precision_final$Number_round, levels = c("4", "4.5", "5.4", "3", "3.5", "5.3"))

# Create labels for each constrain factor
A.labs <- c("Targeted age: children under 5 years", "Targeted age: children under 10 years")
names(A.labs) <- c("5", "10")
C.labs <- c("Coverage reduction\n at each round = 0%", "Coverage reduction\n at each round = 10%")
names(C.labs) <- c("0", "0.1")
N.labs <- c("Number of SMC\nrounds = 4", "Number of SMC\nrounds = 4 + 1 after", "Number of SMC\nrounds = 4 + 1 before", "Number of SMC\nrounds = 3", "Number of SMC\nrounds = 3 + 1 after", "Number of SMC\nrounds = 3 + 1 before")
names(N.labs) <- c("4", "4.5", "5.4", "3", "3.5", "5.3")
S.labs <- c("Moderate seasonality setting", "High seasonality setting")
names(S.labs) <- c("sesonality2", "sesonality3")

# Estimate correlation coefficient and Root mean squared error
cors <- ddply(Precision_final_2, c("Age", "Coverage_reduction", "Number_round", "seasonality"), summarise, cor = round(cor(Test_True, Test_predicted), 3))
RMSEE <-ddply(Precision_final_2, c("Age", "Coverage_reduction", "Number_round", "seasonality"), summarise, cor = round(RMSE(Test_True, Test_predicted), 3)) # See function bellow

# Plot
PLOT <- ggplot(data = Precision_final_2, aes(x = Test_True, y = Test_predicted)) +
  facet_nested(seasonality * Number_round ~ Age * Coverage_reduction , labeller = labeller(seasonality = S.labs, Age = A.labs, Number_round = N.labs, Coverage_reduction = C.labs)) +
  geom_point(size = 2.5 / constant) +
  theme_bw() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, size = 2 / constant) +
  geom_text(data = cors, aes(label = paste("Cor=", cor, sep = "")), x = 0.025, y = 0.05, size = 5.5 / constant) +
  geom_text(data = RMSEE, aes(label = paste("RMSE=", cor, sep = "")), x = 0.025, y = 0.055, size = 5.5 / constant) +
  theme(axis.text.x = element_text(size = 16 / constant),
        axis.text.y = element_text(size = 16 / constant),
        axis.title.x = element_text(size = 18 / constant, face = "bold"),
        axis.title.y = element_text(size = 18 / constant, face = "bold"),
        plot.title = element_text(size = 18 / constant, hjust = 0.5, face = "bold")) +
  ylab("Predicted rate of spread") +
  ylim(0.01, 0.06) +
  xlim(0.01, 0.06) +
  xlab("Observed rate of spread") +
  ggtitle("") +
  theme(strip.text.x = element_text(size = 18 / constant, color = "black", face = "bold"),
    strip.text.y = element_text(size = 18 / constant, color = "black", face = "bold"))

ggsave("/scicore/home/penny/masthi00/smc_resistance/Visualise/Figure/Figure_S2.4pdf", plot = PLOT, width = 16, height = 20, device = "pdf", units = "cm", dpi = 300)

#--------------------------------------------------
# Function to estimate the Root mean squared error.
# --------------------------------------------------
RMSE <- function(x, y) {
  
  # Do a liner regression
  model_regression <- lm(y ~ x)
  
  # Estimate the residuals
  model_summary <- summary(model_regression)
  
  # Estimate the Root mean squared error
  RMSE <- sqrt(mean((model_summary$residuals) ^ 2))
  
  # Return RMSE
  return(RMSE)
}