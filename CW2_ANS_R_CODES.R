# Set working disctionary
setwd("D:/Pranav -UK/Applied Statistics - R language/CW-02")

# Read the vinegar datset csv file
vinegar_data <-read.csv("vinegar.csv")

# CW2_Ans_1_a
# Replace the column names from site to Location and pH to Acidity
Vinegar_dataset <-data.frame(vinegar_data)
colnames(Vinegar_dataset)<- c('Location','Acidity')
print(Vinegar_dataset)

#Basic understanding of Data 
head(Vinegar_dataset)

# Display Summary statistics for initial understanding of dataset.
summary(Vinegar_dataset)

# Graphical representation of acidities for different locations
install.packages("ggplot2")
library(ggplot2)
ggplot(data = vinegar_data, 
       aes(x = Vinegar_dataset$Location, 
       y = Vinegar_dataset$Acidity)) + geom_boxplot(fill = "lightblue",
       color = "red")  + labs(title = "Acidity of Balsamic Vinegar in Different Locations",
       x = "Location", y = "Acidity") + theme_minimal()

# CW2_Ans1b
# Perform one-way ANOVA
anova_result <- aov(Vinegar_dataset$Acidity ~ Vinegar_dataset$Location,
                    data = vinegar_data)
# Print ANOVA summary
summary(anova_result)

#CW2_Ans1c
# Check normality of residuals by  Q-Q plot – (Visual representation)
qqnorm(residuals(anova_result), col = "blue", pch = 20)
qqline(residuals(anova_result), col = 2, lty = 2)

# Further Check the normality of residuals by using Shapiro – Wilk test:
shapiro.test(residuals(anova_result))

# Check for homogeneity of variances statistical tests like Levene's test.
# Perform Levene's test and display the result:
install.packages("car")
library(car)
levene_test_result <- leveneTest(Vinegar_dataset$Acidity ~ Vinegar_dataset$Location, 
                                 data = vinegar_data)
summary(levene_test_result)

# Check homogeneity of variances
plot(anova_result, which = 1)  # Residuals vs. Fitted plot

#CW2_Ans1d

# Perform Tukey's HSD test & print Result:
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Perform Levene's test for homogeneity of variances and display the results:
levene_test_result <- leveneTest(Vinegar_dataset$Acidity ~ Vinegar_dataset$Location, 
                                 data = vinegar_data)
print(levene_test_result)

#CW2_Ans1e

# Store the p-values from Tukey's HSD test

p_values <- tukey_result$`Vinegar_dataset$Location`[, "p adj"]

# Holm Correction
holm_correction <- p.adjust(p_values, method = "holm")
print(holm_correction)

# Bonferroni Correction
bonferroni_correction <- p.adjust(p_values, method = "bonferroni")
print(bonferroni_correction)

# Combine the results into a data frame for comparison
correction_results <- data.frame(
  Original_P_Value = p_values,
  Holm_Correction = holm_correction,
  Bonferroni_Correction = bonferroni_correction)

# Display the correction results
print(correction_results)

#CW2_Ans1f
#conclusion of experiment
# Comment on the results of the Experiment as follows:
#	The experiment, employing ANOVA and Tukey's HSD test, unveiled significant acidity variations across locations, notably with Sydney. 
#	Correcting for multiple tests (Holm and Bonferroni) confirmed Sydney's distinctiveness. 
# Improvement details as Follows:
#	To enhance the study, increasing sample sizes, employing randomization, and accounting for potential confounding variables are suggested. 
#	These measures would contribute to more robust and generalizable conclusions, underlining the importance of statistical corrections in multiple testing scenarios.

# CW2_ANS_2_a
library(gridExtra)
# Read the Datafile venomyield
venom_yield <- read.csv("VenomYield.csv")
#Basic statistical understanding 
summary(venom_yield)

# Scatter plot of venom yield by body length & Display it.
p1 <- ggplot(venom_yield, aes(x=venom_yield$Body.Length..cm.,
                              y=venom_yield$Yield..mg., color=venom_yield$Body.Class, 
                              shape=venom_yield$Expression)) + geom_point() + 
  labs(title="Venom Yield by Body Length", x="Body Length (cm)", y="Yield (mg)")
print(p1)

# Box plot of venom yield by body class and gene expression & Display it.
p2 <- ggplot(venom_yield, aes(x=venom_yield$Body.Class, 
                              y=venom_yield$Yield..mg., fill=venom_yield$Expression)) + 
  geom_boxplot() + labs(title="Venom Yield by Body Class and  Gene Expression")
print(p2)

# Box plot of venom yield by gene expression & Display it
p3 <- ggplot(venom_yield, aes(x=venom_yield$Expression, y=venom_yield$Yield..mg.)) +
  geom_boxplot() +
  labs(title="Venom Yield by Gene Expression")
print(p3)

# Box plot of body length by gene expression & Display it
p4 <- ggplot(venom_yield, aes(x=venom_yield$Expression, y=venom_yield$Body.Length..cm.)) +
  geom_boxplot() +
  labs(title="Body Length by Gene Expression")
print(p4)

# Display all plots in one frame for summary
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# CW2_Ans2b
# Run a two-way ANOVA & Display the summary of the ANOVA results
anova_results <- aov(venom_yield$Yield..mg. ~ venom_yield$Body.Class*venom_yield$Expression, 
                     data=venom_yield)
summary(anova_results)

# post-hoc tests to examine pairwise differences between levels of significant factors.
posthoc_results <- TukeyHSD(anova_results)

# Display the results
print(posthoc_results)

#CW2_Ans_2c
expression_c <- factor(venom_yield$Expression)
contrasts(expression_c) <- contr.sum # set Contrast
Body_length_covariate <- venom_yield$Body.Length..cm.- mean(venom_yield$Body.Length..cm.)
ancova_model_test <- lm(venom_yield$Yield..mg. ~ Body_length_covariate + expression_c ,data = venom_yield)
anova(ancova_model_test)

# Display the Summary of ANCOVA model
summary(ancova_model_test)

# CW2_Ans_2d
#CW2_Ans_2e

# CW2_ANS_3a
# Define the hazard function
hazard_function <- function(t) {return(1 - exp(-t))}

# Survival function
survival_function <- function(t) { return(exp(-integrate(hazard_function, lower = 0, upper = t)$value))}

# Failure probability density function
failure_density <- function(t) { return(hazard_function(t) * survival_function(t))}

# Create a sequence of time points
time_points <- seq(0, 10, by = 0.1)

# Calculate S(t) and f(t) for the time points
survival_values <- sapply(time_points, survival_function)
failure_density_values <- sapply(time_points, failure_density)

# Plotting the survival function S(t)
plot(time_points, survival_values, type = 'l', xlab = 'Time (years)', ylab = 'Survival Probability',
     main = 'Survival Function S(t)')

# Plotting the failure probability density function f(t)
plot(time_points, failure_density_values, type = 'l', xlab = 'Time (years)', ylab = 'Failure Density',
     main = 'Failure Probability Density Function f(t)')

# CW2_ANS_3b
# Avialable Information and Data
time_intervals <- c(0:10) 
no_of_units <- 100
censoring_events <- c(0, 0, 2, 0, 0, 5, 3, 4, 2, 1)
failure_unit <- c(1, 0, 3, 4, 11, 8, 8, 15, 17, 10)

# Generating the life table
install.packages('KMsurv')
library(KMsurv)
life_table <- lifetab(time_intervals, no_of_units,censoring_events,failure_unit)

print(life_table)
# Extracting S, f, and h
Survival <- life_table[, 5]
failure <- life_table[, 6]
hazard <- life_table[, 7]

# Adjusted time intervals
t <- 0.5 + c(0:9)

# Setting up a single plot area to combine all plots
par(mfrow = c(3, 1))  # 3 rows, 1 column

# Plotting all functions in one figure
plot(t, Survival, type = 'l', col = 'black', xlab = 'Time (years)', ylab = 'Survival Probability', main = 'Survival Function S(t)')

plot(t, failure, type = 'l', col = 'purple', xlab = 'Time (years)', ylab = 'Failure Probability Density', main = 'Failure Density Function f(t)')

plot(t, hazard, type = 'l', col = 'orange', xlab = 'Time (years)', ylab = 'Hazard Function', main = 'Hazard Function h(t)')
##
ggplot(life_table, aes(x = t)) +
  geom_step(aes(y = Survival, color = "Survival"), direction = "hv") +
  geom_point(aes(y = hazard, color = "Hazard")) +
  geom_step(aes(y = failure, color = "Failure"), direction = "hv", linetype = "dashed") +
  labs(title = "Graphical presentation of Lifetable and Functions", y = "Function Value", x = "Time") +
  scale_color_manual(values = c("Survival" = "darkgreen", "Hazard" = "darkblue", "Failure" = "brown")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.3),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))



######################
# Lifetimes of industrial air conditioning units
failures <- c(1, 0, 3, 4, 11, 8, 8, 15, 17, 10)

# Censoring events
censoring <- c(0, 0, 2, 0, 0, 5, 3, 4, 2, 1)

# Calculate the lifetable
lifetable <- data.frame(time = 1:10, failures = failures, censoring = censoring)

# Calculate the number of units at risk
lifetable$at_risk <- 100 - cumsum(failures) - cumsum(censoring)

# Calculate the survival function (S(t))
lifetable$S_t <- cumprod((lifetable$at_risk - lifetable$failures - lifetable$censoring) / lifetable$at_risk)

# Calculate the hazard function (h(t))
lifetable$h_t <- lifetable$failures / lifetable$at_risk

# Calculate the failure function (f(t))
lifetable$f_t <- lifetable$h_t * lifetable$S_t

# Print the lifetable
print(lifetable)

# Plot the functions
ggplot(lifetable, aes(x = time)) +
  geom_step(aes(y = S_t, color = "Survival"), direction = "hv") +
  geom_point(aes(y = h_t, color = "Hazard")) +
  geom_step(aes(y = f_t, color = "Failure"), direction = "hv", linetype = "dashed") +
  labs(title = "Lifetable and Functions", y = "Function Value", x = "Time") +
  scale_color_manual(values = c("Survival" = "blue", "Hazard" = "red", "Failure" = "green")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.3),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", size = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))