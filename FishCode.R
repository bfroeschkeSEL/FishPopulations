#####ENVIRONMENTAL STATISTICS LAB
# COMPARING TWO FISH POPULATION MEANS
#
# OBJECTIVES:
# In this lab you will:
#   1. Explore two fish populations (EDA first!)
#   2. Estimate confidence intervals for each mean
#   3. Estimate confidence intervals using bootstrapping
#   4. Compare the two means using:
#         - Independent two-sample t-test
#         - One-way ANOVA
#
# IMPORTANT STATISTICAL PRINCIPLE:
# We NEVER begin with hypothesis testing.
# We first understand the data.
#
# The order of this lab reflects proper statistical workflow:
#   DATA → EXPLORATION → ESTIMATION → INFERENCE
############################################################

# Sampled Red Drum from two habitats:
#   Population 1: Seagrass meadow
#   Population 2: Artificial reef
#
# Measured total length (mm).
#
# Question:
# Are Red Drum larger on reefs than in seagrass?
############################################################

##data = fish_data

####Part 1############
############################################################
# WHY DO WE START WITH EDA?
#
# Hypothesis tests assume:
#   - Normality (approximately)
#   - Similar variances
#   - No extreme outliers
#
# If we skip exploration, we do blind statistics.
############################################################


############################################################
############################################################
# 2.1 Calculate Basic Descriptive Statistics per Habitat
############################################################

# Mean
mean_seagrass <- mean(fish_data$length[fish_data$habitat == "Seagrass"])
mean_reef     <- mean(fish_data$length[fish_data$habitat == "Reef"])

# Median
median_seagrass <- median(fish_data$length[fish_data$habitat == "Seagrass"])
median_reef     <- median(fish_data$length[fish_data$habitat == "Reef"])

# Standard Deviation
sd_seagrass <- sd(fish_data$length[fish_data$habitat == "Seagrass"])
sd_reef     <- sd(fish_data$length[fish_data$habitat == "Reef"])

# Minimum / Maximum
min_seagrass <- min(fish_data$length[fish_data$habitat == "Seagrass"])
max_seagrass <- max(fish_data$length[fish_data$habitat == "Seagrass"])
min_reef     <- min(fish_data$length[fish_data$habitat == "Reef"])
max_reef     <- max(fish_data$length[fish_data$habitat == "Reef"])

# Quantiles
quantiles_seagrass <- quantile(fish_data$length[fish_data$habitat == "Seagrass"])
quantiles_reef     <- quantile(fish_data$length[fish_data$habitat == "Reef"])

# Print all
mean_seagrass; mean_reef
median_seagrass; median_reef
sd_seagrass; sd_reef
c(min_seagrass, max_seagrass); c(min_reef, max_reef)
quantiles_seagrass; quantiles_reef


#############Part 2 Confidence Intervals###############
############################################################
#  CONFIDENCE INTERVALS (STEP-BY-STEP EXPLANATION)
############################################################

# -------------------------
# SEAGRASS HABITAT
# -------------------------

# Extract lengths for Seagrass habitat
seagrass_lengths <- fish_data$length[fish_data$habitat == "Seagrass"]  

# Count how many fish were sampled
n_seagrass <- length(seagrass_lengths)  

# Calculate the sample mean
mean_seagrass <- mean(seagrass_lengths)  

# Calculate the sample standard deviation
sd_seagrass <- sd(seagrass_lengths)  

# Calculate the margin of error for 95% confidence interval
# qt(0.975, df) gives the t critical value for 95% CI (two-tailed)
error_seagrass <- qt(0.975, df = n_seagrass-1) * sd_seagrass / sqrt(n_seagrass)  

# Calculate the lower and upper bounds of the CI
CI_seagrass <- c(mean_seagrass - error_seagrass, mean_seagrass + error_seagrass)  

# Print the 95% confidence interval
CI_seagrass  


# -------------------------
# REEF HABITAT
# -------------------------

# Extract lengths for Reef habitat
reef_lengths <- fish_data$length[fish_data$habitat == "Reef"]  

# Count how many fish were sampled
n_reef <- length(reef_lengths)  

# Calculate the sample mean
mean_reef <- mean(reef_lengths)  

# Calculate the sample standard deviation
sd_reef <- sd(reef_lengths)  

# Calculate the margin of error for 95% confidence interval
error_reef <- qt(0.975, df = n_reef-1) * sd_reef / sqrt(n_reef)  

# Calculate the lower and upper bounds of the CI
CI_reef <- c(mean_reef - error_reef, mean_reef + error_reef)  

# Print the 95% confidence interval
CI_reef

############################################################
# 5 — VISUALIZE CONFIDENCE INTERVALS USING EXISTING VARIABLES
############################################################

library(ggplot2)

# -------------------------
# 5.1 Prepare a small data frame for plotting
# -------------------------

# Create a data frame using the means and CI values we already calculated
plot_data <- data.frame(
  habitat = c("Seagrass", "Reef"),           # Habitat names
  mean_length = c(mean_seagrass, mean_reef), # Means from CI section
  CI_lower = c(CI_seagrass[1], CI_reef[1]), # Lower CI bounds
  CI_upper = c(CI_seagrass[2], CI_reef[2])  # Upper CI bounds
)

plot_data

# -------------------------
# 5.2 Create the plot
# -------------------------

ggplot(plot_data, aes(x = habitat, y = mean_length)) +
  geom_point(size = 4, color = "blue") +                    
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),      
                width = 0.2, color = "red", size = 1) +
  # Automatically set limits with a small buffer above/below
  ylim(min(plot_data$CI_lower) - 10, max(plot_data$CI_upper) + 10) +
  labs(
    title = "Mean Fish Length by Habitat with 95% CI",
    x = "Habitat",
    y = "Mean Length (mm)"
  ) +
  theme_minimal(base_size = 14)                             

##Do the intervals overlap?

##If they overlap slightly, does that guarantee "no difference"?

##Are the intervals narrow or wide?

##What controls CI width? (Sample size? Variability?)

#############Part 3 Bootstrapping################
############################################################


# Load boot package
library(boot)

# -------------------------
# 5.1 Define a function to calculate the mean
# -------------------------
# This function will be applied to bootstrap samples
mean_stat <- function(data, indices) {
  sampled_data <- data[indices]  # select rows for this bootstrap sample
  return(mean(sampled_data))      # calculate mean for this sample
}

# -------------------------
# 5.2 Run bootstrap for each habitat
# -------------------------

# Seagrass habitat
seagrass_lengths <- fish_data$length[fish_data$habitat == "Seagrass"]
boot_seagrass <- boot(data = seagrass_lengths, statistic = mean_stat, R = 5000)

# Reef habitat
reef_lengths <- fish_data$length[fish_data$habitat == "Reef"]
boot_reef <- boot(data = reef_lengths, statistic = mean_stat, R = 5000)

# -------------------------
# 5.3 Estimate population mean and SD from bootstrap
# -------------------------

# Seagrass
boot_mean_seagrass <- mean(boot_seagrass$t)    # bootstrap estimate of mean
boot_sd_seagrass   <- sd(boot_seagrass$t)      # bootstrap estimate of SD
boot_mean_seagrass
boot_sd_seagrass

# Reef
boot_mean_reef <- mean(boot_reef$t)           # bootstrap estimate of mean
boot_sd_reef   <- sd(boot_reef$t)             # bootstrap estimate of SD
boot_mean_reef
boot_sd_reef

# -------------------------
# 5.4  visualize the bootstrap distributions
# -------------------------

# Seagrass
hist(boot_seagrass$t,
     main = "Bootstrap Distribution of Seagrass Mean",
     xlab = "Mean Length (mm)",
     col = "lightgreen",
     breaks = 30)

# Reef
hist(boot_reef$t,
     main = "Bootstrap Distribution of Reef Mean",
     xlab = "Mean Length (mm)",
     col = "brown",
     breaks = 30)


#############Part 4 - Independent Two-Sample t-Test ##################
############################################################
# Two-Sample t-Test
############################################################

t_result <- t.test(length ~ habitat, data = fish_data)
t_result


##State null hypothesis:
##H0: μ_seagrass = μ_reef

##State alternative:
## HA: μ_seagrass ≠ μ_reef

## Report test statistic.

##Report p-value.

##Compare to α = 0.05.

##Conclude in biological terms.


#############Part 5 - One-way ANOVA ###############
############################################################
# One-Way ANOVA
############################################################
##Are all indepdent variables factors?

fish_data$habitat<-as.factor(fish_data$habitat)
anova_model <- aov(length ~ habitat, data = fish_data)
summary(anova_model)

par(mfrow=c(2,2))###plot the model
plot(anova_model)

par(mfrow=c(1,1))##change back to 1 plot panel

##Test the assumptions
##Test of Normality on the residuals
anova_res<-residuals(anova_model)##retrieve the residuals
shapiro.test(anova_res)##test of normality

##Test of variance
library(car)
leveneTest(anova_model)

##boxplots
ggplot(fish_data, aes(x = habitat, y = length, fill = habitat)) +
  geom_boxplot(width = 0.6, alpha = 0.7) +   # Boxplot, slightly transparent
  scale_fill_manual(values = c("Seagrass" = "forestgreen", "Reef" = "goldenrod")) +  # Custom colors
  labs(
    title = "Fish Lengths by Habitat",
    x = "Habitat",
    y = "Length (mm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



###What did exploratory analysis suggest?

###Did confidence intervals suggest separation?

###Did bootstrap agree with parametric inference?

###Did t-test and ANOVA agree?

###Why is starting with hypothesis testing bad practice?

###What ecological explanation might exist for size differences?
