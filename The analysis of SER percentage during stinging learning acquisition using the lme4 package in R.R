# The analysis of SER percentage during aversive learning acquisition using the lme4 package (version 1.1-37) in R (version 4.3.3）
# https://CRAN.R-project.org/package=lme4
# https://www.r-project.org/

# 1. Install and load the required packages
install.packages("lme4")   # Used for GLMM modelling
install.packages("lmerTest")   # Offer the p value of the model coefficients
install.packages("emmeans")   # Used for the post hoc comparisons
install.packages("dplyr")   # Used for data processing
install.packages("tidyr")   # Used for the processing of the missing values
install.packages("ggplot2")   # Used for visualization

library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(tidyr)
library(ggplot2)


# 2. Data preprocessing
# (1) Read the data and delete the missing values
data <- read.csv("Results.csv", na.strings = "")   # Read the raw data
sum(is.na(data$Response))   # Count the number of missing values
data <- na.omit(data)   # Delete the missing values

# (2) Variable type conversion
data$Group <- as.factor(data$Group)
data$Trial <- as.numeric(data$Trial)   # Trial number: 1, 2, 3, 4, 5
data$Cage <- as.factor(data$Cage)   # 6 cages per group
data$BeeID <- as.factor(data$BeeID)   # 10 honeybees per cage
data$Response <- as.integer(data$Response)   # Response: 0 or 1

# (3) Data structure check
str(data)
table(data$Group, data$Trial)   # Check the distribution of each group of trials


# 3. Construct the generalized linear mixture model (GLMM)
# (1) Full model (including interactions of group and trial)
Full_model <- glmer(
  Response ~ Group * Trial + (1 | Cage/BeeID),   # Fixed effects: Group, Trial and their interaction terms; Random effect: Cage and BeeID
  data = data,
  family = binomial(link = "logit"),   # Binomial distribution (with logit link function)
  control = glmerControl(optimizer = "bobyqa")  # Improve convergence stability
)

# (2) Simplified model (no interaction)
Simplified_model <- glmer(
  Response ~ Group + Trial + (1 | Cage/BeeID), # Fixed effects: Group and Trial; Random effect: Cage and BeeID
  data = data,
  family = binomial(link = "logit"),   # Binomial distribution (with logit link function)
  control = glmerControl(optimizer = "bobyqa")   # Improve convergence stability
)

# The number 1 in (1 | Cage/BeeID) represents a random intercept, which allows the baseline response probability at each level to be different.


# 4. Model checking and results
# (1) Significance test of interaction (Likelihood ratio test, LRT)
anova(Full_model, Simplified_model, test = "LRT")
# If anova$Pr(>Chisq) < 0.05, the message shows the interactions are significant and the interaction model needs to be retained, thus the final model is Full_model. Otherwise, the final model is Simplified_model.

# (2) Fixed effect coefficient explanation
summary(Full_model)
summary(Simplified_model)


# 5. Result explanation
# (1) Calculate the difference in response rates between groups
response_rates <- data %>%
  group_by(Group, Trial) %>%
  summarise(
    ResponseRate = mean(Response, na.rm = TRUE),
    .groups = 'drop'
  )

print(response_rates)

# (2) Overall level difference (main effect)
# If the interaction is significant:
emtrends(Full_model, pairwise ~ Group, var = "Trial")   # “Trial” as a continuous variable

# If the interaction is not significant:
emmeans(Simplified_model, pairwise ~ Group, type = "response")   # Output the differences in response rates and the p values for the two groups

# (3) Learning trend differences (interaction effect)
# Visualization of learning trends:
ggplot(data, aes(x = Trial, y = Response, color = Group)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
  stat_summary(aes(group = Group), fun = mean, geom = "line") +
  labs(y = "Response rate", title = "Learning acquisition")

# Statistical test (when Trial is used as a factor):
contrast(emmeans(Full_model, ~ Group * Trial),
         method = "pairwise", by = "Trial")

# 6. Model diagnostic checking
# Overdispersion checking:
overdisp_fun <- function(model){
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  ratio <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  return(c(ratio = ratio, p.value = pval))
}

overdisp_fun(Full_model)
overdisp_fun(Simplified_model)

# Check the variance of random effects:
summary(Full_model)$varcor
summary(Simplified_model)$varcor

# When the variance of the random effect is close to 0, this random effect can be removed.
