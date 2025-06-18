# The analysis of SER percentage during stinging learning acquisition using the lme4 package (version 1.1-37) in R (version 4.1.3）
# https://CRAN.R-project.org/package=lme4

# 1. Data preprocessing
# (1) Read the data and delete the missing values
install.packages("dplyr")
library(dplyr)
data <- read.csv("Results.CSV", na.strings = "")
data <- na.omit(data)

# (2) Variable type conversion
data$Group <- as.factor(data$Group)
data$Trial <- as.numeric(data$Trial)  # Trial number include 1, 2, 3, 4, 5
data$Cage <- as.factor(data$Cage)
data$BeeID <- as.factor(data$BeeID)
data$Response <- as.integer(data$Response)  # Response: 0 or 1

# 2. Construct the Generalized Linear Mixture Model (GLMM)
# (1) Full model (including interactions of group and trial)
install.packages("lm4")
library(lme4)
Full_model <- glmer(
  Response ~ Group * Trial + (1 | Cage) + (1 | BeeID),  # Fixed effects: Group, Trial and their interaction terms; Random effect: Cage and BeeID
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")  # Improve convergence stability
)

# (2) Simplified model (no interaction)
Simplified_model <- glmer(
  Response ~ Group + Trial + (1 | Cage) + (1 | BeeID), # Fixed effects: Group and Trial; Random effect: Cage and BeeID
  data = data, 
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# 3. Model Checking and Results
# (1) Significance test of interaction (Likelihood ratio test, LRT)
anova(Full_model, Simplified_model, test = "LRT")
# If anova$Pr(>Chisq) < 0.05, the message shows the interactions are significant and the interaction model needs to be retained, thus the final model is Full_model. Otherwise, the final model is Simplified_model.

# (2) Fixed effect coefficient explanation
summary(Full_model)
summary(Simplified_model)
