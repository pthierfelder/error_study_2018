library(dplyr)
library(emmeans)
library(lme4)

# Import and filter data --------------------------------------------------
Data <- read.table("/cloud/project/Error_Disruption_2018.txt", header=TRUE, sep="\t")
Data <- filter(Data, Char_ID != 48 & Char_ID != 74) # Remove trials with 庸 and 崖 (unfamiliar to 50% of deaf participants)
Data$Condition <- factor(Data$Condition, levels = c(5, 2, 3, 4, 1), labels=c("Unr", "Ort", "Hom", "Vis", "Cor"))

# Center and scale continous variables
Data$Predictability_Value_Z <- as.numeric(scale(Data$Predictability_Value, center = TRUE, scale = TRUE))
Data$Reading_Score_Z <- as.numeric(scale(Data$Reading_Score, center = TRUE, scale = TRUE))

# Set min/max fixation times for filtering
Min_FFD <- 60
Max_GD <- 1200

# Pre-process data
Deaf_Data <- subset(Data, Deaf == "D")

Missing_Observations <- 1 - length(which(Deaf_Data$FFD_ms > 0))/length(Deaf_Data$FFD_ms)

Deaf_TRT_Data <- subset(Deaf_Data, FFD_ms >= Min_FFD & GD_ms <= Max_GD)
Deaf_Filtered <- 1 - length(Deaf_TRT_Data$TRT_ms)/length(which(Deaf_Data$FFD_ms > 0))
Deaf_Filtered

cat("\n", round(Missing_Observations * 100), "percent of FFD/GD trials are missed observations.\n",
    "\n", round(Deaf_Filtered * 100), "percent of the FFD/GD data was outside of min/max thresholds.\n")

# Build models ------------------------------------------------------------

Deaf_TRT_GLMM_1 <- glmer(TRT_ms ~ Condition * Predictability_Value_Z * Reading_Score_Z + (1 | Participant) + (1 | Frame_ID), data = Deaf_TRT_Data,
                        Gamma(link = "identity"), nAGQ = 0,
                        glmerControl(optimizer = "bobyqa"))

summary(Deaf_TRT_GLMM_1)

Deaf_TRT_Post_Hoc <- emmeans(Deaf_TRT_GLMM_1, ~ Condition)
Deaf_TRT_Post_Hoc_Pairs <- pairs(Deaf_TRT_Post_Hoc) 
summary(Deaf_TRT_Post_Hoc_Pairs[8], adjust="none")