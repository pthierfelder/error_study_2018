library(dplyr)
library(emmeans)
library(lme4)

# Import and filter data --------------------------------------------------
Data <- read.table("/cloud/project/Error_Disruption_2018.txt", header=TRUE, sep="\t")
Data <- filter(Data, Char_ID != 48 & Char_ID != 74) # Remove trials with 庸 and 崖 (unfamiliar to 50% of Hearing participants)
Data$Condition <- factor(Data$Condition, levels = c(5, 2, 3, 4, 1), labels=c("Unr", "Ort", "Hom", "Vis", "Cor"))

# Center and scale continous variables
Data$Predictability_Value_Z <- as.numeric(scale(Data$Predictability_Value, center = TRUE, scale = TRUE))

# Set min/max fixation times for filtering
Min_FFD <- 60
Max_GD <- 1200

# Pre-process data
Hearing_Data <- subset(Data, Deaf == "H")

Missing_Observations <- 1 - length(which(Hearing_Data$FFD_ms > 0))/length(Hearing_Data$FFD_ms)

Hearing_TRT_Data <- subset(Hearing_Data, FFD_ms >= Min_FFD & GD_ms <= Max_GD)
Hearing_Filtered <- 1 - length(Hearing_TRT_Data$TRT_ms)/length(which(Hearing_Data$FFD_ms > 0))
Hearing_Filtered

cat("\n", round(Missing_Observations * 100), "percent of FFD/GD trials are missed observations.\n",
    "\n", round(Hearing_Filtered * 100), "percent of the FFD/GD data was outside of min/max thresholds.\n")

# Build models ------------------------------------------------------------

Hearing_TRT_GLMM_1 <- glmer(TRT_ms ~ Condition * Predictability_Value_Z + (1 | Participant) + (1 | Frame_ID), data = Hearing_TRT_Data,
                         Gamma(link = "identity"), nAGQ=0,
                         glmerControl(optimizer = "bobyqa"))

summary(Hearing_TRT_GLMM_1)

Hearing_TRT_Post_Hoc <- emmeans(Hearing_TRT_GLMM_1, ~ Condition)
Hearing_TRT_Post_Hoc_Pairs <- pairs(Hearing_TRT_Post_Hoc) 
summary(Hearing_TRT_Post_Hoc_Pairs[8], adjust="none")