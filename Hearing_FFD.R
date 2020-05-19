library(dplyr)
library(lme4)

# Import and filter data --------------------------------------------------
Data <- read.table("/cloud/project/T_W_T_2019_Error_Disruption.txt", header=TRUE, sep="\t")
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

Hearing_FFD_Data <- subset(Hearing_Data, FFD_ms >= Min_FFD & GD_ms <= Max_GD)
Hearing_Filtered <- 1 - length(Hearing_FFD_Data$FFD_ms)/length(which(Hearing_Data$FFD_ms > 0))
Hearing_Filtered

cat("\n", round(Missing_Observations * 100), "percent of FFD/GD trials are missed observations.\n",
    "\n", round(Hearing_Filtered * 100), "percent of the FFD/GD data was outside of min/max thresholds.\n")

# Build models ------------------------------------------------------------

Hearing_FFD_GLMM_1 <- glmer(FFD_ms ~ Condition * Predictability_Value_Z + (1 | Participant) + (1 | Frame_ID), data = Hearing_FFD_Data,
                         Gamma(link = "identity"),
                         glmerControl(optimizer = "bobyqa"))

summary(Hearing_FFD_GLMM_1)
