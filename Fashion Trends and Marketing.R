library(dplyr)
library(ggplot2)

qualtrics <- read.csv(file = "/Users/mihuynh/Documents/UT Dallas/Spring 2024/PSY 3393/Fashion Trends and Marketing_March 25, 2024_20.47.csv")
str(qualtrics)

# QUESTIONNAIRE
columns_to_inverse <- c("Questionnaire_4", "Questionnaire_7", "Questionnaire_9", "Questionnaire_11", "Questionnaire_13", "Questionnaire_17", "Questionnaire_18", "Questionnaire_19", "Questionnaire_21")

qualtrics <- qualtrics %>%
  mutate(across(all_of(columns_to_inverse), ~ 4 - .)) 

cols_to_score <- names(qualtrics)[grepl("Questionnaire", names(qualtrics))]

# Norms. The mean for the appropriate items forms the subscale scores. The response
# format ranges from 0 (Never) to 4 (Always). The higher the score for a particular subscale, the
# more positive is the respondent's body esteem on that dimension. Subscale means and standard
# deviations for males and females combined and at each age are presented in Table 2. Overall,
# women scored lower than men on each body-esteem subscale, Fs(1,1332) = 20.68 to 209.03, ps
# < .001, which provides construct validation of the scale (Mendelson, B.K. et al 1997)

qualtrics <- qualtrics %>%
  rowwise() %>%
  mutate(Questionnaire_Scores = mean(c_across(cols_to_score), na.rm = TRUE))

qualtrics$Participant <- row.names(qualtrics)

# median and mean of questionnaire
median <- median(qualtrics$Questionnaire_Scores, na.rm = TRUE)
qualtrics$Median_Group <- cut(qualtrics$Questionnaire_Scores, breaks = c(-Inf, median, Inf), labels = c("Below Median", "Above Median"))

# Scatter plot to show Participant vs. Their Sum
qualtrics$Masc_fem <- ifelse(qualtrics$Masc_fem == 1, "Masculine", "Feminine")
qualtrics$Participant <- as.numeric(qualtrics$Participant)

ggplot(data = qualtrics, mapping = aes(x = Participant, y = Questionnaire_Scores)) +
  geom_point(mapping = aes(color = Masc_fem)) +
  scale_x_continuous(breaks = qualtrics$Participant) +
  geom_smooth(aes(y = median), method = "lm", size = 0.5, se = FALSE, color = "blue") +
  geom_text(data = NULL, mapping = aes(x = 25, y = 2.1, label = "Median"), color = "blue")
  

# Feminine
feminine <- subset(qualtrics, Masc_fem == "Feminine")
cols_to_remove <- grep("Masc", names(feminine), value = TRUE)
cols_to_remove <- setdiff(cols_to_remove, "Masc_fem")
feminine <- feminine[, !(names(feminine) %in% cols_to_remove)]

fem_plus_cols <- names(feminine)[grep("Fem_Plus", names(feminine))]
fem_plus_attract <- names(feminine)[grep("Fem_Plus_Attract", names(feminine))]
fem_plus_purchase <- names(feminine)[grep("Fem_Plus_Purchase", names(feminine))]
fem_thin_cols <- names(feminine)[grep("Fem_Thin", names(feminine))]
fem_thin_attract <- names(feminine)[grep("Fem_Thin_Attract", names(feminine))]
fem_thin_purchase <- names(feminine)[grep("Fem_Thin_Purchase", names(feminine))]

feminine <- feminine %>%
  rowwise() %>%
  mutate(Mean_Plus_Results = mean(c_across(fem_plus_cols), na.rm = TRUE))
feminine <- feminine %>%
  rowwise() %>%
  mutate(Mean_Thin_Results = mean(c_across(fem_thin_cols), na.rm = TRUE))
feminine <- feminine %>%
  rowwise() %>%
  mutate(Mean_Plus_Attract_Results = mean(c_across(fem_plus_attract), na.rm = TRUE))
feminine <- feminine %>%
  rowwise() %>%
  mutate(Mean_Plus_Purchase_Results = mean(c_across(fem_plus_purchase), na.rm = TRUE))
feminine <- feminine %>%
  rowwise() %>%
  mutate(Mean_Thin_Attract_Results = mean(c_across(fem_thin_attract), na.rm = TRUE))
feminine <- feminine %>%
  rowwise() %>%
  mutate(Mean_Thin_Purchase_Results = mean(c_across(fem_thin_purchase), na.rm = TRUE))

correlation_plus <- cor.test(feminine$Mean_Plus_Results, feminine$Questionnaire_Scores)
correlation_thin <- cor.test(feminine$Mean_Thin_Results, feminine$Questionnaire_Scores)
correlation_plus_attract <- cor.test(feminine$Mean_Plus_Attract_Results, feminine$Questionnaire_Scores)
correlation_plus_purchase <- cor.test(feminine$Mean_Plus_Purchase_Results, feminine$Questionnaire_Scores)
correlation_thin_attract <- cor.test(feminine$Mean_Thin_Attract_Results, feminine$Questionnaire_Scores)
correlation_thin_purchase <- cor.test(feminine$Mean_Thin_Purchase_Results, feminine$Questionnaire_Scores)

# ggplots
library(gridExtra)

# FEMININE - PLUS VS THIN BOXPLOTS

plot1 <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Plus_Results, fill = Median_Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Below and Above Median Body Esteem Score 
       vs. Plus-sized Models Responses (Calculated by Mean)", 
       subtitle = "P-value = 0.368")
plot2 <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Thin_Results, fill = Median_Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Below and Above Median Body Esteem Score 
       vs. Straight-sized Models Responses (Calculated by Mean)",
       subtitle = "P-value = 0.005")
grid.arrange(plot1, plot2, nrow = 2)

# ALL WOMEN PLUS PLOTS
plot_plus <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Plus_Results, color = Median_Group)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Below and Above Median Body Esteem Score
       vs. Plus-sized Models Responses (Calculated by Mean)", subtitle = "P-value = 0.368")
plot_plus_attract <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Plus_Attract_Results, color = Median_Group)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Below and Above Median Body Esteem Score
       vs. Plus-sized Models Attractiveness Responses (Calculated by Mean)", subtitle = "P-value = 0.172")
plot_plus_purchase <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Plus_Purchase_Results, color = Median_Group)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Below and Above Median Body Esteem Score
       vs. Plus-sized Models Purchasability Responses (Calculated by Mean)", subtitle = "P-value = 0.769")
grid.arrange(plot_plus, plot_plus_attract, plot_plus_purchase, nrow = 3)

# ALL WOMEN THIN PLOTS
plot_thin <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Thin_Results, color = Median_Group)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Below and Above Median Body Esteem Score
       vs. Straight-sized Models Responses (Calculated by Mean)", subtitle = "P-value = 0.0046")
plot_thin_attract <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Thin_Attract_Results, color = Median_Group)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Below and Above Median Body Esteem Score
       vs. Straight-sized Models Attractiveness Responses (Calculated by Mean)", subtitle = "P-value = 0.0004")
plot_thin_purchase <- ggplot(data = feminine, mapping = aes(x = Median_Group, y = Mean_Thin_Purchase_Results, color = Median_Group)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Below and Above Median Body Esteem Score
       vs. Straight-sized Models Purchasability Responses (Calculated by Mean)", subtitle = "P-value = 0.0992")
grid.arrange(plot_thin, plot_thin_attract, plot_thin_purchase, nrow = 3)


# Masculine
masculine <- subset(qualtrics, Masc_fem == "Masculine")
cols_to_remove_m <- grep("Fem", names(masculine), value = TRUE)
masculine <- masculine[, !(names(masculine) %in% cols_to_remove_m)]

masc_plus_cols <- names(masculine)[grep("Masc_Plus", names(masculine))]
masc_plus_attract <- names(masculine)[grep("Masc_Plus_Attract", names(masculine))]
masc_plus_purchase <- names(masculine)[grep("Masc_Plus_Purchase", names(masculine))]
masc_thin_cols <- names(masculine)[grep("Masc_Thin", names(masculine))]
masc_thin_attract <- names(masculine)[grep("Masc_Thin_Attract", names(masculine))]
masc_thin_purchase <- names(masculine)[grep("Masc_Thin_Purchase", names(masculine))]

masculine <- masculine %>%
  rowwise() %>%
  mutate(Mean_Plus_Results = mean(c_across(masc_plus_cols), na.rm = TRUE))
masculine <- masculine %>%
  rowwise() %>%
  mutate(Mean_Thin_Results = mean(c_across(masc_thin_cols), na.rm = TRUE))
masculine <- masculine %>%
  rowwise() %>%
  mutate(Mean_Plus_Attract_Results = mean(c_across(masc_plus_attract), na.rm = TRUE))
masculine <- masculine %>%
  rowwise() %>%
  mutate(Mean_Plus_Purchase_Results = mean(c_across(masc_plus_purchase), na.rm = TRUE))
masculine <- masculine %>%
  rowwise() %>%
  mutate(Mean_Thin_Attract_Results = mean(c_across(masc_thin_attract), na.rm = TRUE))
masculine <- masculine %>%
  rowwise() %>%
  mutate(Mean_Thin_Purchase_Results = mean(c_across(masc_thin_purchase), na.rm = TRUE))

correlation_plus_m <- cor.test(masculine$Mean_Plus_Results, masculine$Questionnaire_Scores)
correlation_thin_m <- cor.test(masculine$Mean_Thin_Results, masculine$Questionnaire_Scores)
correlation_plus_attract_m <- cor.test(masculine$Mean_Plus_Attract_Results, masculine$Questionnaire_Scores)
correlation_plus_purchase_m <- cor.test(masculine$Mean_Plus_Purchase_Results, masculine$Questionnaire_Scores)
correlation_thin_attract_m <- cor.test(masculine$Mean_Thin_Attract_Results, masculine$Questionnaire_Scores)
correlation_thin_purchase_m <- cor.test(masculine$Mean_Thin_Purchase_Results, masculine$Questionnaire_Scores)

masculine <- na.omit(masculine)

# MASCULINE - PLUS VS THIN
masc_plus_plot <- ggplot(data = masculine, mapping = aes(x = Median_Group, y = Mean_Plus_Results, fill = Median_Group, na.omit = TRUE)) +
  geom_boxplot() +
  labs(title = "Boxplot of Below and Above Median Body Esteem Score 
       vs. Plus-sized Models Responses (Calculated by Mean)", 
       subtitle = "P-value = 0.081")
masc_thin_plot <- ggplot(data = masculine, mapping = aes(x = Median_Group, y = Mean_Thin_Results, fill = Median_Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Below and Above Median Body Esteem Score 
       vs. Straight-sized Models Responses (Calculated by Mean)",
       subtitle = "P-value = 0.584")
grid.arrange(masc_plus_plot, masc_thin_plot, nrow = 2)
