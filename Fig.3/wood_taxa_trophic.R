library(ComplexHeatmap)
library(circlize)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(ggdist)
library(agricolae)

wood_saprotrophs  <- read.csv("wood_pathogenic_saprotroph.csv")
head(wood_saprotrophs)


# Ensure sample_ID and soil are treated as factors with levels in the order they appear
wood_saprotrophs <- wood_saprotrophs %>%
  mutate(
    sample_id = factor(sample_id, levels = unique(sample_id)))


aov_model <- aov(percentage ~ sample_id, data = wood_saprotrophs)
summary(aov_model)

# Step 2: Run Duncan test using the ANOVA model
duncan_result <- duncan.test(aov_model, "sample_id", console = TRUE)

# Step 3: Extract groups and merge with original data
duncan_letters <- duncan_result$groups
duncan_letters$sample_id <- rownames(duncan_letters)

# Step 4: Merge the letters with your data for plotting
label_data <- wood_saprotrophs %>%
  group_by(sample_id) %>%
  summarise(y_position = max(percentage) + 0.2) %>%
  left_join(duncan_letters, by = "sample_id")

# Step 5: Plot with letters
patho_wood_trophs = wood_saprotrophs %>%
  ggplot(aes(x = sample_id, y = percentage)) +
  #geom_boxplot(aes(fill = trophic_mode), width = 0.4, outlier.shape = NA) +  # Use fill for coloring boxplots
  geom_boxplot(fill = "coral2", color = "black", width = 0.5, outlier.shape = NA) +  # Use fill for coloring boxplots
  geom_text(data = label_data, aes(x = sample_id, y = y_position, label = groups), 
            size = 8, vjust = 5) +
  ylab("relative abundance [%]") +
  xlab("vineyard") +
  geom_jitter(width = 0.1, alpha = 0.5, size = 1.5) +
  expand_limits(y = c(0, 0.7))+
  #label.y = 0.5 +
  geom_hline(yintercept = mean(wood_saprotrophs$percentage), linetype = 2) +
  theme_classic(base_size = 28) 
patho_wood_trophs


#################################################################

wood_saprotrophs  <- read.csv("wood_nonpathogenic_saprotroph.csv")
head(wood_saprotrophs)


# Ensure sample_ID and soil are treated as factors with levels in the order they appear
wood_saprotrophs <- wood_saprotrophs %>%
  mutate(
    sample_id = factor(sample_id, levels = unique(sample_id)))


aov_model <- aov(percentage ~ sample_id, data = wood_saprotrophs)
summary(aov_model)

# Step 2: Run Duncan test using the ANOVA model
duncan_result <- duncan.test(aov_model, "sample_id", console = TRUE)

# Step 3: Extract groups and merge with original data
duncan_letters <- duncan_result$groups
duncan_letters$sample_id <- rownames(duncan_letters)

# Step 4: Merge the letters with your data for plotting
label_data <- wood_saprotrophs %>%
  group_by(sample_id) %>%
  summarise(y_position = max(percentage) + 0.2) %>%
  left_join(duncan_letters, by = "sample_id")

# Step 5: Plot with letters
nonpatho_wood_trophs = wood_saprotrophs %>%
  ggplot(aes(x = sample_id, y = percentage)) +
  geom_boxplot(fill = "cyan", color = "black", width = 0.4, outlier.shape = NA) +  # controlled style
  geom_text(data = label_data, aes(x = sample_id, y = y_position, label = groups), 
            size = 8, vjust = 5) +
  ylab("relative abundance [%]") +
  xlab("vineyard") +
  geom_jitter(width = 0.1, alpha = 0.5, size = 1.5) +
  #expand_limits(y = c(0, 0.7))+
  #label.y = 0.5 +
  geom_hline(yintercept = mean(wood_saprotrophs$percentage), linetype = 2) +
  labs(title = "nonpathogenic wood saprotrophs") +
  theme_classic(base_size = 28) 
nonpatho_wood_trophs

