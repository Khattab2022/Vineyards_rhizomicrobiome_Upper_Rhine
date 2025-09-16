
## Load libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(RColorBrewer)

# Step 1: Read data (from clipboard or use read.delim for a file)
df <- read.csv("fig_1.csv")

# Step 2: Clean phylum names to match your custom colors
df$Phylum <- gsub("^p_", "p__", df$Phylum)  # normalize to 'p__' format

# Step 3: Clean taxon names (remove g_ or f_) and reorder
df <- df %>%
  mutate(
    CleanTaxon = gsub("^[gf]_?", "", Taxon),
    CleanTaxon = fct_reorder(CleanTaxon, LFC),
    Significant = ifelse(p_value < 0.05, "p < 0.05", "n.s.")
  )

# Step 4: Custom color palette
phyla_color <- c(
  "p__Actinomycetota" = "#9d9338",
  "p__Pseudomonadota" = "coral1",
  "p_Patescibacteria" = "bisque",
  "p__Chloroflexota" = "cornflowerblue",
  "p__Bacillota" = "green3",
  "p__Entotheonellaeota" = "coral3",
  "p__Planctomycetota" = "red",
  "p__Bacteroidota" = "deepskyblue",
  "p__Crenarchaeota" = "blue",
  "p__Verrucomicrobiota" = "azure3",
  "p__Myxococcota" = "darkgoldenrod1",
  "p__Acidobacteriota" = "pink"
)

# Step 5: Plot
p <- ggplot(df, aes(x = LFC, y = CleanTaxon, fill = Phylum)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(aes(color = Significant), size = 3, shape = 21, stroke = 1) +
  scale_fill_manual(values = phyla_color) +
  scale_color_manual(values = c("p < 0.05" = "black", "n.s." = NA), guide = "none") +
  labs(
    title = "Differential Abundance of Bacterial Taxa",
    x = "Log Fold Change (LFC)",
    y = NULL,
    fill = "Phylum"
  ) +
  theme_classic(18) +
  theme(
    axis.text.y = element_text(size = 18),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p




