
library(ComplexHeatmap)
library(circlize)
library(ggplot2)

library(DT)
library(tidyverse)
library(mia)


filename <- ("rawdata_significantly_shifted.csv")
ITS_asym_shifted_df <- read.table(filename, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )
head (ITS_asym_shifted_df)
dim(ITS_asym_shifted_df)
ITS_asym_shifted_df
data_df = (ITS_asym_shifted_df[ ,c(2:4)])
data_df

barplot = ggplot(data_df, aes(x= reorder(Taxon, -LFC), y=LFC, family="serif", fill=shift)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.3)) +
  scale_fill_discrete(name = NULL) +
  scale_color_discrete(name = NULL) +
  #coord_cartesian() +
  coord_flip() +
  theme_classic(24) + 
  theme(text = element_text(hjust = 0.5, family="serif", fontsize(20)),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1, family="serif"))
barplot

filename <- ("rawdata_significantly_shifted.csv")
trophic_df <- read.table(filename, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )
head (trophic_df)
trophic_annotation <- as.matrix(trophic_df[ ,c(1)])

trophic_annotation

phyla_color = c( "p_Ascomycota" = "bisque","p_Basidiomycota" = "azure3", "p_Glomeromycota" = "cadetblue1", 
                 "p_Chytridiomycota" = "burlywood4", "p_Mucoromycota" = "deepskyblue")

ht2= Heatmap(trophic_annotation, row_labels = trophic_df$Taxon, col= phyla_color,
             row_names_side = "left",
             width = unit(10, "mm"), row_names_gp = gpar(fontsize=20), name = "phylum")
ht2





# Load libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(ggtext)

# Step 1: Read your data
df <- read.csv("rawdata_significantly_shifted.csv")  # Or read.delim("your_file.tsv")

# Step 2: Standardize phylum names
df$Phylum <- gsub("^p_", "p__", df$phylum)

# Step 3: Define phylum color palette

phyla_color = c( "p__Ascomycota" = "darkgoldenrod3","p__Basidiomycota" = "azure4", "p__Glomeromycota" = "darkgreen", 
                 "p__Chytridiomycota" = "burlywood4", "p__Mucoromycota" = "deepskyblue")
# Step 4: Prepare dataset
df <- df %>%
  mutate(
    CleanTaxon = gsub("^[gf]_?", "", Taxon),
    Direction = ifelse(LFC >= 0, "Positive", "Negative"),
    CleanTaxon = fct_reorder(CleanTaxon, LFC),
    LabelColor = phyla_color[Phylum],
    TaxonLabel = paste0("<span style='color:", LabelColor, "'><b>", CleanTaxon, "</b></span>")
  )

# Step 5: Create a dummy dataset for phylum legend
phylum_legend_df <- df %>%
  distinct(Phylum) %>%
  mutate(dummy_x = 1, dummy_y = Phylum)

# Step 6: Build the plot
p <- ggplot(df, aes(x = LFC, y = CleanTaxon, fill = Direction)) +
  geom_col(width = 0.7) +
  # Phantom points for phylum color legend (invisible on plot, but appear in legend)
  geom_point(
    data = phylum_legend_df,
    aes(x = dummy_x, y = dummy_y, color = Phylum),
    inherit.aes = FALSE,
    size = 0, show.legend = TRUE
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(
    name = "Shift Direction",
    values = c("Negative" = "cyan2", "Positive" = "firebrick2")
  ) +
  scale_color_manual(
    name = "Phylum (Label Color)",
    values = phyla_color,
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  scale_y_discrete(labels = df$TaxonLabel) +
  labs(
    title = "Significantly Shifted Fungal Taxa",
    x = "Log Fold Change (LFC)",
    y = NULL
  ) +
  theme_classic (base_size = 18) +
  theme(
    axis.text.y = ggtext::element_markdown(size = 18),
    legend.position = "right",
  )

# Step 7: Show and save
print(p)
ggsave("LFC_colored_bars_and_label_legend.png", p, width = 10, height = 6, dpi = 300)






