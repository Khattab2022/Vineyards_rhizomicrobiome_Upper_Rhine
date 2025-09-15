

library(ggplot2)
library(tidyr)

library(ggplot2)

filename <- ("trophic_mode.csv")
trophic_data <- read.table(filename, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )
#trophic_data

trophic_data
# Convert to long format
library(tidyr)
#long_data <- pivot_longer(taxa_data, 
#cols = starts_with("Condition"), 
#names_to = "Condition", 
#values_to = "Abundance")
# Prevent alphabetical ordering
trophic_data$vineyard <- factor(trophic_data$vineyard, levels = unique(trophic_data$vineyard))
# Plot with ggplot2

ggplot(trophic_data, aes(x = vineyard, y = relative_abundance, fill = trophic_mode)) +
  geom_bar(position = "fill", stat = "identity") +
  #expand_limits(y = c(0, 1))+
  labs(x = "vinyard", y = "relative abundance [%]") +
  theme_bw(16)+
  scale_fill_manual(values = c(
    "Pathotroph" = "#b2182b",                      # deep red
    "Pathotroph_Saprotroph" = "#ef8a62",           # soft coral
    "Pathotroph_Saprotroph_Symbiotroph" = "#fddbc7", # light peach
    "Pathotroph_Symbiotroph" = "#80cdc1",          # muted teal
    "Saprotroph" = "#35978f",                      # earthy turquoise
    "Saprotroph_Symbiotroph" = "#1a4c48",          # deep green-teal
    "Symbiotroph" = "black",   #1a4c48                  # dark green
    "na" = "#d9d9d9"                               # neutral gray
  )) +
  theme_classic() +
  theme(text=element_text(size=20)) 
  #theme(axis.text.x = element_text(angle = 90,  hjust = 1)) 









