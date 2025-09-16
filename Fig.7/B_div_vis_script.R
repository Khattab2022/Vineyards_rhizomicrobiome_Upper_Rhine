
install.packages("vegan")
install.packages("ape")
remotes::install_github('vqv/ggbiplot')


library(devtools)
library(tidyverse)
library(ggbiplot)
library(vegan)
library(ape)
library(ggplot2)


filename <- ("w_unifrac_metadata.txt")
metadata_pcoa <- read.table(filename, sep="\t", quote="", stringsAsFactors=FALSE, header=TRUE) 
view (metadata_pcoa)
head(metadata_pcoa)

ggplot(metadata_pcoa, aes(bray_pcoa1, bray_pcoa2, col = location, fill = location)) +
  stat_ellipse(geom = "polygon", alpha=0.2,
    position = "identity", linetype= 1,
    type = "t",
    level = 0.95,
  ) +
  geom_point(aes(colour = samplingcode), size = 4) +
  theme_bw(14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())


ggplot(metadata_pcoa, aes(W_Unifrac_pcoa1, W_Unifrac_pcoa2, col = location, fill = location)) +
  stat_ellipse(geom = "polygon", alpha=0.2,
               position = "identity", linetype= 1,
               type = "t",
               level = 0.95,
  ) +
  geom_point(aes(colour = samplingcode), size = 4) +
  theme_bw(14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())






################
filename <- ("w_unifrac_metadata_Sil.txt")
metadata_pcoa <- read.table(filename, sep="\t", quote="", stringsAsFactors=FALSE, header=TRUE) 
#view (metadata_pcoa)

ggplot(metadata_pcoa, aes(W_Unifrac_X, Y, col = location_var, fill = location_var)) +
  stat_ellipse(geom = "polygon", alpha=0.1,
               position = "identity", linetype= 1,
               type = "t",
               level = 0.95,
  ) +
  geom_point(shape = 20, size=4) +
  theme_classic(14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())


###########################
####VARIETY

filename <- ("w_unifrac_metadata.txt")
metadata_pcoa <- read.table(filename, sep="\t", quote="", stringsAsFactors=FALSE, header=TRUE)

ggplot(metadata_pcoa, aes(W_Unifrac_X, Y, col = variety, fill = variety)) +
  stat_ellipse(geom = "polygon", alpha=0.1,
               position = "identity", linetype= 1,
               type = "t",
               level = 0.95,
  ) +
  geom_point(shape = 20, size=4) +
  theme_classic(14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())



