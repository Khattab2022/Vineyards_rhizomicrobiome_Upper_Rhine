


library(ComplexHeatmap)
library(circlize)

###########################################################################################################################

filename <- ("top_20_taxa.txt")
BACT_df <- read.table(filename, sep="\t", quote="", stringsAsFactors=FALSE, header=TRUE )
head (BACT_df)
dim(BACT_df)
BACT_df
BACT_matrix <- as.matrix(BACT_df[ ,c(3:12)])
class (BACT_matrix)
head (BACT_matrix)
##########assigning col.fun#########

col_fun = colorRamp2(c(0, 2, 4), c("darkslateblue", "beige", "darkorange"))
######call up locatiokn#####
# Define your own colors for the 'location' variable (change to match your actual values)
location_colors <- c(
  "Rauenberg" = "steelblue", 
  "Ringsheim" = "orchid", 
  "Eichstetten" = "green",
  "Ihringen" = "azure4"
)
variety_id <- ("variety_assignment.txt")
variety_df <- read.table(variety_id, sep="\t", quote="", stringsAsFactors=FALSE, header=TRUE )
head (variety_df)

variety_annotation <- data.frame(location=variety_df$location)
variety_annotation

# Define annotation colors
ha_colors <- list(location = location_colors)

# Apply to HeatmapAnnotation
col_ha <- HeatmapAnnotation(df = variety_annotation, 
                             col = ha_colors, 
                             annotation_name_side = "left")

ht1 = Heatmap(BACT_matrix, row_labels = BACT_df$Genus,
              cluster_columns = TRUE,
              cluster_rows = TRUE,
              show_row_names = TRUE,
              column_names_rot = 0,
              column_names_centered = TRUE,
              column_names_side = "top",
              clustering_distance_columns = "euclidean",
              clustering_distance_rows = "euclidean",
              show_row_dend = FALSE,
              col = col_fun,
              top_annotation = col_ha,
              column_title = "top 20 prokaryotic taxa under different soil chemical profiles",
              heatmap_legend_param = list(
                title_gp = gpar(fontsize = 14, fontface = "bold"),  # Title text
                labels_gp = gpar(fontsize = 14),                    # Label text
                legend_height = unit(4, "cm"),                      # Height of the vertical legend bar
                grid_width = unit(4, "mm")),
              #column_km = 2,
              name = "relative abundance [%]", border = TRUE, width = unit(5, "cm"), height = unit(12, "cm"))
ht1 


phyla_annotation <- as.matrix(BACT_df[ ,c(1)])
phyla_annotation
phyla_color = c("p_Actinomycetota" = "#9d9338", "p_Pseudomonadota" = "coral1", "p_Patescibacteria" = "bisque", "p_Chloroflexota" = "cornflowerblue",
                "p_Bacillota" = "green3", "p_Entotheonellaeota" = "coral3", "p_Planctomycetota" = "red", "p_Bacteroidota" = "deepskyblue", "p_Methylomirabilota" = "azure4",
                "p_Crenarchaeota" = "blue", "p_Verrucomicrobiota" = "azure", "p_Myxococcota" = "darkgoldenrod1", "p_Acidobacteriota" = "pink" )
ht2= Heatmap(phyla_annotation, row_names_side = "right", row_labels = BACT_df$Genus, col= phyla_color,
             heatmap_legend_param = list(
               title_gp = gpar(fontsize = 16, fontface = "bold"),  # Title text
               labels_gp = gpar(fontsize = 14),                    # Label text
               legend_height = unit(8, "cm"),                      # Height of the vertical legend bar
               grid_width = unit(4, "mm")),                          # Width of the color blocks )
             width = unit(3, "mm"),
             name = "Phylum", row_names_gp = gpar(fontsize=14))
ht2

ht_list = ht1 + ht2
draw(ht_list)


