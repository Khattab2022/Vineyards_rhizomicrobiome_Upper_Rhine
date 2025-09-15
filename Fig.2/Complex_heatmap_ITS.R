


library(ComplexHeatmap)
library(circlize)

###########################################################################################################################

filename <- ("top_20_fungal_taxa.txt")
fungal_df <- read.table(filename, sep="\t", quote="", stringsAsFactors=FALSE, header=TRUE )
head (fungal_df)
dim(fungal_df)
fungal_df
BACT_matrix <- as.matrix(fungal_df[ ,c(3:12)])
class (BACT_matrix)
head (BACT_matrix)
##########assigning col.fun#########

col_fun = colorRamp2(c(0, 3, 6, 12, 20), c("darkslateblue", "beige", "orange", "brown1", "darkred"))
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

ht1 = Heatmap(BACT_matrix, row_labels = fungal_df$Genus,
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
              column_title = "top 20 fungal taxa under different soil chemical profiles",
              heatmap_legend_param = list(
                title_gp = gpar(fontsize = 14, fontface = "bold"),  # Title text
                labels_gp = gpar(fontsize = 14),                    # Label text
                legend_height = unit(4, "cm"),                      # Height of the vertical legend bar
                grid_width = unit(4, "mm")),
              #column_km = 2,
              name = "relative abundance [%]", border = TRUE, width = unit(5, "cm"), height = unit(12, "cm"))
ht1 


phyla_annotation <- as.matrix(fungal_df[ ,c(1)])
phyla_annotation
phyla_color = c("p_Ascomycota" = "bisque", "p_Basidiomycota" = "azure4", "p_Mortierellomycota" = "cadetblue", "p_Chloroflexota" = "cornflowerblue")
ht2= Heatmap(phyla_annotation, row_names_side = "right", row_labels = fungal_df$Genus, col= phyla_color,
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

##############################

















