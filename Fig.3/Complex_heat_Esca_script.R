


library(ComplexHeatmap)
library(circlize)

###########################################################################################################################

filename <- ("esca_relative-abundance.csv")
ESCA_df <- read.table(filename, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )
head (ESCA_df)
dim(ESCA_df)
ESCA_df
GTD_matrix <- as.matrix(ESCA_df[ ,c(3:12)])
class (GTD_matrix)
head (GTD_matrix)
##########assigning col.fun#########

col_fun = colorRamp2(c( 0, 1, 3), c("white", "red", "darkred"))

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

ht1 = Heatmap(GTD_matrix, row_labels = ESCA_df$Taxon,
              cluster_columns = TRUE,
              cluster_rows = FALSE,
              show_row_names = TRUE,
              column_names_rot = 0,
              column_names_centered = TRUE,
              column_names_side = "top",
              clustering_distance_columns = "euclidean",
              clustering_method_columns = "complete",
              clustering_distance_rows = "euclidean",
              clustering_method_rows = "complete",
              col = col_fun,
              rect_gp = gpar(col = "gray", lwd = 2),
              top_annotation = col_ha,
              column_title = "Distribution of GTS community",
              row_names_gp = gpar(fontsize=14),
              #column_km = 2, column_gap = unit(1, "mm"), 
              heatmap_legend_param = list(
                title_gp = gpar(fontsize = 16, fontface = "bold"),  # Title text
                labels_gp = gpar(fontsize = 14),                    # Label text
                grid_width = unit(4, "mm")),                          # Width of the color blocks )
              name = "relative abundance [%]", border = TRUE, width = unit(7.5, "cm"), height = unit(11, "cm"))
ht1 


GTD_annotation <- as.matrix(ESCA_df[ ,c(1)])

GTD_annotation

col_classifier_patho = c("Black foot disease" = "cornflowerblue", "Botryosphaeria dieback" = "chocolate4",  "Esca" = "darkcyan", 
                         "Diaporthe dieback" = "gray", "Eutypa dieback" = "yellow")
GTD_ht2= Heatmap(GTD_annotation, row_labels = ESCA_df$Taxon, col= col_classifier_patho,
                 heatmap_legend_param = list(
                   title_gp = gpar(fontsize = 16, fontface = "bold"),  # Title text
                   labels_gp = gpar(fontsize = 14),                    # Label text
                   legend_height = unit(8, "cm"),                      # Height of the vertical legend bar
                   grid_width = unit(4, "mm")),                          # Width of the color blocks )
                 width = unit(5, "mm"), row_names_gp = gpar(fontsize=14), name = "GTD type")

GTD_ht2

ht_list_GTD = ht1 + GTD_ht2
draw(ht_list_GTD)



##################


filename <- ("relative_symp_asymp.csv")
ESCA_df2 <- read.table(filename, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )
head (ESCA_df2)
dim(ESCA_df2)
ESCA_df2
GTD_matrix <- as.matrix(ESCA_df2[ ,c(2:3)])
class (GTD_matrix)
head (GTD_matrix)
##########assigning col.fun#########

col_fun = colorRamp2(c( 0, 1, 3), c("white", "red", "darkred"))

ht2 = Heatmap(GTD_matrix, row_labels = ESCA_df2$taxon,
              cluster_columns = FALSE,
              cluster_rows = FALSE,
              show_row_names = FALSE,
              column_names_rot = 60,
              column_names_centered = TRUE,
              column_names_side = "top",
              clustering_distance_columns = "euclidean",
              clustering_method_columns = "complete",
              clustering_distance_rows = "euclidean",
              clustering_method_rows = "complete",
              col = col_fun,
              rect_gp = gpar(col = "gray", lwd = 2),
              #top_annotation = col_ha,
              #column_title = "Distribution of GTS community",
              #row_names_gp = gpar(fontsize=14),
              #column_km = 2, column_gap = unit(1, "mm"), 
              name = "relative abundance [%]",
              border = TRUE, width = unit(1.5, "cm"), height = unit(11, "cm"))
ht2 



ht_list_GTD = ht1 + ht2 + GTD_ht2
draw(ht_list_GTD)






















