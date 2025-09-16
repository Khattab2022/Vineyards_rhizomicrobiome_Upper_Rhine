
library(ComplexHeatmap)
library(circlize)

###########################################################################################################################

filename <- ("heatmap.csv")
corr_df <- read.table(filename, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )
head (corr_df)
dim(corr_df)
corr_df
COR_matrix <- as.matrix(corr_df[ ,c(2:15)])
class (COR_matrix)
head (COR_matrix)
##########assigning col.fun#########
col_fun = colorRamp2(c( -1, 0, 1), c("deepskyblue4", "white", "darkred"))
p_values <- read.csv("pvalues.csv")  # Example random p-values
p_matrix <- as.matrix(p_values[ ,c(2:15)])
diag(p_matrix) <- 1  # Set diagonal to 1 for correlation with itself

# Names should match COR_matrix
rownames(p_matrix) <- rownames(COR_matrix)
colnames(p_matrix) <- colnames(COR_matrix)

# Create asterisk annotation based on significance level (e.g., p < 0.05)
significance_level <- 0.05
stars <- ifelse(p_matrix < significance_level, "*", "")

# Display asterisks in the heatmap
ht1 <- Heatmap(COR_matrix,
               row_labels = corr_df$factor,
               cluster_columns = FALSE,
               cluster_rows = FALSE,
               show_row_names = TRUE,
               column_names_rot = 90,
               column_names_side = "top",
               row_names_side = "left",
               clustering_distance_columns = "euclidean",
               clustering_method_columns = "complete",
               clustering_distance_rows = "euclidean",
               clustering_method_rows = "complete",
               col = col_fun,
               rect_gp = gpar(col = "azure4", lwd = 1),
               row_names_gp = gpar(fontsize = 12),
               column_names_gp = gpar(fontsize = 12),
               name = "corr_coefficient", 
               border = TRUE,
               width = unit(9, "cm"), 
               height = unit(9, "cm"),
               
               # Add asterisks based on p-value thresholds
               cell_fun = function(j, i, x, y, width, height, fill) {
                 pval <- p_matrix[i, j]
                 if (!is.na(pval)) {
                   if (pval < 0.001) {
                     grid.text("***", x, y, gp = gpar(fontsize = 14, fontface = "bold", col = "black"))
                   } else if (pval < 0.01) {
                     grid.text("**", x, y, gp = gpar(fontsize = 14, fontface = "bold", col = "black"))
                   } else if (pval < 0.05) {
                     grid.text("*", x, y, gp = gpar(fontsize = 14, fontface = "bold", col = "black"))
                   }
                 }
               })

ht1

