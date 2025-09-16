library(phyloseq)
library(Hmisc)
library(igraph)
library(RColorBrewer)
library(scales)  # for rescale()

# 1. Load and process input data
otu_matrix <- as.matrix(read.csv("16S_OTU_matrix.csv")[, 2:57])
rownames(otu_matrix) <- paste0("OTU", 1:nrow(otu_matrix))

taxonomy_matrix <- as.matrix(read.csv("16S_OTU_taxonomy_matrix.csv")[, 2:7])
rownames(taxonomy_matrix) <- paste0("OTU", 1:nrow(taxonomy_matrix))

sample_metadata <- read.csv("16S_sample_metadata.csv", row.names = 1)

# 2. Create phyloseq object
phy <- phyloseq(
  otu_table(otu_matrix, taxa_are_rows = TRUE),
  tax_table(taxonomy_matrix),
  sample_data(sample_metadata)
)

# 3. Normalize and aggregate to Genus level
phy_genus <- tax_glom(transform_sample_counts(phy, function(x) x / sum(x)), taxrank = "Genus")
abundance_matrix <- t(as.data.frame(otu_table(phy_genus)))

# 4. Correlation analysis (Spearman)
cor_mat <- rcorr(as.matrix(abundance_matrix), type = "spearman")
r_vals <- cor_mat$r
p_vals <- cor_mat$P

# 5. Filter by significance and strength
sig_mask <- (p_vals < 0.05) & (abs(r_vals) > 0.6)
filtered_r <- r_vals * sig_mask
edges <- which(sig_mask, arr.ind = TRUE)

# 6. Create edge list
edge_list <- data.frame(
  Source = rownames(filtered_r)[edges[, 1]],
  Target = colnames(filtered_r)[edges[, 2]],
  Weight = filtered_r[edges]
)

# Remove duplicate edges in undirected graph (Source < Target)
edge_list <- edge_list[edge_list$Source < edge_list$Target, ]

# 7. Extract and map genus names correctly
tax_tab <- as.data.frame(tax_table(phy_genus))
tax_tab$Genus <- as.character(tax_tab$Genus)
tax_tab$Phylum <- as.character(tax_tab$Phylum)

# Remove OTUs with missing genus
tax_tab <- tax_tab[!is.na(tax_tab$Genus), ]

# Set rownames as OTU IDs (important for mapping!)
rownames(tax_tab) <- rownames(tax_table(phy_genus))

# Create mapping: OTU ID -> Genus
otu_to_genus <- tax_tab$Genus
names(otu_to_genus) <- rownames(tax_tab)

# Replace OTU IDs in edge list with genus names
edge_list$Source <- otu_to_genus[edge_list$Source]
edge_list$Target <- otu_to_genus[edge_list$Target]

# Remove edges with NA after mapping
edge_list <- na.omit(edge_list)

# Remove duplicate edges again after mapping
edge_list <- edge_list[edge_list$Source < edge_list$Target, ]

# 8. Create graph object from genus-level edges
g <- graph_from_data_frame(edge_list, directed = FALSE)

# 9. Assign node metadata (Phylum)
# Prepare unique genus->phylum mapping
tax_tab_unique <- tax_tab[!duplicated(tax_tab$Genus), ]
rownames(tax_tab_unique) <- tax_tab_unique$Genus

V(g)$genus <- V(g)$name
V(g)$phylum <- tax_tab_unique[V(g)$name, "Phylum"]

# 10. Assign custom colors to top 10 phyla
top_phyla <- names(sort(table(V(g)$phylum), decreasing = TRUE))[1:10]

# Base colors for first 6 phyla
base_colors <- c("red", "green", "blue", "cyan4", "deeppink", "orange")

# Calculate how many extra colors needed
extra_colors_needed <- max(0, length(top_phyla) - length(base_colors))

# Get extra colors from brewer.pal if needed
extra_colors <- if (extra_colors_needed > 0) {
  brewer.pal(min(12, extra_colors_needed), "Paired")[1:extra_colors_needed]
} else {
  character(0)
}

# Combine colors and assign names
custom_colors <- c(base_colors[1:length(top_phyla)], extra_colors)
names(custom_colors) <- top_phyla

# Assign colors to nodes; default gray if phylum not in top_phyla
V(g)$color <- ifelse(V(g)$phylum %in% top_phyla,
                     custom_colors[V(g)$phylum],
                     "gray70")

# 11. Set node size based on number of correlations (degree)
layout_fr <- layout_with_fr(g)
deg <- degree(g)
V(g)$size <- scales::rescale(deg, to = c(4,12))
V(g)$label <- ifelse(deg >= 4, V(g)$name, NA)
E(g)$color <- rgb(0, 0, 0, alpha = 0.2)
E(g)$curved <- 0.1

# 13. Layout
layout <- layout_with_fr(g, niter = 1000)
layout <- igraph::norm_coords(layout, xmin = -1, xmax = 1, ymin = -1, ymax = 1)

# 14. Show labels only for selected taxa
highlighted_taxa <- c(
  "g__Isoptericola", "g__Thioprofundum", "g__Caulobacter", "g__Rhodomicrobium", 
  "g__Chryseolinea", "g__Promicromonospora", "g__Methylothermalis", 
  "g__Ilumatobacter", "g__Steroidobacter", "g__Entotheonellaceae", 
  "g__Actinomarinicola", "g__Xylanivirga", "g__Kitasatospora"
)

V(g)$label <- ifelse(V(g)$name %in% highlighted_taxa, V(g)$name, "")

# 15. Plot network
plot(g,
     layout = layout,
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.frame.color = "gray40",
     edge.color = "gray70",
     main = "Bacterial Co-occurrence Network (Genus Level)")

# 16. Save high-res PNG
png("bacterial_network.png", width = 2400, height = 2400, res = 300)
plot(g,
     layout = layout_fr,
     vertex.label = V(g)$label,
     vertex.size = V(g)$size,
     vertex.color = V(g)$color,
     vertex.frame.color = "azure4", #No border around nodes
     edge.width = E(g)$Weight * 4,
     edge.color = E(g)$color,
     vertex.label.cex = 1,
     vertex.label.color = "black",
     main = "Bacterial Correlation Network (Genus-Level, Spearman > 0.6)")
dev.off()

# 17. Plot legend separately
plot.new()
legend("center", legend = names(custom_colors), fill = custom_colors, cex = 0.8,
       title = "Top Bacterial Phyla", border = NA, ncol = 2)
