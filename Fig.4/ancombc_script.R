

library(DT)
library(tidyverse)
library(ANCOMBC)
library(mia)

featureTableFile <- "U:/IBG5/1-M4F/1-Results/Vineyards_project/Amp_results/ANCOMBC-survey/16S/Esca/for_publication/table.qza"
taxonomyTableFile <-"U:/IBG5/1-M4F/1-Results/Vineyards_project/Amp_results/ANCOMBC-survey/16S/Esca/for_publication/flash_taxonomy.qza"
sampleMetaFile <- "U:/IBG5/1-M4F/1-Results/Vineyards_project/Amp_results/ANCOMBC-survey/16S/Esca/for_publication/metadata_vineyards_16S_filtered.tsv"


Database_imported <- loadFromQIIME2(
  featureTableFile = featureTableFile,
  taxonomyTableFile = taxonomyTableFile,
  sampleMetaFile = sampleMetaFile,
)
analysis <- ancombc(
  data = Database_imported,
  assay_name = "counts",
  tax_level = "Genus",
  phyloseq = NULL,
  formula = "Plant_status",
  p_adj_method = "BH",
  prv_cut = 0.1,
  lib_cut = 0,
  group = "Plant_status",
  struc_zero = TRUE,
  neg_lb = FALSE,
  tol = 1e-05,
  max_iter = 100,
  conserve = TRUE,
  alpha = 0.05,
  global = TRUE,
  n_cl = 1,
  verbose = FALSE
)


res = analysis$res
res_global = analysis$res_global


#1) log fold changes;
#2) standard errors;
#3) test statistics;
#4) p-values;
#5) adjusted p-values;
#6) indicators whether the taxon is differentially abundant (TRUE) or not (FALSE).

#1) log fold changes
tab_lfc = res$lfc

col_name = c("Taxon", "Intercept", "symptomatic")

colnames(tab_lfc) = col_name
tab_lfc %>% 
  datatable(caption = "Log Fold Changes from the Primary Result") %>%
  formatRound(col_name[-1], digits = 2)

#2) standard errors
tab_se = res$se
colnames(tab_se) = col_name
tab_se %>% 
  datatable(caption = "Standard Errors from the Primary Result") %>%
  formatRound(col_name[-1], digits = 2)

#3) test statistics
tab_test_statistics = res$W
colnames(tab_test_statistics) = col_name
tab_test_statistics %>% 
  datatable(caption = "Test Statistics from the Primary Result") %>%
  formatRound(col_name[-1], digits = 2)

#4) p-values
tab_p = res$p_val
colnames(tab_p) = col_name
tab_p %>% 
  datatable(caption = "P-values from the Primary Result") %>%
  formatRound(col_name[-1], digits = 2)

#5) adjusted p-values
tab_adj_p = res$q
colnames(tab_adj_p) = col_name
tab_adj_p %>% 
  datatable(caption = "Adjusted p-values from the Primary Result") %>%
  formatRound(col_name[-1], digits = 2)

#6) indicators whether the taxon is differentially abundant (TRUE) or not (FALSE).
tab_diff = res$diff_abn
colnames(tab_diff) = col_name
tab_diff %>% 
  datatable(caption = "Differentially Abundant Taxa from the Primary Result")

#table without filtering

df_all_nomixed = tab_lfc %>%
  dplyr::select(Taxon, `symptomatic`)

df_heat_all_nomixed = df_all_nomixed %>%
  pivot_longer(cols = -one_of("Taxon"),
               names_to = "region", values_to = "value") %>%
  mutate(value = round(value, 2))

low = floor(min(df_heat_all_nomixed$value))
up = ceiling(max(df_heat_all_nomixed$value))
mid = 0
p_heat_all_nomixed = df_heat_all_nomixed %>%
  ggplot(aes(x = region, y = Taxon, fill = value)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       na.value = "white", midpoint = mid, limit = c(low, up),
                       name = NULL) +
  geom_text(aes(region, Taxon, label = value), color = "black", size = 3, family="serif") +
  labs(x = NULL, y = NULL,
       title = "Differential abundance of prokaryotic phyla in vineyards rhizosphere under GTDs outbreak") +
  theme_minimal() +
  theme(text = element_text(hjust = 0.5, family="serif"))

p_heat_all_nomixed

#(0,8197 cm pro row in der Tabelle)
ggsave(p_heat_all_nomixed, filename = "U:/IBG5/1-M4F/1-Results/Vineyards_project/Amp_results/ANCOMBC-survey/16S/Esca/for_publication/heatmap_LFC_Genus_symptomatic.pdf", width = 50, height = 200, units = "cm", limitsize = FALSE)

write.table(tab_lfc, file = "U:/IBG5/1-M4F/1-Results/Vineyards_project/Amp_results/ANCOMBC-survey/16S/Esca/for_publication/LFC_Genus_symptomatic.csv", quote = FALSE, sep = ',')
write.table(tab_p, file = "U:/IBG5/1-M4F/1-Results/Vineyards_project/Amp_results/ANCOMBC-survey/16S/Esca/for_publication/tab_p_Genus_symptomatic.csv", quote = FALSE, sep = ',')
write.table(tab_diff, file = "U:/IBG5/1-M4F/1-Results/Vineyards_project/Amp_results/ANCOMBC-survey/16S/Esca/for_publication/tab_diff_Genus_symptomatic.csv", quote = FALSE, sep = ',')

