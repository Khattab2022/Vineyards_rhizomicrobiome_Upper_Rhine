
###########packages to use#########
library("Hmisc")
library("corrplot")
####assinging data file##########
bdiv_all <- ("ITS_correlation_rawdata.csv")
my_data <- read.table(bdiv_all, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )

my_data
corr_data <- as.matrix(my_data[ ,c(2:22)])
corr_data
#####starting correlation analysis###########
res <- cor(corr_data)
round(res, 2)

res2 <- rcorr(as.matrix(my_data[ ,c(2:22)]), type = c("pearson"))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

corrplot(res, type = "full", order = "original", addrect = 2, sig.level = 0.05, insig = "blank",
         col=colorRampPalette(c("darkorange","white","darkgreen"))(100),cl.lim=c(0,1), method = c("color"), number.cex = 1,
         tl.col = "black", tl.srt = 45)

# Insignificant correlation are crossed
#corrplot(res2$r, type="fill", order="original", 
         #p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="original",
         method = c("circle"), addrect = 2,  tl.col = "black",
         #addshade = c("negative", "positive", "all"),
         col=colorRampPalette(c("darkorange","white","darkgreen"))(100),cl.lim=c(0,1), number.cex = 1.5,
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

######################
help("corrplot")

write.table(res2$P, file = "U:/IBG5/1-M4F/1-Results/Vineyards_project/figures/16S/rawdata_figures/fig.2/corr_res$P.csv", quote = FALSE, sep = ',')

write.table(res2$r, file = "U:/IBG5/1-M4F/1-Results/Vineyards_project/figures/16S/rawdata_figures/fig.2/corr_res$r.csv", quote = FALSE, sep = ',')

###############
bdiv_micro <- ("ITS_correlation_rawdata_micro.csv")
my_data <- read.table(bdiv_micro, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )

my_data
corr_data <- as.matrix(my_data[ ,c(2:15)])
corr_data
#####starting correlation analysis###########
res <- cor(corr_data)
round(res, 2)

res2 <- rcorr(as.matrix(my_data[ ,c(2:15)]), type = c("pearson"))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# Insignificant correlation are crossed
#corrplot(res2$r, type="fill", order="original", 
#p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="original",
         method = c("circle"), tl.col = "black", tl.srt=90,  tl.cex = 1.5, number.font = 3,
         col=colorRampPalette(c("darkorange","white","darkgreen"))(100),cl.lim=c(0,1), number.cex = 1,
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
###############################################################

bdiv_macro <- ("ITS_correlation_rawdata_macro.csv")
my_data <- read.table(bdiv_macro, sep=",", quote="", stringsAsFactors=FALSE, header=TRUE )

my_data
corr_data <- as.matrix(my_data[ ,c(2:19)])
corr_data
#####starting correlation analysis###########
res <- cor(corr_data)
round(res, 2)

res2 <- rcorr(as.matrix(my_data[ ,c(2:19)]), type = c("pearson"))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

#corrplot(res, type = "full", order = "original", addrect = 2, sig.level = 0.05, insig = "blank",
#         col=colorRampPalette(c("darkorange","white","darkgreen"))(100),cl.lim=c(0,1), method = c("color"), number.cex = 1.5,
#         tl.col = "black", tl.srt = 45)

# Insignificant correlation are crossed
#corrplot(res2$r, type="fill", order="original", 
#p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="original",
         method = c("circle"), tl.col = "black", tl.srt=90,  tl.cex = 1.5, number.font = 6,
         col=colorRampPalette(c("darkorange","white","darkgreen"))(100),cl.lim=c(0,1), number.cex = 1,
         p.mat = res2$P, sig.level = 0.05, insig = "blank")



??corrplot



