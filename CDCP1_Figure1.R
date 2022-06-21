# Package imports 
library(dplyr) # For parsing
library(ggpubr) # For pretty scatter plots; http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

# Import data. Ignore warning
dat <- read.csv("CDCP1_PTEN_EXP.csv",  header = TRUE, row.names = "samples")
dat2 <- read.csv("CDCP1_FOLH1_EXP.csv",  header = TRUE, row.names = "samples")

# Convert to vectors for plotting
pten <- as.numeric(dat['PTEN',]) # Vector with PTEN EXP
cdcp <- as.numeric(dat['CDCP1',]) # Vector with CDCP1 EXP
psma <- as.numeric(dat2['FOLH1',]) # Vector with PSMA EXP

nepc <- dat['Subtype',] # Vector with whether NEPC/AdPC

# Convert NEPC/AdPC to colors for the scatterplot
ne <- vector(mode = "list", length = 119)
for (i in 1:119){
  if (nepc[i] == 'NEPC'){ne[i] = 'red'}
  else{ne[i] = 'grey'}
}

jpeg("Plots/bargraph_figure1.jpg", units='in', width = 15, height = 5, res = 300)
fig1 <- gghistogram(cdcp, bins = 20, fill = 'blue', xlab = 'normalized expression', ylab = '% mCRPC samples', main = 'CDCP1 Expression')
fig2 <- gghistogram(psma, bins = 20, fill = 'blue', xlab = 'normalized expression', ylab = '% mCRPC samples', main = 'FOLH1 Expression')

# Plot the scatterplot of folh1 vs cdcp1. Ignore the warning message. 
df <- data.frame("normalized_CDCP1" = cdcp, "normalized_PSMA" = psma)
fig3 <- ggscatter(df, y = "normalized_CDCP1", x = "normalized_PSMA",
                  main = 'FOLH1 vs CDCP1',
                  xlab = 'normalized PSMA expression',
                  ylab = 'normalized CDCP1 expression',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 14.5, label.y = 8.5)+ylim(8,20)+xlim(7.5,22.5)  

# Plot the scatterplot of pten vs cdcp1. Ignore the warning message. 
df <- data.frame("normalized_CDCP1" = cdcp, "normalized_PTEN" = pten)
fig4 <- ggscatter(df, y = "normalized_CDCP1", x = "normalized_PTEN",
                  main = 'PTEN vs CDCP1',
                  xlab = 'normalized PTEN expression',
                  ylab = 'normalized CDCP1 expression',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 13.5, label.y = 8.5)+ylim(8,20)+xlim(12,20)  

# Arrange for figure
ggarrange(ggarrange(fig1, fig2, nrow = 2, labels = c("A", "B")), # First column with histograms
          ggarrange(fig3, labels = "C"), # Second column with scatterplot
          ggarrange(fig4, labels = "D"), # Third column with scatterplot
          ncol = 3
) 

# Turn off
dev.off()

