library(ggplot2)
library("ggpubr")
library(cowplot)

# Load GWAS metadata
met <- read.delim("metadata_62diseases.csv", sep=";")

# Load heritability
h2 <- read.delim("heritability_62diseases.tsv", sep="\t")

# Load genetic correlations
gc <- read.delim("fertility-genetic-correlations.txt", sep="\t")
names(gc)[1] <- c("Code")

# Load pleiotropies
pleio <- read.delim("fertility-commonSNPs-pleioFDR-conjfdr005-pleiotropies.list", sep=",")
names(pleio)[1] <- c("Code")

# Merge
df <- Reduce(function(x, y) merge(x, y, by = "Code"), list(met, h2, gc, pleio))

# Create a second plot to remove the two continuous diseases
# Important because they have NA values for number of cases and controls
df2 <- df[!df$Code %in% c("PGC2019ALCUSE", "GCST010003"),]

# Create the plots with Genetic Correlation

ylabel = expression(paste("Genetic correlation (", italic(r)[g], ")"))
base_theme <- theme(
  axis.title.x = element_text(),
  axis.text.x  = element_text(angle = 45, vjust = 0.5),
  axis.text    = element_text(size = 6, family = "Helvetica"),
  axis.title   = element_text(size = 7, family = "Helvetica")
)

add_cor_label <- function(p, x, y, data) {
  ct   <- cor.test(data[[x]], data[[y]], method = "pearson")
  lab  <- sprintf("r = %.2f, p = %.2g", unname(ct$estimate), ct$p.value)
  xpos <- max(data[[x]], na.rm = TRUE)
  ypos <- max(data[[y]], na.rm = TRUE)
  p + annotate("text", x = xpos, y = ypos, label = lab,
               hjust = 1, vjust = 1, family = "Helvetica", size = 6/.pt)
}

# Plot 1
plot1 <- ggplot(df, aes(x = N, y = GC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Sample size", y = ylabel) +
  scale_x_continuous(
    breaks = c(seq(0, 300000, by = 50000),
               seq(300000, max(df$N, na.rm = TRUE), by = 200000))
  ) +
  base_theme
plot1 <- add_cor_label(plot1, "N", "GC", df)

# Plot 2
plot2 <- ggplot(df2, aes(x = NCases, y = GC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of cases", y = ylabel) +
  scale_x_continuous(
    breaks = c(seq(0, 60000, by = 10000),
               seq(60000, max(df2$NCases, na.rm = TRUE), by = 50000))
  ) +
  base_theme +
  theme(axis.title.y = element_text(colour = "white"))
plot2 <- add_cor_label(plot2, "NCases", "GC", df2)

# Plot 3
plot3 <- ggplot(df2, aes(x = NControls, y = GC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of controls", y = ylabel) +
  scale_x_continuous(
    breaks = c(seq(0, 300000, by = 50000),
               seq(300000, max(df2$NControls, na.rm = TRUE), by = 200000))
  ) +
  base_theme +
  theme(axis.title.y = element_text(colour = "white"))
plot3 <- add_cor_label(plot3, "NControls", "GC", df2)

# Plot 4
plot4 <- ggplot(df, aes(x = N.SNPs, y = GC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of SNPs", y = ylabel) +
  base_theme
plot4 <- add_cor_label(plot4, "N.SNPs", "GC", df)

# Plot 5
plot5 <- ggplot(df, aes(x = N.genome.wide.SNPs, y = GC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of genome-wide SNPs", y = ylabel) +
  base_theme +
  theme(axis.title.y = element_text(colour = "white"))
plot5 <- add_cor_label(plot5, "N.genome.wide.SNPs", "GC", df)

# Plot 6
plot6 <- ggplot(df, aes(x = SNP.h2, y = GC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = expression(paste("SNP ", italic(h)[2], " (LDSC)")), y = ylabel) +
  base_theme +
  theme(axis.title.y = element_text(colour = "white"))
plot6 <- add_cor_label(plot6, "SNP.h2", "GC", df)

# Aggregate
fig <- plot_grid(
  plot1, plot2, plot3, plot4, plot5, plot6,
  ncol = 3,
  labels = c("a", "b", "c", "d", "e", "f"),
  label_fontface = "bold",
  label_fontfamily = "Helvetica",
  label_size = 7,
  align = "hv",
  rel_widths = c(1, 1)
)

ggsave(
  filename = "supplementary_fig_5.pdf",
  plot = fig,
  device = "pdf",
  width = 180,
  height = 170,
  units = "mm",
  useDingbats = FALSE
)

# Create the plots with Pleiotropies

ylabel = "Number of pleiotropic SNPs"

# Plot 7
plot7 <- ggplot(df, aes(x = N, y = TOTAL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Sample size", y = ylabel) +
  scale_x_continuous(
    breaks = c(seq(0, 300000, by = 50000),
               seq(300000, max(df$N, na.rm = TRUE), by = 200000))
  ) +
  base_theme +
  theme(
    axis.title.x = element_text(),
    axis.text.x  = element_text(angle = 45, vjust = 0.5)
  )
plot7 <- add_cor_label(plot7, "N", "TOTAL", df)

# Plot 8
plot8 <- ggplot(df2, aes(x = NCases, y = TOTAL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of cases", y = ylabel) +
  scale_x_continuous(
    breaks = c(seq(0, 60000, by = 10000),
               seq(60000, max(df2$NCases, na.rm = TRUE), by = 50000))
  ) +
  base_theme +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(colour = "white"),
    axis.text.x  = element_text(angle = 45, vjust = 0.5)
  )
plot8 <- add_cor_label(plot8, "NCases", "TOTAL", df2)

# Plot 9
plot9 <- ggplot(df2, aes(x = NControls, y = TOTAL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of controls", y = ylabel) +
  scale_x_continuous(
    breaks = c(seq(0, 300000, by = 50000),
               seq(300000, max(df2$NControls, na.rm = TRUE), by = 200000))
  ) +
  base_theme +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(colour = "white"),
    axis.text.x  = element_text(angle = 45, vjust = 0.5)
  )
plot9 <- add_cor_label(plot9, "NControls", "TOTAL", df2)

# Plot 10
plot10 <- ggplot(df, aes(x = N.SNPs, y = TOTAL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of SNPs", y = ylabel) +
  base_theme +
  theme(axis.title.x = element_text())
plot10 <- add_cor_label(plot10, "N.SNPs", "TOTAL", df)

# Plot 11
plot11 <- ggplot(df, aes(x = N.genome.wide.SNPs, y = TOTAL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "Number of genome-wide SNPs", y = ylabel) +
  base_theme +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(colour = "white")
  )
plot11 <- add_cor_label(plot11, "N.genome.wide.SNPs", "TOTAL", df)

# Plot 12
plot12 <- ggplot(df, aes(x = SNP.h2, y = TOTAL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(x = "SNP h2 (LDSC)", y = ylabel) +
  base_theme +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(colour = "white")
  )
plot12 <- add_cor_label(plot12, "SNP.h2", "TOTAL", df)

# Aggregate
fig2 <- plot_grid(
  plot7, plot8, plot9, plot10, plot11, plot12,
  ncol = 3,
  labels = c("a", "b", "c", "d", "e", "f"),
  label_fontface = "bold",
  label_fontfamily = "Helvetica",
  label_size = 7,
  align = "hv",
  rel_widths = c(1, 1)
)

ggsave(
  filename = "supplementary_fig_7.pdf",
  plot = fig2,
  device = "pdf",
  width = 180,
  height = 170,
  units = "mm",
  useDingbats = FALSE
)
