library(ggplot2)
library(ggrepel)
library("ggpubr")

####################
#### SUB-PLOT 1 ####
####################

# Prepare Dataframe
ages <- read.delim("onset_age_62diseases_v17.01.2025_FinRegistryR11.tsv", sep="\t")
names <- read.delim("disease_names.csv", sep=";")

# Rename
colnames(ages)[1] <- c("Code")

# Merge
df <- merge(ages, names, by="Code")

# Correlation plot ADO_5 and ADO_50
p2 = ggplot(df, aes(ADO_5, ADO_50)) +
  geom_point() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.y = element_text(size = 7)) +
  xlab("Age at disease onset (5% prevalence)") +
  ylab("Age at disease onset (50% prevalence)") +
  geom_text_repel(data=df, aes(label=DiseaseName), max.overlaps = Inf, size = 7,
                  family = "Helvetica")

# Scatter plot
ct <- cor.test(df$ADO_5, df$ADO_50, method = "pearson")
r2 <- unname(ct$estimate)^2
pval <- ct$p.value

scatter_plot <- ggplot(df, aes(x = ADO_5, y = ADO_50)) +
  geom_point(size = 1, color = "blue", alpha = 1) +
  labs(x = "Age at disease onset (5% prevalence)", y = "Age at disease onset (50% prevalence)") +
  geom_text_repel(data=df, aes(label=DiseaseName), max.overlaps = Inf, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 70, by = 10), limits = c(0, 70)) +
  scale_y_continuous(breaks = seq(0, 90, by = 10), limits = c(0, 90)) +
  annotate(
    "text",
    x = -Inf, y = Inf,
    hjust = -0.05, vjust = 1.2,
    size = 7/.pt, fontface = "bold", family = "Helvetica",
    label = sprintf("R² = %.3f\nP = %.2e", r2, pval)
  ) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 6, family = "Helvetica", color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 7, family = "Helvetica", color = "black", margin = margin(t = 5)),
    axis.title.y = element_text(size = 7, family = "Helvetica", color = "black", margin = margin(r = 5)),
    plot.margin = margin(t = 5, r = 10, b = 10, l = 10)
  )

scatter_plot

####################
#### SUB-PLOT 2 ####
####################

# Inputs
data <- read.delim("merged_finregistry_ghdx_onset_ages.tsv", sep = "\t")

# Correlation
cor_test <- cor.test(data$ADO_5, data$Prev5, method = "pearson")
p <- cor_test$p.value
r <- unname(cor_test$estimate)
lab <- sprintf("Pearson r = %.2f\nP = %s",
               r, format.pval(p, digits = 2, eps = 1e-16))

# Plot
plot <- ggplot(data, aes(x=ADO_5, y=Prev5)) + 
  geom_point(size=1) + 
  geom_smooth(method=lm, se=TRUE, linewidth=0.5) + 
  xlab("FinRegistry age at disease onset (5% prevalence)") + 
  ylab("GHDx age at disease onset (5% prevalence)") +
  annotate("text", x = -Inf, y = Inf, label = lab,
           hjust = -0.05, vjust = 1.2, size = 7/.pt, family = "Helvetica", fontface = "bold") +
  theme_bw(base_family = "Helvetica") +
  scale_x_continuous(limits = c(0, 62), breaks = seq(0, 60, by = 10)) +
  scale_y_continuous(limits = c(0, 62), breaks = seq(0, 60, by = 10)) +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Helvetica"),
        axis.title = element_text(size = 7),
        axis.text  = element_text(size = 6),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.x = element_text(margin = margin(t = 5))
  )

######################
#### MERGE & SAVE ####
######################

merge_plot <- plot_grid(
  scatter_plot, plot,
  ncol = 1,
  labels = c("a", "b"),
  label_fontface = "bold",
  label_fontfamily = "Helvetica",
  label_size = 7,
  align = "hv",
  rel_widths = c(1, 1)
)

ggsave("extended_data_fig_8.png",
       merge_plot, width = 180, height = 180, units = "mm", dpi = 400)

# vector:
ggsave(
  filename = "extended_data_fig_8.pdf",
  plot = merge_plot,
  device = "pdf",
  width = 180,
  height = 180,
  units = "mm",
  useDingbats = FALSE
)
