library(ggplot2)
library("ggpubr")

# Inputs
data <- read.delim("../data/merged_finregistry_ghdx_onset_ages.tsv", sep = "\t")

# Correlation
cor_test <- cor.test(data$ADO_5, data$Prev5, method = "pearson")
p <- cor_test$p.value
r <- unname(cor_test$estimate)
lab <- sprintf("Pearson r = %.2f, P = %s",
               r, format.pval(p, digits = 2, eps = 1e-16))

# Plot
plot <- ggplot(data, aes(x=ADO_5, y=Prev5)) + 
  geom_point(size=1) + 
  geom_smooth(method=lm, se=TRUE, linewidth=0.5) + 
  xlab("FinRegistry age at disease onset (5% Prevalence)") + 
  ylab("GHDx age at disease onset (5% Prevalence)") +
  annotate("text", x = 1, y = 55, label = lab,
           hjust = 0, vjust = 1, size = 3, family = "Arial") +
  theme_bw(base_family = "Arial") +
  scale_x_continuous(limits = c(0, 62), breaks = seq(0, 60, by = 10)) +
  scale_y_continuous(limits = c(0, 62), breaks = seq(0, 60, by = 10)) +
  theme(panel.grid.minor = element_line(size=0.3),
        panel.grid.major = element_line(size=0.5),
        text = element_text(family = "Arial"),
        axis.title = element_text(size = 7),
        axis.text  = element_text(size = 6),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.x = element_text(margin = margin(t = 5))
  )

# Save
ggsave("./corr_GHDx_FinRegistry.png",
       plot,
       height = 90, 
       width = 100, 
       dpi = 400, 
       units = "mm")




