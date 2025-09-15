library(ggplot2)
library(dplyr)
library(patchwork)
library(ggpubr)

# Input data
traits <- read.csv("../data/genetic-correlations-fertility-related-traits.txt",
                   sep="\t")

# Select the final traits
names <- c('Vitamin D Levels', 'Age at first sexual intercourse',
           'Number of sexual partners', 'Age at first birth',
           'Ever taken oral contraceptive pill',
           'Age when last used oral contraceptive pill', 'Household income',
           'Educational attainment', 'Townsend Deprivation Index')
codes <- c('VITAMIND2017GRASP', '3304', '3305', 'AFB2016BARBAN', '3346', '3348',
           '3195', '4066', 'TOWNSEND2016GRASP')

dict_names <- c('VITAMIND2017GRASP' = 'Vitamin D Levels',
                '3304' = 'Age at first sexual intercourse',
                '3305' = 'Number of sexual partners',
                'AFB2016BARBAN' = 'Age at first birth',
                '3346' = 'Ever taken oral contraceptive pill',
                '3348' = 'Age when last used oral contraceptive pill',
                '3195' = 'Household income',
                '4066' = 'Educational attainment',
                'TOWNSEND2016GRASP' = 'Townsend Deprivation Index')

dict_domains <- c('VITAMIND2017GRASP' = 'Biology of Reproduction',
                '3304' = 'Reproductive Behaviour',
                '3305' = 'Reproductive Behaviour',
                'AFB2016BARBAN' = 'Reproductive Behaviour',
                '3346' = 'Reproductive Behaviour',
                '3348' = 'Reproductive Behaviour',
                '3195' = 'Socioeconomic Factors',
                '4066' = 'Socioeconomic Factors',
                'TOWNSEND2016GRASP' = 'Socioeconomic Factors')

selected_traits <- traits %>%
  filter(Trait %in% codes) %>%
  mutate(Name = dict_names[Trait]) %>%
  mutate(Domain = dict_domains[Trait]) %>%
  mutate(FDR = p.adjust(PValue, method = "BH")) %>%
  arrange(desc(GC))

# Order to plot by genetic correlation
selected_traits$Name <- factor(selected_traits$Name, levels = unique(selected_traits$Name[order(selected_traits$GC, decreasing = FALSE)]))

# Order2 (by domain)
dict_order <- c('VITAMIND2017GRASP' = 1,
                  '3304' = 8,
                  '3305' = 5,
                  'AFB2016BARBAN' = 9,
                  '3346' = 6,
                  '3348' = 7,
                  '3195' = 3,
                  '4066' = 4,
                  'TOWNSEND2016GRASP' = 2)
selected_traits$Order2 <- dict_order[selected_traits$Trait]
selected_traits$Name <- factor(selected_traits$Name, 
                               levels = selected_traits$Name[order(selected_traits$Order2, decreasing = TRUE)])

# Global font type
par(family = "Arial")

# Do the plot
p <- ggplot(selected_traits, aes(x = GC, y = Name)) +
  geom_errorbarh(aes(xmin = GC - GC_SE, xmax = GC + GC_SE), height = 0) +
  geom_point(aes(alpha = (FDR < 0.05)), color = "black", size = 3) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.5), guide = "none") +
  labs(x = expression("Genetic correlation " ~(r[g]))) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.title.y   = element_blank(),
    axis.text.y    = element_text(size = 5, color = "black"),
    axis.text.x    = element_text(size = 7, color = "black"),
    axis.title.x = element_text(size = 7, margin = margin(t = 5)),
    plot.margin    = margin(10, 10, 10, 10),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
# add vertical dotted line at 0
p <- p + geom_vline(xintercept = 0, linetype = "dotted", color = 'black')
# set y range from -1 to 1
p <- p + coord_cartesian(xlim = c(-1, 1), ylim = c(1, nrow(selected_traits)))
  
# Color the background by domain
domain_colors <- c("Biology of Reproduction" = "#FEE34F", 
                   "Reproductive Behaviour" = "#7ED1F3", 
                   "Socioeconomic Factors" = "#F08758")

final_plot <- p + 
  geom_rect(data = selected_traits, aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(Name) - 0.3, ymax = as.numeric(Name) + 0.3, fill = Domain), alpha = 0.5) +
  scale_fill_manual(values = domain_colors, guide = "none") +
  geom_errorbarh(aes(xmin = GC - GC_SE, xmax = GC + GC_SE), height = 0) +
  geom_point(aes(alpha = (FDR < 0.05)), color = "black", size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted", color = 'black') +
  theme(legend.position = "top")

# Subplot1: biology of reproduction
subplot1 <- selected_traits[selected_traits$Domain == "Biology of Reproduction", ]
p1 <- ggplot(subplot1, aes(x = GC, y = Name)) +
  geom_errorbarh(aes(xmin = GC - GC_SE, xmax = GC + GC_SE), height = 0) +
  geom_point(aes(alpha = (FDR < 0.05)), color = "black", size = 1.5) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.5), guide = "none") +
  labs(x = expression("Genetic correlation " ~(r[g]))) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.title.y   = element_blank(),
    axis.text.y    = element_text(size = 5, color = "black"),
    axis.text.x    = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin    = margin(10, 10, 5, 10),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
p1 <- p1 + geom_vline(xintercept = 0, linetype = "dotted", color = 'black')
p1 <- p1 + coord_cartesian(xlim = c(-1, 1), ylim = c(1, nrow(subplot1)))

# Subplot2: reproductive behavior
subplot2 <- selected_traits[selected_traits$Domain == "Reproductive Behaviour", ]
subplot2$Name <- factor(subplot2$Name, levels = unique(subplot2$Name[order(subplot2$GC, decreasing = FALSE)]))
p2 <- ggplot(subplot2, aes(x = GC, y = Name)) +
  geom_errorbarh(aes(xmin = GC - GC_SE, xmax = GC + GC_SE), height = 0) +
  geom_point(aes(alpha = (FDR < 0.05)), color = "black", size = 1.5) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.5), guide = "none") +
  labs(x = expression("Genetic correlation " ~(r[g]))) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.title.y   = element_blank(),
    axis.text.y    = element_text(size = 5, color = "black"),
    axis.text.x    = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin    = margin(5, 10, 5, 10),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
p2 <- p2 + geom_vline(xintercept = 0, linetype = "dotted", color = 'black')
p2 <- p2 + coord_cartesian(xlim = c(-1, 1), ylim = c(1, nrow(subplot2)))

# Subplot3: socioeconomic factors
subplot3 <- selected_traits[selected_traits$Domain == "Socioeconomic Factors", ]
subplot3$Name <- factor(subplot3$Name, levels = unique(subplot3$Name[order(subplot3$GC, decreasing = FALSE)]))
p3 <- ggplot(subplot3, aes(x = GC, y = Name)) +
  geom_errorbarh(aes(xmin = GC - GC_SE, xmax = GC + GC_SE), height = 0) +
  geom_point(aes(alpha = (FDR < 0.05)), color = "black", size = 1.5) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.5), guide = "none") +
  labs(x = expression("Genetic correlation " ~(r[g]))) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.title.y   = element_blank(),
    axis.text.y    = element_text(size = 5, color = "black"),
    axis.text.x    = element_text(size = 7, color = "black"),
    axis.title.x = element_text(size = 7, margin = margin(t = 5)),
    plot.margin    = margin(5, 10, 5, 10),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
p3 <- p3 + geom_vline(xintercept = 0, linetype = "dotted", color = 'black')
p3 <- p3 + coord_cartesian(xlim = c(-1, 1), ylim = c(1, nrow(subplot3)))

# Aggregate the plots
figure <- ggarrange(p1, p2, p3,
                     ncol = 1, nrow = 3, heights = c(1, 5, 3), align = "v")
# Add text to figure
figure <- figure + annotate("text", x = 0.25, y = 0.98, label = "Biology of Reproduction:", 
                            color = "black", size = 2, family = "Arial", fontface = "bold")
figure <- figure + annotate("text", x = 0.25, y = 0.88, label = "Reproductive Behaviour:", 
                            color = "black", size = 2, family = "Arial", fontface = "bold")
figure <- figure + annotate("text", x = 0.245, y = 0.33, label = "Socioeconomic Factors:", 
                            color = "black", size = 2, family = "Arial", fontface = "bold")


# Save
ggsave("./fertility_related_traits_plot.png",
       figure,
       width  = 100, height = 115,
       dpi    = 400, units = "mm")
