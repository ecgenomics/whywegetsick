library(ggplot2)
library(ggrepel)

# Prepare Dataframe
ages <- read.delim("../data/onset_age_62diseases_v17.01.2025_FinRegistryR11.tsv", sep="\t")
names <- read.delim("../data/disease_names.csv", sep=";")

# Rename
colnames(ages)[1] <- c("Code")

# Merge
df <- merge(ages, names, by="Code")

# Base theme
base_theme <- theme(
  axis.title.x = element_text(face = "bold"),
  axis.text    = element_text(size = 7, family = "Arial"),
  axis.title   = element_text(size = 7, family = "Arial"),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank(),
  axis.title.y=element_blank(),
)

# Plot
p = ggplot(df, aes(ADO_5, ADO_5*40)) +
  geom_point(size=1.5) +
  theme_bw() +
  base_theme +
  xlab("Age at Disease Onset (5% Prevalence)") +
  geom_text_repel(data=df, aes(label=DiseaseName), max.overlaps = Inf, size = 2.7, segment.size = 0.2)

# Save plot
ggsave("./onset_age_62diseases_ado5_fullname.png", p, height = 170, width = 180, units = "mm", dpi = 400)
