library(circlize)
library(grDevices)
library(dplyr)

# General Inputs
names <- read.csv("../data/disease_names.csv", sep = ";")
age <- read.csv("../data/onset_age_62diseases_v17.01.2025_FinRegistryR11.tsv", sep = "\t")

# Change onset by 0.01 just to avoid exactly the same values for GCST90014023 and GCST010003
age$ADO_5[age$DISEASE_CODE == "GCST010003"] <- 1.27

# Manually change disease names to fit them in the plot
names$DiseaseName[names$Code == "2"] <- "ADHD"
names$DiseaseName[names$Code == "4266"] <- "Asthma\nchild-onset"
names$DiseaseName[names$Code == "PGC2019ASD"] <- "Autism spectrum\ndisorder"
names$DiseaseName[names$Code == "4333"] <- "Polycystic ovary\nsyndrome"
names$DiseaseName[names$Code == "PGC2019PTSD"] <- "Post-traumatic\nstress disorder"
names$DiseaseName[names$Code == "PGC2019MDD"] <- "Major depressive\ndisorder"
names$DiseaseName[names$Code == "PGC2019ALCUSE"] <- "Alcohol\nuse disorder"
names$DiseaseName[names$Code == "3669"] <- "Gastro-oesophageal\nreflux disease"
names$DiseaseName[names$Code == "GCST003763"] <- "Age-related\nhearing impairment"
names$DiseaseName[names$Code == "3666"] <- "Benign neoplasm\ncolon-rectum-anus"
names$DiseaseName[names$Code == "3688"] <- "Lipoprotein\ndisorders"
names$DiseaseName[names$Code == "3693"] <- "Coronary artery\ndisease"
names$DiseaseName[names$Code == "GCST90011819"] <- "Non-Hodgkin's\nlymphoma"
names$DiseaseName[names$Code == "GCST90011766"] <- "Primary open-angle\nglaucoma"
names$DiseaseName[names$Code == "2052"] <- "Age-related\nmacular degeneration"
names$DiseaseName[names$Code == "4276"] <- "Carpal tunnel\nsyndrome"
names$DiseaseName[names$Code == "3698"] <- "Diaphragmatic\nhernia"
names$DiseaseName[names$Code == "GCST011364"] <- "Myocardial\ninfarction"
names$DiseaseName[names$Code == "GCST90011820"] <- "Endometrial\ncancer"
names$DiseaseName[names$Code == "3670"] <- "Gastritis and\nDuodenitis"
names$DiseaseName[names$Code == "PGC2021ALZwoUKBB"] <- "Alzheimer’s\ndisease"

# Inputs Longevity UVMR
long_uvmr <- read.csv("../data/outcome_longevity_simpleMR_62diseases.csv", sep = ",")
long_uvmr_v1 <- merge(long_uvmr, age, by.x = "Exposure", by.y = "DISEASE_CODE")
long_uvmr_v2 <- merge(long_uvmr_v1, names, by.x = "Exposure", by.y = "Code")
order1 <- long_uvmr_v2[order(long_uvmr_v2$ADO_5), ]

# Inputs Longevity MVMR
long_mvmr <- read.csv("../data/outcome_longevity_multivariableMR_62diseases.csv", sep = ",")
long_mvmr_filtered <- subset(long_mvmr, is.na(Method) | Method == "IVW")
long_mvmr_v1 <- merge(long_mvmr_filtered, age, by.x = "Exposure1", by.y = "DISEASE_CODE")
long_mvmr_v2 <- merge(long_mvmr_v1, names, by.x = "Exposure1", by.y = "Code")
order2 <- long_mvmr_v2[order(long_mvmr_v2$ADO_5), ]

# Inputs Fertility Exposure UVMR
fert_exp_uvmr <- read.csv("../data/exposure_fertility_simpleMR_62diseases.csv", sep = ",")
fert_exp_uvmr_v1 <- merge(fert_exp_uvmr, age, by.x = "Outcome", by.y = "DISEASE_CODE")
fert_exp_uvmr_v2 <- merge(fert_exp_uvmr_v1, names, by.x = "Outcome", by.y = "Code")
order3 <- fert_exp_uvmr_v2[order(fert_exp_uvmr_v2$ADO_5), ]

# Inputs Fertility Exposure MVMV
fert_exp_mvmr <- read.csv("../data/exposure_fertility_multivariableMR_62diseases.csv", sep = ",")
fert_exp_mvmr_filtered <- subset(fert_exp_mvmr, is.na(Method) | Method == "IVW")
fert_exp_mvmr_v1 <- merge(fert_exp_mvmr_filtered, age, by.x = "Outcome", by.y = "DISEASE_CODE")
fert_exp_mvmr_v2 <- merge(fert_exp_mvmr_v1, names, by.x = "Outcome", by.y = "Code")
order4 <- fert_exp_mvmr_v2[order(fert_exp_mvmr_v2$ADO_5), ]

# Inputs Fertility Outcome UVMR
fert_out_uvmr <- read.csv("../data/outcome_fertility_simpleMR_62diseases.csv", sep = ",")
fert_out_uvmr_v1 <- merge(fert_out_uvmr, age, by.x = "Exposure", by.y = "DISEASE_CODE")
fert_out_uvmr_v2 <- merge(fert_out_uvmr_v1, names, by.x = "Exposure", by.y = "Code")
order5 <- fert_out_uvmr_v2[order(fert_out_uvmr_v2$ADO_5), ]

# Inputs Fertility Outcome MVMR
fert_out_mvmr <- read.csv("../data/outcome_fertility_multivariableMR_62diseases.csv", sep = ",")
fert_out_mvmr_filtered <- subset(fert_out_mvmr, is.na(Method) | Method == "IVW")
fert_out_mvmr_v1 <- merge(fert_out_mvmr_filtered, age, by.x = "Exposure1", by.y = "DISEASE_CODE")
fert_out_mvmr_v2 <- merge(fert_out_mvmr_v1, names, by.x = "Exposure1", by.y = "Code")
order6 <- fert_out_mvmr_v2[order(fert_out_mvmr_v2$ADO_5), ]

# To clean the plot, remove CI of the non-significant results
for (i in which(order1$Significance == "NS")) {
  order1[i, 8:9] <- NA
}

for (i in which(order2$Significance_Exposure1 == "NS")) {
  order2[i, 12:13] <- NA
}

for (i in which(order3$Significance == "NS")) {
  order3[i, 8:9] <- NA
}

for (i in which(order4$Significance_Exposure1 == "NS")) {
  order4[i, 12:13] <- NA
}

for (i in which(order5$Significance == "NS")) {
  order5[i, 8:9] <- NA
}

for (i in which(order6$Significance_Exposure1 == "NS")) {
  order6[i, 12:13] <- NA
}

# Introduce NA values when Significance on the UVMR is not-avaliable:
for (i in which(is.na(order1$Significance))) {
  order2[i, 5:22] <- NA
  order2[i, 28:29] <- NA
}

for (i in which(is.na(order3$Significance))) {
  order4[i, 5:22] <- NA
  order4[i, 28:29] <- NA
}

for (i in which(is.na(order5$Significance))) {
  order6[i, 5:22] <- NA
  order6[i, 28:29] <- NA
}

# For fertility as exposure remove diseases with ADO < 5
for (i in which(order3$ADO_5 < 5)) {
  order3[i, 4:9] <- NA
  order4[i, 5:7] <- NA
  order4[i, 11:13] <- NA
}

# For fertility as outcome remove diseases with ADO > 45
for (i in which(order5$ADO_5 > 45)) {
  order5[i, 4:9] <- NA
  order6[i, 5:7] <- NA
  order6[i, 11:13] <- NA
}

# Compute the log(OR)
order1$OR <- log(order1$OR)
order2$OR_Exposure1 <- log(order2$OR_Exposure1)
order3$OR <- log(order3$OR)
order4$OR_Exposure1 <- log(order4$OR_Exposure1)
order5$OR <- log(order5$OR)
order6$OR_Exposure1 <- log(order6$OR_Exposure1)
# Compute the log(OR) confidence intervals
order1$Lower_OR_CI_95. <- log(order1$Lower_OR_CI_95.)
order2$Lower_OR_CI_95._Exposure1 <- log(order2$Lower_OR_CI_95._Exposure1)
order3$Lower_OR_CI_95. <- log(order3$Lower_OR_CI_95.)
order4$Lower_OR_CI_95._Exposure1 <- log(order4$Lower_OR_CI_95._Exposure1)
order5$Lower_OR_CI_95. <- log(order5$Lower_OR_CI_95.)
order6$Lower_OR_CI_95._Exposure1 <- log(order6$Lower_OR_CI_95._Exposure1)
order1$Upper_OR_CI_95. <- log(order1$Upper_OR_CI_95.)
order2$Upper_OR_CI_95._Exposure1 <- log(order2$Upper_OR_CI_95._Exposure1)
order3$Upper_OR_CI_95. <- log(order3$Upper_OR_CI_95.)
order4$Upper_OR_CI_95._Exposure1 <- log(order4$Upper_OR_CI_95._Exposure1)
order5$Upper_OR_CI_95. <- log(order5$Upper_OR_CI_95.)
order6$Upper_OR_CI_95._Exposure1 <- log(order6$Upper_OR_CI_95._Exposure1)

# Number of categories
n_cat     <- 62
categories <- order1$DiseaseName

# Extract statistics + p-values
get_stats <- function(df, or_col, lower_col, upper_col, p_col) {
  list(
    est   = df[[or_col]],
    lower = df[[lower_col]],
    upper = df[[upper_col]],
    pval  = df[[p_col]]
  )
}

# Prepare stats for each dataset pair
st1 <- get_stats(order1, "OR",               "Lower_OR_CI_95.",           "Upper_OR_CI_95.",           "Pvalue")
st2 <- get_stats(order2, "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")
st3 <- get_stats(order3, "OR",               "Lower_OR_CI_95.",           "Upper_OR_CI_95.",           "Pvalue")
st4 <- get_stats(order4, "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")
st5 <- get_stats(order5, "OR",               "Lower_OR_CI_95.",           "Upper_OR_CI_95.",           "Pvalue")
st6 <- get_stats(order6, "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")

ylim1 <- c(-1, 1)  # adjust for track 1 (st1, st2)
ylim3 <- c(-1.5, 1)  # adjust for track 3 (st3, st4)
ylim5 <- c(-1, 1)  # adjust for track 5 (st5, st6)

# Pick bg colour by OR and p‐value
bg_col_fun <- function(or, pval) {
  if (is.na(or) || is.na(pval) || pval > 0.05) {
    # 1) missing OR or non‐significant → white
    "#D3D3D380"
  } else if (pval < 0.05 && or < 0) {
    # 2) significant & OR < 1 → semi‐transparent red
    "#9900004D"
  } else if (pval < 0.05 && or > 0) {
    # 3) significant & OR > 1 → semi‐transparent blue
    "#023E8A4D"
  } else {
    # other (e.g. OR == 0) → white
    "#D3D3D380"
  }
}

# open a 400 dpi PNG
png("./Figure3_MR_circular.png",
    width  = 510, height = 510,
    units  = "mm", res    = 400)

# set inner margins (bottom, left, top, right) in lines
par(mar = c(1, 1, 1, 1),
    # add a half-inch outer margin on all sides (bcz labels are outside plot)
    oma = c(11, 11, 11, 11),
    xpd = NA,
    family = "Arial")

# initialize circos
circos.clear()
circos.par(start.degree = 50, gap.degree = 40, cell.padding = c(0,0,0,0),
           track.margin = c(0.005,0.005))
circos.initialize("all", xlim = c(0.5, n_cat + 0.5))

# function to choose color by p-value
choose_col <- function(p, col) if(!is.na(p) && p < 0.05) col else adjustcolor(col, alpha.f = 0.4)

# Measurement separator:
sep <- 0.2

# ---- Track 1: order1 (circle) & order2 (triangle) ----
circos.trackPlotRegion(
  track.index  = 1,
  ylim         = ylim1,
  track.height = 0.20,
  bg.border    = NA,
  panel.fun    = function(x, y) {
    cell_ylim <- get.cell.meta.data("cell.ylim")
    for(i in seq_len(n_cat)) {
      # background color
      circos.rect(i-0.5, cell_ylim[1], i+0.5, cell_ylim[2],
                  col    = bg_col_fun(st1$est[i], st1$pval[i]),
                  border = NA)
      # draw CI for Measure 1
      if(!is.na(st1$lower[i]) && !is.na(st1$upper[i])) {
        if(st1$est[i] < 0) {
          circos.segments(i - sep, st1$lower[i],
                          i - sep, st1$upper[i],
                          col = "#990000")
        } else {
          circos.segments(i - sep, st1$lower[i],
                          i - sep, st1$upper[i],
                          col = "#023E8A")
        }
      }
      # draw CI for Measure 2
      if(!is.na(st2$lower[i]) && !is.na(st2$upper[i])) {
        if(st2$est[i] < 0) {
          circos.segments(i + sep, st2$lower[i],
                          i + sep, st2$upper[i],
                          col = "#990000")
        } else {
          circos.segments(i + sep, st2$lower[i],
                          i + sep, st2$upper[i],
                          col = "#023E8A")
        }
      }
      
      # ─── points for Measure 1 ────────────────────────────────
      if(!is.na(st1$est[i])) {
        if(st1$pval[i] < 0.05) {
          if(st1$est[i] < 0) {
            # significant & OR < 1 → filled red circle
            circos.points(i - sep, st1$est[i],
                          pch = 16, cex = 2.5, col = "#990000")
          } else {
            # significant & OR > 1 → filled blue circle
            circos.points(i - sep, st1$est[i],
                          pch = 16, cex = 2.5, col = "#023E8A")
          }
        } else {
          # non-significant
          circos.points(i, st1$est[i],
                        pch = 1, cex = 2.5, col = "black")
        }
      }
      
      # ─── points for Measure 2 ────────────────────────────────
      if(!is.na(st2$est[i])) {
        if(st2$pval[i] < 0.05) {
          if(st2$est[i] < 0) {
            # significant & OR < 1 → filled red circle
            circos.points(i + sep, st2$est[i],
                          pch = 17, cex = 2.5, col = "#990000")
          } else {
            # significant & OR > 1 → filled blue circle
            circos.points(i + sep, st2$est[i],
                          pch = 17, cex = 2.5, col = "#023E8A")
          }
        } else {
          # non-significant & OR < 1 → red circle
          circos.points(i + sep, st2$est[i],
                        pch = 2, cex = 2.5, col = "black")
        }
      }
    }
    # reference line at OR = 1
    circos.lines(c(0.5, n_cat + 0.5), c(0, 0), lty = "longdash", col = "black")
    # y-axis
    circos.yaxis(side = "left", at = c(-1, 0, 1), labels.cex = 1.5)
    # Track Name
    circos.text(
      x            = 64,  # shift left of the axis a bit
      y            = 0,
      labels       = "Disease\n\nLongevity",
      sector.index = "all",
      track.index  = 1,
      facing       = "downward",   # rotates the text to follow the circle
      niceFacing   = TRUE,          # keeps it upright
      adj          = c(0.55, 0.45),   # center the text vertically and horizontally
      cex          = 2,            # label size
      font         = 2               # bold font for the track name
    )
  }
)

# x-axis labels for track 1
circos.axis(h = "top", track.index = 1, major.at = seq_len(n_cat), labels = categories,
            labels.cex = 1.8, labels.facing = "clockwise", direction = "outside", major.tick = FALSE)

# separator
circos.trackPlotRegion(track.index = 2, ylim = c(0,1), track.height = 0.02, bg.border = NA, panel.fun = function(x,y){})

# ---- Track 5: order5 & order6 ----
circos.trackPlotRegion(
  track.index  = 3,
  ylim         = ylim5,
  track.height = 0.20,
  bg.border    = NA,
  panel.fun    = function(x, y) {
    cell_ylim <- get.cell.meta.data("cell.ylim")
    for(i in seq_len(n_cat)) {
      # background color
      circos.rect(i-0.5, cell_ylim[1], i+0.5, cell_ylim[2],
                  col    = bg_col_fun(st5$est[i], st5$pval[i]),
                  border = NA)
      # draw CI for Measure 1
      if(!is.na(st5$lower[i]) && !is.na(st5$upper[i])) {
        if(st5$est[i] < 0) {
          circos.segments(i - sep, st5$lower[i],
                          i - sep, st5$upper[i],
                          col = "#990000")
        } else {
          circos.segments(i - sep, st5$lower[i],
                          i - sep, st5$upper[i],
                          col = "#023E8A")
        }
      }
      # draw CI for Measure 2
      if(!is.na(st6$lower[i]) && !is.na(st6$upper[i])) {
        if(st6$est[i] < 0) {
          circos.segments(i + sep, st6$lower[i],
                          i + sep, st6$upper[i],
                          col = "#990000")
        } else {
          circos.segments(i + sep, st6$lower[i],
                          i + sep, st6$upper[i],
                          col = "#023E8A")
        }
      }
      
      # ─── points for Measure 1 ────────────────────────────────
      if(!is.na(st5$est[i])) {
        if(st5$pval[i] < 0.05) {
          if(st5$est[i] < 0) {
            # significant & OR < 1 → filled red circle
            circos.points(i - sep, st5$est[i],
                          pch = 16, cex = 2.5, col = "#990000")
          } else {
            # significant & OR > 1 → filled blue circle
            circos.points(i - sep, st5$est[i],
                          pch = 16, cex = 2.5, col = "#023E8A")
          }
        } else {
          # non-significant
          circos.points(i, st5$est[i],
                        pch = 1, cex = 2.5, col = "black")
        }
      }
      
      # ─── points for Measure 2 ────────────────────────────────
      if(!is.na(st6$est[i])) {
        if(st6$pval[i] < 0.05) {
          if(st6$est[i] < 0) {
            # significant & OR < 1 → filled red circle
            circos.points(i + sep, st6$est[i],
                          pch = 17, cex = 2.5, col = "#990000")
          } else {
            # significant & OR > 1 → filled blue circle
            circos.points(i + sep, st6$est[i],
                          pch = 17, cex = 2.5, col = "#023E8A")
          }
        } else {
          # non-significant & OR < 1 → red circle
          circos.points(i + sep, st6$est[i],
                        pch = 2, cex = 2.5, col = "black")
        }
      }
    }
    # reference line at OR = 1
    circos.lines(c(0.5, n_cat + 0.5), c(0, 0), lty = "longdash", col = "black")
    # y-axis
    circos.yaxis(side = "left", at = c(-1, 0, 1), labels.cex = 1.5)
    circos.text(
      x            = -1,
      y            = 0,
      labels       = "log(OR)",
      sector.index = "all",
      track.index  = 3,
      facing       = "clockwise",
      niceFacing   = FALSE,
      adj          = c(0.4, 0.6),
      cex          = 2,
      font = 3                     # italic
    )
    # Track Name
    circos.text(
      x            = 64,
      y            = 0,
      labels       = "Disease\n\nFertility",
      sector.index = "all",
      track.index  = 3,
      facing       = "downward",
      niceFacing   = TRUE,
      adj          = c(0.35, 0.45),
      cex          = 2,
      font         = 2
    )
  }
)

# separator
circos.trackPlotRegion(track.index = 4, ylim = c(0,1), track.height = 0.02, bg.border = NA, panel.fun = function(x,y){})

# ---- Track 3: order3 & order4 ----
circos.trackPlotRegion(
  track.index  = 5,
  ylim         = ylim3,
  track.height = 0.20,
  bg.border    = NA,
  panel.fun    = function(x, y) {
    cell_ylim <- get.cell.meta.data("cell.ylim")
    for(i in seq_len(n_cat)) {
      # background color
      circos.rect(i-0.5, cell_ylim[1], i+0.5, cell_ylim[2],
                  col    = bg_col_fun(st3$est[i], st3$pval[i]),
                  border = NA)
      # draw CI for Measure 1
      if(!is.na(st3$lower[i]) && !is.na(st3$upper[i])) {
        if(st3$est[i] < 0) {
          circos.segments(i - sep, st3$lower[i],
                          i - sep, st3$upper[i],
                          col = "#990000")
        } else {
          circos.segments(i - sep, st3$lower[i],
                          i - sep, st3$upper[i],
                          col = "#023E8A")
        }
      }
      # draw CI for Measure 2
      if(!is.na(st4$lower[i]) && !is.na(st4$upper[i])) {
        if(st4$est[i] < 0) {
          circos.segments(i + sep, st4$lower[i],
                          i + sep, st4$upper[i],
                          col = "#990000")
        } else {
          circos.segments(i + sep, st4$lower[i],
                          i + sep, st4$upper[i],
                          col = "#023E8A")
        }
      }
      
      # ─── points for Measure 1 ────────────────────────────────
      if(!is.na(st3$est[i])) {
        if(st3$pval[i] < 0.05) {
          if(st3$est[i] < 0) {
            # significant & OR < 1 → filled red circle
            circos.points(i - sep, st3$est[i],
                          pch = 16, cex = 2.5, col = "#990000")
          } else {
            # significant & OR > 1 → filled blue circle
            circos.points(i - sep, st3$est[i],
                          pch = 16, cex = 2.5, col = "#023E8A")
          }
        } else {
          # non-significant
          circos.points(i, st3$est[i],
                        pch = 1, cex = 2.5, col = "black")
        }
      }
      
      # ─── points for Measure 2 ────────────────────────────────
      if(!is.na(st4$est[i])) {
        if(st4$pval[i] < 0.05) {
          if(st4$est[i] < 0) {
            # significant & OR < 1 → filled red circle
            circos.points(i + sep, st4$est[i],
                          pch = 17, cex = 2.5, col = "#990000")
          } else {
            # significant & OR > 1 → filled blue circle
            circos.points(i + sep, st4$est[i],
                          pch = 17, cex = 2.5, col = "#023E8A")
          }
        } else {
          # non-significant & OR < 1 → red circle
          circos.points(i + sep, st4$est[i],
                        pch = 2, cex = 2.5, col = "black")
        }
      }
    }
    # reference line at OR = 1
    circos.lines(c(0.5, n_cat + 0.5), c(0, 0), lty = "longdash", col = "black")
    # y-axis
    circos.yaxis(side = "left", at = c(-1, 0, 1), labels.cex = 1.5)
    # Track Name
    circos.text(
      x            = 64,
      y            = 0,
      labels       = "Fertility\n\nDisease",
      sector.index = "all",
      track.index  = 5,
      facing       = "downward",
      niceFacing   = TRUE,
      adj          = c(0.15, 0.55),
      cex          = 2,
      font         = 2
    )
  }
)

# separator
circos.trackPlotRegion(track.index = 6, ylim = c(0,1), track.height = 0.02, bg.border = NA, panel.fun = function(x,y){})

# Onset‐age heat-map (Track 6)
ages <- order1$ADO_5

# build a color ramp from low → high
col_fun_age <- colorRamp2(
  c(min(ages, na.rm = TRUE), max(ages, na.rm = TRUE)),
  c("#FEE08B", "#B7E7A7")
)

# draw the track
circos.trackPlotRegion(
  track.index  = 7,
  ylim         = c(0, 1),       # fill from y=0 to y=1
  track.height = 0.10,
  bg.border    = NA,
  panel.fun    = function(x, y) {
    for(i in seq_len(n_cat)) {
      ai <- ages[i]
      if (!is.na(ai)) {
        circos.rect(
          xleft   = i - 0.5,
          ybottom = 0,
          xright  = i + 0.5,
          ytop    = 1,
          col     = col_fun_age(ai),
          border  = NA
        )
      }
    }
    circos.text(
      x            = 61,  # shift right of the axis a bit
      y            = 0.5,
      labels       = "65",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),   # center
      cex          = 2,            # label size
      font = 2
    )
    circos.text(
      x            = 57.5,
      y            = 0.5,
      labels       = "50",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),
      cex          = 2,
      font = 2
    )
    circos.text(
      x            = 51,
      y            = 0.5,
      labels       = "45",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),
      cex          = 2,
      font = 2
    )
    circos.text(
      x            = 39,
      y            = 0.5,
      labels       = "40",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),
      cex          = 2,
      font = 2
    )
    circos.text(
      x            = 32,
      y            = 0.5,
      labels       = "30",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),
      cex          = 2,
      font = 2
    )
    circos.text(
      x            = 23,
      y            = 0.5,
      labels       = "20",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),
      cex          = 2,
      font = 2
    )
    circos.text(
      x            = 14,
      y            = 0.5,
      labels       = "10",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),
      cex          = 2,
      font = 2
    )
    circos.text(
      x            = 1.5,
      y            = 0.5,
      labels       = "1",
      sector.index = "all",
      track.index  = 7,
      facing       = "inside",
      adj          = c(0.5, 0.5),
      cex          = 2,
      font = 2
    )
  }
)

# Legend
# allow legend to draw outside the plot region
par(xpd = NA)

x_leg <- grconvertX(0.80, from = "ndc", to = "user")
y_leg <- grconvertY(0.99, from = "ndc", to = "user")

legend(x = x_leg,
       y = y_leg,
       legend   = c("Univariate MR",
                    "Multivariate MR"),
       pch      = c(16, 17),   # filled/open circle, filled/open triangle
       col      = c("black","black"),
       pt.cex   = 3,               # point size in legend
       cex = 2,
       bty      = "y",               # no box around legend
)


# finalize
circos.clear()
dev.off()

