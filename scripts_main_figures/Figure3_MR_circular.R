library(circlize)
library(grDevices)
library(dplyr)

# =========================
# General Inputs
# =========================
names <- read.csv("disease_names.csv", sep = ";")
age <- read.csv("onset_age_62diseases_v17.01.2025_FinRegistryR11.tsv", sep = "\t")
age$ADO_5[age$DISEASE_CODE == "GCST010003"] <- 1.27

# Manually change disease names
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

# =========================
# Inputs Longevity UVMR
# =========================
long_uvmr <- read.csv("outcome_longevity_simpleMR_62diseases.csv", sep = ",")
long_uvmr_v1 <- merge(long_uvmr, age, by.x = "Exposure", by.y = "DISEASE_CODE")
long_uvmr_v2 <- merge(long_uvmr_v1, names, by.x = "Exposure", by.y = "Code")
order1 <- long_uvmr_v2[order(long_uvmr_v2$ADO_5), ]

# =========================
# Inputs Longevity MVMR-Education
# =========================
long_mvmr_edu <- read.csv("outcome_longevity_multivariableMR_62diseases.csv", sep = ",")
long_mvmr_edu_filtered <- subset(long_mvmr_edu, is.na(Method) | Method == "IVW")
long_mvmr_edu_v1 <- merge(long_mvmr_edu_filtered, age, by.x = "Exposure1", by.y = "DISEASE_CODE")
long_mvmr_edu_v2 <- merge(long_mvmr_edu_v1, names, by.x = "Exposure1", by.y = "Code")
order2 <- long_mvmr_edu_v2[order(long_mvmr_edu_v2$ADO_5), ]

# =========================
# Inputs Longevity MVMR-Income
# =========================
long_mvmr_inc <- read.csv("longevity_outcome_mvMR_income.csv", sep = ",")
long_mvmr_inc_filtered <- subset(long_mvmr_inc, is.na(Method) | Method == "IVW")
long_mvmr_inc_v1 <- merge(long_mvmr_inc_filtered, age, by.x = "Exposure1", by.y = "DISEASE_CODE")
long_mvmr_inc_v2 <- merge(long_mvmr_inc_v1, names, by.x = "Exposure1", by.y = "Code")
order2b <- long_mvmr_inc_v2[order(long_mvmr_inc_v2$ADO_5), ]

# =========================
# Inputs Fertility Exposure UVMR
# =========================
fert_exp_uvmr <- read.csv("exposure_fertility_simpleMR_62diseases.csv", sep = ",")
fert_exp_uvmr_v1 <- merge(fert_exp_uvmr, age, by.x = "Outcome", by.y = "DISEASE_CODE")
fert_exp_uvmr_v2 <- merge(fert_exp_uvmr_v1, names, by.x = "Outcome", by.y = "Code")
order3 <- fert_exp_uvmr_v2[order(fert_exp_uvmr_v2$ADO_5), ]

# =========================
# Inputs Fertility Exposure MVMR-Education
# =========================
fert_exp_mvmr_edu <- read.csv("exposure_fertility_multivariableMR_62diseases.csv", sep = ",")
fert_exp_mvmr_edu_filtered <- subset(fert_exp_mvmr_edu, is.na(Method) | Method == "IVW")
fert_exp_mvmr_edu_v1 <- merge(fert_exp_mvmr_edu_filtered, age, by.x = "Outcome", by.y = "DISEASE_CODE")
fert_exp_mvmr_edu_v2 <- merge(fert_exp_mvmr_edu_v1, names, by.x = "Outcome", by.y = "Code")
order4 <- fert_exp_mvmr_edu_v2[order(fert_exp_mvmr_edu_v2$ADO_5), ]

# =========================
# Inputs Fertility Exposure MVMR-Income
# =========================
fert_exp_mvmr_inc <- read.csv("fertility_exposure_mvMR_income.csv", sep = ",")
fert_exp_mvmr_inc_filtered <- subset(fert_exp_mvmr_inc, is.na(Method) | Method == "IVW")
fert_exp_mvmr_inc_v1 <- merge(fert_exp_mvmr_inc_filtered, age, by.x = "Outcome", by.y = "DISEASE_CODE")
fert_exp_mvmr_inc_v2 <- merge(fert_exp_mvmr_inc_v1, names, by.x = "Outcome", by.y = "Code")
order4b <- fert_exp_mvmr_inc_v2[order(fert_exp_mvmr_inc_v2$ADO_5), ]

# =========================
# Inputs Fertility Outcome UVMR
# =========================
fert_out_uvmr <- read.csv("outcome_fertility_simpleMR_62diseases.csv", sep = ",")
fert_out_uvmr_v1 <- merge(fert_out_uvmr, age, by.x = "Exposure", by.y = "DISEASE_CODE")
fert_out_uvmr_v2 <- merge(fert_out_uvmr_v1, names, by.x = "Exposure", by.y = "Code")
order5 <- fert_out_uvmr_v2[order(fert_out_uvmr_v2$ADO_5), ]

# =========================
# Inputs Fertility Outcome MVMR-Education
# =========================
fert_out_mvmr_edu <- read.csv("outcome_fertility_multivariableMR_62diseases.csv", sep = ",")
fert_out_mvmr_edu_filtered <- subset(fert_out_mvmr_edu, is.na(Method) | Method == "IVW")
fert_out_mvmr_edu_v1 <- merge(fert_out_mvmr_edu_filtered, age, by.x = "Exposure1", by.y = "DISEASE_CODE")
fert_out_mvmr_edu_v2 <- merge(fert_out_mvmr_edu_v1, names, by.x = "Exposure1", by.y = "Code")
order6 <- fert_out_mvmr_edu_v2[order(fert_out_mvmr_edu_v2$ADO_5), ]

# =========================
# Inputs Fertility Outcome MVMR-Income
# =========================
fert_out_mvmr_inc <- read.csv("fertility_outcome_mvMR_income.csv", sep = ",")
fert_out_mvmr_inc_filtered <- subset(fert_out_mvmr_inc, is.na(Method) | Method == "IVW")
fert_out_mvmr_inc_v1 <- merge(fert_out_mvmr_inc_filtered, age, by.x = "Exposure1", by.y = "DISEASE_CODE")
fert_out_mvmr_inc_v2 <- merge(fert_out_mvmr_inc_v1, names, by.x = "Exposure1", by.y = "Code")
order6b <- fert_out_mvmr_inc_v2[order(fert_out_mvmr_inc_v2$ADO_5), ]

# =========================
# Clean non-significant CI
# =========================
for (i in which(order1$Significance == "NS")) {
  order1[i, 8:9] <- NA
}

for (i in which(order2$Significance_Exposure1 == "NS")) {
  order2[i, 12:13] <- NA
}

for (i in which(order2b$Significance_Exposure1 == "NS")) {
  order2b[i, 12:13] <- NA
}

for (i in which(order3$Significance == "NS")) {
  order3[i, 8:9] <- NA
}

for (i in which(order4$Significance_Exposure1 == "NS")) {
  order4[i, 12:13] <- NA
}

for (i in which(order4b$Significance_Exposure1 == "NS")) {
  order4b[i, 12:13] <- NA
}

for (i in which(order5$Significance == "NS")) {
  order5[i, 8:9] <- NA
}

for (i in which(order6$Significance_Exposure1 == "NS")) {
  order6[i, 12:13] <- NA
}

for (i in which(order6b$Significance_Exposure1 == "NS")) {
  order6b[i, 12:13] <- NA
}

# =========================
# Introduce NA values when Significance on the UVMR is not available
# =========================
for (i in which(is.na(order1$Significance))) {
  order2[i, 5:22] <- NA
  order2[i, 28:29] <- NA
  order2b[i, 5:22] <- NA
  order2b[i, 28:29] <- NA
}

for (i in which(is.na(order3$Significance))) {
  order4[i, 5:22] <- NA
  order4[i, 28:29] <- NA
  order4b[i, 5:22] <- NA
  order4b[i, 28:29] <- NA
}

for (i in which(is.na(order5$Significance))) {
  order6[i, 5:22] <- NA
  order6[i, 28:29] <- NA
  order6b[i, 5:22] <- NA
  order6b[i, 28:29] <- NA
}

# =========================
# Disease-age restrictions
# =========================
# For fertility as exposure remove diseases with ADO < 5
for (i in which(order3$ADO_5 < 5)) {
  order3[i, 4:9] <- NA
  order4[i, 5:7] <- NA
  order4[i, 11:13] <- NA
  order4b[i, 5:7] <- NA
  order4b[i, 11:13] <- NA
}

# For fertility as outcome remove diseases with ADO > 45
for (i in which(order5$ADO_5 > 45)) {
  order5[i, 4:9] <- NA
  order6[i, 5:7] <- NA
  order6[i, 11:13] <- NA
  order6b[i, 5:7] <- NA
  order6b[i, 11:13] <- NA
}

# =========================
# Compute log(OR)
# =========================
order1$OR <- log(order1$OR)
order2$OR_Exposure1 <- log(order2$OR_Exposure1)
order2b$OR_Exposure1 <- log(order2b$OR_Exposure1)

order3$OR <- log(order3$OR)
order4$OR_Exposure1 <- log(order4$OR_Exposure1)
order4b$OR_Exposure1 <- log(order4b$OR_Exposure1)

order5$OR <- log(order5$OR)
order6$OR_Exposure1 <- log(order6$OR_Exposure1)
order6b$OR_Exposure1 <- log(order6b$OR_Exposure1)

# Compute log(OR) confidence intervals
order1$Lower_OR_CI_95. <- log(order1$Lower_OR_CI_95.)
order1$Upper_OR_CI_95. <- log(order1$Upper_OR_CI_95.)

order2$Lower_OR_CI_95._Exposure1 <- log(order2$Lower_OR_CI_95._Exposure1)
order2$Upper_OR_CI_95._Exposure1 <- log(order2$Upper_OR_CI_95._Exposure1)

order2b$Lower_OR_CI_95._Exposure1 <- log(order2b$Lower_OR_CI_95._Exposure1)
order2b$Upper_OR_CI_95._Exposure1 <- log(order2b$Upper_OR_CI_95._Exposure1)

order3$Lower_OR_CI_95. <- log(order3$Lower_OR_CI_95.)
order3$Upper_OR_CI_95. <- log(order3$Upper_OR_CI_95.)

order4$Lower_OR_CI_95._Exposure1 <- log(order4$Lower_OR_CI_95._Exposure1)
order4$Upper_OR_CI_95._Exposure1 <- log(order4$Upper_OR_CI_95._Exposure1)

order4b$Lower_OR_CI_95._Exposure1 <- log(order4b$Lower_OR_CI_95._Exposure1)
order4b$Upper_OR_CI_95._Exposure1 <- log(order4b$Upper_OR_CI_95._Exposure1)

order5$Lower_OR_CI_95. <- log(order5$Lower_OR_CI_95.)
order5$Upper_OR_CI_95. <- log(order5$Upper_OR_CI_95.)

order6$Lower_OR_CI_95._Exposure1 <- log(order6$Lower_OR_CI_95._Exposure1)
order6$Upper_OR_CI_95._Exposure1 <- log(order6$Upper_OR_CI_95._Exposure1)

order6b$Lower_OR_CI_95._Exposure1 <- log(order6b$Lower_OR_CI_95._Exposure1)
order6b$Upper_OR_CI_95._Exposure1 <- log(order6b$Upper_OR_CI_95._Exposure1)

# =========================
# General settings
# =========================
n_cat <- 62
categories <- order1$DiseaseName

get_stats <- function(df, or_col, lower_col, upper_col, p_col) {
  list(
    est   = df[[or_col]],
    lower = df[[lower_col]],
    upper = df[[upper_col]],
    pval  = df[[p_col]]
  )
}

# UVMR + MVMR-Education + MVMR-Income
st1  <- get_stats(order1,  "OR",               "Lower_OR_CI_95.",           "Upper_OR_CI_95.",           "Pvalue")
st2  <- get_stats(order2,  "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")
st2b <- get_stats(order2b, "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")

st3  <- get_stats(order3,  "OR",               "Lower_OR_CI_95.",           "Upper_OR_CI_95.",           "Pvalue")
st4  <- get_stats(order4,  "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")
st4b <- get_stats(order4b, "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")

st5  <- get_stats(order5,  "OR",               "Lower_OR_CI_95.",           "Upper_OR_CI_95.",           "Pvalue")
st6  <- get_stats(order6,  "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")
st6b <- get_stats(order6b, "OR_Exposure1",     "Lower_OR_CI_95._Exposure1", "Upper_OR_CI_95._Exposure1", "Pvalue_Exposure1")

ylim1 <- c(-1, 1)
ylim3 <- c(-1.5, 1)
ylim5 <- c(-1, 1)

bg_col_fun <- function(or, pval) {
  if (is.na(or) || is.na(pval) || pval > 0.05) {
    "#D3D3D380"
  } else if (pval < 0.05 && or < 0) {
    "#9900004D"
  } else if (pval < 0.05 && or > 0) {
    "#023E8A4D"
  } else {
    "#D3D3D380"
  }
}

clip_to_ylim <- function(x, ylim, eps = 0.02) {
  ifelse(
    is.na(x), NA,
    pmax(pmin(x, ylim[2] - eps), ylim[1] + eps)
  )
}

point_cex <- 0.7
draw_point <- function(x, y, pval, pch_sig, pch_ns, cex = point_cex) {
  if (is.na(y)) return(invisible(NULL))
  
  if (!is.na(pval) && pval < 0.05) {
    if (y < 0) {
      circos.points(x, y, pch = pch_sig, cex = point_cex, col = "#990000")
    } else {
      circos.points(x, y, pch = pch_sig, cex = point_cex, col = "#023E8A")
    }
  } else {
    par(lwd = 0.5)
    circos.points(x, y, pch = pch_ns, cex = point_cex, col = "black")
  }
}

draw_ci <- function(x, est, lower, upper, ylim) {
  if (is.na(lower) || is.na(upper) || is.na(est)) return(invisible(NULL))
  
  lower2 <- clip_to_ylim(lower, ylim)
  upper2 <- clip_to_ylim(upper, ylim)
  
  if (est < 0) {
    circos.segments(x, lower2, x, upper2, col = "#990000")
  } else {
    circos.segments(x, lower2, x, upper2, col = "#023E8A")
  }
}

# =========================
# Variable widths for less-informative diseases
# =========================

find_method_col <- function(df) {
  candidates <- c("MR Method", "MR_Method", "MR.Method", "Method")
  out <- candidates[candidates %in% colnames(df)]
  if (length(out) == 0) return(NA_character_)
  out[1]
}

method_col1 <- find_method_col(order1)
method_col3 <- find_method_col(order3)
method_col5 <- find_method_col(order5)

mr1_na <- if (!is.na(method_col1)) is.na(order1[[method_col1]]) else rep(FALSE, nrow(order1))
mr3_na <- if (!is.na(method_col3)) is.na(order3[[method_col3]]) else rep(FALSE, nrow(order3))
mr5_na <- if (!is.na(method_col5)) is.na(order5[[method_col5]]) else rep(FALSE, nrow(order5))

shrink_disease <- (
  order1$Significance == "NS" &
    order3$Significance == "NS" &
    order5$Significance == "NS"
) | (
  mr1_na & mr3_na & mr5_na
)

shrink_disease[is.na(shrink_disease)] <- FALSE

full_width   <- 1.00
small_width  <- 0.55
cat_widths   <- ifelse(shrink_disease, small_width, full_width)

x_left  <- c(0, cumsum(cat_widths)[-n_cat])
x_right <- cumsum(cat_widths)
x_mid   <- (x_left + x_right) / 2
total_width <- sum(cat_widths)

get_offsets <- function(i) {
  w <- cat_widths[i]
  c(
    uv  = -0.24 * w,
    edu =  0.00 * w,
    inc =  0.24 * w
  )
}

# =========================
# Center for NS
# =========================
get_offsets <- function(i, st_uv, st_edu, st_inc) {
  w <- cat_widths[i]
  
  has_uv  <- !is.na(st_uv$est[i])
  has_edu <- !is.na(st_edu$est[i])
  has_inc <- !is.na(st_inc$est[i])
  
  delta <- 0.18 * w
  
  out <- c(uv = NA, edu = NA, inc = NA)
  
  present <- c(uv = has_uv, edu = has_edu, inc = has_inc)
  n_present <- sum(present)
  
  if (n_present == 1) {
    out[names(present)[present]] <- 0
  } else if (n_present == 2) {
    present_names <- names(present)[present]
    out[present_names[1]] <- -delta
    out[present_names[2]] <-  delta
  } else if (n_present == 3) {
    out["uv"]  <- -0.24 * w
    out["edu"] <-  0.00 * w
    out["inc"] <-  0.24 * w
  }
  
  out
}

# =========================
# PDF output
# =========================
pdf(
  file = "Figure3_MR_circular_education_income.pdf",
  width = 180 / 25.4,
  height = 180 / 25.4,
  family = "Helvetica",
  useDingbats = FALSE
)

par(
  mar = c(1, 1, 1, 1),
  oma = c(1.9, 1.9, 2.1, 1.9),
  xpd = NA,
  family = "Helvetica",
  ps = 7
)
par(lwd = 1)

cex_lines <- 5 / 7
cex_disease <- 6 / 7  # disease names = 6 pt
cex_other   <- 1

circos.clear()
circos.par(
  start.degree = 55,
  gap.degree = 35,
  cell.padding = c(0, 0, 0, 0),
  track.margin = c(0.005, 0.005)
)

circos.initialize("all", xlim = c(0, total_width))

# =========================
# Track 1: Disease -> Longevity
# =========================
circos.trackPlotRegion(
  track.index = 1,
  ylim = ylim1,
  track.height = 0.23,
  bg.border = NA,
  panel.fun = function(x, y) {
    cell_ylim <- get.cell.meta.data("cell.ylim")
    
    for (i in seq_len(n_cat)) {
      offs <- get_offsets(i, st1, st2, st2b)
      
      circos.rect(
        x_left[i], cell_ylim[1], x_right[i], cell_ylim[2],
        col = bg_col_fun(st1$est[i], st1$pval[i]),
        border = NA
      )
      
      draw_ci(x_mid[i] + offs["uv"],  st1$est[i],  st1$lower[i],  st1$upper[i],  ylim1)
      draw_ci(x_mid[i] + offs["edu"], st2$est[i],  st2$lower[i],  st2$upper[i],  ylim1)
      draw_ci(x_mid[i] + offs["inc"], st2b$est[i], st2b$lower[i], st2b$upper[i], ylim1)
      
      draw_point(x_mid[i] + offs["uv"],  clip_to_ylim(st1$est[i],  ylim1), st1$pval[i],  pch_sig = 16, pch_ns = 1, cex = 2)
      draw_point(x_mid[i] + offs["edu"], clip_to_ylim(st2$est[i],  ylim1), st2$pval[i],  pch_sig = 17, pch_ns = 2, cex = 2)
      draw_point(x_mid[i] + offs["inc"], clip_to_ylim(st2b$est[i], ylim1), st2b$pval[i], pch_sig = 15, pch_ns = 0, cex = 2)
    }
    
    par(lwd = 1)
    circos.lines(c(0, total_width), c(0, 0), lty = "longdash", col = "black", lwd = 0.5)
    circos.yaxis(side = "left", at = c(-1, 0, 1), labels.cex = cex_lines)
    
    circos.text(
      x = total_width + 1.2,
      y = 0,
      labels = "Disease   \n\n\nLongevity",
      sector.index = "all",
      track.index = 1,
      facing = "downward",
      niceFacing = TRUE,
      adj = c(0.68, 0.45),
      cex = cex_disease,
      font = 2
    )
  }
)

circos.axis(
  h = "top",
  track.index = 1,
  major.at = x_mid,
  labels = categories,
  labels.cex = cex_disease,
  labels.facing = "clockwise",
  direction = "outside",
  major.tick = FALSE
)

# separator
circos.trackPlotRegion(
  track.index = 2, ylim = c(0,1), track.height = 0.02, bg.border = NA,
  panel.fun = function(x,y){}
)

# =========================
# Track 3: Disease -> Fertility
# =========================
circos.trackPlotRegion(
  track.index = 3,
  ylim = ylim5,
  track.height = 0.23,
  bg.border = NA,
  panel.fun = function(x, y) {
    cell_ylim <- get.cell.meta.data("cell.ylim")
    
    for (i in seq_len(n_cat)) {
      offs <- get_offsets(i, st5, st6, st6b)
      
      circos.rect(
        x_left[i], cell_ylim[1], x_right[i], cell_ylim[2],
        col = bg_col_fun(st5$est[i], st5$pval[i]),
        border = NA
      )
      
      draw_ci(x_mid[i] + offs["uv"],  st5$est[i],  st5$lower[i],  st5$upper[i],  ylim5)
      draw_ci(x_mid[i] + offs["edu"], st6$est[i],  st6$lower[i],  st6$upper[i],  ylim5)
      draw_ci(x_mid[i] + offs["inc"], st6b$est[i], st6b$lower[i], st6b$upper[i], ylim5)
      
      draw_point(x_mid[i] + offs["uv"],  clip_to_ylim(st5$est[i],  ylim5), st5$pval[i],  pch_sig = 16, pch_ns = 1, cex = 2)
      draw_point(x_mid[i] + offs["edu"], clip_to_ylim(st6$est[i],  ylim5), st6$pval[i],  pch_sig = 17, pch_ns = 2, cex = 2)
      draw_point(x_mid[i] + offs["inc"], clip_to_ylim(st6b$est[i], ylim5), st6b$pval[i], pch_sig = 15, pch_ns = 0, cex = 2)
    }
    
    par(lwd = 1)
    circos.lines(c(0, total_width), c(0, 0), lty = "longdash", col = "black", lwd = 0.5)
    circos.yaxis(side = "left", at = c(-1, 0, 1), labels.cex = cex_lines)
    
    circos.text(
      x = -1.1,
      y = 0,
      labels = "log(OR)",
      sector.index = "all",
      track.index = 3,
      facing = "clockwise",
      niceFacing = FALSE,
      adj = c(0.4, 0.8),
      cex = cex_lines,
      font = 3
    )
    
    circos.text(
      x = total_width + 1.2,
      y = 0,
      labels = "Disease\n\n\nFertility",
      sector.index = "all",
      track.index = 3,
      facing = "downward",
      niceFacing = TRUE,
      adj = c(0.58, 0.45),
      cex = cex_disease,
      font = 2
    )
  }
)

# separator
circos.trackPlotRegion(
  track.index = 4, ylim = c(0,1), track.height = 0.02, bg.border = NA,
  panel.fun = function(x,y){}
)

# =========================
# Track 5: Fertility -> Disease
# =========================
circos.trackPlotRegion(
  track.index = 5,
  ylim = ylim3,
  track.height = 0.23,
  bg.border = NA,
  panel.fun = function(x, y) {
    cell_ylim <- get.cell.meta.data("cell.ylim")
    
    for (i in seq_len(n_cat)) {
      offs <- get_offsets(i, st3, st4, st4b)
      
      circos.rect(
        x_left[i], cell_ylim[1], x_right[i], cell_ylim[2],
        col = bg_col_fun(st3$est[i], st3$pval[i]),
        border = NA
      )
      
      draw_ci(x_mid[i] + offs["uv"],  st3$est[i],  st3$lower[i],  st3$upper[i],  ylim3)
      draw_ci(x_mid[i] + offs["edu"], st4$est[i],  st4$lower[i],  st4$upper[i],  ylim3)
      draw_ci(x_mid[i] + offs["inc"], st4b$est[i], st4b$lower[i], st4b$upper[i], ylim3)
      
      draw_point(x_mid[i] + offs["uv"],  clip_to_ylim(st3$est[i],  ylim3), st3$pval[i],  pch_sig = 16, pch_ns = 1, cex = 2)
      draw_point(x_mid[i] + offs["edu"], clip_to_ylim(st4$est[i],  ylim3), st4$pval[i],  pch_sig = 17, pch_ns = 2, cex = 2)
      draw_point(x_mid[i] + offs["inc"], clip_to_ylim(st4b$est[i], ylim3), st4b$pval[i], pch_sig = 15, pch_ns = 0, cex = 2)
    }
    
    par(lwd = 1)
    circos.lines(c(0, total_width), c(0, 0), lty = "longdash", col = "black", lwd = 0.5)
    circos.yaxis(side = "left", at = c(-1, 0, 1), labels.cex = cex_lines)
    
    circos.text(
      x = total_width + 1.2,
      y = 0,
      labels = "Fertility\n\n\nDisease",
      sector.index = "all",
      track.index = 5,
      facing = "downward",
      niceFacing = TRUE,
      adj = c(0.33, 0.55),
      cex = cex_disease,
      font = 2,
      family = "Helvetica"
    )
  }
)

# separator
circos.trackPlotRegion(
  track.index = 6, ylim = c(0,1), track.height = 0.02, bg.border = NA,
  panel.fun = function(x,y){}
)

# =========================
# Onset-age heatmap
# =========================
ages <- order1$ADO_5

col_fun_age <- colorRamp2(
  c(min(ages, na.rm = TRUE), max(ages, na.rm = TRUE)),
  c("#FEE08B", "#B7E7A7")
)

circos.trackPlotRegion(
  track.index = 7,
  ylim = c(0, 1),
  track.height = 0.08,
  bg.border = NA,
  panel.fun = function(x, y) {
    for (i in seq_len(n_cat)) {
      ai <- ages[i]
      if (!is.na(ai)) {
        circos.rect(
          xleft = x_left[i],
          ybottom = 0,
          xright = x_right[i],
          ytop = 1,
          col = col_fun_age(ai),
          border = NA
        )
      }
    }
    
    # keep your manual labels, but move them to nearest disease centers
    idx_65 <- which.min(abs(ages - 65))
    idx_50 <- which.min(abs(ages - 50))
    idx_45 <- which.min(abs(ages - 45))
    idx_40 <- which.min(abs(ages - 40))
    idx_30 <- which.min(abs(ages - 30))
    idx_20 <- which.min(abs(ages - 20))
    idx_10 <- which.min(abs(ages - 10))
    idx_1  <- which.min(abs(ages - 1))
    
    circos.text(x = x_mid[idx_65], y = 0.5, labels = "65", sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
    circos.text(x = x_mid[idx_50], y = 0.5, labels = "50", sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
    circos.text(x = x_mid[idx_45], y = 0.5, labels = "45", sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
    circos.text(x = x_mid[idx_40], y = 0.5, labels = "40", sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
    circos.text(x = x_mid[idx_30], y = 0.5, labels = "30", sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
    circos.text(x = x_mid[idx_20], y = 0.5, labels = "20", sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
    circos.text(x = x_mid[idx_10], y = 0.5, labels = "10", sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
    circos.text(x = x_mid[idx_1],  y = 0.5, labels = "1",  sector.index = "all", track.index = 7, facing = "inside", adj = c(0.5, 0.5), cex = cex_lines, font = 2)
  }
)

# =========================
# Legend
# =========================
par(xpd = NA)

x_leg <- grconvertX(0.80, from = "ndc", to = "user")
y_leg <- grconvertY(0.99, from = "ndc", to = "user")

legend(
  x = x_leg,
  y = y_leg,
  legend = c("Univariate MR", "Multivariate MR - Education", "Multivariate MR - Income"),
  pch = c(16, 17, 15),
  col = c("black", "black", "black"),
  pt.cex = c(cex_other, cex_other, cex_other),
  cex = cex_disease,
  bty = "y",
  box.lwd = 0.3
)

library(grid)
# Track 1 (Disease -> Longevity) arrow
grid.segments(
  x0 = unit(0.525, "npc"),
  y0 = unit(0.849, "npc"),
  x1 = unit(0.525, "npc"),
  y1 = unit(0.822, "npc"),
  arrow = arrow(length = unit(0.20, "cm"), type = "open"),
  gp = gpar(lwd = 0.8)
)

# Track 3 (Disease -> Fertility) arrow
grid.segments(
  x0 = unit(0.525, "npc"),
  y0 = unit(0.746, "npc"),
  x1 = unit(0.525, "npc"),
  y1 = unit(0.719, "npc"),
  arrow = arrow(length = unit(0.20, "cm"), type = "open"),
  gp = gpar(lwd = 0.8)
)

# Track 6 (Fertility -> Disease) arrow
grid.segments(
  x0 = unit(0.525, "npc"),
  y0 = unit(0.646, "npc"),
  x1 = unit(0.525, "npc"),
  y1 = unit(0.618, "npc"),
  arrow = arrow(length = unit(0.20, "cm"), type = "open"),
  gp = gpar(lwd = 0.8)
)

circos.clear()
dev.off()
