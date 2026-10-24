## ==============================================================================
## MASTER ANALYSIS SCRIPT
## Global Regression Coefficient for Predicting Stature from Tibial Length:
## Evidence from Systematic Review and Meta-Regression
##
## Reproduces every table, statistic and figure in the manuscript. Run top to
## bottom from a folder containing the input files below.
##
## INPUT FILES
##   meta_analysis_dataset.csv            k = 95 models (all risk-of-bias strata)
##   primary_dataset_exclHighRoB.csv      k = 58 models (high analysis-domain RoB excluded)
##   offset_analysis_dataset_corrected.csv k = 86 models with SEE available
##   probast_domain_detail.csv            PROBAST judgements (Low/High/Unclear) for the
##                                         99 models extracted from the 54 included studies,
##                                         4 RoB domains, 3 applicability domains, overall
##   prisma_counts.csv                    record counts for the PRISMA flow diagram
##   validation_status.csv                validation reported for each extracted equation,
##                                         classified by TRIPOD definitions
##
##
## PACKAGES: metafor, ggplot2, dplyr, tidyr, forcats, patchwork
## ==============================================================================

suppressPackageStartupMessages({
  library(metafor); library(ggplot2); library(dplyr)
  library(tidyr);   library(forcats); library(patchwork)
})
set.seed(123456)

out <- list()   # every number reported in the manuscript is stored here

## Figures are written twice: a vector PDF (fonts embedded) for submission and a 300-dpi PNG
#save_base() wraps base-graphics figures.
save_base <- function(stem, width, height, draw) {
  png(paste0(stem, ".png"), width = width, height = height, units = "in", res = 300)
  draw(); dev.off()
  grDevices::cairo_pdf(paste0(stem, ".pdf"), width = width, height = height)
  draw(); dev.off()
}
save_gg <- function(stem, plot, width, height) {
  ggsave(paste0(stem, ".png"), plot, width = width, height = height, dpi = 300, bg = "white")
  ggsave(paste0(stem, ".pdf"), plot, width = width, height = height, device = grDevices::cairo_pdf, bg = "white")
}
add <- function(name, value) out[[name]] <<- value

## ------------------------------------------------------------------------
## 0. DATA
## ------------------------------------------------------------------------
dat_full    <- read.csv("meta_analysis_dataset.csv", stringsAsFactors = FALSE)
dat_primary <- read.csv("primary_dataset_exclHighRoB.csv", stringsAsFactors = FALSE)
dat_offset  <- read.csv("offset_analysis_dataset_corrected.csv", stringsAsFactors = FALSE)
rob         <- read.csv("probast_domain_detail.csv", stringsAsFactors = FALSE)
prisma      <- read.csv("prisma_counts.csv", stringsAsFactors = FALSE)
dat_full$vi    <- dat_full$SEb^2
dat_primary$vi <- dat_primary$SEb^2
dat_full$label    <- paste0(dat_full$author_sex, ", ", dat_full$year)
dat_primary$label <- paste0(dat_primary$author_sex, ", ", dat_primary$year)

r2_analogue <- function(base, mod) max(0, (base$tau2 - mod$tau2) / base$tau2 * 100)

## ==============================================================================
## A. POOLED ESTIMATES AND GEOGRAPHIC MODERATORS (Table 1)
## ==============================================================================
res_primary    <- rma(b, vi, data = dat_primary, method = "REML", slab = label)
res_allstudies <- rma(b, vi, data = dat_full,    method = "REML", slab = label)

res_country_primary   <- rma(b, vi, mods = ~ factor(country),   data = dat_primary, method = "REML")
res_continent_primary <- rma(b, vi, mods = ~ factor(continent), data = dat_primary, method = "REML")
res_country_full      <- rma(b, vi, mods = ~ factor(country),   data = dat_full,    method = "REML")
res_continent_full    <- rma(b, vi, mods = ~ factor(continent), data = dat_full,    method = "REML")

for (nm in c("primary", "allstudies")) {
  r <- get(paste0("res_", nm))
  add(paste0(nm, "_k"), r$k); add(paste0(nm, "_b"), r$b[1])
  add(paste0(nm, "_lb"), r$ci.lb); add(paste0(nm, "_ub"), r$ci.ub)
  add(paste0(nm, "_z"), r$zval); add(paste0(nm, "_p"), r$pval)
  add(paste0(nm, "_tau2"), r$tau2); add(paste0(nm, "_I2"), r$I2)
  add(paste0(nm, "_QE"), r$QE); add(paste0(nm, "_QEp"), r$QEp)
}
for (nm in c("country_primary", "continent_primary", "country_full", "continent_full")) {
  r <- get(paste0("res_", nm))
  base <- if (grepl("primary", nm)) res_primary else res_allstudies
  add(paste0(nm, "_QM"), r$QM); add(paste0(nm, "_df"), r$QMdf[1]); add(paste0(nm, "_p"), r$QMp)
  add(paste0(nm, "_I2res"), r$I2); add(paste0(nm, "_R2"), r2_analogue(base, r))
}
## Country coefficients significant at p < 0.05 (primary model)
cc <- coef(summary(res_country_primary))
cc <- cc[-1, , drop = FALSE]
add("country_primary_reference", levels(factor(dat_primary$country))[1])
add("country_primary_sig", paste(sprintf("%s (%.3f, p=%.3f)", sub("factor\\(country\\)", "", rownames(cc)[cc$pval < 0.05]),
                                         cc$estimate[cc$pval < 0.05], cc$pval[cc$pval < 0.05]), collapse = "; "))
ct <- coef(summary(res_continent_primary))[-1, , drop = FALSE]
add("continent_primary_reference", levels(factor(dat_primary$continent))[1])
add("continent_primary_coefs", paste(sprintf("%s (%.3f, p=%.3f)", sub("factor\\(continent\\)", "", rownames(ct)), ct$estimate, ct$pval), collapse = "; "))

table1 <- data.frame(
  Model = c("Primary (high RoB excluded)", "+ Country moderator", "+ Continent moderator",
            "All-studies sensitivity", "+ Country moderator", "+ Continent moderator"),
  k = c(rep(res_primary$k, 3), rep(res_allstudies$k, 3)),
  Pooled_b_95CI = c(sprintf("%.3f (%.3f-%.3f)", res_primary$b[1], res_primary$ci.lb, res_primary$ci.ub),
                    sprintf("QM p = %.3f", res_country_primary$QMp), sprintf("QM p = %.3f", res_continent_primary$QMp),
                    sprintf("%.3f (%.3f-%.3f)", res_allstudies$b[1], res_allstudies$ci.lb, res_allstudies$ci.ub),
                    sprintf("QM p = %.3f", res_country_full$QMp), sprintf("QM p = %.3f", res_continent_full$QMp)),
  I2_pct = round(c(res_primary$I2, res_country_primary$I2, res_continent_primary$I2,
                   res_allstudies$I2, res_country_full$I2, res_continent_full$I2), 2))
write.csv(table1, "Table1_pooled_estimates.csv", row.names = FALSE)

## ==============================================================================
## B. RISK-OF-BIAS AND AGE SENSITIVITY (Table 2)
## ==============================================================================
res_lowOnly <- rma(b, vi, data = subset(dat_full, RoB_Domain4 == "Low RoB"), method = "REML")
res_age18   <- rma(b, vi, data = subset(dat_primary, !age_lower17), method = "REML")
res_age18_full <- rma(b, vi, data = subset(dat_full, !age_lower17), method = "REML")

sens_row <- function(lab, r) data.frame(Analysis = lab, k = r$k, b = round(r$b[1], 3),
  CI = sprintf("%.3f to %.3f", r$ci.lb, r$ci.ub), tau2 = round(r$tau2, 3), I2 = round(r$I2, 2),
  Q = sprintf("%.2f (%d)", r$QE, r$k - 1), p = ifelse(r$QEp < 0.0001, "<0.0001", sprintf("%.4f", r$QEp)))
table2 <- rbind(sens_row("Full sample (all risk-of-bias levels)", res_allstudies),
                sens_row("Primary (high RoB excluded)", res_primary),
                sens_row("Low RoB only", res_lowOnly),
                sens_row("Primary, excluding samples with lower age bound of 17 y", res_age18))
write.csv(table2, "Table2_sensitivity.csv", row.names = FALSE)
for (nm in c("lowOnly", "age18", "age18_full")) {
  r <- get(paste0("res_", nm))
  add(paste0(nm, "_k"), r$k); add(paste0(nm, "_b"), r$b[1]); add(paste0(nm, "_lb"), r$ci.lb)
  add(paste0(nm, "_ub"), r$ci.ub); add(paste0(nm, "_tau2"), r$tau2); add(paste0(nm, "_I2"), r$I2)
  add(paste0(nm, "_QE"), r$QE)
}

dat_full$RoB_Domain4 <- factor(dat_full$RoB_Domain4, levels = c("Low RoB", "High RoB", "Unclear"))
res_rob_mod <- rma(b, vi, mods = ~ RoB_Domain4, data = dat_full, method = "REML")
add("robmod_QM", res_rob_mod$QM); add("robmod_df", res_rob_mod$QMdf[1]); add("robmod_p", res_rob_mod$QMp)
add("robmod_R2", r2_analogue(res_allstudies, res_rob_mod))
rc <- coef(summary(res_rob_mod))
add("robmod_high_est", rc["RoB_Domain4High RoB", "estimate"]); add("robmod_high_p", rc["RoB_Domain4High RoB", "pval"])
add("robmod_unclear_est", rc["RoB_Domain4Unclear", "estimate"]); add("robmod_unclear_p", rc["RoB_Domain4Unclear", "pval"])

## ==============================================================================
## C. PUBLICATION BIAS
## ==============================================================================
egger_primary <- regtest(res_primary,    model = "rma")
egger_full    <- regtest(res_allstudies, model = "rma")
tf_primary    <- trimfill(res_primary)
tf_full       <- trimfill(res_allstudies)
add("egger_primary_z", egger_primary$zval); add("egger_primary_p", egger_primary$pval)
add("egger_full_z", egger_full$zval);       add("egger_full_p", egger_full$pval)
add("tf_primary_k0", tf_primary$k0); add("tf_primary_side", tf_primary$side); add("tf_primary_b", tf_primary$b[1])
add("tf_full_k0", tf_full$k0);       add("tf_full_side", tf_full$side);       add("tf_full_b", tf_full$b[1])

outlier_models <- c(26, 27, 72, 83)   # Esomonu M/F, Tiwari & Datta M, Okwan F
res_primary_sens   <- rma(b, vi, data = subset(dat_primary, !model %in% outlier_models), method = "REML")
egger_primary_sens <- regtest(res_primary_sens, model = "rma")
add("outlier_k", res_primary_sens$k); add("outlier_b", res_primary_sens$b[1])
add("outlier_lb", res_primary_sens$ci.lb); add("outlier_ub", res_primary_sens$ci.ub)
add("outlier_I2", res_primary_sens$I2)
add("outlier_egger_z", egger_primary_sens$zval); add("outlier_egger_p", egger_primary_sens$pval)
## Check the four studies are still the most extreme on b and SE in the primary set
add("outlier_check", paste(dat_primary$author_sex[order(-dat_primary$b)][1:4], collapse = "; "))

save_base("Figure5_Funnel_Primary", 7.5, 6.25, function() {
  par(family = "serif"); funnel(res_primary, xlab = "Regression coefficient (b)") })
save_base("Figure6_Funnel_AllStudies", 7.5, 6.25, function() {
  par(family = "serif"); funnel(res_allstudies, xlab = "Regression coefficient (b)") })

## ==============================================================================
## D. LEAVE-ONE-OUT (primary)
## ==============================================================================
loo <- as.data.frame(leave1out(res_primary))
loo$model <- dat_primary$model; loo$study <- dat_primary$label
write.csv(loo, "LeaveOneOut_primary.csv", row.names = FALSE)
add("loo_min", min(loo$estimate)); add("loo_max", max(loo$estimate))
add("loo_min_study", loo$study[which.min(loo$estimate)]); add("loo_max_study", loo$study[which.max(loo$estimate)])
add("loo_I2_min", min(loo$I2)); add("loo_I2_max", max(loo$I2))

## ==============================================================================
## E. SEX
## ==============================================================================
sex_models <- function(d, tag) {
  m <- rma(b, vi, data = subset(d, sex_cat == "Male"),   method = "REML")
  f <- rma(b, vi, data = subset(d, sex_cat == "Female"), method = "REML")
  s <- rma(b, vi, mods = ~ factor(sex_cat), data = subset(d, sex_cat %in% c("Male", "Female")), method = "REML")
  add(paste0(tag, "_male_k"), m$k); add(paste0(tag, "_male_b"), m$b[1]); add(paste0(tag, "_male_lb"), m$ci.lb); add(paste0(tag, "_male_ub"), m$ci.ub)
  add(paste0(tag, "_female_k"), f$k); add(paste0(tag, "_female_b"), f$b[1]); add(paste0(tag, "_female_lb"), f$ci.lb); add(paste0(tag, "_female_ub"), f$ci.ub)
  add(paste0(tag, "_sex_QM"), s$QM); add(paste0(tag, "_sex_p"), s$QMp)
  list(m = m, f = f)
}
sex_p <- sex_models(dat_primary, "primary")
sex_f <- sex_models(dat_full, "full")

## ==============================================================================
## F. OFFSET FROM POOLED b vs SEE (reference = primary pooled b; test set = all
##    models with SEE, including high-RoB models)
## ==============================================================================
b_ref <- res_primary$b[1]
dat_offset$abs_offset <- abs(dat_offset$b - b_ref)
dat_offset$high <- dat_offset$RoB_Domain4 == "High RoB"
pe <- cor.test(dat_offset$abs_offset, dat_offset$SEE)
sp <- suppressWarnings(cor.test(dat_offset$abs_offset, dat_offset$SEE, method = "spearman"))
lm_u <- lm(SEE ~ abs_offset, data = dat_offset)
lm_w <- lm(SEE ~ abs_offset, data = dat_offset, weights = 1 / SEb^2)
ch <- cor.test(dat_offset$abs_offset[dat_offset$high],  dat_offset$SEE[dat_offset$high])
cl <- cor.test(dat_offset$abs_offset[!dat_offset$high], dat_offset$SEE[!dat_offset$high])
add("offset_k", nrow(dat_offset)); add("offset_ref_b", b_ref)
add("offset_r", unname(pe$estimate)); add("offset_p", pe$p.value)
add("offset_rho", unname(sp$estimate)); add("offset_rho_p", sp$p.value)
add("offset_slope", coef(lm_u)[2]); add("offset_slope_se", coef(summary(lm_u))[2, 2])
add("offset_slope_p", coef(summary(lm_u))[2, 4]); add("offset_R2", summary(lm_u)$r.squared)
add("offset_wslope", coef(lm_w)[2]); add("offset_wslope_se", coef(summary(lm_w))[2, 2]); add("offset_wslope_p", coef(summary(lm_w))[2, 4])
add("offset_high_n", sum(dat_offset$high)); add("offset_high_r", unname(ch$estimate)); add("offset_high_p", ch$p.value)
add("offset_low_n", sum(!dat_offset$high)); add("offset_low_r", unname(cl$estimate)); add("offset_low_p", cl$p.value)
add("offset_high_mean", mean(dat_offset$abs_offset[dat_offset$high])); add("offset_low_mean", mean(dat_offset$abs_offset[!dat_offset$high]))

save_base("Figure7_OffsetVsSEE", 8.33, 6.67, function() {
par(family = "serif")
plot(dat_offset$abs_offset, dat_offset$SEE,
     xlab = expression("Absolute offset from primary pooled coefficient, |" * b[i] - b[pooled] * "|"),
     ylab = "Reported standard error of the estimate (SEE, cm)",
     pch = ifelse(dat_offset$high, 17, 19),
     col = ifelse(dat_offset$high, rgb(0.8, 0, 0, 0.7), rgb(0, 0, 0.6, 0.6)))
abline(lm_u, col = "red", lwd = 2)
legend("topright", bty = "n", cex = 0.8, pch = c(19, 17, NA), lty = c(NA, NA, 1), col = c(rgb(0, 0, 0.6), rgb(0.8, 0, 0), "red"),
       legend = c(sprintf("Low/unclear RoB (n = %d): r = %.3f", sum(!dat_offset$high), cl$estimate),
                  sprintf("High RoB (n = %d): r = %.3f", sum(dat_offset$high), ch$estimate),
                  sprintf("All (n = %d): r = %.3f, p = %.3f", nrow(dat_offset), pe$estimate, pe$p.value)))
})

## ==============================================================================
## G. STUDY CHARACTERISTICS (Figure 3) and descriptive numbers
## ==============================================================================
add("primary_countries", length(unique(dat_primary$country)))
add("primary_country_counts", paste(sprintf("%s (%d)", names(sort(table(dat_primary$country), decreasing = TRUE)),
                                            sort(table(dat_primary$country), decreasing = TRUE)), collapse = ", "))
add("primary_N_min", min(dat_primary$N)); add("primary_N_max", max(dat_primary$N))
add("primary_N_median", median(dat_primary$N)); add("primary_N_total", sum(dat_primary$N))
add("primary_year_min", min(dat_primary$year)); add("primary_year_max", max(dat_primary$year))
add("full_countries", length(unique(dat_full$country))); add("full_continents", length(unique(dat_full$continent)))
add("full_N_total", sum(dat_full$N)); add("full_india", sum(dat_full$country == "India"))
add("primary_india", sum(dat_primary$country == "India"))
add("full_continent_counts", paste(sprintf("%s (%d)", names(sort(table(dat_full$continent), decreasing = TRUE)),
                                           sort(table(dat_full$continent), decreasing = TRUE)), collapse = ", "))

theme_char <- theme_minimal(base_family = "serif", base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 12))
pa <- dat_full %>% count(country) %>% mutate(country = fct_reorder(country, n)) %>%
  ggplot(aes(country, n)) + geom_col(fill = "#2C5F8A", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.2, family = "serif") + coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "A. Models by country", x = NULL, y = "Number of models") + theme_char +
  theme(panel.grid.major.y = element_blank())
pb <- dat_full %>% count(sex_cat) %>% mutate(sex_cat = factor(sex_cat, levels = c("Male", "Female"))) %>%
  ggplot(aes(sex_cat, n, fill = sex_cat)) + geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5, family = "serif") +
  scale_fill_manual(values = c(Male = "#4C72B0", Female = "#DD8452")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "B. Models by sex", x = NULL, y = "Number of models") + theme_char +
  theme(panel.grid.major.x = element_blank())
pc <- ggplot(dat_full, aes(N)) + geom_histogram(bins = 20, fill = "#55A868", color = "white") +
  labs(title = "C. Sample size per model", x = "N", y = "Number of models") + theme_char
pd <- ggplot(dat_full, aes(year)) + geom_histogram(binwidth = 2, fill = "#C44E52", color = "white", boundary = 2000) +
  labs(title = "D. Publication year", x = "Year", y = "Number of models") + theme_char
save_gg("Figure3_Study_Characteristics", (pa | (pb / pc / pd)) + plot_layout(widths = c(1.3, 1)),
       width = 11, height = 8)

## ==============================================================================
## H. PRISMA 2020 FLOW DIAGRAM (Figure 1)
## ==============================================================================
pv <- setNames(prisma$n, prisma$item)
box <- function(x1, x2, y1, y2, label, size = 3.1)
  list(annotate("rect", xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = "white", colour = "black", linewidth = 0.4),
       annotate("text", x = (x1 + x2) / 2, y = (y1 + y2) / 2, label = label, size = size, family = "serif", lineheight = 0.95))
arr <- function(x1, y1, x2, y2) annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
                                         arrow = arrow(length = unit(2.2, "mm"), type = "closed"), linewidth = 0.4)
fmt <- function(x) format(x, big.mark = ",")
p_prisma <- ggplot() + coord_cartesian(xlim = c(0, 100), ylim = c(0, 103), expand = FALSE) + theme_void(base_family = "serif") +
  box(2, 46, 85, 98, sprintf("Records identified from databases\n(n = %s):\nGoogle Scholar (n = %s)\nPubMed (n = %s)\nScopus (n = %s)",
                             fmt(pv["google"] + pv["pubmed"] + pv["scopus"]), fmt(pv["google"]), fmt(pv["pubmed"]), fmt(pv["scopus"]))) +
  box(54, 98, 85, 98, sprintf("Records removed before screening:\nDuplicate records removed\n(n = %s)",
                              fmt(pv["google"] + pv["pubmed"] + pv["scopus"] - pv["after_dedup"]))) +
  arr(46, 91.5, 54, 91.5) + arr(24, 85, 24, 78) +
  box(2, 46, 69, 78, sprintf("Records screened (title/abstract)\n(n = %s)", fmt(pv["after_dedup"]))) +
  box(54, 98, 69, 78, sprintf("Records excluded\n(n = %s)", fmt(pv["after_dedup"] - pv["after_title_abstract"]))) +
  arr(46, 73.5, 54, 73.5) + arr(24, 69, 24, 62) +
  box(2, 46, 53, 62, sprintf("Reports sought for retrieval\n(n = %s)", fmt(pv["after_title_abstract"]))) +
  box(54, 98, 53, 62, sprintf("Reports not retrieved\n(n = %s)", fmt(pv["not_retrieved"]))) +
  arr(46, 57.5, 54, 57.5) + arr(24, 53, 24, 46) +
  box(2, 46, 37, 46, sprintf("Reports assessed for eligibility\n(n = %s)", fmt(pv["after_title_abstract"] - pv["not_retrieved"]))) +
  box(54, 98, 37, 46, sprintf("Reports excluded\n(n = %s)\nReasons: see Supplementary Table S2", fmt(pv["after_title_abstract"] - pv["not_retrieved"] - pv["after_fulltext"]))) +
  arr(46, 41.5, 54, 41.5) + arr(24, 37, 24, 30) +
  box(2, 46, 3, 30, sprintf(paste0("Studies included in review\n(n = %d)\n\nModels extracted\n(n = %d)\n\n",
                                   "Models in meta-regression\n(n = %d from %d studies)"),
                            pv["after_fulltext"], nrow(rob), nrow(dat_full), pv["studies_in_synthesis"]), size = 3.2) +
  box(54, 98, 3, 30, paste0("Extracted models not pooled (n = ", nrow(rob) - nrow(dat_full), "):\n",
                            "SE(b) not derivable (n = 1)\n",
                            "Combined-sex equation re-using\nparticipants of sex-specific\nequations (n = 1)\n",
                            "National survey overlapping\nincluded regional samples (n = 2)"), size = 2.9) +
  arr(46, 16.5, 54, 16.5) +
  annotate("text", x = 50, y = 101.3, label = "Identification", fontface = "bold", family = "serif", size = 3.8) +
  annotate("text", x = 50, y = 81.5, label = "Screening", fontface = "bold", family = "serif", size = 3.8) +
  annotate("text", x = 50, y = 33.5, label = "Included", fontface = "bold", family = "serif", size = 3.8)
save_gg("Figure1_PRISMA_flow_diagram", p_prisma, width = 9.5, height = 11.5)

## ==============================================================================
## I. PROBAST SUMMARY (Figure 2) and traffic-light plot
## ==============================================================================
pal <- c(Low = "#92D050", High = "#F4A0A0", Unclear = "#FFE699")
make_panel <- function(df, cols, labels, title, legend_labels) {
  summ <- df %>% select(model, all_of(cols)) %>%
    pivot_longer(-model, names_to = "domain", values_to = "judgement") %>%
    mutate(domain = factor(domain, levels = cols, labels = labels),
           judgement = factor(judgement, levels = c("Low", "High", "Unclear"))) %>%
    count(domain, judgement, .drop = FALSE) %>% group_by(domain) %>% mutate(pct = n / sum(n) * 100) %>% ungroup()
  ggplot(summ, aes(domain, pct, fill = judgement)) +
    geom_col(width = 0.72, colour = "white", linewidth = 0.4, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(pct >= 3, paste0(round(pct), "%"), "")),
              position = position_stack(vjust = 0.5, reverse = TRUE), size = 3.1, colour = "grey20") +
    coord_flip() +
    scale_fill_manual(values = pal, labels = legend_labels, drop = FALSE, name = NULL) +
    scale_y_continuous(breaks = seq(0, 100, 10), labels = paste0(seq(0, 100, 10), "%"), expand = c(0, 0)) +
    labs(title = title, x = NULL, y = NULL) + theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text.y = element_text(size = 10, colour = "grey20"),
          axis.text.x = element_text(size = 9, colour = "grey30"), axis.ticks = element_blank(),
          legend.position = "bottom", legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 9),
          plot.title = element_text(face = "bold", size = 11.5, colour = "grey20"),
          plot.background = element_rect(fill = "grey97", colour = NA), plot.margin = margin(12, 16, 8, 12))
}
p_rob <- make_panel(rob, c("D1_RoB", "D2_RoB", "D3_RoB", "D4_RoB", "Overall_RoB"),
                    c("1. Participants", "2. Predictor", "3. Outcome", "4. Analysis", "Overall"),
                    "(a) Risk of bias", c("Low", "High", "Unclear"))
p_app <- make_panel(rob, c("D1_App", "D2_App", "D3_App", "Overall_App"),
                    c("1. Participants", "2. Predictor", "3. Outcome", "Overall"),
                    "(b) Applicability concerns", c("Low concern", "High concern", "Unclear"))
save_gg("Figure2_RoB_Applicability_Summary", p_rob / p_app + plot_layout(heights = c(1.15, 1)),
       width = 8.2, height = 7.2)
for (d in c("D1_RoB", "D2_RoB", "D3_RoB", "D4_RoB", "Overall_RoB", "D1_App", "D2_App", "D3_App", "Overall_App"))
  for (j in c("Low", "High", "Unclear")) add(paste0("rob_", d, "_", j), sum(rob[[d]] == j))
add("rob_n", nrow(rob))

## validation reported by the source studies (TRIPOD definitions: a holdout or split
## sample from the derivation data is internal validation; external validation needs
## independent data from a different setting, time or population)
val <- read.csv("validation_status.csv", stringsAsFactors = FALSE)
stopifnot(nrow(val) == nrow(rob))
add("val_external",          sum(val$validation == "External"))
add("val_internal",          sum(val$validation == "Internal"))
add("val_internal_studies",  length(unique(val$study[val$validation == "Internal"])))
add("val_none",              sum(val$validation == "None reported"))
add("val_other",             sum(!val$validation %in% c("External", "Internal", "None reported")))
add("val_other_studies",     length(unique(val$study[!val$validation %in% c("External", "Internal", "None reported")])))
add("val_internal_primary",  sum(val$validation == "Internal" & val$model %in% dat_primary$model))

traffic <- rob %>% select(model, D1 = D1_RoB, D2 = D2_RoB, D3 = D3_RoB, D4 = D4_RoB) %>%
  pivot_longer(-model, names_to = "domain", values_to = "judgement") %>%
  mutate(domain = recode(domain, D1 = "1. Participants", D2 = "2. Predictor", D3 = "3. Outcome", D4 = "4. Analysis"),
         judgement = factor(judgement, levels = c("Low", "High", "Unclear")),
         symbol = recode(as.character(judgement), Low = "+", Unclear = "?", High = "−"))
save_gg("FigureS1_RoB_TrafficLight",
       ggplot(traffic, aes(domain, fct_rev(factor(model)), fill = judgement)) + geom_tile(colour = "white", linewidth = 0.3) +
         geom_text(aes(label = symbol), size = 2.6, family = "serif", fontface = "bold", colour = "grey20") +
         scale_fill_manual(values = pal, name = "Judgement") + labs(x = NULL, y = "Model number") +
         theme_minimal(base_family = "serif", base_size = 8) +
         theme(axis.text.y = element_text(size = 5), axis.text.x = element_text(size = 9, face = "bold"),
               panel.grid = element_blank(), legend.position = "top"),
       width = 6, height = 14)

## ==============================================================================
## J. FOREST PLOTS (Figures 4 and 8)
## ==============================================================================
save_base("Figure4_Forest_Overall", 9, 15, function() {
par(family = "serif")
forest(res_primary, xlab = "Regression coefficient (b): cm of stature per cm of tibial length",
       header = c("Study (sex), year", "b [95% CI]"),
       mlab = sprintf("Random-effects model (k = %d; I² = %.1f%%)", res_primary$k, res_primary$I2),
       cex = 0.68, cex.lab = 0.9, cex.axis = 0.8, col = "#2C5F8A", border = "#2C5F8A", shade = TRUE)
})

mk <- function(d) { d$vi <- d$SEb^2; rma(b, vi, data = d, method = "REML", slab = label) }
res_m <- mk(subset(dat_primary, sex_cat == "Male")); res_f <- mk(subset(dat_primary, sex_cat == "Female"))
save_base("Figure8_Forest_BySex", 15, 11, function() {
par(mfrow = c(1, 2), family = "serif")
forest(res_m, xlab = "Regression coefficient (b)", header = c("Male models", "b [95% CI]"),
       mlab = sprintf("Pooled, males (k = %d; I² = %.1f%%)", res_m$k, res_m$I2),
       cex = 0.75, col = "#4C72B0", border = "#4C72B0", shade = TRUE)
title("A. Male-specific models", cex.main = 1.1)
forest(res_f, xlab = "Regression coefficient (b)", header = c("Female models", "b [95% CI]"),
       mlab = sprintf("Pooled, females (k = %d; I² = %.1f%%)", res_f$k, res_f$I2),
       cex = 0.75, col = "#DD8452", border = "#DD8452", shade = TRUE)
title("B. Female-specific models", cex.main = 1.1)
})

## ==============================================================================
## OUTPUT: key numbers
## ==============================================================================
kn <- data.frame(name = names(out), value = sapply(out, function(x) if (is.numeric(x)) format(x, digits = 10) else as.character(x)))
write.csv(kn, "manuscript_key_numbers.csv", row.names = FALSE)
cat("Done. Key numbers written to manuscript_key_numbers.csv\n")
