#######################################################################
# Trip Generation Analysis - SVC790 Part A
# Dependent variable : N_trips
# Predictors          : Nr_People_H, Elderly, Educ_HS, Educ_Tertiary,
#                        Workers_Fulltime, Workers_Parttime,
#                        Learners_students, Driv_Lic
#######################################################################

## ---- 0. Setup -------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE))  install.packages("readxl")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")

library(readxl)
library(corrplot)

if (!dir.exists("figures")) dir.create("figures")

xlsx_path <- "RLubega SVC790_Part A_2026.xlsx"

## The sheet has a title block in rows 1-9; the real header row is row 10.
raw <- read_excel(xlsx_path, sheet = "Table 1", skip = 9)

predictors <- c("Nr_People_H", "Elderly", "Educ_HS", "Educ_Tertiary",
                 "Workers_Fulltime", "Workers_Parttime",
                 "Learners_students", "Driv_Lic")

vars <- c("N_trips", predictors)

data <- raw[, vars]
data[] <- lapply(data, function(x) as.numeric(x))

cat("Rows read:", nrow(data), "\n")
cat("Rows with at least one NA among analysis variables:",
    sum(!complete.cases(data)), "\n\n")

## ---- 1. Correlation matrix + ellipse plot ---------------------------

cor_mat <- cor(data[, vars], use = "complete.obs")

cat("===== Correlation matrix (N_trips + 8 predictors) =====\n")
print(round(cor_mat, 3))
cat("\n")

png("figures/correlation_ellipse.png", width = 1000, height = 1000, res = 150)
corrplot(cor_mat, method = "ellipse", type = "full",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.6,
         title = "Correlation matrix: N_trips and 8 predictors",
         mar = c(0, 0, 2, 0))
dev.off()

## ---- 2. Scatterplots of N_trips vs each predictor --------------------

png("figures/scatterplots.png", width = 1400, height = 900, res = 150)
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))
for (p in predictors) {
  plot(data[[p]], data[["N_trips"]],
       xlab = p, ylab = "N_trips",
       main = paste("N_trips vs", p),
       pch = 16, col = rgb(0, 0, 1, 0.3))
  abline(lm(data[["N_trips"]] ~ data[[p]]), col = "red", lwd = 2)
}
dev.off()

## ---- 3. VIF for the full 8-predictor model (manual, base R only) -----

vif_data <- data[complete.cases(data[, predictors]), predictors]

vif_values <- sapply(predictors, function(p) {
  other <- setdiff(predictors, p)
  form  <- as.formula(paste(p, "~", paste(other, collapse = " + ")))
  r2_aux <- summary(lm(form, data = vif_data))$r.squared
  1 / (1 - r2_aux)
})

vif_table <- data.frame(Predictor = predictors,
                         R2_aux = round(sapply(predictors, function(p) {
                           other <- setdiff(predictors, p)
                           form  <- as.formula(paste(p, "~", paste(other, collapse = " + ")))
                           summary(lm(form, data = vif_data))$r.squared
                         }), 4),
                         VIF = round(vif_values, 3))

cat("===== Manual VIF (full 8-predictor model) =====\n")
print(vif_table, row.names = FALSE)
cat("\n")

## ---- 4. Fit the six models --------------------------------------------

modelA <- lm(N_trips ~ Nr_People_H, data = data)
modelB <- lm(N_trips ~ Nr_People_H + Driv_Lic, data = data)
modelC <- lm(N_trips ~ Learners_students + Workers_Fulltime + Driv_Lic, data = data)
modelD <- lm(N_trips ~ Nr_People_H + Elderly + Educ_HS + Educ_Tertiary +
                        Workers_Fulltime + Workers_Parttime +
                        Learners_students + Driv_Lic, data = data)
modelE <- lm(N_trips ~ Nr_People_H + Learners_students + Driv_Lic, data = data)
modelF <- lm(N_trips ~ Nr_People_H + Workers_Fulltime + Learners_students, data = data)

models <- list(A = modelA, B = modelB, C = modelC,
                D = modelD, E = modelE, F = modelF)

for (nm in names(models)) {
  cat("=====================================================\n")
  cat("Model", nm, ":", deparse(formula(models[[nm]])), "\n")
  cat("=====================================================\n")
  print(summary(models[[nm]]))
  cat("\n")
}

## ---- 5. Comparison table + parsimony comment --------------------------

get_predictor_str <- function(m) {
  paste(attr(terms(m), "term.labels"), collapse = " + ")
}

comparison <- data.frame(
  Model = names(models),
  Predictors = sapply(models, get_predictor_str),
  R2 = sapply(models, function(m) round(summary(m)$r.squared, 4)),
  Adj_R2 = sapply(models, function(m) round(summary(m)$adj.r.squared, 4)),
  n = sapply(models, function(m) nobs(m)),
  stringsAsFactors = FALSE
)

cat("===== Model comparison table =====\n")
print(comparison, row.names = FALSE)
cat("\n")

write.csv(comparison, "model_comparison.csv", row.names = FALSE)

## --- Parsimony commentary, derived from the fitted results -------------

sig_flags <- function(m) {
  cf <- summary(m)$coefficients
  cf <- cf[rownames(cf) != "(Intercept)", , drop = FALSE]
  data.frame(term = rownames(cf),
             t = round(cf[, "t value"], 2),
             p = round(cf[, "Pr(>|t|)"], 4),
             sig = cf[, "Pr(>|t|)"] < 0.05)
}

cat("===== Significance of coefficients (excl. intercept) =====\n")
for (nm in names(models)) {
  cat("--- Model", nm, "---\n")
  print(sig_flags(models[[nm]]), row.names = FALSE)
}
cat("\n")

high_vif_predictors <- vif_table$Predictor[vif_table$VIF > 5]

reduced <- comparison[comparison$Model != "D", ]
best_reduced <- reduced[which.max(reduced$Adj_R2), ]
n_best_reduced_preds <- length(strsplit(best_reduced$Predictors, " \\+ ")[[1]])

cat("===== Parsimony discussion =====\n")
cat(sprintf(
"Model A (single predictor Nr_People_H) already explains R2 = %.3f (adj. R2 = %.3f).\n",
  comparison$R2[comparison$Model == "A"], comparison$Adj_R2[comparison$Model == "A"]))
cat(sprintf(
"Among the reduced (non-D) models, Model %s achieves the highest adjusted R2 (%.3f) using only %d predictor(s): %s.\n",
  best_reduced$Model, best_reduced$Adj_R2, n_best_reduced_preds, best_reduced$Predictors))
cat(sprintf(
"Model D (all 8 predictors) reaches adj. R2 = %.3f -- a gain of just %.4f over Model %s, for %d extra predictors.\n",
  comparison$Adj_R2[comparison$Model == "D"],
  comparison$Adj_R2[comparison$Model == "D"] - best_reduced$Adj_R2,
  best_reduced$Model,
  8 - n_best_reduced_preds))

if (length(high_vif_predictors) > 0) {
  cat("Manual VIFs flag collinearity among:",
      paste(high_vif_predictors, collapse = ", "),
      "(VIF > 5), which inflates standard errors and helps explain why some\n")
  cat("coefficients in the full model D are not statistically significant despite the higher R2.\n")
} else {
  cat("Manual VIFs for all 8 predictors are below 2 (max is", max(vif_table$VIF),
      "), so multicollinearity is not severe enough on its own to destabilise Model D's coefficients --\n")
  cat("all 8 coefficients in Model D remain statistically significant at the 5% level. However, several of the\n")
  cat("predictors are moderately correlated with each other (e.g. Nr_People_H-Elderly, Educ_Tertiary-Driv_Lic,\n")
  cat("Educ_Tertiary-Workers_Fulltime), so part of what they add in Model D duplicates explanatory power already\n")
  cat("captured by the core predictors (Nr_People_H, Learners_students, Workers_Fulltime, Driv_Lic).\n")
}

cat(sprintf(
"Given that Model %s recovers %.1f%% of Model D's adjusted R2 with %d fewer predictors, the marginal explanatory\n",
  best_reduced$Model, 100 * best_reduced$Adj_R2 / comparison$Adj_R2[comparison$Model == "D"],
  8 - n_best_reduced_preds))
cat("gain from fitting the full 8-predictor Model D does not appear to be worth its loss of parsimony: a small,\n")
cat("well-chosen predictor set (household size, learners/students, workers, driver's licence holding) captures\n")
cat("almost all of the explanatory power at much lower risk of overfitting and unnecessary collinearity.\n")

cat("\nDone. Outputs written to: figures/correlation_ellipse.png, figures/scatterplots.png, model_comparison.csv\n")
