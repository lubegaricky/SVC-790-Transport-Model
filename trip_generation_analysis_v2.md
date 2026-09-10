---
title: "Trip Generation Analysis - SVC790 Part A"
author: "R. Lubega"
date: "2026-09-10"
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
    includes:
      in_header: preamble.tex
  html_document:
    toc: true
    toc_depth: 2
    number_sections: true
geometry: margin=1in
fontsize: 11pt
---



# Overview

Dependent variable: `N_trips`.
Predictors: `Nr_People_H`, `Elderly`, `Educ_HS`, `Educ_Tertiary`,
`Workers_Fulltime`, `Workers_Parttime`, `Learners_students`, `Driv_Lic`.

Every task below is done two ways:

- **Version A - Base R**: uses only functions shipped with base R / the
  `stats` package (no extra packages, beyond what is unavoidably needed to
  read the `.xlsx` file in the first place).
- **Version B - Packages**: the same task using a common contributed
  package that makes it faster or nicer to look at.

Both versions compute the same underlying numbers; only the mechanics differ.


``` r
# readxl has no base-R equivalent (base R cannot read .xlsx files), so it is
# loaded once here as shared infrastructure rather than as part of a
# "package version" of any one task.
ensure_pkg("readxl")

if (!dir.exists("figures")) dir.create("figures")
```

## Load data

The sheet has a title block in rows 1-9; the real header row is row 10, so
we skip the first 9 rows when reading.


``` r
xlsx_path <- "RLubega SVC790_Part A_2026.xlsx"

raw <- read_excel(xlsx_path, sheet = "Table 1", skip = 9)

predictors <- c("Nr_People_H", "Elderly", "Educ_HS", "Educ_Tertiary",
                 "Workers_Fulltime", "Workers_Parttime",
                 "Learners_students", "Driv_Lic")

vars <- c("N_trips", predictors)

data <- raw[, vars]
data[] <- lapply(data, function(x) as.numeric(x))

cat("Rows read:", nrow(data), "\n")
```

```
## Rows read: 5387
```

``` r
cat("Rows with at least one NA among analysis variables:",
    sum(!complete.cases(data)), "\n")
```

```
## Rows with at least one NA among analysis variables: 71
```

# 1. Correlation matrix and plot

The correlation values themselves come from base R `cor()` either way; only
the plot differs between the two versions.


``` r
cor_mat <- cor(data[, vars], use = "complete.obs")
round(cor_mat, 3)
```

```
##                   N_trips Nr_People_H Elderly Educ_HS Educ_Tertiary
## N_trips             1.000       0.411   0.196   0.240         0.121
## Nr_People_H         0.411       1.000   0.568   0.139         0.026
## Elderly             0.196       0.568   1.000   0.043         0.018
## Educ_HS             0.240       0.139   0.043   1.000        -0.279
## Educ_Tertiary       0.121       0.026   0.018  -0.279         1.000
## Workers_Fulltime    0.230       0.118   0.069   0.252         0.343
## Workers_Parttime    0.142       0.017   0.005   0.044        -0.028
## Learners_students   0.545       0.271   0.135  -0.013        -0.010
## Driv_Lic            0.108       0.017  -0.013   0.074         0.385
## Workers_Fulltime Workers_Parttime Learners_students Driv_Lic
## N_trips 0.230 0.142 0.545 0.108
## Nr_People_H 0.118 0.017 0.271 0.017
## Elderly 0.069 0.005 0.135 -0.013
## Educ_HS 0.252 0.044 -0.013 0.074
## Educ_Tertiary 0.343 -0.028 -0.010 0.385
## Workers_Fulltime 1.000 -0.164 -0.012 0.373
## Workers_Parttime -0.164 1.000 0.025 -0.041
## Learners_students -0.012 0.025 1.000 -0.073
## Driv_Lic 0.373 -0.041 -0.073 1.000
```

## Version A - Base R: correlation heatmap (`image()`)

Base R has no built-in ellipse-style correlation plot, so the base-R
equivalent here is a colour-coded heatmap built with `image()`, with the
correlation values overlaid as text.


``` r
plot_cor_heatmap <- function(m, title = "") {
  n <- ncol(m)
  m_flipped <- m[n:1, ]
  op <- par(mar = c(1, 7, 3, 1))
  image(1:n, 1:n, t(m_flipped), axes = FALSE, xlab = "", ylab = "",
        col = colorRampPalette(c("firebrick3", "white", "steelblue4"))(101),
        zlim = c(-1, 1), main = title)
  axis(3, at = 1:n, labels = colnames(m), las = 2, cex.axis = 0.7)
  axis(2, at = 1:n, labels = rev(rownames(m)), las = 2, cex.axis = 0.7)
  for (i in 1:n) {
    for (j in 1:n) text(j, n - i + 1, sprintf("%.2f", m[i, j]), cex = 0.55)
  }
  box()
  par(op)
}

plot_cor_heatmap(cor_mat, "Correlation matrix: N_trips and 8 predictors")
```

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{trip_generation_analysis_v2_files/figure-latex/correlation-heatmap-baseR-1} 

}

\caption{Base R correlation heatmap}\label{fig:correlation-heatmap-baseR}
\end{figure}



## Version B - Package (`corrplot`): ellipse plot


``` r
ensure_pkg("corrplot")

corrplot(cor_mat, method = "ellipse", type = "full",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.6,
         title = "Correlation matrix: N_trips and 8 predictors",
         mar = c(0, 0, 2, 0))
```

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{trip_generation_analysis_v2_files/figure-latex/correlation-ellipse-plot-1} 

}

\caption{Correlation matrix: N\_trips and 8 predictors (corrplot ellipse)}\label{fig:correlation-ellipse-plot}
\end{figure}



# 2. Scatterplots of N_trips against each predictor

## Version A - Base R


``` r
plot_scatter_grid <- function() {
  par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))
  for (p in predictors) {
    plot(data[[p]], data[["N_trips"]],
         xlab = p, ylab = "N_trips",
         main = paste("N_trips vs", p),
         pch = 16, col = rgb(0, 0, 1, 0.3))
    abline(lm(data[["N_trips"]] ~ data[[p]]), col = "red", lwd = 2)
  }
}
plot_scatter_grid()
```

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{trip_generation_analysis_v2_files/figure-latex/scatterplots-baseR-1} 

}

\caption{N\_trips vs each of the 8 predictors (base R)}\label{fig:scatterplots-baseR}
\end{figure}



## Version B - Package (`ggplot2`)


``` r
ensure_pkg("ggplot2")
ensure_pkg("tidyr")

data_long <- tidyr::pivot_longer(data, cols = all_of(predictors),
                                  names_to = "predictor", values_to = "value")

gg_scatter <- ggplot2::ggplot(data_long, ggplot2::aes(x = value, y = N_trips)) +
  ggplot2::geom_point(alpha = 0.25, colour = "steelblue4") +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = "firebrick3",
                        se = FALSE, linewidth = 0.8) +
  ggplot2::facet_wrap(~predictor, scales = "free_x", ncol = 4) +
  ggplot2::labs(x = NULL, y = "N_trips",
                title = "N_trips vs each of the 8 predictors") +
  ggplot2::theme_bw(base_size = 9)

gg_scatter
```

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{trip_generation_analysis_v2_files/figure-latex/scatterplots-ggplot-1} 

}

\caption{N\_trips vs each of the 8 predictors (ggplot2)}\label{fig:scatterplots-ggplot}
\end{figure}



# 3. Variance Inflation Factors (VIF)

## Version A - Base R (manual auxiliary regressions)

Each predictor is regressed on the other 7 using base R `lm()`, and
$$VIF = \frac{1}{1 - R^2_{aux}}.$$
No `car` package is used for this version.


``` r
vif_data <- data[complete.cases(data[, predictors]), predictors]

r2_aux <- sapply(predictors, function(p) {
  other <- setdiff(predictors, p)
  form  <- as.formula(paste(p, "~", paste(other, collapse = " + ")))
  summary(lm(form, data = vif_data))$r.squared
})

vif_table_baseR <- data.frame(Predictor = predictors,
                               R2_aux = round(r2_aux, 4),
                               VIF = round(1 / (1 - r2_aux), 3))

vif_table_baseR_display <- vif_table_baseR
if (knitr::is_latex_output()) {
  vif_table_baseR_display$Predictor <- gsub("_", "\\\\_", vif_table_baseR_display$Predictor)
}
colnames(vif_table_baseR_display) <- c("Predictor", "$R^2_{aux}$", "VIF")

knitr::kable(vif_table_baseR_display, row.names = FALSE, escape = FALSE,
             caption = "Base R manual VIF")
```



Table: Base R manual VIF

|Predictor          | $R^2_{aux}$|   VIF|
|:------------------|-----------:|-----:|
|Nr\_People\_H      |      0.3795| 1.612|
|Elderly            |      0.3253| 1.482|
|Educ\_HS           |      0.2473| 1.329|
|Educ\_Tertiary     |      0.3363| 1.507|
|Workers\_Fulltime  |      0.3151| 1.460|
|Workers\_Parttime  |      0.0407| 1.042|
|Learners\_students |      0.0829| 1.090|
|Driv\_Lic          |      0.2286| 1.296|

## Version B - Package (`car::vif()`)

`car::vif()` reads the VIFs off a fitted model object directly.


``` r
ensure_pkg("car")

full_formula <- reformulate(predictors, response = "N_trips")
vif_full_model <- lm(full_formula, data = data)

vif_table_car <- data.frame(Predictor = names(car::vif(vif_full_model)),
                             VIF = round(car::vif(vif_full_model), 3))

knitr::kable(vif_table_car, row.names = FALSE,
             caption = "car::vif() result")
```



Table: car::vif() result

|Predictor         |   VIF|
|:-----------------|-----:|
|Nr_People_H       | 1.612|
|Elderly           | 1.482|
|Educ_HS           | 1.329|
|Educ_Tertiary     | 1.507|
|Workers_Fulltime  | 1.460|
|Workers_Parttime  | 1.042|
|Learners_students | 1.090|
|Driv_Lic          | 1.296|



Both versions agree (checked with `all.equal()` above): manual VIFs are all
below 2, so multicollinearity is not severe among the 8 predictors.

# 4. Fit the six models

`lm()` itself is base R either way; what differs between the two versions
below is how the fitted models are *reported*.


``` r
modelA <- lm(N_trips ~ Nr_People_H, data = data)
modelB <- lm(N_trips ~ Nr_People_H + Driv_Lic, data = data)
modelC <- lm(N_trips ~ Learners_students + Workers_Fulltime + Driv_Lic, data = data)
modelD <- vif_full_model
modelE <- lm(N_trips ~ Nr_People_H + Learners_students + Driv_Lic, data = data)
modelF <- lm(N_trips ~ Nr_People_H + Workers_Fulltime + Learners_students, data = data)

models <- list(A = modelA, B = modelB, C = modelC,
                D = modelD, E = modelE, F = modelF)
```

## Version A - Base R: `summary()` printouts

Long lines from `summary()` (e.g. the `Call:`) are manually wrapped at 78
characters before being placed in a fenced block, so nothing runs off the
page.

\needspace{25\baselineskip}

**Model A :** N_trips ~ Nr_People_H 

```

Call:
lm(formula = N_trips ~ Nr_People_H, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.1149 -0.2996 -0.0403  0.1817  4.1817 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.780985   0.019963   39.12   <2e-16 ***
Nr_People_H 0.259329   0.007157   36.24   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6959 on 5380 degrees of freedom
  (5 observations deleted due to missingness)
Multiple R-squared:  0.1962,	Adjusted R-squared:  0.196 
F-statistic:  1313 on 1 and 5380 DF,  p-value: < 2.2e-16
 
```

\needspace{26\baselineskip}

**Model B :** N_trips ~ Nr_People_H + Driv_Lic 

```

Call:
lm(formula = N_trips ~ Nr_People_H + Driv_Lic, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.0341 -0.3621 -0.1043  0.2475  4.2549 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.713830   0.021370   33.40   <2e-16 ***
Nr_People_H 0.257808   0.007113   36.25   <2e-16 ***
Driv_Lic    0.132613   0.015713    8.44   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6914 on 5379 degrees of freedom
  (5 observations deleted due to missingness)
Multiple R-squared:  0.2067,	Adjusted R-squared:  0.2064 
F-statistic: 700.7 on 2 and 5379 DF,  p-value: < 2.2e-16
 
```

\needspace{27\baselineskip}

**Model C :** N_trips ~ Learners_students + Workers_Fulltime + Driv_Lic 

```

Call:
lm(formula = N_trips ~ Learners_students + Workers_Fulltime + 
    Driv_Lic, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.5201 -0.3902 -0.1098  0.2392  5.8902 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        1.10984    0.01214  91.428  < 2e-16 ***
Learners_students  1.20513    0.02110  57.122  < 2e-16 ***
Workers_Fulltime   0.28037    0.01475  19.013  < 2e-16 ***
Driv_Lic           0.09025    0.01481   6.095 1.17e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6022 on 5383 degrees of freedom
Multiple R-squared:  0.4141,	Adjusted R-squared:  0.4138 
F-statistic:  1268 on 3 and 5383 DF,  p-value: < 2.2e-16
 
```

\needspace{32\baselineskip}

**Model D :** N_trips ~ Nr_People_H + Elderly + Educ_HS + Educ_Tertiary + Workers_Fulltime +      Workers_Parttime + Learners_students + Driv_Lic 

```

Call:
lm(formula = full_formula, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.4694 -0.3225 -0.0975  0.1641  4.3515 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)        0.706093   0.016793  42.048   <2e-16 ***
Nr_People_H        0.129836   0.006671  19.464   <2e-16 ***
Elderly           -0.021507   0.008972  -2.397   0.0166 *  
Educ_HS            0.233347   0.012779  18.260   <2e-16 ***
Educ_Tertiary      0.187435   0.018083  10.365   <2e-16 ***
Workers_Fulltime   0.144875   0.014090  10.282   <2e-16 ***
Workers_Parttime   0.336228   0.024048  13.982   <2e-16 ***
Learners_students  0.984266   0.020920  47.049   <2e-16 ***
Driv_Lic           0.040696   0.013166   3.091   0.0020 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.4992 on 5307 degrees of freedom
  (71 observations deleted due to missingness)
Multiple R-squared:  0.4781,	Adjusted R-squared:  0.4773 
F-statistic: 607.8 on 8 and 5307 DF,  p-value: < 2.2e-16
 
```

\needspace{28\baselineskip}

**Model E :** N_trips ~ Nr_People_H + Learners_students + Driv_Lic 

```

Call:
lm(formula = N_trips ~ Nr_People_H + Learners_students + Driv_Lic, 
    data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.8759 -0.3244 -0.1441  0.1873  4.3676 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       0.818586   0.017927   45.66   <2e-16 ***
Nr_People_H       0.162760   0.006236   26.10   <2e-16 ***
Learners_students 1.040378   0.021337   48.76   <2e-16 ***
Driv_Lic          0.180322   0.013123   13.74   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.5758 on 5378 degrees of freedom
  (5 observations deleted due to missingness)
Multiple R-squared:  0.4499,	Adjusted R-squared:  0.4496 
F-statistic:  1466 on 3 and 5378 DF,  p-value: < 2.2e-16
 
```

\needspace{28\baselineskip}

**Model F :** N_trips ~ Nr_People_H + Workers_Fulltime + Learners_students 

```

Call:
lm(formula = N_trips ~ Nr_People_H + Workers_Fulltime + Learners_students, 
    data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.7753 -0.3801 -0.1084  0.1744  4.4461 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       0.811424   0.016976   47.80   <2e-16 ***
Nr_People_H       0.148495   0.006153   24.14   <2e-16 ***
Workers_Fulltime  0.271651   0.012889   21.08   <2e-16 ***
Learners_students 1.036471   0.020824   49.77   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.563 on 5378 degrees of freedom
  (5 observations deleted due to missingness)
Multiple R-squared:  0.474,	Adjusted R-squared:  0.4737 
F-statistic:  1615 on 3 and 5378 DF,  p-value: < 2.2e-16
 
```

## Version B - Package (`broom`): tidy coefficient and fit tables


``` r
ensure_pkg("broom")

for (nm in names(models)) {
  tidy_tbl <- broom::tidy(models[[nm]])
  needspace(nrow(tidy_tbl) + 10)
  cat("**Model", nm, ":**", deparse(formula(models[[nm]])), "\n\n")
  tidy_tbl[ , -1] <- round(tidy_tbl[ , -1], 4)
  print(knitr::kable(tidy_tbl, row.names = FALSE))
  cat("\n")
  glance_tbl <- broom::glance(models[[nm]])[c("r.squared", "adj.r.squared",
                                               "statistic", "p.value", "nobs")]
  glance_tbl <- round(glance_tbl, 4)
  colnames(glance_tbl) <- c("$R^2$", "$R^2_{adj}$", "statistic", "p.value", "nobs")
  print(knitr::kable(glance_tbl, row.names = FALSE, escape = FALSE))
  cat("\n\n")
}
```

\needspace{12\baselineskip}

**Model A :** N_trips ~ Nr_People_H 



|term        | estimate| std.error| statistic| p.value|
|:-----------|--------:|---------:|---------:|-------:|
|(Intercept) |   0.7810|    0.0200|   39.1219|       0|
|Nr_People_H |   0.2593|    0.0072|   36.2368|       0|



|  $R^2$| $R^2_{adj}$| statistic| p.value| nobs|
|------:|-----------:|---------:|-------:|----:|
| 0.1962|       0.196|  1313.108|       0| 5382|


\needspace{13\baselineskip}

**Model B :** N_trips ~ Nr_People_H + Driv_Lic 



|term        | estimate| std.error| statistic| p.value|
|:-----------|--------:|---------:|---------:|-------:|
|(Intercept) |   0.7138|    0.0214|   33.4026|       0|
|Nr_People_H |   0.2578|    0.0071|   36.2469|       0|
|Driv_Lic    |   0.1326|    0.0157|    8.4395|       0|



|  $R^2$| $R^2_{adj}$| statistic| p.value| nobs|
|------:|-----------:|---------:|-------:|----:|
| 0.2067|      0.2064|  700.7369|       0| 5382|


\needspace{14\baselineskip}

**Model C :** N_trips ~ Learners_students + Workers_Fulltime + Driv_Lic 



|term              | estimate| std.error| statistic| p.value|
|:-----------------|--------:|---------:|---------:|-------:|
|(Intercept)       |   1.1098|    0.0121|   91.4284|       0|
|Learners_students |   1.2051|    0.0211|   57.1224|       0|
|Workers_Fulltime  |   0.2804|    0.0147|   19.0127|       0|
|Driv_Lic          |   0.0902|    0.0148|    6.0951|       0|



|  $R^2$| $R^2_{adj}$| statistic| p.value| nobs|
|------:|-----------:|---------:|-------:|----:|
| 0.4141|      0.4138|  1268.247|       0| 5387|


\needspace{19\baselineskip}

**Model D :** N_trips ~ Nr_People_H + Elderly + Educ_HS + Educ_Tertiary + Workers_Fulltime +      Workers_Parttime + Learners_students + Driv_Lic 



|term              | estimate| std.error| statistic| p.value|
|:-----------------|--------:|---------:|---------:|-------:|
|(Intercept)       |   0.7061|    0.0168|   42.0480|  0.0000|
|Nr_People_H       |   0.1298|    0.0067|   19.4641|  0.0000|
|Elderly           |  -0.0215|    0.0090|   -2.3972|  0.0166|
|Educ_HS           |   0.2333|    0.0128|   18.2600|  0.0000|
|Educ_Tertiary     |   0.1874|    0.0181|   10.3653|  0.0000|
|Workers_Fulltime  |   0.1449|    0.0141|   10.2822|  0.0000|
|Workers_Parttime  |   0.3362|    0.0240|   13.9815|  0.0000|
|Learners_students |   0.9843|    0.0209|   47.0492|  0.0000|
|Driv_Lic          |   0.0407|    0.0132|    3.0911|  0.0020|



|  $R^2$| $R^2_{adj}$| statistic| p.value| nobs|
|------:|-----------:|---------:|-------:|----:|
| 0.4781|      0.4773|  607.7632|       0| 5316|


\needspace{14\baselineskip}

**Model E :** N_trips ~ Nr_People_H + Learners_students + Driv_Lic 



|term              | estimate| std.error| statistic| p.value|
|:-----------------|--------:|---------:|---------:|-------:|
|(Intercept)       |   0.8186|    0.0179|   45.6624|       0|
|Nr_People_H       |   0.1628|    0.0062|   26.1003|       0|
|Learners_students |   1.0404|    0.0213|   48.7583|       0|
|Driv_Lic          |   0.1803|    0.0131|   13.7411|       0|



|  $R^2$| $R^2_{adj}$| statistic| p.value| nobs|
|------:|-----------:|---------:|-------:|----:|
| 0.4499|      0.4496|  1465.998|       0| 5382|


\needspace{14\baselineskip}

**Model F :** N_trips ~ Nr_People_H + Workers_Fulltime + Learners_students 



|term              | estimate| std.error| statistic| p.value|
|:-----------------|--------:|---------:|---------:|-------:|
|(Intercept)       |   0.8114|    0.0170|   47.7995|       0|
|Nr_People_H       |   0.1485|    0.0062|   24.1355|       0|
|Workers_Fulltime  |   0.2717|    0.0129|   21.0755|       0|
|Learners_students |   1.0365|    0.0208|   49.7738|       0|



| $R^2$| $R^2_{adj}$| statistic| p.value| nobs|
|-----:|-----------:|---------:|-------:|----:|
| 0.474|      0.4737|  1615.479|       0| 5382|

# 5. Model comparison and parsimony discussion

The full predictor list per model is long for Model D, so - to keep every
table column comfortably inside the page margins - it is listed once here
as plain text (which wraps like any other paragraph) rather than repeated
inside a table cell.

- **A**: N_trips ~ Nr_People_H
- **B**: N_trips ~ Nr_People_H + Driv_Lic
- **C**: N_trips ~ Learners_students + Workers_Fulltime + Driv_Lic
- **D**: N_trips ~ Nr_People_H + Elderly + Educ_HS + Educ_Tertiary + Workers_Fulltime + Workers_Parttime + Learners_students + Driv_Lic
- **E**: N_trips ~ Nr_People_H + Learners_students + Driv_Lic
- **F**: N_trips ~ Nr_People_H + Workers_Fulltime + Learners_students

## Version A - Base R comparison table


``` r
comparison <- data.frame(
  Model = names(models),
  Predictors = sapply(models, get_predictor_str),
  R2 = sapply(models, function(m) round(summary(m)$r.squared, 4)),
  Adj_R2 = sapply(models, function(m) round(summary(m)$adj.r.squared, 4)),
  n = sapply(models, function(m) nobs(m)),
  stringsAsFactors = FALSE
)

write.csv(comparison, "model_comparison.csv", row.names = FALSE)

comparison_display <- comparison[, c("Model", "R2", "Adj_R2", "n")]
colnames(comparison_display) <- c("Model", "$R^2$", "$R^2_{adj}$", "n")

knitr::kable(comparison_display, row.names = FALSE, escape = FALSE,
             caption = "Base R model comparison table (see predictor list above)")
```



Table: Base R model comparison table (see predictor list above)

|Model |  $R^2$| $R^2_{adj}$|    n|
|:-----|------:|-----------:|----:|
|A     | 0.1962|      0.1960| 5382|
|B     | 0.2067|      0.2064| 5382|
|C     | 0.4141|      0.4138| 5387|
|D     | 0.4781|      0.4773| 5316|
|E     | 0.4499|      0.4496| 5382|
|F     | 0.4740|      0.4737| 5382|

## Version B - Package (`broom` + `dplyr`) comparison table


``` r
ensure_pkg("dplyr")
ensure_pkg("purrr")

comparison_broom <- purrr::map_dfr(names(models), function(nm) {
  g <- broom::glance(models[[nm]])
  dplyr::tibble(Model = nm,
                Predictors = get_predictor_str(models[[nm]]),
                R2 = round(g$r.squared, 4),
                Adj_R2 = round(g$adj.r.squared, 4),
                n = g$nobs)
})

comparison_broom_display <- comparison_broom[, c("Model", "R2", "Adj_R2", "n")]
colnames(comparison_broom_display) <- c("Model", "$R^2$", "$R^2_{adj}$", "n")

knitr::kable(comparison_broom_display, row.names = FALSE, escape = FALSE,
             caption = "broom/dplyr model comparison table (see predictor list above)")
```



Table: broom/dplyr model comparison table (see predictor list above)

|Model |  $R^2$| $R^2_{adj}$|    n|
|:-----|------:|-----------:|----:|
|A     | 0.1962|      0.1960| 5382|
|B     | 0.2067|      0.2064| 5382|
|C     | 0.4141|      0.4138| 5387|
|D     | 0.4781|      0.4773| 5316|
|E     | 0.4499|      0.4496| 5382|
|F     | 0.4740|      0.4737| 5382|



The two comparison tables agree exactly (checked above), as expected since
both simply reformat the same fitted models.

### Coefficient significance


``` r
sig_flags <- function(m) {
  cf <- summary(m)$coefficients
  cf <- cf[rownames(cf) != "(Intercept)", , drop = FALSE]
  data.frame(term = rownames(cf),
             t = round(cf[, "t value"], 2),
             p = round(cf[, "Pr(>|t|)"], 4),
             sig = cf[, "Pr(>|t|)"] < 0.05)
}

for (nm in names(models)) {
  flags <- sig_flags(models[[nm]])
  needspace(nrow(flags) + 8)
  cat("**Model", nm, "**\n\n")
  print(knitr::kable(flags, row.names = FALSE))
  cat("\n\n")
}
```

\needspace{9\baselineskip}

**Model A **



|term        |     t|  p|sig  |
|:-----------|-----:|--:|:----|
|Nr_People_H | 36.24|  0|TRUE |


\needspace{10\baselineskip}

**Model B **



|term        |     t|  p|sig  |
|:-----------|-----:|--:|:----|
|Nr_People_H | 36.25|  0|TRUE |
|Driv_Lic    |  8.44|  0|TRUE |


\needspace{11\baselineskip}

**Model C **



|term              |     t|  p|sig  |
|:-----------------|-----:|--:|:----|
|Learners_students | 57.12|  0|TRUE |
|Workers_Fulltime  | 19.01|  0|TRUE |
|Driv_Lic          |  6.10|  0|TRUE |


\needspace{16\baselineskip}

**Model D **



|term              |     t|      p|sig  |
|:-----------------|-----:|------:|:----|
|Nr_People_H       | 19.46| 0.0000|TRUE |
|Elderly           | -2.40| 0.0166|TRUE |
|Educ_HS           | 18.26| 0.0000|TRUE |
|Educ_Tertiary     | 10.37| 0.0000|TRUE |
|Workers_Fulltime  | 10.28| 0.0000|TRUE |
|Workers_Parttime  | 13.98| 0.0000|TRUE |
|Learners_students | 47.05| 0.0000|TRUE |
|Driv_Lic          |  3.09| 0.0020|TRUE |


\needspace{11\baselineskip}

**Model E **



|term              |     t|  p|sig  |
|:-----------------|-----:|--:|:----|
|Nr_People_H       | 26.10|  0|TRUE |
|Learners_students | 48.76|  0|TRUE |
|Driv_Lic          | 13.74|  0|TRUE |


\needspace{11\baselineskip}

**Model F **



|term              |     t|  p|sig  |
|:-----------------|-----:|--:|:----|
|Nr_People_H       | 24.14|  0|TRUE |
|Workers_Fulltime  | 21.08|  0|TRUE |
|Learners_students | 49.77|  0|TRUE |

### Parsimony commentary

The commentary below is computed directly from the fitted results above
(not hard-coded), so it stays accurate if the underlying data changes.

Model A (single predictor Nr_People_H) already explains $R^2$ = 0.196 ($R^2_{adj}$ = 0.196).

Among the reduced (non-D) models, Model F achieves the highest $R^2_{adj}$ (0.474) using only 3 predictor(s): Nr_People_H + Workers_Fulltime + Learners_students.

Model D (all 8 predictors) reaches $R^2_{adj}$ = 0.477 -- a gain of just 0.0036 over Model F, for 5 extra predictors.

Manual VIFs for all 8 predictors are below 2 (max is 1.612), so multicollinearity is not severe enough on its own to destabilise Model D's coefficients -- all 8 coefficients in Model D remain statistically significant at the 5% level. However, several of the predictors are moderately correlated with each other (e.g. Nr_People_H-Elderly, Educ_Tertiary-Driv_Lic, Educ_Tertiary-Workers_Fulltime), so part of what they add in Model D duplicates explanatory power already captured by the core predictors (Nr_People_H, Learners_students, Workers_Fulltime, Driv_Lic).

Given that Model F recovers 99.2% of Model D's $R^2_{adj}$ with 5 fewer predictors, the marginal explanatory gain from fitting the full 8-predictor Model D does not appear to be worth its loss of parsimony: a small, well-chosen predictor set (household size, learners/students, workers, driver's licence holding) captures almost all of the explanatory power at much lower risk of overfitting and unnecessary collinearity.

# Outputs

Outputs written to the working directory:

- `figures/correlation_heatmap_baseR.png`
- `figures/correlation_ellipse.png`
- `figures/scatterplots_baseR.png`
- `figures/scatterplots_ggplot.png`
- `model_comparison.csv`
