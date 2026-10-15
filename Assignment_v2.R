# =============================================================================
# Customer Segmentation Analysis - Purchasing Data
# Marketing Analytics - Group 6  |  Revised version
#
# Changes vs. Assignment.R:
#   - Correctly diagnoses missingness as SURVEY SKIP LOGIC, not dropout
#   - Adds gap statistic + full diagnostic table for choosing k
#   - Adds EXTERNAL VALIDATION on variables not used in clustering (Q04/C1/Q10/D08/D09)
#   - Adds top-code sensitivity analysis and Gower/PAM robustness check
#   - Adds bootstrap cluster stability (fpc::clusterboot)
#   - Profiles the 1,631 excluded non-card-holders as an unserved segment
#   - Fixes the broken ggplot chain that stripped Figure 3's labels
#   - install.packages() guarded; single seed; no stray YAML
# =============================================================================

## ---- 0. SETUP --------------------------------------------------------------
pkgs <- c("tidyverse", "readxl", "cluster", "factoextra", "mclust", "fpc")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) {
  install.packages(p, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(cluster)
  library(factoextra); library(mclust); library(fpc)
})

SEED <- 123          # single seed used throughout
DATA_PATH <- "C:/Users/israe/Desktop/Marketing Analytics Assignment/Purchasing_data.xlsx"


## ---- 1. LOAD ---------------------------------------------------------------
raw <- read_xlsx(DATA_PATH, sheet = "Sheet1")

# Row 1 holds the full question text; real responses start on row 2 of the
# tibble. Keep the question text -- it is useful for labelling output.
question_text <- setNames(as.character(raw[1, ]), names(raw))
dat <- raw[-1, ]

cat("Respondents:", nrow(dat), " Variables:", ncol(dat), "\n")


## ---- 2. MISSINGNESS DIAGNOSIS ----------------------------------------------
# The original script assumed respondents "stopped part-way through". Test it.

skip_check <- table(used_credit_card = dat$Q01_3, Q02_missing = is.na(dat$Q02))
print(skip_check)
# -> 1,631 respondents who did NOT select "Credit card" in Q01 are missing Q02.
#    The association is perfect: this is questionnaire ROUTING, not attrition.

miss_profile <- tibble(
  n_total                    = nrow(dat),
  routed_out_no_credit_card  = sum(dat$Q01_3 == "0"),
  true_dropout_after_Q02     = sum(!is.na(dat$Q02) & is.na(dat$D11)),
  complete_on_seg_vars       = sum(complete.cases(dat[, c("Q02","Q03","Q05","D11")]))
)
print(miss_profile)

# IMPLICATION: the clustering sample contains ONLY active credit card holders.
# The 1,631 non-holders are a separate, un-analysable-by-this-model group.


## ---- 3. RECODE ORDINAL RESPONSES TO NUMERIC MIDPOINTS ----------------------
no_cards_map <- c("None" = 0, "One" = 1, "Two" = 2, "Three" = 3,
                  "Four" = 4, "Five" = 5, "More than five" = 6)
balance_map  <- c("Zero" = 0, "Less than $500" = 250, "$501 to $1,000" = 750,
                  "$1,001 to $2,000" = 1500, "$2,001 to $3,000" = 2500,
                  "$3,001 to $4,000" = 3500, "$4,001 to $5,000" = 4500,
                  "$5,001 to $7,000" = 6000, "$7,001 to $10,000" = 8500,
                  "More than $10,000" = 12000)   # top-coded; tested in Section 8
pct_cc_map   <- c("Never used credit card" = 0, "Less than 20 percent" = 10,
                  "At least 20 percent but less than 40 percent" = 30,
                  "At least 40 percent but less than 60 percent" = 50,
                  "At least 60 percent but less than 80 percent" = 70,
                  "At least 80 percent" = 90)
income_map   <- c("Below $25,000" = 12500, "Between $25,000 and $49,999" = 37500,
                  "Between $50,000 and $74,999" = 62500,
                  "Between $75,000 and $99,999" = 87500,
                  "More than $100,000" = 125000) # also top-coded

# Guard: fail loudly if any observed level is unmapped (silent NA is a common bug)
check_map <- function(x, map, nm) {
  bad <- setdiff(unique(na.omit(trimws(x))), names(map))
  if (length(bad)) stop("Unmapped levels in ", nm, ": ", paste(bad, collapse = " | "))
  invisible(TRUE)
}
check_map(dat$Q02, no_cards_map, "Q02"); check_map(dat$Q03, balance_map, "Q03")
check_map(dat$Q05, pct_cc_map,  "Q05"); check_map(dat$D11, income_map,  "D11")

analysis <- dat %>%
  mutate(across(c(Q02, Q03, Q05, D11), trimws)) %>%
  mutate(no_cards = as.numeric(no_cards_map[Q02]),
         balance  = as.numeric(balance_map[Q03]),
         pct_cc   = as.numeric(pct_cc_map[Q05]),
         income   = as.numeric(income_map[D11])) %>%
  filter(!is.na(no_cards), !is.na(balance), !is.na(pct_cc), !is.na(income))

seg_vars <- c("no_cards", "balance", "pct_cc", "income")
seg_data <- analysis[, seg_vars]
cat("Complete cases retained:", nrow(seg_data), "\n")

# Evidence that no non-users survive: the 0 categories never appear.
print(summary(seg_data))
cat("Skewness of balance:",
    round(mean((seg_data$balance - mean(seg_data$balance))^3) / sd(seg_data$balance)^3, 2), "\n")
print(round(cor(seg_data), 3))


## ---- 4. SCALE --------------------------------------------------------------
scaled <- scale(seg_data)


## ---- 5. CHOOSING k: FULL DIAGNOSTICS ---------------------------------------
d <- dist(scaled)

diagnostics <- map_dfr(2:8, function(k) {
  set.seed(SEED); km <- kmeans(scaled, k, nstart = 25)
  tibble(k = k,
         silhouette = mean(silhouette(km$cluster, d)[, 3]),
         wcss       = km$tot.withinss,
         pct_BSS    = 100 * km$betweenss / km$totss)
}) %>% mutate(across(-k, ~round(.x, 3)))
print(diagnostics)

set.seed(SEED)
gap <- clusGap(scaled, FUN = kmeans, nstart = 25, K.max = 8, B = 50)
print(gap, method = "firstSEmax")
cat("Gap statistic (firstSEmax) selects k =",
    maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "firstSEmax"), "\n")

set.seed(SEED); p_wss <- fviz_nbclust(scaled, kmeans, method = "wss",        nstart = 25)
set.seed(SEED); p_sil <- fviz_nbclust(scaled, kmeans, method = "silhouette", nstart = 25)
print(p_wss); print(p_sil)

# HONEST READING: silhouette is flat (0.26-0.29) across k = 2..8 and never
# exceeds 0.30 -> weak natural structure. Gap favours k = 6. No internal index
# selects k = 4. k = 4 is therefore justified on MANAGERIAL grounds and
# defended empirically by the external validation in Section 7.


## ---- 6. FIT k-MEANS AT k = 4 -----------------------------------------------
K <- 4
set.seed(SEED)
km <- kmeans(scaled, centers = K, nstart = 25)
analysis$cluster <- factor(km$cluster)

cluster_profile <- analysis %>%
  group_by(cluster) %>%
  summarise(n = n(),
            avg_cards   = round(mean(no_cards), 1),
            avg_balance = round(mean(balance)),
            avg_pct_cc  = round(mean(pct_cc), 1),
            avg_income  = round(mean(income)))
print(cluster_profile)
cat("Between/Total SS:", round(100 * km$betweenss / km$totss, 1), "%\n")

# Cluster 4 is essentially a threshold on `balance` alone -- show it:
print(table(analysis$Q03, analysis$cluster))


## ---- 7. EXTERNAL VALIDATION (variables NOT used to build the clusters) -----
# This is the non-circular test that the segments are real.
cs_map <- c("300 to 400"=350,"401 to 500"=450,"501 to 600"=550,"601 to 650"=625,
            "651 to 700"=675,"701 to 750"=725,"751 to 800"=775,"801 to 850"=825)

validation <- analysis %>%
  mutate(credit_score = as.numeric(cs_map[C1]),
         pays_in_full = Q04 == "Always",
         used_bnpl    = Q10 %in% c("Once", "A few times", "Many times")) %>%
  group_by(cluster) %>%
  summarise(n = n(),
            mean_credit_score = round(mean(credit_score, na.rm = TRUE)),
            pct_pays_in_full  = round(100 * mean(pays_in_full), 1),
            pct_used_BNPL     = round(100 * mean(used_bnpl), 1),
            pct_65_plus       = round(100 * mean(D08 == "65 or older"), 1),
            pct_grad_degree   = round(100 * mean(grepl("Graduate|Master|Doctor|Professional", D09)), 1))
print(validation)

# Significance tests
val_dat <- analysis %>% mutate(credit_score = as.numeric(cs_map[C1]))
cat("\nANOVA: credit score ~ cluster\n"); print(summary(aov(credit_score ~ cluster, val_dat)))
cat("\nChi-square: Q04 (pay in full) x cluster\n"); print(chisq.test(table(analysis$cluster, analysis$Q04)))
cat("\nChi-square: Q10 (BNPL) x cluster\n");        print(chisq.test(table(analysis$cluster, analysis$Q10)))

# Profile the EXCLUDED non-card-holders as a fifth, unserved segment
excluded <- dat %>% filter(Q01_3 == "0")
cat("\nExcluded non-card-holders:", nrow(excluded), "\n")
cat("Their payment mix (% using each method):\n")
print(round(100 * colMeans(excluded[, paste0("Q01_", 1:8)] != "0"), 1))


## ---- 8. ROBUSTNESS AND SENSITIVITY -----------------------------------------

# 8a. Sensitivity to the arbitrary top-code for "More than $10,000"
topcode_sensitivity <- map_dfr(c(11000, 12000, 15000, 20000, 25000), function(v) {
  bm <- balance_map; bm["More than $10,000"] <- v
  sd2 <- analysis %>% mutate(balance = as.numeric(bm[Q03])) %>% select(all_of(seg_vars))
  set.seed(SEED); k2 <- kmeans(scale(sd2), K, nstart = 25)
  tibble(topcode = v,
         ARI_vs_baseline  = round(adjustedRandIndex(k2$cluster, km$cluster), 3),
         smallest_cluster = min(k2$size))
})
print(topcode_sensitivity)

# 8b. Method robustness: all inputs are ORDINAL. Gower + PAM makes no
#     equal-interval assumption. Low ARI => solution is method-dependent.
ordf <- function(x, lv) factor(x, levels = lv, ordered = TRUE)
gow_df <- data.frame(
  cards = ordf(analysis$Q02, names(no_cards_map)[-1]),
  bal   = ordf(analysis$Q03, c("Less than $500","$501 to $1,000","$1,001 to $2,000",
                               "$2,001 to $3,000","$3,001 to $4,000","$4,001 to $5,000",
                               "$5,001 to $7,000","$7,001 to $10,000","More than $10,000")),
  pct   = ordf(analysis$Q05, names(pct_cc_map)[-1]),
  inc   = ordf(analysis$D11, names(income_map)))
gow_d  <- daisy(gow_df, metric = "gower")
pam4   <- pam(gow_d, K, diss = TRUE)
cat("Gower/PAM avg silhouette:", round(pam4$silinfo$avg.width, 3),
    "| ARI vs k-means:", round(adjustedRandIndex(pam4$clustering, km$cluster), 3), "\n")

# 8c. Bootstrap stability. Jaccard > 0.75 = stable; < 0.60 = unstable.
set.seed(SEED)
boot <- clusterboot(scaled, B = 100, clustermethod = kmeansCBI, krange = K,
                    seed = SEED, count = FALSE)
cat("Bootstrap mean Jaccard by cluster:", round(boot$bootmean, 3), "\n")

# 8d. What if pay-in-full (Q04) were a segmentation variable?
payfull_num <- as.numeric(ordf(analysis$Q04, c("Never","Rarely","Sometimes","Usually","Always")))
m5 <- scale(cbind(scaled, payfull = payfull_num))
set.seed(SEED); km5 <- kmeans(m5, K, nstart = 25)
cat("5-variable solution ARI vs 4-variable:", round(adjustedRandIndex(km5$cluster, km$cluster), 3), "\n")
print(cbind(aggregate(as.matrix(seg_data) ~ cl, data.frame(seg_data, cl = km5$cluster), mean),
            payfull = round(tapply(payfull_num, km5$cluster, mean), 2),
            n = as.integer(table(km5$cluster))))


## ---- 9. VISUALISATIONS -----------------------------------------------------
seg_labels <- c("1: Low-Intensity Card Users", "2: Multi-Card Optimizers",
                "3: Convenience / Premium Payers", "4: High-Balance Revolvers")

# Fig 3 -- FIXED: the original lost its labels because the `+` chain was broken
# after geom_jitter(). geom_count() also shows the discrete grid honestly.
p3 <- ggplot(analysis, aes(x = balance, y = pct_cc, colour = cluster)) +
  geom_jitter(width = 120, height = 3, size = 0.7, alpha = 0.45) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Customer Segments: Balance vs. Credit Card Usage",
       x = "Typical Monthly Credit Card Balance",
       y = "Share of Purchases Made by Credit Card (%)",
       colour = "Segment") +
  scale_colour_discrete(labels = seg_labels) +
  theme_minimal()
print(p3)

p4 <- ggplot(analysis, aes(x = cluster, fill = cluster)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  labs(title = paste0("Segment Sizes (N = ", nrow(analysis), ")"),
       x = "Segment", y = "Number of Customers") +
  theme_minimal() + theme(legend.position = "none")
print(p4)

p5 <- ggplot(analysis, aes(x = cluster, y = income, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Income Distribution by Segment",
       subtitle = "Ceiling at $125,000 is the recoded top category, not a real maximum",
       x = "Segment", y = "Personal Income") +
  theme_minimal() + theme(legend.position = "none")
print(p5)

# PCA view -- now reporting variance explained, and flagged as descriptive only
pca <- prcomp(scaled)
var_exp <- round(100 * summary(pca)$importance[2, 1:2], 1)
cat("PC1 =", var_exp[1], "% ; PC2 =", var_exp[2], "% ; cumulative =", sum(var_exp), "%\n")

p6 <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(cluster = analysis$cluster) %>%
  ggplot(aes(PC1, PC2, colour = cluster)) +
  geom_point(size = 1.6, alpha = 0.6) +
  labs(title = "Segments on the First Two Principal Components",
       subtitle = paste0("PC1 ", var_exp[1], "% + PC2 ", var_exp[2],
                         "% = ", sum(var_exp), "% of variance. Descriptive only: ",
                         "PCA uses the same inputs as the clustering."),
       x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)"),
       colour = "Segment") +
  theme_minimal()
print(p6)


## ---- 10. EXPORT ------------------------------------------------------------
out_dir <- "C:/Users/israe/Desktop/Marketing Analytics Assignment"
write.csv(analysis[, c(seg_vars, "cluster")], file.path(out_dir, "clustered_data.csv"),
          row.names = FALSE)
write.csv(cluster_profile, file.path(out_dir, "cluster_profile.csv"), row.names = FALSE)
write.csv(validation,      file.path(out_dir, "external_validation.csv"), row.names = FALSE)

sessionInfo()

