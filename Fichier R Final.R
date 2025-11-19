### TP – Estimation du nombre total d’élèves
library(dplyr)
library(ggplot2)

### ---------------------------
### 1. Phase 1 : Tirage aléatoire simple
### ---------------------------

df <- read.csv2("C:/Users/lnugier/OneDrive - univ-lyon2.fr/Pièces jointes/S3/Technique de sondage et méthodologie d'enquete/TP noté/elementaire_2024_id.csv")

set.seed(123)
echantillon <- sample(df$code_ecole, size = 1000, replace = FALSE)
write.csv(data.frame(code_ecole = echantillon),
          "echantillon_code_ecole.csv",
          row.names = FALSE)

### ---------------------------
### 2. Phase 2 : Lecture des résultats
### ---------------------------

df2 <- read.csv2("resultats nugier.csv")

N <- 47413
y <- df2$eleves
n <- length(y)

### ---------------------------
### 3. Estimation T1 (TAS)
### ---------------------------

mu <- mean(y)
SE_mu <- sqrt((1 - n/N) * var(y) / n)

IC1_inf <- (mu - 1.96 * SE_mu) * N
IC1_sup <- (mu + 1.96 * SE_mu) * N
T1 <- mu * N

### ---------------------------
### 4. Estimation T2 (ratio secteur)
### ---------------------------

N_public <- 42811
N_prive  <- 4602

n_public <- sum(df2$secteur == "PUBLIC")
n_prive  <- sum(df2$secteur == "PRIVE")

ybar_public <- mean(df2$eleves[df2$secteur == "PUBLIC"])
ybar_prive  <- mean(df2$eleves[df2$secteur == "PRIVE"])

T2 <- N_public * ybar_public + N_prive * ybar_prive

s2_public <- var(df2$eleves[df2$secteur == "PUBLIC"])
s2_prive  <- var(df2$eleves[df2$secteur == "PRIVE"])

Var_T2 <- (N_public^2 * (1 - n_public/N_public) * s2_public / n_public) +
  (N_prive^2  * (1 - n_prive/N_prive)   * s2_prive  / n_prive)

SE_T2 <- sqrt(Var_T2)

IC2_inf <- T2 - 1.96 * SE_T2
IC2_sup <- T2 + 1.96 * SE_T2

### ---------------------------
### 5. Estimation T3 (ratio REP)
### ---------------------------

N_REP1 <- 6589
N_REP0 <- 40824

n_REP1 <- sum(df2$rep == 1)
n_REP0 <- sum(df2$rep == 0)

ybar_REP1 <- mean(df2$eleves[df2$rep == 1])
ybar_REP0 <- mean(df2$eleves[df2$rep == 0])

T3 <- N_REP1 * ybar_REP1 + N_REP0 * ybar_REP0

s2_REP1 <- var(df2$eleves[df2$rep == 1])
s2_REP0 <- var(df2$eleves[df2$rep == 0])

Var_T3 <- (N_REP1^2 * (1 - n_REP1/N_REP1) * s2_REP1 / n_REP1) +
  (N_REP0^2 * (1 - n_REP0/N_REP0) * s2_REP0 / n_REP0)

SE_T3 <- sqrt(Var_T3)

IC3_inf <- T3 - 1.96 * SE_T3
IC3_sup <- T3 + 1.96 * SE_T3

### ---------------------------
### 6. Tableau des résultats Phase 2
### ---------------------------

resultats_phase2 <- data.frame(
  Méthode = c("T1 (TAS)", "T2 (ratio secteur)", "T3 (ratio REP)"),
  Estimation = round(c(T1, T2, T3)),
  IC_inf = round(c(IC1_inf, IC2_inf, IC3_inf)),
  IC_sup = round(c(IC1_sup, IC2_sup, IC3_sup))
)

print(resultats_phase2)

### ---------------------------
### 7. Graphiques Phase 2
### ---------------------------

# Histogramme du nombre d’élèves
ggplot(df2, aes(x = eleves)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  labs(title = "Histogramme du nombre d’élèves par école",
       x = "Nombre d’élèves", y = "Effectif")

# Boxplot secteur sans outliers (strat)
lim <- quantile(df2$eleves, 0.95)
ggplot(df2 %>% filter(eleves <= lim), aes(x = secteur, y = eleves, fill = secteur)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Boxplot du nombre d’élèves par secteur (95e percentile)",
       x = "Secteur", y = "Nombre d’élèves") +
  scale_fill_brewer(palette = "Set2")

# Nuage de points secteur (sans outliers)
ggplot(df2 %>% filter(eleves <= lim),
       aes(x = secteur, y = eleves, color = secteur)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  labs(title = "Nuage de points : élèves par école selon le secteur",
       x = "Secteur", y = "Nombre d’élèves") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()


### ---------------------------
### 8. Phase 3 – Tirage stratifié selon secteur
### ---------------------------

df_aux <- read.csv2("elementaire_2024_auxiliaires_secteur.csv")

set.seed(2025)
ech_public <- df_aux %>% filter(secteur == "PUBLIC") %>% sample_n(903)
ech_prive <- df_aux %>% filter(secteur == "PRIVE") %>% sample_n(97)
echantillon <- bind_rows(ech_public, ech_prive) %>% select(code_ecole)

### ---------------------------
### 9. Phase 3 – Estimation sur l'échantillon stratifié
### ---------------------------

df2 <- read.csv2("resultats echantillon_1000_nugier_secteur.csv")

n_public <- sum(df2$secteur == "PUBLIC")
n_prive  <- sum(df2$secteur == "PRIVE")

ybar_public <- mean(df2$eleves[df2$secteur == "PUBLIC"])
ybar_prive  <- mean(df2$eleves[df2$secteur == "PRIVE"])

T_est <- N_public * ybar_public + N_prive * ybar_prive

s2_public <- var(df2$eleves[df2$secteur == "PUBLIC"])
s2_prive  <- var(df2$eleves[df2$secteur == "PRIVE"])

Var_T <- (N_public^2 * (1 - n_public/N_public) * s2_public / n_public) +
  (N_prive^2 * (1 - n_prive/N_prive) * s2_prive / n_prive)

SE_T <- sqrt(Var_T)

IC_inf <- T_est - 1.96 * SE_T
IC_sup <- T_est + 1.96 * SE_T

resultats_phase3 <- data.frame(
  Méthode = "T stratifié secteur (phase 3)",
  Estimation = round(T_est),
  IC_inf = round(IC_inf),
  IC_sup = round(IC_sup)
)

print(resultats_phase3)

### ---------------------------
### 10. Graphiques Phase 3
### ---------------------------

# Histogramme par secteur
ggplot(df2, aes(x = eleves, fill = secteur)) +
  geom_histogram(position = "dodge", bins = 20, color = "white") +
  labs(title = "Histogramme du nombre d’élèves par secteur (échantillon stratifié)",
       x = "Nombre d’élèves", y = "Effectif") +
  scale_fill_brewer(palette = "Set2")

# Boxplot par secteur (sans outliers)
lim <- quantile(df2$eleves, 0.95)
ggplot(df2 %>% filter(eleves <= lim), aes(x = secteur, y = eleves, fill = secteur)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Boxplot du nombre d’élèves par secteur (échantillon stratifié)",
       x = "Secteur", y = "Nombre d’élèves") +
  scale_fill_brewer(palette = "Set1")



### ---------------------------
### 11. Synthèse finale – Tableau comparatif
### ---------------------------

resultats_synthese <- bind_rows(resultats_phase2, resultats_phase3)
print(resultats_synthese)
