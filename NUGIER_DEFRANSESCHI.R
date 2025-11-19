library(readxl)
library(dplyr)
#Phase 1 action 1 : Choix des 1000 écoles.
# Charger le fichier CSV
df <- read.csv2("C:/Users/lnugier/OneDrive - univ-lyon2.fr/Pièces jointes/S3/Technique de sondage et méthodologie d'enquete/TP noté/elementaire_2024_id.csv")
echantillon <- sample(df$code_ecole, size = 1000, replace = FALSE)
echantillon_df <- data.frame(code_ecole = echantillon)
write.csv(echantillon_df, "echantillon_code_ecole.csv", row.names = FALSE)

df2 <- read.csv2("C:/Users/lnugier/OneDrive - univ-lyon2.fr/Pièces jointes/S3/Technique de sondage et méthodologie d'enquete/TP noté/resultats nugier.csv")


# Phase 1 : Estimation T chapeau 1 (nb elèves moy par etablissement en fr)
df2 <- read.csv2("C:/Users/lnugier/OneDrive - univ-lyon2.fr/Pièces jointes/S3/Technique de sondage et méthodologie d'enquete/TP noté/resultats nugier.csv")

N <- 47413
y <- df2$eleves
n <- length(y)

# Moyenne estimée
mu_chapeau <- mean(y)

# Erreur standard de la moyenne
SE_mu <- sqrt((1 - n/N) * var(y) / n)

# Intervalle de confiance 95 %
IC_inf <- mu_chapeau - 1.96 * SE_mu
IC_sup <- mu_chapeau + 1.96 * SE_mu
# IC]128;141[ pour une moy (mu_chapeau) 134

IC_total_inf <- IC_inf * N
IC_total_sup <- IC_sup * N
mu_chapeau_total <- mu_chapeau * N


