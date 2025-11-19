#Phase 1 action 1 : Choix des 1000 écoles.
# Charger le fichier CSV
df <- read.csv2("C:/Users/lnugier/OneDrive - univ-lyon2.fr/Pièces jointes/S3/Technique de sondage et méthodologie d'enquete/TP noté/elementaire_2024_id.csv")

echantillon <- sample(df$code_ecole, size = 1000, replace = FALSE)

echantillon_df <- data.frame(code_ecole = echantillon)

write.csv(echantillon_df, "echantillon_code_ecole.csv", row.names = FALSE)

