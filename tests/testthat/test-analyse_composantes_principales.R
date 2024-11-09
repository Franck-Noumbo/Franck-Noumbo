# 1. Test de base avec PCA
donnees <- data.frame(
  var_1 = rnorm(100, mean = 5, sd = 2),
  var_2 = rnorm(100, mean = 3, sd = 1),
  var_3 = rnorm(100, mean = 4, sd = 1.5)
)
cat("Test 1 : PCA sur un jeu de données simple\n")
pca_resultat <- prcomp(donnees, scale. = TRUE)
print(summary(pca_resultat))  # Résumé des résultats PCA

# 2. Test avec des valeurs manquantes (NA)
donnees_na <- data.frame(
  var_1 = c(1, 2, NA, 4, 5),
  var_2 = c(2, NA, 4, 5, 6),
  var_3 = c(NA, 3, 2, 6, 1)
)
cat("\nTest 2 : PCA avec des valeurs manquantes (NA)\n")
try(pca_na <- prcomp(donnees_na, scale. = TRUE), silent = TRUE)  # Gestion des NA (Erreur attendue)

# 3. Test avec un jeu de données vide
donnees_vides <- data.frame()
cat("\nTest 3 : PCA avec un jeu de données vide (doit échouer)\n")
try(pca_vides <- prcomp(donnees_vides, scale. = TRUE), silent = TRUE)  # Erreur attendue

# 4. Test avec une seule variable (PCA devrait échouer ou donner une seule composante)
donnees_1var <- data.frame(var_1 = rnorm(100, mean = 5, sd = 2))
cat("\nTest 4 : PCA avec une seule variable\n")
pca_1var <- prcomp(donnees_1var, scale. = TRUE)
print(summary(pca_1var))  # Résumé des résultats PCA

# 5. Test avec des variables hautement corrélées
donnees_corr <- data.frame(
  var_1 = rnorm(100, mean = 5, sd = 2),
  var_2 = rnorm(100, mean = 5, sd = 2)
)
donnees_corr$var_2 <- donnees_corr$var_1 + rnorm(100, mean = 0, sd = 0.1)  # Haute corrélation
cat("\nTest 5 : PCA avec des variables fortement corrélées\n")
pca_corr <- prcomp(donnees_corr, scale. = TRUE)
print(summary(pca_corr))  # Résumé des résultats PCA
