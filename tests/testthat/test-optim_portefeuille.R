library(quadprog)
# 1. Test de base avec une matrice de covariance et des rendements simples
covariance <- matrix(c(0.04, 0.02, 0.02, 0.03), nrow = 2)  # Matrice de covariance des actifs
rendements <- c(0.06, 0.08)  # Rendements attendus des actifs
niveau_risque <- 0.005  # Niveau de risque cible

cat("Test 1 : Optimisation du portefeuille avec des rendements simples\n")
resultat <- optim_portefeuille(covariance, rendements, niveau_risque)
print(resultat)  # Afficher les pondérations optimales des actifs

# 2. Test avec une matrice de covariance non définie positive
covariance_non_pos_def <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)  # Matrice de covariance non définie positive
cat("\nTest 2 : Matrice de covariance non définie positive (doit échouer)\n")
tryCatch({
  optim_portefeuille(covariance_non_pos_def, rendements, niveau_risque)
}, error = function(e) {
  cat("Erreur : ", e$message, "\n")
})

# 3. Test avec un vecteur de rendements avec des valeurs négatives
rendements_negatifs <- c(-0.02, -0.01)  # Rendements attendus négatifs
cat("\nTest 3 : Optimisation avec des rendements négatifs\n")
resultat_negatifs <- optim_portefeuille(covariance, rendements_negatifs, niveau_risque)
print(resultat_negatifs)  # Afficher les pondérations optimales pour des rendements négatifs

# 4. Test avec une seule action (une dimension)
covariance_1d <- matrix(0.02, nrow = 1, ncol = 1)  # Matrice de covariance 1x1
rendements_1d <- 0.07  # Rendement attendu d'une seule action
cat("\nTest 4 : Optimisation pour une seule action\n")
resultat_1d <- optim_portefeuille(covariance_1d, rendements_1d, niveau_risque)
print(resultat_1d)  # Afficher les pondérations optimales pour une seule action

# 5. Test avec une matrice de covariance diagonale (pas de corrélation entre actifs)
covariance_diag <- diag(c(0.04, 0.03))  # Matrice de covariance diagonale
rendements_diag <- c(0.06, 0.07)  # Rendements attendus pour chaque actif
cat("\nTest 5 : Optimisation avec une matrice de covariance diagonale\n")
resultat_diag <- optim_portefeuille(covariance_diag, rendements_diag, niveau_risque)
print(resultat_diag)  # Afficher les pondérations optimales pour des actifs non corrélés
