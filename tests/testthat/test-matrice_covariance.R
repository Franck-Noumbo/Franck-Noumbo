# Définition de la fonction matrice_covariance
matrice_covariance <- function(donnees, na.rm = TRUE) {
  if (!is.data.frame(donnees)) stop("Les données doivent être sous forme de data.frame")
  if (any(sapply(donnees, is.numeric) == FALSE)) stop("Toutes les colonnes doivent être numériques")
  cov(donnees, use = if (na.rm) "complete.obs" else "everything")
}

# 1. Test avec un jeu de données simple
donnees <- data.frame(actif_1 = c(0.01, 0.02, -0.01, 0.03), actif_2 = c(0.03, 0.02, 0.01, 0.02))
print("Test 1: Matrice de covariance simple")
print(matrice_covariance(donnees))

# 2. Test avec des valeurs manquantes (NA)
donnees_na <- data.frame(actif_1 = c(0.01, NA, -0.01, 0.03), actif_2 = c(NA, 0.02, 0.01, 0.02))
print("Test 2: Matrice de covariance avec NA")
print(matrice_covariance(donnees_na, na.rm = TRUE))

# 3. Test avec des colonnes non numériques
donnees_non_numeriques <- data.frame(actif_1 = c(0.01, 0.02, -0.01, 0.03), actif_2 = c(0.03, 0.02, 0.01, 0.02), facteur_3 = c("A", "B", "C", "D"))
print("Test 3: Colonnes non numériques")
try(print(matrice_covariance(donnees_non_numeriques)), silent = TRUE)
