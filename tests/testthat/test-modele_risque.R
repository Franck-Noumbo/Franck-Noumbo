# Définition de la fonction modele_risque
modele_risque <- function(donnees, facteurs) {
  if (!is.data.frame(donnees) | !is.data.frame(facteurs)) {
    stop("Les données et les facteurs doivent être sous forme de data.frame")
  }
  if (any(!sapply(donnees, is.numeric))) stop("Les rendements des actifs doivent être numériques")
  if (any(!sapply(facteurs, is.numeric))) stop("Les facteurs de risque doivent être numériques")
  if (nrow(donnees) != nrow(facteurs)) stop("Le nombre de lignes dans les données et les facteurs doit être le même")

  actifs <- names(donnees)
  formule <- as.formula(paste(paste(actifs, collapse = " + "), "~", paste(names(facteurs), collapse = " + ")))
  modele <- lm(formule, data = data.frame(donnees, facteurs))
  coef(modele)[-1]  # Exclure l'intercept
}

# 1. Test avec un jeu de données simple
donnees <- data.frame(
  actif_1 = rnorm(100, mean = 0.05, sd = 0.2),
  actif_2 = rnorm(100, mean = 0.03, sd = 0.))

