#' Modélise le risque avec un modèle factoriel
#'
#' Cette fonction utilise un modèle de régression linéaire pour calculer les
#' coefficients de sensibilité des actifs par rapport à des facteurs de risque.
#'
#' @param donnees Un data.frame contenant les rendements des actifs.
#' @param facteurs Un data.frame contenant les facteurs de risque.
#'
#' @return Un vecteur de coefficients représentant la sensibilité des actifs
#' aux facteurs de risque.
#'
#' @examples
#' # Exemple de données fictives pour les actifs et les facteurs de risque
#' donnees <- data.frame(actif_1 = rnorm(100, mean = 0.05, sd = 0.2),  # Actif 1 : rendements simulés
#'   actif_2 = rnorm(100, mean = 0.03, sd = 0.15), # Actif 2 : rendements simulés
#' actif_3 = rnorm(100, mean = 0.04, sd = 0.1),  # Actif 3 : rendements simulés
#' actif_4 = rnorm(100, mean = 0.06, sd = 0.25), # Actif 4 : rendements simulés
#' actif_5 = rnorm(100, mean = 0.02, sd = 0.18)  # Actif 5 : rendements simulés
#' )
#'
#' facteurs <- data.frame(facteur_1 = rnorm(100, mean = 0.02, sd = 0.1), # Facteur 1 : exemple
#'                       facteur_2 = rnorm(100, mean = 0.03, sd = 0.08)  # Facteur 2 : exemple
#'  )
#'
#' modele_risque(donnees, facteurs)
#'
#' @export
modele_risque <- function(donnees, facteurs) {
  # Vérifier si les données et les facteurs sont sous forme de data.frame
  if (!is.data.frame(donnees) | !is.data.frame(facteurs)) {
    stop("Les données et les facteurs doivent être sous forme de data.frame")
  }

  # Vérifier que le nombre de lignes dans les données et les facteurs est identique
  if (nrow(donnees) != nrow(facteurs)) {
    stop("Le nombre de lignes dans les données et les facteurs doit être le même")
  }

  # Convertir les rendements des actifs (donnees) en un data.frame
  # Combiner les rendements et les facteurs dans un seul data.frame
  donnees_comb <- data.frame(donnees, facteurs)

  # Créer la formule pour la régression (on veut que les rendements des actifs dépendent des facteurs)
  formule <- as.formula(paste("actif_1 + actif_2 + actif_3 + actif_4 + actif_5 ~", paste(names(facteurs), collapse = " + ")))

  # Modéliser le risque à l'aide d'un modèle de régression linéaire
  modele <- lm(formule, data = donnees_comb)

  # Extraire les coefficients de sensibilité (béta)
  beta <- coef(modele)

  # Retourner les coefficients de sensibilité
  return(beta)
}
