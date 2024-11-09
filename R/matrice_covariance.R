#' Calcul de la matrice de covariance
#'
#' Cette fonction calcule la matrice de covariance à partir d'un jeu de données.
#'
#' @param donnees Un data.frame contenant les données pour lesquelles la matrice de covariance
#'   doit être calculée.
#'
#' @return Une matrice de covariance.
#'
#' @examples
#' # Exemple d'utilisation de la fonction matrice_covariance
#' donnees <- data.frame(
#'   actif_1 = c(0.01, 0.02, -0.01, 0.03),
#'   actif_2 = c(0.03, 0.02, 0.01, 0.02)
#' )
#' matrice_covariance(donnees)  # Appel de la fonction matrice_covariance
#'
#' @export
matrice_covariance <- function(donnees) {
  # Vérifier si les données sont sous forme de data.frame
  if (!is.data.frame(donnees)) {
    stop("Les données doivent être sous forme de data.frame")
  }

  # Calculer la matrice de covariance
  covariance <- cov(donnees)

  # Retourner la matrice de covariance
  return(covariance)
}

