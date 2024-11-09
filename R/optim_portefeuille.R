#' Optimisation du portefeuille
#'
#' Cette fonction optimise un portefeuille en utilisant une fonction objectif quadratique,
#' avec comme paramètres la matrice de covariance des rendements des actifs, les rendements attendus
#' et un niveau de risque cible.
#'
#' @param covariance Une matrice de covariance des rendements des actifs.
#' @param rendements Un vecteur des rendements attendus des actifs.
#' @param niveau_risque Un nombre représentant le niveau de risque (la variance cible) du portefeuille.
#'
#' @return Un vecteur contenant les pondérations optimales des actifs dans le portefeuille.
#'
#' @examples
#' # Exemple d'utilisation
#' covariance <- matrix(c(0.04, 0.02, 0.02, 0.03), nrow = 2)  # Matrice de covariance des actifs
#' rendements <- c(0.06, 0.08)  # Rendements attendus des actifs
#' niveau_risque <- 0.005  # Niveau de risque cible
#' optim_portefeuille(covariance, rendements, niveau_risque)
#'
#' @export
optim_portefeuille <- function(covariance, rendements, niveau_risque) {
  # Vérifier si la matrice de covariance est définie positive
  is_positive_definite <- function(M) {
    eigenvalues <- eigen(M)$values
    all(eigenvalues > 0)  # Vérifie si toutes les valeurs propres sont positives
  }

  if (!is_positive_definite(covariance)) {
    stop("La matrice de covariance doit être définie positive")
  }

  # Définir la fonction objectif quadratique
  # La fonction objectif est de minimiser x' * covariance * x - rendements' * x
  # où x est le vecteur des pondérations du portefeuille
  Dmat <- covariance
  dvec <- -rendements  # Maximisation du rendement

  # Matrice des contraintes : on impose que la somme des pondérations égale 1
  Amat <- cbind(rep(1, length(rendements)))  # Une seule contrainte: somme(pondérations) = 1
  bvec <- 1  # La somme des pondérations doit être égale à 1

  # Résoudre le problème d'optimisation quadratique
  solution <- solve.QP(Dmat, dvec, Amat = Amat, bvec = bvec, meq = 1)

  # Retourner les pondérations optimales
  return(solution$solution)
}
