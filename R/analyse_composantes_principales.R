#' Analyse en Composantes Principales (ACP) et génération d'un biplot
#'
#' Cette fonction effectue une analyse en composantes principales (ACP) sur un jeu de données
#' et génère un biplot pour visualiser les projections des observations et les contributions des variables.
#'
#' @param donnees Un data.frame contenant les données pour lesquelles l'ACP est effectuée.
#'   Les colonnes doivent être des variables quantitatives sur lesquelles l'ACP sera réalisée.
#'
#' @return Un objet de type `ggplot` représentant le biplot des deux premières composantes principales.
#'   Le biplot permet de visualiser la relation entre les observations (points) et les contributions des variables (flèches).
#'
#' @details
#' Cette fonction applique l'Analyse en Composantes Principales (ACP) à un jeu de données. Elle effectue d'abord une standardisation des variables (centrage et réduction), puis génère un biplot qui projette les observations dans l'espace des premières composantes principales.
#' Le biplot affiche également les contributions des variables sous forme de flèches pointant dans la direction de chaque composante principale.
#'
#' La taille des flèches est automatiquement ajustée pour garantir leur visibilité. Les observations sont représentées par des points, et les flèches sont colorées en rouge avec les noms des variables associés à chaque flèche.
#'
#' @note
#' Si le jeu de données contient des valeurs manquantes (`NA`), celles-ci seront automatiquement supprimées avant de réaliser l'ACP.
#'
#' @examples
#' # Exemple d'utilisation de la fonction analyse_composantes_principales
#' donnees <- data.frame(
#'   actif_1 = c(0.01, 0.02, -0.01, 0.03),
#'   actif_2 = c(0.03, 0.02, 0.01, 0.02),
#'   actif_3 = c(0.04, -0.03, 0.02, 0.05)
#' )
#' analyse_composantes_principales(donnees)
#'
#' @import ggplot2
#' @import grid
#' @export
analyse_composantes_principales <- function(donnees) {

  # Vérifier si les données sont sous forme de data.frame
  if (!is.data.frame(donnees)) {
    stop("Les données doivent être sous forme de data.frame")
  }

  # Vérifier s'il y a des NA dans les données et les supprimer si nécessaire
  if (any(is.na(donnees))) {
    warning("Les données contiennent des valeurs manquantes (NA), qui seront ignorées.")
    donnees <- na.omit(donnees)
  }

  # Appliquer l'ACP (centrer et réduire les données)
  pca <- prcomp(donnees, scale. = TRUE)

  # Extraire les projections des observations
  pca_df <- as.data.frame(pca$x)  # Les projections sur les composantes principales

  # Extraire les contributions des variables originales
  pca_contrib <- as.data.frame(pca$rotation)  # Les contributions des variables aux composantes principales

  # Calculer un facteur d'échelle dynamique pour les flèches
  scale_factor <- max(abs(pca_contrib$PC1), abs(pca_contrib$PC2)) * 1.2

  # Créer le biplot avec ggplot
  ggplot(pca_df, aes(x = PC1, y = PC2)) +
    geom_point(color = "blue", size = 3) +  # Points représentant les observations projetées
    geom_segment(data = pca_contrib,
                 aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),  # Flèches ajustées
                 arrow = arrow(length = unit(0.2, "inches")),
                 color = "red", size = 1) +  # Flèches représentant les contributions des variables
    geom_text(data = pca_contrib,
              aes(x = PC1 * scale_factor, y = PC2 * scale_factor, label = rownames(pca_contrib)),
              color = "red", size = 4, vjust = -1) +  # Ajouter les noms des variables à côté des flèches
    ggtitle("Biplot des Composantes Principales") +
    xlab("Composante Principale 1") +
    ylab("Composante Principale 2") +
    theme_minimal() +  # Style minimaliste
    theme(plot.title = element_text(hjust = 0.5))  # Centrer le titre
}

