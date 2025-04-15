#' creates a graph centered on median values for x and y series
#'
#' @param data data frame with data to show
#' @param symbol_col column name containing symbols
#' @param x_col x column name
#' @param y_col y column name
#' @param title string - graph title
#' @return ggplot2 object
#' @importFrom scales percent percent_format
#' @importFrom stats median
#' @export
#'
#' @examples
#' data <- data.frame(
#'   Symbole = c("A", "B", "C", "D", "E"),
#'   IVP = c(0.01, 0.02, 0.03, 0.04, 0.05),
#'   `IV-1month` = c(0.05, 0.04, 0.03, 0.02, 0.01)
#' )
#' create_centered_plot(data, "Symbole", "IVP", "IV-1month", "Exemple de graphique")
create_centered_plot <- function(data, symbol_col, x_col, y_col, title = "IMplied vol Dashboard") {

  # Verify that req cols are here
  required_cols <- c(symbol_col, x_col, y_col)
  if (!all(required_cols %in% names(data))) {
    stop(paste("Dataframe must contain columns:", paste(required_cols, collapse = ", ")))
  }

  # Median compute for axis definition
  x_median <- stats::median(data[[x_col]], na.rm = TRUE)
  y_median <- stats::median(data[[y_col]], na.rm = TRUE)

  # Create graph using aes to determiine dynamically axis names
  p <- ggplot2::ggplot(data) +
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      label = .data[[symbol_col]]
    ) +
    # Ajout des points
    ggplot2::geom_point() +
    # Ajout des étiquettes (noms des symboles)
    ggplot2::geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
    # Ajout des lignes pour les axes centrés sur les médianes
    ggplot2::geom_hline(yintercept = y_median, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = x_median, linetype = "dashed", color = "gray50") +
    # Personnalisation du thème
    ggplot2::theme_minimal() +
    # Formatage des axes en pourcentage
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    # Titre du graphique et labels des axes qui incluent les médianes
    ggplot2::labs(title = title,
                  x = paste(x_col, "(médiane =", scales::percent(x_median, accuracy = 0.1), ")"),
                  y = paste(y_col, "(médiane =", scales::percent(y_median, accuracy = 0.1), ")"))

  return(p)
}

# Exemple d'utilisation:
# data <- data.frame(
#   Symbole = c("ESTX50", "SPY", "SLV", "USO", "HOLN"),
#   IVP = c(0.01, 0.02, 0.03, 0.04, 0.05),
#   IV.1month = c(0.05, 0.04, 0.03, 0.02, 0.01)
# )
#
# create_centered_plot(data, "Symbole", "IVP", "IV.1month", "Volatilité comparée")
