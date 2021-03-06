#' Perform PCA on dataframe
#'
#' @param .data a dataframe or tibble.
#' @param product name of column containing product information.
#' @param liking name of column containing hedonic liking (default to NULL).
#' @param scale whether to scale data to unit of variance (default to TRUE)
#'
#' @import dplyr magrittr FactoMineR
#' @return Returns a list containing information of PCA results.
#' @export
#'
#' @examples
#' sensochoc_means %>% do_pca(product = "Product")
#' do_pca(sensochoc_means, product = "Product")
do_pca <- function(.data, product, liking = NULL, scale = TRUE) {
  if (missing(.data)) {
    stop("Data is not supplied", call. = FALSE)
  } else if (missing(product)) {
    stop("Arguments are not completely supplied", call. = FALSE)
  }

  df <-
    .data %>%
    select(product, everything()) %>%
    as.data.frame() %>%
    set_rownames(.[[1]]) %>%
    select_if(is.numeric)

  if (is.null(liking)) {
    index_liking <- NULL
  } else {
    index_liking <- which(names(df) %in% liking)
  }

  res <- PCA(df, quanti.sup = index_liking, scale.unit = scale, graph = FALSE)
  return(res)
}
