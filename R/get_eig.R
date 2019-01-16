#' Get eigenvalues
#'
#' Function to augment eigenvalues from multivariate analysis
#'
#' @import dplyr magrittr
#' @return dataframe
#' @export


get_eig <- function(.res) {
  if (missing(.res)) {
    stop("Data is not supplied", call. = FALSE)
  } else if (!any(class(.res) %in% c("PCA", "CA", "MFA", "MCA"))) {
    stop("Data is not one of PCA, CA, MFA, MCA class", call. = FALSE)
  }
  .res %>%
    extract2("eig") %>%
    as_tibble(rownames = "dim") %>%
    rename(
      "var_percent" = `percentage of variance`,
      "var_cummulative" = `cumulative percentage of variance`
    ) %>%
    mutate(dim = str_replace_all(dim, "comp", "Dim"))
}
