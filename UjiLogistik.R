# Fungsi Uji Logistik
uji_logistik <- function(data, response, predictors) {
  # Validasi input
  if (!response %in% colnames(data)) {
    stop("Kolom respons tidak ditemukan dalam data.")
  }
  if (!all(predictors %in% colnames(data))) {
    stop("Beberapa kolom prediktor tidak ditemukan dalam data.")
  }

  # Membuat formula model
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))

  # Model regresi logistik
  model <- glm(formula, data = data, family = binomial())

  # Ringkasan hasil
  return(summary(model))
}
#' Uji Regresi Logistik
#'
#' Fungsi ini melakukan analisis regresi logistik pada data yang diberikan.
#'
#' @param data Data frame yang digunakan untuk analisis.
#' @param response Nama kolom respons (variabel dependen) dengan nilai biner (0/1).
#' @param predictors Vektor karakter berisi nama-nama kolom prediktor (variabel independen).
#' @return Ringkasan hasil regresi logistik.
#' @examples
#' @export
# Contoh penggunaan:
# data <- data.frame(
#   y = rbinom(100, 1, 0.5),
#   x1 = rnorm(100),
#   x2 = runif(100)
# )
# uji_logistik(data, "y", c("x1", "x2"))
