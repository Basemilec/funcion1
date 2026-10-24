#' Entrenar un perceptron simple
#'
#' Entrena un modelo de clasificacion binaria mediante
#' el algoritmo del perceptron simple.
#'
#' Las variables predictoras se estandarizan antes
#' del entrenamiento. Los pesos y el sesgo se actualizan
#' cuando existe un error de clasificacion.
#'
#' @param X Matriz o data frame con variables predictoras numericas.
#' @param y Vector de clases binarias codificadas como 0 y 1.
#' @param epocas Numero de epocas de entrenamiento.
#' @param tasa Tasa de aprendizaje del perceptron.
#'
#' @return Una lista con los siguientes elementos:
#' \describe{
#'   \item{pesos}{Vector de pesos entrenados.}
#'   \item{sesgo}{Intercepto del modelo.}
#'   \item{medias}{Medias utilizadas para estandarizar X.}
#'   \item{desviaciones}{Desviaciones estandar utilizadas.}
#' }
#'
#' @examples
#' X <- matrix(c(2, 4, 3, 5, 6, 7, 8, 9), ncol = 2)
#' y <- c(0, 0, 1, 1)
#' modelo <- entrenar_perceptron(X, y, epocas = 100, tasa = 0.01)
#' modelo$pesos
#'
#' @export

entrenar_perceptron <- function(X, y, epocas = 100, tasa = 0.01) {

  # -------------------------------------------------
  # 1. VALIDAR LOS DATOS
  # -------------------------------------------------

  X <- as.matrix(X)
  y <- as.numeric(y)

  if (!is.numeric(X) || any(!is.finite(X)) ||
      nrow(X) < 2 || ncol(X) < 1) {
    stop("X debe ser una matriz numerica valida con al menos dos filas.")
  }

  if (length(y) != nrow(X) ||
      anyNA(y) ||
      !all(y %in% c(0, 1))) {
    stop("y debe contener clases 0 y 1 y coincidir con las filas de X.")
  }

  if (length(epocas) != 1 || !is.numeric(epocas) ||
      is.na(epocas) || !is.finite(epocas) ||
      epocas < 1 || epocas %% 1 != 0) {
    stop("epocas debe ser un numero entero positivo.")
  }

  if (length(tasa) != 1 || !is.numeric(tasa) ||
      is.na(tasa) || !is.finite(tasa) || tasa <= 0) {
    stop("tasa debe ser un numero positivo.")
  }

  # -------------------------------------------------
  # 2. ESTANDARIZAR VARIABLES
  # -------------------------------------------------

  medias <- colMeans(X)

  desviaciones <- apply(X, 2, stats::sd)

  desviaciones[desviaciones == 0] <- 1

  Xs <- scale(
    X,
    center = medias,
    scale = desviaciones
  )

  # -------------------------------------------------
  # 3. INICIALIZAR PESOS Y SESGO
  # -------------------------------------------------

  pesos <- rep(0, ncol(Xs))

  sesgo <- 0

  # -------------------------------------------------
  # 4. ENTRENAMIENTO DEL PERCEPTRON
  # -------------------------------------------------

  for (e in seq_len(epocas)) {

    for (i in sample(seq_len(nrow(Xs)))) {

      # Calcular combinacion lineal
      z <- sum(Xs[i, ] * pesos) + sesgo

      # Funcion de activacion escalon
      pred <- as.integer(z >= 0)

      # Error de clasificacion
      error <- y[i] - pred

      # Actualizar pesos
      pesos <- pesos + tasa * error * Xs[i, ]

      # Actualizar sesgo
      sesgo <- sesgo + tasa * error

    }
  }

  # -------------------------------------------------
  # 5. DEVOLVER MODELO ENTRENADO
  # -------------------------------------------------

  return(
    list(
      pesos = pesos,
      sesgo = sesgo,
      medias = medias,
      desviaciones = desviaciones
    )
  )
}
