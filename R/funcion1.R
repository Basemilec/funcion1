entrenar_perceptron <- function(X, y, epocas = 100, tasa = 0.01) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  # Estandarizar las variables
  medias <- colMeans(X)
  desviaciones <- apply(X, 2, sd)
  desviaciones[desviaciones == 0] <- 1
  Xs <- scale(X, center = medias, scale = desviaciones)
  # Inicializar pesos y sesgo
  pesos <- rep(0, ncol(Xs))
  sesgo <- 0
  # Entrenamiento
  for (e in 1:epocas) {
    for (i in sample(seq_len(nrow(Xs)))) {
      z <- sum(Xs[i, ] * pesos) + sesgo
      pred <- as.integer(z >= 0)
      error <- y[i] - pred
      pesos <- pesos + tasa * error * Xs[i, ]
      sesgo <- sesgo + tasa * error
    }
  }
  list(
    pesos = pesos,
    sesgo = sesgo,
    medias = medias,
    desviaciones = desviaciones
  )
}
