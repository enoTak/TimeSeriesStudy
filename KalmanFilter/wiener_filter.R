

calc_beta_ar1 <- function(phi, W, V)
{
  r <- V / W
  b <- 1/ (r * phi) + 1 / phi + phi
  (b - sqrt(b * b - 4)) / 2
}


wiener_smoothing <- function(phi, W, V, t_max, x)
{
  beta <- calc_beta_ar1(phi, W, V)
  d <- (1 / phi - beta) * (phi - beta) / (1 - beta^2) *
    filter(method="convolution",
           filter=beta^abs(-(t_max-1):(t_max-1)),
           x=x)
  d
}