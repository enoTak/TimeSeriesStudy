mine.glm <- setClass("mine.glm",
                     representation(transition="matrix",
                                    transtion_covariance="matrix",
                                    observation="matrix",
                                    observation_covariance="matrix")
                     )


kalman_filtering <- function(mean_base, cov_base,
                             transition_mat,
                             observed,
                             state_noise_cov_mat,
                             observation_mat,
                             observation_noise_cov_mat)
{
  # prediction distribution
  mean_pd <- transition_mat %*% mean_base
  cov_pd  <- transition_mat %*% cov_base %*% t(transition_mat) + state_noise_cov_mat
  
  # prediction likelihood
  mean_pl <- observation_mat %*% mean_pd
  cov_pl  <- observation_mat %*% cov_pd %*% t(observation_mat) + observation_noise_cov_mat
  
  # Kalman profit
  kalman_profit <- cov_pd %*% t(observation_mat) %*% solve(cov_pl)
  
  # update state
  mean_new <- mean_pd + kalman_profit %*% (observed - mean_pl)
  cov_new  <- (diag(nrow(cov_pd)) - kalman_profit %*% observation_mat) %*% cov_pd
  
  return(list(mean_state = mean_new,
              cov_state = cov_new,
              prediction_dist_mean = mean_pd,
              prediction_dist_cov = cov_pd))
}