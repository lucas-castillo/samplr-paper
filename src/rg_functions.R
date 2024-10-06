get_shape <- function(v){
  v <- v[!is.na(v)]
  lb <- min(v)
  ub <- max(v)
  m <- mean(v)
  s <- sd(v)
  
  sum(dnorm(v, m, s, log = T) - dunif(v, lb, ub, log = T)) / length(v)
}
get_measures <- function(v){
  v <- round(v)
  v <- v[!is.na(v)]
  v2 <- v[c(T, diff(v) != 0)]
  R <- mean(diff(v) == 0)
  A <- mean(diff(v2) == 1)
  one <-   v2[1:(length(v2) - 2)]
  two <-   v2[2:(length(v2) - 1)]
  three <- v2[3:(length(v2) - 0)]
  TP <- mean((one > two & two < three) | (one < two & two > three) )
  D <- mean(abs(diff(v2)))
  S <- get_shape(v)
  return(tibble(R,A,TP,D,S))
}

prior <- function(N=1){
  ## As described in Castillo et al. 2024 --- 
  list(
    "proposal_width"=runif(N, 0, 50),
    "delta_t"=runif(N, 1, 4),
    "n_chains"=sample(4:6, N, T),
    "swap_all"=sample(c(T,F), N,T),
    "L"=sample(1:100, N, T),
    "alpha"=runif(N, 0, 1)
  )
}

simulate <- function(model, params, target="norm"){
  if (target == "norm"){
    start <- rnorm(1, 176.4, 12)
    distr_params <- c(176.4,12)
  } else {
    target <- "unif"
    start <- runif(1, 122, 219)
    distr_params <- c(122, 219)
  }
  
  if (model == "MH"){
    v <- samplr::sampler_mh(
      start=start,
      distr_name = target, 
      distr_params = distr_params,
      sigma_prop = params[["proposal_width"]], 
      iterations = 200, 
    )$Samples[,1]
  } else if (model == "MC3"){
    v <- samplr::sampler_mc3(
      start=start,
      distr_name = target, 
      distr_params = distr_params,
      sigma_prop = params[["proposal_width"]], 
      nChains = params[["n_chains"]], 
      delta_T = params[["delta_t"]], 
      swap_all = params[["swap_all"]],
      iterations = 200, 
    )$Samples[,,1]
  } else if (model == "HMC"){
    v <- samplr::sampler_hmc(
      start=start,
      distr_name = target, 
      distr_params = distr_params,
      epsilon = .1,
      L = params[["L"]], 
      iterations = 200, 
    )$Samples[,1]
  } else if (model == "REC"){
    v <- samplr::sampler_rec(
      start=start,
      distr_name = target, 
      distr_params = distr_params,
      epsilon = .1,
      L = params[["L"]], 
      alpha = params[["alpha"]],
      iterations = 200, 
    )$Samples[,1]
  } else if (model == "MCHMC"){
    v <- samplr::sampler_mchmc(
      start=start,
      distr_name = target, 
      distr_params = distr_params,
      epsilon = .1,
      L = params[["L"]], 
      nChains = params[["n_chains"]], 
      delta_T = params[["delta_t"]], 
      swap_all = params[["swap_all"]],
      iterations = 200, 
    )$Samples[,,1]
  } else if (model == "MCREC"){
    v <- samplr::sampler_mcrec(
      start=start,
      distr_name = target, 
      distr_params = distr_params,
      epsilon = .1,
      L = params[["L"]], 
      nChains = params[["n_chains"]], 
      delta_T = params[["delta_t"]], 
      swap_all = params[["swap_all"]], 
      alpha = params[["alpha"]],
      iterations = 200, 
    )$Samples[,,1]
  }
  
  return(v)
}
