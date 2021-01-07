# R/seasonal_model.R

seasonal_sim <- function(inc_data, mod_data, incl_A = TRUE, De = 5,
                         gamma = 10, eir = 0.005, R_init = 0,
                         R0, pIR = 0.2, pAQ = 0.3,
                         inf_seed = NULL, mods_fix = TRUE) {
  # Get data in order ----
  # inc_data <- inputs$incidents
  # mod_data <- inputs$modules

  set.seed(123)

  # Parameters ----
  N <- nrow(inc_data)
  t <- 1
  delta_t <- 1
  tend <- ncol(inc_data) - 2 # first 2 cols are res_id and res_gacc

  exp_thres <- 1
  DiI <- 8-De
  DiAI <- 8
  Dq <- 14
  z <- 1.65
  R0int <- 1
  pI <- 0.4286

  BA <- ifelse(incl_A==TRUE, R0int*R0 / ( (z*pI/((pIR/DiI) + ((1-pIR)/DiAI))) + ((1-pI)/((pAQ/DiAQ) + ((1-pAQ)/DiAI))) ), 0)
  BI <- ifelse(incl_A==TRUE, R0int*BA*z, R0/DiI)

  init_df <- data.frame(res_id = inc_data[["res_id"]],
                        res_gacc = inc_data[["res_gacc"]],
                        inc_id = inc_data[[3]],
                        mod_id = mod_data[[3]],
                        role = 0,
                        state = 0,
                        q_status = 0,
                        q_days = 0)

  init_df <- assign_roles(init_df)
  if (mods_fix == TRUE) {
    agent_df <- clean_mods(init_df)
  } else {
    agent_df <- init_df
  }

  outputs <- list() # create outputs list

  while (t < tend + 1) {
    # recording outputs
    outputs[[t]] <- agent_df

    # incident level operations ----
    # module exposure / lead exposure
    agent_split <- split(agent_df, agent_df[["inc_id"]])
    new_split <- agent_split

    # leads exposure
    new_split <- purrr::map2(agent_split,
                             new_split,
                             expose_leads,
                             BI = BI,
                             BA = BA,
                             exp_thres = exp_thres,
                             delta_t = delta_t)
    # modules exposure
    new_split <- purrr::map2(agent_split,
                             new_split,
                             expose_modules,
                             BI = BI,
                             BA = BA,
                             exp_thres = exp_thres,
                             delta_t = delta_t)
    # module based quarantine
    new_split <- purrr:map2(agent_split,
                            new_split,
                            modular_quarantine)
    # season level operations ----
    # mobilization
    new_df <- dplyr::bind_rows(new_split)
    new_df[["inc_id"]] <- inc_data[[t + 2]]
    new_df[["mod_id"]] <- mod_data[[t + 2]]

    # state changes ----
    # random rolls
    rE <- runif(N) # draw for exposure
    rI <- runif(N) # draw to become infective
    rS <- runif(N) # draw to become symptomatic
    rQ <- runif(N) # getting caught and moving to quarantine
    rR <- runif(N) # draw to recover
    pRecover   <- 1 - exp(-1/gamma * delta_t) # p of recovery
    pInfective <- 1 - exp(-1/gamma * delta_t) # p of becoming infective

    # Agents that have changed incidents can be infected during mobilization
    new_df$state[which(agent_df$inc_id != new$inc_id && rE < eir)] <- 1

    # After incubation period Exposed move to Infected or Asymptomatic
    new_df$state[which(agent_df$state == 1 && rI < pInfective && rS < pI)] <- 2
    new_df$state[which(agent_df$state == 1 && rI < pInfective && rS > pI)] <- 4

    # Infected(Symptomatic) recognize symptoms and quarantine or don't catch
    # symptoms and recover
    new_df$state[which(agent_df$state == 2 && rR < pRecover)] <- 3
    new_df$q_status[which(agent_df$state == 2 && rQ < 1-pIR)] <- 1

    # Asymptomatic are caught by testing/screening and quarantine or they are
    # not caught and recover
    new_df$state[which(agent_df$state == 4 && rR < pRecover)] <- 3
    new_df$q_status[which(agent_df$state == 4 && rQ < pAQ)] <- 1

    # Increment days quarantined
    new_df$days_q[which(agent_df$q_status == 1)] %+=% 1

    # When qaurantined agents reach the recommended quarantine days they recover
    # and leave quarantine
    new_df$state[which(agent_df$state != 0 && agent_df$q_status == 1 && agent_df$days_q == Dq)] <- 3
    new_df$days_q[which(agent_df$q_status == 1 && agent_df$days_q == Dq)] <- 0
    new_df$q_status[which(agent_df$q_status == 1 && agent_df$days_q == Dq)] <- 0

    # clean up
    agent_df <- new_df
    t <- t + delta_t
  }

  dplyr::bind_rows(outputs)
}
