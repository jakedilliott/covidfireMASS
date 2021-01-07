#' Group overhead modules into 1 module
#' @param agent_df data frame containing firefighter agents data
#'
clean_mods <- function(agent_df) {
  new_df <- agent_df
  index_overhead <- grepl("^O-\\d", agent_df[["mod_id"]])

  if (length(index_overhead) > 0) {
    new_df[["mod_id"]][index_overhead] <- "O-100"
  }

  new_df
}

#' Assign roles to agents
#' @param agent_df data frame containing agent data
#' @param n number of leads per module
assign_roles <- function(df_in, n) {
  if (df_in[["inc_id"]][1] > 0) {
    inc_split <- split(df_in, df_in[["mod_id"]])

    new_df <- purrr::map_dfr(
      inc_split,
      function(module, new_module) {
        leads <- length(which(module[["role"]] == 1))

        if (leads < n) {
          new_leads <- sample(nrow(module), n - leads)
          new_module[["role"]][new_leads] <- 1
        } else if (leads > n) {
          no_longer_leads <- sample(which(module[["role"]]==1), leads - n)
          new_module[["role"]][no_longer_leads] <- 0
        }

        new_module
      }
    )
    return(new_df)
  } else {
    return(new_df)
  }
}

#' Move agents between fires
#' @param agent_df data frame containing agent data
#' @param inc_req data frame containing incident requests for agents
#' @param mod_req data frame containing module requests for agents
#' @param t numeric time value
#' @param eir Entry Infection Rate, numeric 0<=x<=1, or NULL

mv_agents <- function(agent_df, new_df,
                      inc_req, mod_req, t, eir) {
  sapply(list(agent_df, inc_req, mod_req),
         function(x) {
           if (!is.data.frame(x)) {
             stop(paste0("Input '", x, "' is not a data frame"))
           }
         })

  if (!is.numeric(t)) {
    stop("Input 't' is not numeric")
  }
  if (!is.numeric(eir)) {
    stop("Input 'eir' is not numeric")
  }
  if (eir < 0 | eir >= 1) {
    stop("eir must be between 0 and 1")
  }

  new_df <- agent_df
  new_df$inc_id <- dplyr::pull(inc_req, t + 2)
  new_df$mod_id <- dplyr::pull(mod_req, t + 2)

  new_df$role[which(new_df$inc_id != agent_df$inc_id)] <- 0
  new_df <- clean_mods(new_df)

  if (!is.null(eir)) {
    probE <- stats::runif(nrow(agent_df))
    new_df$state[which(agent_df$state == 0 & agent_df$inc_id != new_df$inc_id & probE < eir)]
  }

  new_df
}
