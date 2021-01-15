
#' Group overhead modules into 1 module
#' @param agent_df data frame containing firefighter agents data
#' @export
clean_mods <- function(mod_id_list) {
  overhead <- grepl("^O-\\d", as.vector(mod_id_list))
  mod_id_list[overhead] <- "O-100"

  as.character(mod_id_list)
}

unique_mod_split <- function(input_df) {
  split_by_inc <- split(input_df, input_df[["inc_id"]])

  split_by_mod <- lapply(split_by_inc,
                         function(x) {
                           split(x, x[["mod_id"]])
                         })

  unlist(split_by_mod, recursive = FALSE)
}

#' Assign roles to agents
#' @param mod_df_list data frame containing agent data
#' @param n_leads number of leads per module
#' @export
assign_roles <- function(input_df, n_leads) {

  split_by_mod <- unique_mod_split(input_df)

  new_modules <- lapply(
    split_by_mod,
    function(module) {
      if (module$inc_id[1]>0) {
        N <- nrow(module)
        current_leads <- length(which(module$role > 0))

        if (N < n_leads) {
          module$role <- 1
        } else {
          if (current_leads < n_leads) {
            module$role[sample(N, n_leads - current_leads)] <- 1
          } else if (current_leads > n_leads) {
            module$role[sample(N, current_leads - n_leads)] <- 0
          }
        }
      }
      module
    })
  dplyr::bind_rows(new_modules)
}

mk_agents <- function(inc_data, mod_data, day) {
  data.frame(res_id = inc_data[["res_id"]],
             res_gacc = inc_data[["res_gacc"]],
             inc_id = inc_data[[day]],
             mod_id = clean_mods(mod_data[[day]]),
             role = 0,
             state = 0,
             q_status = 0,
             q_days = 0)
}

#' Move agents between fires
#' @param agent_df data frame containing agent data
#' @param new_df data frame to be modified
#' @param inc_req data frame containing incident requests for agents
#' @param mod_req data frame containing module requests for agents
#' @param t numeric time value
#' @param eir Entry Infection Rate, numeric 0<=x<=1, or NULL
#' @export
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
