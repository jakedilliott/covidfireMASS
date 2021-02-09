
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
#' @param input_df data frame containing agent data
#' @param max_leads number of leads per module
#' @export
assign_roles <- function(input_df, max_leads) {
  split_df <- split(input_df, paste0(input_df$inc_id, ".", input_df$mod_id))

  out <- lapply(
    split_df,
    function(mod_df) {
      # only assign roles to agents on a fire
      if (mod_df$inc_id[1] == 0) {
        mod_df$leader <- FALSE
      } else {
        total <- nrow(mod_df)
        num_leads <- length(which(mod_df$leader))

        if (total <= max_leads) { # agents in module <= max leads
          mod_df$leader <- TRUE
        } else { # more agents than desired leads
          if (num_leads > max_leads) { # too many leaders, this should always fail
            to_demote <- sample(which(mod_df$leader), num_leads - max_leads)
            mod_df$leader[to_demote] <- FALSE
          } else if (num_leads < max_leads) { # too few leaders
            non_leads <- which(!mod_df$leader)
            to_promote <- if (length(non_leads) > max_leads - num_leads) {
              sample(non_leads, max_leads - num_leads)
            } else {
              non_leads
            }
            mod_df$leader[to_promote] <- TRUE
          }
        }
      }
      return(mod_df$res_id[mod_df$leader]) # lapply output
    }
  )
  return(as.numeric(unlist(out))) # final
}

mk_agents <- function(inc_data, mod_data, day) {
  total <- nrow(inc_data)

  data.frame(
    res_id = inc_data[["res_id"]],
    res_gacc = inc_data[["res_gacc"]],
    inc_id = inc_data[[day + 2]], # first 2 columns are res_id & res_gacc
    mod_id = mod_data[[day + 2]],
    leader = vector("logical", total),
    state = "S",
    quarantine = vector("logical", total),
    q_days = 0
    )
}
