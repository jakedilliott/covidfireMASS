load_2017 <- function() {
  files <- c("/home/jaked/CovidFire/Data/2017_daily_res_inc_id.RData",
             "/home/jaked/CovidFire/Data/2017_daily_res_mod_id.RData",
             "/home/jaked/CovidFire/Data/2017_inc_info.RData")

  lapply(files, load, envir = globalenv())
}
