# Make Agent Tests --------------------------------------------------------

agent_df <- mk_agents(faux_incidents, faux_modules, day = 2)

test_that("mk_agents handles incorrect inputs", {
  expect_error(mk_agents("str", faux_modules, day = 1), "not a data frame")
  expect_error(mk_agents(1, faux_modules, day = 1), "not a data frame")
  expect_error(mk_agents(faux_incidents, "str", day = 1), "not a data frame")
  expect_error(mk_agents(faux_incidents, 1, day = 1), "not a data frame")
  expect_error(mk_agents(faux_incidents, faux_modules, day = "str"), "not numeric")
})

test_that("mk_agents returns the correct dimensions", {
  expect_equal(nrow(agent_df), nrow(faux_incidents))
  expect_equal(nrow(agent_df), nrow(faux_modules))
  expect_equal(ncol(agent_df), 9)
})

test_that("mk_agents returns the correct outputs", {
  expect_equal(colnames(agent_df), c("res_id", "res_gacc", "inc_id",
                                     "mod_id", "module", "role",
                                     "state", "q_status", "days_q"))
  expect_equal(unique(agent_df$mod_id), c('O-1', 'O-2', 'E-3', 'C-4', '0'))
  expect_equal(unique(agent_df$module), c('O-1', 'E-3', 'C-4', '0'))
  expect_equal(unique(agent_df$inc_id), c(1, 0))
  expect_equal(agent_df[, 3], faux_incidents[, 4], ignore_attr = TRUE)
  expect_equal(agent_df[, 4], faux_modules[, 4], ignore_attr = TRUE)
})

# Clean Mods Tests --------------------------------------------------------

test_that("clean_mods handles incorrect inputs", {
  expect_error(clean_mods("str"), "not a data frame")
  expect_error(clean_mods(123), "not a data frame")
})

test_that("clean_mods returns the correct dimensions", {
  expect_equal(dim(clean_mods(agent_df)), dim(agent_df))
})

test_that("clean_mods returns the correct outputs", {
  expect_equal(colnames(clean_mods(agent_df)), colnames(agent_df))
  expect_equal(clean_mods(agent_df)[, 1:2], agent_df[, 1:2])
})

# Subsetter Tests ---------------------------------------------------------

test_that("mod_subsetter handles incorrect inputs", {
  expect_error(mod_subsetter("str"), "not a data frame")
  expect_error(mod_subsetter(123), "not a data frame")
})

test_that("inc_subsetter handles incorrect inputs", {
  expect_error(inc_subsetter("str"), "not a data frame")
  expect_error(inc_subsetter(123), "not a data frame")
})

test_that("subsetters have correct outputs", {
  expect_length(mod_subsetter(agent_df), 4)
  expect_length(inc_subsetter(agent_df), 2)
})


# Assign Role Tests -------------------------------------------------------
test_that("assign_roles handles incorrect inputs", {
  expect_error(assign_roles("str"), "not a data frame")
  expect_error(assign_roles(123), "not a data frame")
})

test_that("assign_roles has correct outputs", {
  expect_equal(dim(assign_roles(agent_df)), dim(agent_df))
  expect_equal(colnames(assign_roles(agent_df)), colnames(agent_df))
  expect_setequal(unique(assign_roles(agent_df)$role), c(0,1))
  expect_equal(length(which(assign_roles(agent_df)$role == 1)), 3)
})

# Move Agents Tests -------------------------------------------------------
test_that("mv_agents handles incorrect inputs", {
  expect_error(mv_agents("str", faux_incidents, faux_modules, 3, 0), "not a data frame")
  expect_error(mv_agents(123, faux_incidents, faux_modules, 3, 0), "not a data frame")
  expect_error(mv_agents(agent_df, "str", faux_modules, 3, 0), "not a data frame")
  expect_error(mv_agents(agent_df, 123, faux_modules, 3, 0), "not a data frame")
  expect_error(mv_agents(agent_df, faux_incidents, "str", 3, 0), "not a data frame")
  expect_error(mv_agents(agent_df, faux_incidents, 123, 3, 0), "not a data frame")
  expect_error(mv_agents(agent_df, faux_incidents, faux_modules, "str", 0), "not numeric")
  expect_error(mv_agents(agent_df, faux_incidents, faux_modules, 3, "str"), "not numeric")
  expect_error(mv_agents(agent_df, faux_incidents, faux_modules, 3, 1), "between 0 and 1")
})

test_that("mv_agents outputs the correct dimensions", {
  expect_equal(dim(mv_agents(agent_df, faux_incidents, faux_modules, 3, 0.01)),
               dim(agent_df))
  expect_equal(mv_agents(agent_df, faux_incidents, faux_modules, 3, 0.01)[, 1:2],
               agent_df[, 1:2])
})

