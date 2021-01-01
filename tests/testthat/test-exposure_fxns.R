# Module Exposure Tests ---------------------------------------------------
agent_df <- mk_agents(faux_incidents, faux_modules, day=2)
inc_list <- inc_subsetter(agent_df)
faux_params <- list(BA=0.01, BI=0.03, delta_t=1, exp_thres=1)

test_that("expose_modules handles incorrect inputs", {
  expect_error(expose_modules("str", faux_params))
  expect_error(expose_modules(123, faux_params))
  expect_error(expose_modules(inc_list[[1]], "str"))
  expect_error(expose_modules(inc_list[[1]], 123))
  expect_error(expose_modules(inc_list[[1]], faux_params[[1:3]]))
})

test_that("expose_modules returns the correct outputs", {
  expect_equal(dim(expose_modules(inc_list[[1]], faux_params)), dim(inc_list[[1]]))
  expect_equal(colnames(expose_modules(inc_list[[1]], faux_params)),
               colnames(inc_list[[1]]))
})
