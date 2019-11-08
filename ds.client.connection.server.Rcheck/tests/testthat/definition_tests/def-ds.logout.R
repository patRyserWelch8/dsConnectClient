source("connection_to_datasets/init_all_datasets.R")
.test.no.connection <- function()
{
  log.out.data.server()
  expect_error(.logout(NULL, NULL))
}

.test.valid.connection <- function()
{
  
  init.all.datasets()
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(ds.logout())
  
  init.dataset.1()
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(ds.logout())
  
  init.dataset.2()
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(ds.logout())
  
  init.dataset.3()
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(ds.logout())
  
}

.test.incorrect.connection <- function()
{
  
  
}