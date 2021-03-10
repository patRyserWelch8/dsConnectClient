
.test.no.connection <- function()
{
  log.out.data.server()
  expect_error(dslo.logout(NULL, NULL))
}

.test.valid.connection <- function(connections)
{
  
  init.all.datasets()
  connection <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  expect_true(ds.logout(connections))
  
}
