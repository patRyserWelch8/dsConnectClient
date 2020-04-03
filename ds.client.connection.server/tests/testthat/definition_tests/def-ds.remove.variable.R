
source("connection_to_datasets/init_all_datasets.R")


.test.all.parameters.correct <- function(connection, variable.name)
{
   DSI::datashield.assign(connection, variable.name, value = as.symbol("D$INTEGER"), async = FALSE)
   expect_true(ds.exists.on.server(connection,variable.name,".GlobalEnv","integer"))
   .remove(connection, variable.name, "integer")
   expect_false(ds.exists.on.server(connection,variable.name,".GlobalEnv","integer"))
   DSI::datashield.assign(connection, variable.name, value = as.symbol("D$INTEGER"), async = FALSE)
   expect_true(ds.exists.on.server(connection,variable.name,".GlobalEnv","integer"))
   expect_true(ds.remove.variable(connection,variable.name,"integer"))
   expect_false(ds.exists.on.server(connection,variable.name,".GlobalEnv","integer"))
}

.test.no.connection <- function()
{
  expect_error(.remove())
  expect_error(.remove(NULL, "new.var"))
  expect_false(ds.remove.variable())
  expect_false(ds.remove.variable(NULL, "new.var"))
}

.test.no.variable.names <- function(connection)
{
  expect_error(.remove(connection))
  expect_error(.remove(connection, NULL))
  expect_error(.remove(connection, ""))
  expect_false(ds.remove.variable(connection))
  expect_false(ds.remove.variable(connection, NULL))
  expect_false(ds.remove.variable(connection, ""))
}


