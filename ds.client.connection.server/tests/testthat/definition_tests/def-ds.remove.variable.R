source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function(connection, variable.name, class.type, server.variable)
{
  
   DSI::datashield.assign(connection, variable.name, value = as.symbol(server.variable), async = FALSE)
   expect_true(ds.exists.on.server(connection, variable.name,class.type=class.type))
   expect_true(.remove(connection, variable.name, class.type))
   expect_false(ds.exists.on.server(connection,variable.name,class.type))
   
   DSI::datashield.assign(connection, variable.name, value = as.symbol(server.variable), async = FALSE)
   expect_true(ds.exists.on.server(connection, variable.name,class.type=class.type))
   expect_true(ds.remove.variable(connection, variable.name, class.type))
   expect_false(ds.exists.on.server(connection,variable.name,class.type))
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


