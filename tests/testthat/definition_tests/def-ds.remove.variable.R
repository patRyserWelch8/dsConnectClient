source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function(connection, variable.name, class.type, server.variable)
{
   options(datashield.errors.stop = FALSE)
   DSI::datashield.assign(connection, variable.name, value = as.symbol(server.variable), async = FALSE)
   expect_true(ds.exists.on.server(datasources = connection, variable.name = variable.name,class.type = class.type))
   expect_true(dsrv.remove(datasources = connection, variable.name = variable.name, class.type = class.type))
   expect_false(ds.exists.on.server(datasources = connection,variable.name = variable.name,class.type = class.type))
   
   DSI::datashield.assign(connection, variable.name, value = as.symbol(server.variable), async = FALSE)
   expect_true(ds.exists.on.server(datasources = connection, variable.name = variable.name,class.type=class.type))
   expect_true(ds.remove.variable(datasources = connection, variable.name = variable.name, class.type = class.type))
   expect_false(ds.exists.on.server(datasources = connection, variable.name = variable.name,class.type = class.type))
}

.test.no.connection <- function()
{
  expect_error(dsrv.remove())
  expect_error(dsrv.remove(datasources = NULL, variable.name = "new.var"))
  expect_false(ds.remove.variable())
  expect_false(ds.remove.variable(datasources = NULL, variable.name = "new.var"))
}

.test.no.variable.names <- function(connection)
{
  expect_error(dsrv.remove(datasources = connection))
  expect_error(dsrv.remove(datasources = connection, variable.name = NULL))
  expect_error(dsrv.remove(datasources = connection, variable.name = ""))
  expect_false(ds.remove.variable(datasources = connection))
  expect_false(ds.remove.variable(datasources = connection, variable.name = NULL))
  expect_false(ds.remove.variable(datasources = connection, variable.name = ""))
}


