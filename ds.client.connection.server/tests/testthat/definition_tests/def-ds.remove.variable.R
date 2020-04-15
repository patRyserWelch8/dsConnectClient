
source("connection_to_datasets/init_all_datasets.R")


.test.all.parameters.correct <- function(connection, variable.name)
{
   DSI::datashield.assign(connection, variable.name, value = as.symbol("D$INTEGER"), async = FALSE)
   print(ds.aggregate(connection,"environmentInfoDS()", asynchronous = FALSE))
   variable.exist <- ds.exists.on.server(connection, variable.name,".GlobalEnv",class.type="integer")
   print("Results of exists ....")
   print(variable.exist)
   expect_equal(variable.exist,TRUE)
   a <- .remove(connection, variable.name,"", "integer")
   print(a)
   print(ds.aggregate(connection,"ls()", asynchronous = FALSE))
  
   DSI::datashield.assign(connection, variable.name, value = as.symbol("D$INTEGER"), async = FALSE)
   expect_true(ds.exists.on.server(connection,variable.name,".GlobalEnv","integer"))
   expect_true(ds.remove.variable(connection,variable.name,"","integer"))
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


