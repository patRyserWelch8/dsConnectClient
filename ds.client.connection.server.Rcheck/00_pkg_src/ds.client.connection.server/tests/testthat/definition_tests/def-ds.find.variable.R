
source("connection_to_datasets/init_all_datasets.R")


.test.all.parameters.correct <- function(connection,number)
{
  # variable not created 
  
  name.var <- paste("variable.not.created",number, sep="")
  expect_false(.find.variable(connection,name.var))

  
  # variable created 
  expect_false(.find.variable(connection,"variable.created"))
 
  DSI::datashield.assign(connection, symbol ="variable.created", value = as.symbol("D$INTEGER"), async = FALSE)

  expect_true(.find.variable(connection,"variable.created"))

}

.test.no.connection <- function()
{
  expect_error(.find.variable())
  expect_error(.find.variable(NULL, "new.var"))
  expect_false(ds.find.variable())
  expect_false(ds.find.variable(NULL, "new.var"))
}

.test.no.variable.names <- function(connection)
{
  expect_error(.find.variable(connection))
  expect_error(.find.variable(connection, NULL))
  expect_error(.find.variable(connection, ""))
  expect_false(ds.find.variable(connection))
  expect_false(ds.find.variable(connection, NULL))
  expect_false(ds.find.variable(connection, ""))
}



