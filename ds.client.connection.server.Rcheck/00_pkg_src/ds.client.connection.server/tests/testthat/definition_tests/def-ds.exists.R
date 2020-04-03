
source("connection_to_datasets/init_all_datasets.R")


.test.all.parameters.correct <- function(connection,number)
{
  # variable not created 
  
  name.var <- paste("variable.not.created",number, sep="")
  expect_false(.find.variable(connection,name.var,".Globalenv","integer"))

  
  # variable created 
  expect_false(.find.variable(connection,"variable.created",".Globalenv","integer"))
 
  DSI::datashield.assign(connection, symbol ="variable.created", value = as.symbol("D$INTEGER"), async = FALSE)

  expect_true(.find.variable(connection,"variable.created",".Globalenv","integer"))

}

.test.no.connection <- function()
{
  expect_error(.find.variable())
  expect_error(.find.variable(NULL, "new.var"))
  expect_false(ds.exists())
  expect_false(ds.exists(NULL, "new.var"))
}

.test.incorrect.parameters <- function(connection)
{
  expect_error(.find.variable(connection))
  expect_error(.find.variable(connection, NULL))
  expect_error(.find.variable(connection, ""))
  expect_error(.find.variable(connection, "myVar",""))
  expect_error(.find.variable(connection, "myVar",".GlobalEnv",1))
  expect_error(.find.variable(connection, "myVar",".GlobalEnv","not a type"))
  
  
  expect_false(ds.exists(connection))
  expect_false(ds.exists(connection, NULL))
  expect_false(ds.exists(connection, ""))
  expect_false(ds.exists(connection, "myVar",""))
  expect_false(ds.exists(connection, "myVar",".GlobalEnv",1))
  expect_false(ds.exists(connection, "myVar",".GlobalEnv","not a type")) 
  
}



