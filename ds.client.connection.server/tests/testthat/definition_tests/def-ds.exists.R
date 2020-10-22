
source("connection_to_datasets/init_all_datasets.R")


.test.variables.does.not.exists<- function(connection,number)
{
  # variable not created 
  
  name.var <- paste("variable.not.created",number, sep="")
  expect_false(.find.variable(datasources = connection,name.var,"integer"))
  expect_false(ds.exists.on.server(datasources = connection,name.var,"integer"))
}

.test.variables.exists<- function(connection,variable.name,value.to.copy,class.type)
{
  expect_true(ds.assign.value(datasources = connection, variable.name, value.to.copy,class.type))
  expect_true(ds.exists.on.server(datasources = connection,variable.name = variable.name,class.type = class.type))
  #expect_true(.find.variable(datasources = connection,variable.name = variable.name,class.type = class.type))
}

.test.no.connection <- function()
{
  expect_error(.find.variable())
  expect_error(.find.variable(NULL, "new.var"))
  expect_false( ds.exists.on.server())
  expect_false( ds.exists.on.server(NULL, "new.var"))
}

.test.incorrect.parameters <- function(connection)
{
  expect_error(.find.variable(datasources = connection))
  expect_error(.find.variable(datasources = connection, NULL))
  expect_error(.find.variable(datasources = connection, ""))
  expect_error(.find.variable(datasources = connection, "myVar",""))
  expect_error(.find.variable(datasources = connection, "myVar",1))
  expect_error(.find.variable(datasources = connection, "myVar","not a type"))
  
  
  expect_false( ds.exists.on.server(connection))
  expect_false( ds.exists.on.server(datasources = connection, NULL))
  expect_false( ds.exists.on.server(datasources = connection, ""))
  expect_false( ds.exists.on.server(datasources = connection, "myVar",""))
  expect_false( ds.exists.on.server(datasources = connection, "myVar",1))
  expect_false( ds.exists.on.server(datasources = connection, "myVar","not a type")) 
  
}



