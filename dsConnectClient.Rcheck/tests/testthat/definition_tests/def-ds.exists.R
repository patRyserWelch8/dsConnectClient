
source("connection_to_datasets/init_all_datasets.R")


.test.variables.does.not.exists<- function(connection,number)
{
  # variable not created 
  
  name.var <- paste("variable.not.created",number, sep="")
  expect_false(.find.variable(datasources = connection,variable.name = name.var,class.type = "integer"))
  expect_false(ds.exists.on.server(datasources = connection,variable.name = name.var,class.type = "integer"))
}

.test.variables.exists<- function(connection,variable.name,value.to.copy,class.type)
{
  # to do again
  #expect_true(ds.assign.value(datasources = connection, new.variable.name = variable.name, value = value.to.copy,class.type = class.type))
  #expect_true(ds.exists.on.server(datasources = connection,variable.name = variable.name,class.type = class.type))
  #expect_true(.find.variable(datasources = connection,variable.name = variable.name,class.type = class.type))
}

.test.no.connection <- function()
{
  expect_error(.find.variable())
  expect_error(.find.variable(datasources = NULL,variable.name =  "new.var"))
  expect_false( ds.exists.on.server())
  expect_false( ds.exists.on.server(datasources = NULL, variable.name = "new.var"))
}

.test.incorrect.parameters <- function(connection)
{
  expect_error(.find.variable(datasources = connection))
  expect_error(.find.variable(datasources = connection, variable.name = NULL))
  expect_error(.find.variable(datasources = connection, variable.name = ""))
  expect_error(.find.variable(datasources = connection, variable.name = "myVar",class.type = ""))
  expect_error(.find.variable(datasources = connection, variable.name = "myVar",class.type = 1))
  expect_error(.find.variable(datasources = connection, variable.name = "myVar",class.type = "not a type"))
  
  
  expect_false( ds.exists.on.server(datasources =  connection))
  expect_false( ds.exists.on.server(datasources = connection, variable.name = NULL))
  expect_false( ds.exists.on.server(datasources = connection, variable.name = ""))
  expect_false( ds.exists.on.server(datasources = connection, variable.name = "myVar",class.type = ""))
  expect_false( ds.exists.on.server(datasources = connection, variable.name = "myVar",class.type = 1))
  expect_false( ds.exists.on.server(datasources = connection, variable.name = "myVar",class.type = "not a type")) 
  
}



