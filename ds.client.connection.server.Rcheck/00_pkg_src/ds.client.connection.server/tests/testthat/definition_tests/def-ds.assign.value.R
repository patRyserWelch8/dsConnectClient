source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function(connection,variable.name,value,class.type)
{
  
  expect_true(.assign(connection, new.variable.name = variable.name, 
                     value = value, class.type, asynchronous = FALSE))
  expect_true(ds.assign.value(connection, new.variable.name = variable.name, 
                      value = value, class.type, asynchronous = FALSE))
 
}

.test.twice.created.variable <- function(connection)
{
  expect_true(.assign (connection, new.variable.name = "test.var.1", 
                       value ="D$INTEGER", "integer", asynchronous = FALSE))
  expect_true(.assign (connection, new.variable.name = "test.var.1", 
                       value ="D$INTEGER", "integer", asynchronous = FALSE))
  
  expect_true(ds.assign.value(connection, new.variable.name = "test.var.1", 
                              value ="D$INTEGER", "integer",asynchronous = FALSE))
  expect_true(ds.assign.value(connection, new.variable.name = "test.var.1", 
                              value ="D$INTEGER", "integer", asynchronous = FALSE))
} 

.test.values.from.assign.function <- function(connection)
{
  server.call <- paste("rUnifDS(",100,",",14,",",50,",",10,")",sep="")
  expect_true(.assign(connection,new.variable.name = "test.var.1",value=server.call, "numeric", asynchronous = FALSE))
  expect_true(ds.assign.value(connection, new.variable.name = "test.var.1",value=server.call,"numeric",asynchronous = FALSE))
}

.test.values.from.assign.incorrect.function <- function(connection)
{
  expect_error(.assign(connection,new.variable.name = "test.var.1",value="", "integer",asynchronous = FALSE))
  expect_false(ds.assign.value(connection, new.variable.name = "test.var.1",value=server.call,"integer",asynchronous = FALSE))
  expect_false(.assign(connection,new.variable.name = "test.var.1",value="D$RUBBISH", "integer",asynchronous = FALSE))
  expect_false(.assign(connection,new.variable.name = "test.var.1",value="D$RUBBISH", "integer",asynchronous = FALSE))
  expect_false(.assign(connection,new.variable.name = "test.var.1",value="D$RUBBISH", "NULL",asynchronous = FALSE))
 
  expect_false(ds.assign.value(connection,new.variable.name = "test.var.1",value="D$RUBBISH", "integer",asynchronous = FALSE))
  expect_false(ds.assign.value(connection,new.variable.name = "test.var.1",value="D$RUBBISH", "integer",asynchronous = FALSE))
  expect_false(ds.assign.value(connection,new.variable.name = "test.var.1",value="D$RUBBISH", "NULL",asynchronous = FALSE))
  expect_false(ds.assign.value(connection, new.variable.name = "test.var.1",value=server.call,"integer",asynchronous = FALSE))
}


.test.no.connection <- function()
{
  expect_error(.assign())
  expect_error(.assign(NULL, "new.var", value ="D$INTEGER"))
  expect_false(ds.assign.value())
  expect_false(ds.assign.value(NULL, "new.var", value ="D$INTEGER"))
}

.test.no.variable.names <- function(connection)
{
  expect_error(.assign(connection))
  expect_error(.assign(connection, NULL, value ="D$INTEGER"))
  expect_error(.assign(connection, "", value ="D$INTEGER"))
  expect_false(ds.assign.value(connection))
  expect_false(ds.assign.value(connection, NULL, value ="D$INTEGER"))
  expect_false(ds.assign.value(connection, "", value ="D$INTEGER"))
}

.test.no.value <- function(connection)
{
  expect_error(.assign(connection))
  expect_error(.assign(connection, "test.var", ""))
  expect_false(ds.assign.value(connection))
  expect_false(ds.assign.value(connection, "test.var", ""))
}

