source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function(connection,variable.name,value,class.type)
{
  
  expect_true(.assign(datasources = connection, new.variable.name = variable.name, 
                     value = value, class.type = class.type, asynchronous = FALSE))
  results <- testthat::evaluate_promise(.assign( new.variable.name = variable.name, 
                                                value = value, class.type = class.type, 
                                                asynchronous = FALSE,datasources = connection))
  
  expect_true(ds.assign.value(datasources = connection, new.variable.name = variable.name, 
                      value = value, class.type = class.type, asynchronous = FALSE))
 
}

.test.twice.created.variable <- function(connection)
{
  expect_true(.assign (datasources = connection, new.variable.name = "test.var.1", 
                       value ="D$INTEGER", class.type = "integer", asynchronous = FALSE))
  expect_true(.assign (datasources = connection, new.variable.name = "test.var.1", 
                       value ="D$INTEGER", class.type = "integer", asynchronous = FALSE))
  
  expect_true(ds.assign.value(datasources = connection, new.variable.name = "test.var.1", 
                              value ="D$INTEGER", class.type = "integer",asynchronous = FALSE))
  expect_true(ds.assign.value(datasources = connection, new.variable.name = "test.var.1", 
                              value ="D$INTEGER", class.type = "integer", asynchronous = FALSE))
} 

.test.values.from.assign.function <- function(connection)
{
  server.call <- paste("rUnifDS(",100,",",14,",",50,",",10,")",sep="")
  expect_true(.assign(datasources = connection,new.variable.name = "test.var.1",value = server.call, class.type =  "numeric", asynchronous = FALSE))
  expect_true(ds.assign.value(datasources = connection, new.variable.name = "test.var.1",value=server.call,
                              class.type = "numeric",asynchronous = FALSE))
  
  server.call <- call("rUnifDS",100,14,50,10)
  expect_true(.assign(datasources = connection,new.variable.name = "test.var.1",value=server.call, 
                      class.type = "numeric", asynchronous = FALSE))
  expect_true(ds.assign.value(datasources = connection, new.variable.name = "test.var.1",value=server.call,
                              class.type = "numeric",asynchronous = FALSE))
}

.test.values.from.assign.incorrect.function <- function(connection)
{
  expect_error(.assign(datasources = connection,new.variable.name = "test.var.1", value="", class.type = "integer",asynchronous = FALSE))
  expect_false(.assign(datasources = connection,new.variable.name = "test.var.1", value="D$RUBBISH", class.type = "integer",asynchronous = FALSE))
  expect_false(.assign(datasources = connection,new.variable.name = "test.var.1",value="D$RUBBISH", class.type =  "integer",asynchronous = FALSE))
  expect_false(.assign(datasources = connection,new.variable.name = "test.var.1",value="D$RUBBISH", class.type =  "NULL",asynchronous = FALSE))

  expect_false(ds.assign.value(datasources = connection,new.variable.name = "test.var.1",value="D$RUBBISH",class.type =  "integer",asynchronous = FALSE))
  expect_false(ds.assign.value(datasources = connection,new.variable.name = "test.var.1",value="D$RUBBISH", class.type = "integer",asynchronous = FALSE))
  expect_false(ds.assign.value(datasources = connection,new.variable.name = "test.var.1",value="D$RUBBISH", class.type = "NULL",asynchronous = FALSE))
  expect_false(ds.assign.value(datasources = connection, new.variable.name = "test.var.1",value="",class.type = "integer",asynchronous = FALSE))
  
  ds.assign.value(datasources = connection, new.variable.name = "test.var.1",value="D$RUBBISH",class.type = "integer",asynchronous = FALSE)
  
  server.call <- call("rUnifDS_do_not_exist",100,14,50,10)
  expect_false(.assign(datasources = connection,new.variable.name = "test.var.1",value=server.call, class.type = "numeric", asynchronous = FALSE))
  expect_false(ds.assign.value(datasources = connection, new.variable.name = "test.var.1",value=server.call,class.type = "numeric",asynchronous = FALSE))
  results <- testthat::evaluate_promise(ds.assign.value(datasources = connection, new.variable.name = "test.var.1",value=server.call,class.type = "numeric",asynchronous = FALSE))
  
  
}


.test.no.connection.assign <- function()
{
  expect_error(.assign())
  expect_error(.assign(NULL, "new.var", value ="D$INTEGER"))
  expect_false(ds.assign.value())
  expect_false(ds.assign.value(NULL, "new.var", value ="D$INTEGER"))
}

.test.no.variable.names <- function(connection)
{
 
  expect_error(.assign(datasources = connection))
  expect_error(.assign(datasources = connection, class.type = NULL, value ="D$INTEGER"))
  expect_error(.assign(datasources = connection, class.type = "", value ="D$INTEGER"))
  
  expect_false(ds.assign.value(datasources = connection))
  expect_false(ds.assign.value(datasources = connection,class.type =  NULL, value ="D$INTEGER"))
  expect_false(ds.assign.value(datasources = connection, class.type = "", value ="D$INTEGER"))
}

.test.no.value <- function(connection)
{
  expect_error(.assign(datasources = connection, "test.var", ""))
  expect_false(ds.assign.value(datasources = connection, "test.var", ""))
}


.test.assign.server.error <- function(connection)
{
  server.call <- call("stopAssignDS")
  results <- testthat::evaluate_promise(ds.assign.value(datasources = connection, new.variable.name = "test.var.1",value=server.call,class.type = "numeric",asynchronous = FALSE))
  expect_equal(all(nchar(results$messages) > 0), TRUE)
  
}

