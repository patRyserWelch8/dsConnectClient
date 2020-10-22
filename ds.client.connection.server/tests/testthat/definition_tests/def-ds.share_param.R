source("connection_to_datasets/init_all_datasets.R")

.test_assign_settings <- function(connections)
{
  outcome <- ds.remove.variable(datasources = connections,variable.name = "settings", class.type = "list")
  expect_equal(outcome, TRUE)
  outcome <- .assign.settings(connections)
  expect_equal(outcome,TRUE)
  outcome <- ds.exists.on.server(datasources = connections, variable.name = "settings",class.type = "list")
  expect_equal(outcome,TRUE)
}

.test_functions_without_settings <- function(connections)
{
  param.names    <-  c('pi_value','pi_value_b')
  expect_equal(.encrypt_param(connections[[1]]),FALSE)
  expect_equal(.decrypt_data(connections[[1]]), FALSE)
  expect_equal(.transfer.coordinates(connections[[1]], connections[[1]]),FALSE)
  expect_equal(.transfer.encrypted.matrix(connections[[1]], connections[[1]], TRUE), FALSE)
  expect_equal(.remove.encryption.data(connections[[1]],TRUE), FALSE)
  expect_equal(.complete.exchange(connections[[1]], param.names), FALSE)
  expect_equal(.exchange(connections[[1]], connections[[1]], param.names), FALSE)
}

.create.server.var <- function(connections)
{
 
  #create variable and test their have been created
  outcome <- ds.aggregate(datasources = connections[[1]], expression = call("setPiDS",'pi_value'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  outcome <- ds.aggregate(datasources = connections[[1]], expression = call("setPiDS",'pi_value_B'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
}

.test_param <- function(connection)
{
  ds.remove.variable(connection,'pi_value','numeric')
  outcome <- ds.aggregate(connection[[1]], "setPiDS('pi_value')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  expect_true(.share.parameter(connection,c("pi_value")))
  expect_true(ds.share.param(connection, function.name))
}

.test_no_connection <- function()
{
  connections    <-  NULL
  param.names    <-  c('pi_value','pi_value_b')
  expect_equal (ds.share.param(connections,param.names), FALSE)
  expect_warning(.share.parameter(connections))
  expect_equal(.assign.settings(connections), FALSE)
  expect_equal(.complete.exchange(connections, param.names), FALSE)
  expect_equal(.exchange(connections, connections, param.names), FALSE)
  expect_equal(.assign.param.settings(connections, param.names), FALSE)
  expect_equal(.encrypt.data(connections,TRUE,FALSE), FALSE)
  expect_equal(.encrypt.param(connections),FALSE)
  expect_equal(.decrypt.data(connections), FALSE)
  expect_equal(.transfer.coordinates(connections, connections), FALSE)
  expect_equal(.transfer.encrypted.matrix(connections, connections, TRUE),FALSE)
  expect_equal(.remove.encryption.data(connections,TRUE), FALSE)
}



.test_single_connection <- function(connection)
{
  expect_equal(length(connection), 1)
  expect_error(.share.parameter(connection))
  expect_equal(ds.share.param(datasources = connection),FALSE)
  expect_equal(length(connection), 1)
  
  #create variable and test their have been created
  .create.server.var(connection)
  
  expect_error(.share.parameter(connection, c("pi_value")))
  expect_equal(ds.share.param(connection, c("pi_value")),FALSE)
  
}

.test_multiple_connections <- function(connections)
{

  #check length of connections
  expect_equal(length(connections)>1,TRUE)
  
  #check parameters 
  #expect_warning(.share.parameter(connections))
  #expect_equal(ds.share.param(connections),FALSE)
  
  .create.server.var(connections)
  
  #check actual exchange of parameters
  #
  #print(evaluate_promise(.share.parameter(connections,param.names = c('pi_value', 'pi_value_B')),print=TRUE))
  result <- .share.parameter(connections,c('pi_value', 'pi_value_B'),15)
  result <- ds.aggregate(connections, 'DANGERgetparam("pi_value")')
  expect_equal(length(result), length(connections))
  result <- ds.aggregate(connections, 'DANGERgetparam("pi_value_B")')
  expect_equal(length(result), length(connections))
  
  #check ds.share.param
  #clear parameters
  outcome <- ds.remove.variable(connections,"pi_value","numeric")
  expect_equal(outcome, TRUE)
  outcome <- ds.remove.variable(connections,"pi_value_B","numeric")
  expect_equal(outcome, TRUE)
  
  #create variable and test their have been created
  .create.server.var(connections)
  
  # incorrect parameters
  expect_equal(ds.share.param(connections),FALSE)

  # correct parameters
  expect_true(ds.share.param(connections, c('pi_value', 'pi_value_B')))
}

.test_assign_testing <- function(connections)
{
  outcome <- ds.aggregate(expression = call("setPiDS","pi_value"), datasources = connections)
  #print(as.logical(outcome[[1]][1]))
  print(outcome)
  
  ds.remove.variable(connections,variable.name = "settings","list")
  expect_equal(ds.exists.on.server(connections, "settings", "list"), FALSE)
  outcome <- .assign.settings(connections)
  expect_equal(outcome, TRUE)
  expect_equal(ds.exists.on.server(connections, "settings", "list"), TRUE)
  
}