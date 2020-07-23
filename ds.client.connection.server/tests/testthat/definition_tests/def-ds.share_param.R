source("connection_to_datasets/init_all_datasets.R")
.test_param <- function(connection)
{
  outcome <- ds.aggregate(connection, "setPiDS('pi_value')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  .share.parameter(connection,c("pi_value"))
  
  
  #ds.share.param(connection, function.name)
  
}

.test_no_connection <- function()
{
  connections    <-  NULL
  param.names    <-  c('pi_value','pi_value_b')
  expect_equal (ds.share.param(connections,param.names), FALSE)
  expect_warning(.share.parameter(connections))
  expect_error(.assignSettings(connections))
  expect_error(.complete.exchange(connections, param.names))
  expect_error(.exchange(connections, connections, param.names))
  expect_error(.assignParamSettings(connections, param.names))
  expect_equal(.encrypt_data(connections,TRUE,FALSE), "NR")
  expect_equal(.encrypt_param(connections),"NR")
  expect_error(.decrypt_data(connections))
  expect_error(.transfer.coordinates(connections, connections))
  expect_error(.transfer.encrypted.matrix(connections, connections, TRUE))
  expect_equal(.remove.encryption.data(connections,TRUE), "NR")
}

.test_single_connection <- function(connection)
{
  
  expect_equal(length(connection), 1)
  expect_warning(.share.parameter(connection))
  expect_equal(ds.share.param(connection),FALSE)
  
  expect_equal(length(connection), 1)
  expect_warning(.share.parameter(connection, c("pi_value")))
  expect_equal(ds.share.param(connection, c("pi_value")),FALSE)
}

.test_multiple_connections <- function(connections)
{
  #create variable and test their have been created
  outcome <- ds.aggregate(connections[[1]], "setPiDS('pi_value')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  outcome <- ds.aggregate(connections[[1]], "setPiDS('pi_value_B')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  
  #check length of connections
  expect_equal(length(connections)>1,TRUE)
  
  #check parameters 
  expect_warning(.share.parameter(connections))
  expect_equal(ds.share.param(connections),FALSE)
  
  #check actual exchange of parameters
  expect_equal(.share.parameter(connections,param.names = c('pi_value', 'pi_value_B')),TRUE)
  result <- ds.aggregate(connections, 'DANGERgetparam("pi_value")')
  expect_equal(length(result), length(connections))
  result <- ds.aggregate(connections, 'DANGERgetparam("pi_value_B")')
  expect_equal(length(result), length(connections))
  
  #check ds.share.param
  #clear parameters
  outcome <- ds.remove.variable(connections,"pi_value",".GlobalEnv","numeric")
  expect_equal(outcome, TRUE)
  #create variable and test their have been created
  outcome <- ds.aggregate(connections[[1]], "setPiDS('pi_value')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  outcome <- ds.aggregate(connections[[1]], "setPiDS('pi_value_B')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  
  # incorrect parameters
  expect_equal(ds.share.param(connections),FALSE)

  
  # correct parameters
  expect_equal(ds.share.param(connections, c('pi_value', 'pi_value_B')),TRUE)
  result <- ds.aggregate(connections, 'DANGERgetparam("pi_value")')
  expect_equal(length(result), length(connections))
  result <- ds.aggregate(connections, 'DANGERgetparam("pi_value_B")')
  expect_equal(length(result), length(connections))
}

.test_assign_testing <- function(connections)
{
  outcome <- ds.aggregate(connections[[1]], "setPiDS('pi_value')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)

  ds.remove.variable(connections,variable.name = "settings",",GlobalEnv","list")
  expect_equal(ds.exists.on.server(connections, "settings", ".GlabalEnv","list"), FALSE)
  outcome <- .assignSettings(connections)
  expect_equal(outcome, TRUE)
  expect_equal(ds.exists.on.server(connections, "settings", ".GlobalEnv","list"), TRUE)
  
}