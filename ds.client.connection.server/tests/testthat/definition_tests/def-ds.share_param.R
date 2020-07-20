source("connection_to_datasets/init_all_datasets.R")
.test_param <- function(connection)
{
  outcome <- ds.aggregate(connection, "setPiDS('pi_value')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  .share.parameter(connection,c("pi_value"))
  
  
  #ds.share.param(connection, function.name)
  
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
  outcome <- ds.aggregate(connections[[1]], "setPiDS('pi_value')")
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  expect_equal(length(connections)>1,TRUE)
  #expect_warning(.share.parameter(connections))
  #expect_equal(ds.share.param(connections),FALSE)
   
  expect_equal(.share.parameter(connections,param.names = c("pi_value")),TRUE)
  result <- ds.aggregate(connections, 'DANGERgetparam()')
  #expect_equal(ds.share.param(connections),FALSE)
  result <- ds.aggregate(connections, 'DANGERgetparam()')
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