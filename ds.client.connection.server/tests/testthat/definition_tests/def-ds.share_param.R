source("connection_to_datasets/init_all_datasets.R")
.test_param <- function(connection)
{
  function.name <- "setLengthDS()"
  .share.parameter(connection,function.name)
  
  
  #ds.share.param(connection, function.name)
  
}

.test_single_connection <- function(connection)
{
  function.name <- "setLengthDS()"
  expect_equal(length(connection), 1)
  expect_warning(.share.parameter(connection))
  expect_equal(ds.share.param(connection),FALSE)
  
  expect_equal(length(connection), 1)
  expect_warning(.share.parameter(connection, function.name))
  expect_equal(ds.share.param(connection, function.name),FALSE)
}

.test_multiple_connections <- function(connections)
{
  expect_equal(length(connections)>1,TRUE)
  expect_warning(.share.parameter(connections))
  expect_equal(ds.share.param(connections),FALSE)
   
  function.name <- "setLengthDS()"
  expect_equal(.share.parameter(connections,function.name),TRUE)
  result <- ds.aggregate(connections, 'DANGERgetparam()')
  #expect_equal(ds.share.param(connections),FALSE)
  result <- ds.aggregate(connections, 'DANGERgetparam()')
  expect_equal(length(result), length(connections))
  
}

.test_assign_testing <- function(connections)
{
  ds.remove.variable(connections,variable.name = "settings",",GlobalEnv","list")
  expect_equal(ds.exists.on.server(connections, "settings", ".GlabalEnv","list"), FALSE)
  outcome <- .assignSettings(connections)
  expect_equal(outcome, TRUE)
  expect_equal(ds.exists.on.server(connections, "settings", ".GlobalEnv","list"), TRUE)
  
}