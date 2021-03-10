
source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function(connections)
{
  
  server.call <- paste("dimDS('",'D',"')", sep="")
  server.values <- dsag.aggregate.error.stop(datasources = connections, expression = server.call)
  expect_true(length(server.values) == length(connections))
  server.values <- ds.aggregate(datasources = connections, expression = server.call)
  expect_true(length(server.values) == length(connections))
  
  server.call <- call('dimDS','D')
  server.values <- dsag.aggregate.error.stop(datasources = connections, expression = server.call)
  expect_true(length(server.values) == length(connections))
  
  server.call <- call('dimDS', x = 'D')
  server.values <- dsag.aggregate.error.stop(datasources = connections, expression = server.call)
  expect_true(length(server.values) == length(connections))
  
  server.values <- ds.aggregate(datasources = connections, expression = server.call)
  expect_true(length(server.values) == length(connections))
  
  server.call <- 1
  expect_error(server.values <- dsag.aggregate.error.stop(datasources = connections, expression = server.call))
  server.values <- ds.aggregate(datasources = connections, expression = server.call)
  expect_equal(server.values, "NR")
  
  #server.call <- call('DANGER_Error')
  #DSI::datashield.aggregate(datasources = connectionsserver.call)
  #print(DSI::datashield.errors())
  #server.values <- ds.aggregate(datasources = connections , expression = server.call)
  
}


.test.no.connection <- function(connections)
{
  server.call <- paste("dimDS(",'D',")")
  expect_error(dsag.aggregate.error.stop(datasources = connections, expression = server.call))
  server.values <- ds.aggregate(datasources = connections , expression = server.call)
  expect_true(server.values == "NR")
  
}

.test.no.expression <- function(connections)
{
  expect_error(dsag.aggregate.error.stop(datasources = connectionsNULL))
  server.values <- ds.aggregate(datasources = connections , expression = server.call)
  expect_true(server.values == "NR")
}

.test.incorrect.expression <- function(connections)
{
  
  server.call <- paste("dimDSabdce('",'D',"')")
  expect_equal(dsag.aggregate.error.stop(datasources = connections , expression = server.call), "NR")
  server.values <- ds.aggregate(datasources = connections , expression = server.call)
  expect_true(server.values == "NR")
  
}

.test.server.error <- function (connections)
{
  server.call <- call("testObjectTypeErrorDS")
  results <- testthat::evaluate_promise(ds.aggregate(datasources = connections , expression = server.call))
  expect_true(unique(nchar(results$messages) > 0))
  expect_equal(ds.aggregate(datasources = connections, expression = server.call), "NR")
  expect_equal(dsag.aggregate.error.stop(datasources = connections,expression = server.call), "NR")
  
  server.call <- call("testStopDS")
  results <- testthat::evaluate_promise(ds.aggregate(datasources = connections, expression=  server.call))
  expect_equal(ds.aggregate(datasources = connections, expression=  server.call), "NR")
  expect_equal(dsag.aggregate.error.stop(datasources = connections, expression=  server.call), "NR")
  expect_true(unique(nchar(results$messages) > 0))
  
}

