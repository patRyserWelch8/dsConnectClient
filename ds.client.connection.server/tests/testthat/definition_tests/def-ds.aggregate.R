
source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function(connections)
{
  
  server.call <- paste("dimDS('",'D',"')", sep="")
  server.values <- .aggregate(connections, server.call)
  expect_true(length(server.values) == length(connections))
  server.values <- ds.aggregate(connections, server.call)
  expect_true(length(server.values) == length(connections))
  
  server.call <- call('dimDS','D')
  server.values <- .aggregate(connections, server.call)
  expect_true(length(server.values) == length(connections))
  
  server.values <- ds.aggregate(connections, server.call)
  expect_true(length(server.values) == length(connections))
  
  server.call <- 1
  expect_error(server.values <- .aggregate(connections, server.call))
  server.values <- ds.aggregate(connections, server.call)
  expect_equal(server.values, "NR")
  
  #server.call <- call('DANGER_Error')
  #DSI::datashield.aggregate(connections,server.call)
  #print(DSI::datashield.errors())
  #server.values <- ds.aggregate(connections, server.call)
  
}


.test.no.connection <- function(connections)
{
  server.call <- paste("dimDS(",'D',")")
  expect_error(.aggregate(connections,server.call))
  server.values <- ds.aggregate(connections, server.call)
  expect_true(server.values == "NR")
  
}

.test.no.expression <- function(connections)
{
  expect_error(.aggregate(connections,NULL))
  server.values <- ds.aggregate(connections, server.call)
  expect_true(server.values == "NR")
}

.test.incorrect.expression <- function(connections)
{
  
  server.call <- paste("dimDSabdce('",'D',"')")
  expect_equal(.aggregate(connections, server.call), "NR")
  server.values <- ds.aggregate(connections, server.call)
  expect_true(server.values == "NR")
  
}

.test.server.error <- function (connections)
{
  server.call <- call("testObjectTypeErrorDS")
  results <- testthat::evaluate_promise(ds.aggregate(connections, server.call))
  expect_true(nchar(results$messages) > 0)
  expect_equal(ds.aggregate(connections,server.call), "NR")
  expect_equal(.aggregate(connections,server.call), "NR")
  
  server.call <- call("testStopDS")
  results <- testthat::evaluate_promise(ds.aggregate(connections, server.call))
  expect_equal(ds.aggregate(connections,server.call), "NR")
  expect_equal(.aggregate(connections,server.call), "NR")
  expect_true(nchar(results$messages) > 0)
  
}

