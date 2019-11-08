
source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct <- function(connections)
{
  
  server.call <- paste("dimDS(",'D',")")
  server.values <- ds.aggregate(connections, server.call)
  expect_true(length(server.values) == length(connections))
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
  expect_true(is.null(ds.aggregate(connections,NULL)))
}

.test.incorrect.expression <- function(connections)
{
  
  server.call <- paste("dimDSabdce('",'D',"')")
  expect_error(.aggregate(connections, server.call))
  expect_true(is.null(ds.aggregate(connections, server.call)))
  
}
