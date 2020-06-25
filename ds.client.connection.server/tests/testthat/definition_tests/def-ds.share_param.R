source("connection_to_datasets/init_all_datasets.R")
.test_param <- function(connection)
{
  function.name <- "setLengthDS()"
  .share.parameter(connection,function.name)
  #ds.share.param(connection, function.name)
}

.test_single_connection <- function(connection)
{
  expect_equal(length(connection), 1)
  expect_warning(.share.parameter(connection))
  expect_equal(ds.share.param(connection),FALSE)
}

.test_multiple_connections <- function(connections)
{
  expect_equal(length(connections)>1,TRUE)
  expect_equal(.share.parameter(connections),TRUE)
  expect_equal(ds.share.param(connections),TRUE)
}