source('~/Documents/GitHub/ds.client.connection.server/ds.client.connection.server/R/ds.create_environment.R')
.test_success <- function(connection)
{
  
  successful <- ds.create_environment(connection = connection,new.environment.name ="TheFinalFrontier",asynchronous=FALSE)
  expect_equal(successful, TRUE)
  successful <- .create_environment(connection = connection,new.environment.name ="TheFinalFrontier",asynchronous=FALSE)
  expect_equal(successful, TRUE)
  
}

.test_failure <- function(connection)
{
  
  successful <- ds.create_environment()
  expect_equal(successful, FALSE)
  expect_error(.create_environment())
}


.test.no.connection <- function()
{
  expect_error(.create_environment())
  expect_error(.create_environment(NULL, "new.var"))
  expect_false(ds.create_environment())
  expect_false(ds.create_environment(NULL, "new.var"))
}


.test.no.environment.name <- function(connection)
{
  expect_error(.create_environment(connection))
  expect_error(.create_environment(connection, NULL))
  expect_error(.create_environment(connection, ""))
  expect_false(ds.create_environment(connection))
  expect_false(ds.create_environment(connection, NULL))
  expect_false(ds.create_environment(connection, ""))
}

.test.all.parameters.correct <- function(connection)
{
  successful <- ds.create_environment(connection = connection,new.environment.name ="TheFinalFrontier",asynchronous=FALSE)
  expect_equal(successful, TRUE)
  successful <- ds.create_environment(connection = connection,new.environment.name ="TheFinalFrontier",asynchronous=FALSE)
  expect_equal(successful, TRUE)
}


