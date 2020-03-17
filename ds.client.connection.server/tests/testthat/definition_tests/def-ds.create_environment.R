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