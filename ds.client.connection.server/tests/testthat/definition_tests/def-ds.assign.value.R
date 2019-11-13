
source("connection_to_datasets/init_all_datasets.R")


.test.all.parameters.correct <- function(connection)
{
  
   expect_true(.assign (connection, new.variable.name = "test.var.1", 
             value ="D$INTEGER", asynchronous = FALSE))

   expect_true(ds.assign.value(connection, new.variable.name = "test.var.1", 
                       value ="D$INTEGER", asynchronous = FALSE))
}

.test.no.connection <- function()
{
  expect_error(.assign())
  expect_error(.assign(NULL, "new.var", value ="D$INTEGER"))
  expect_false(ds.assign.value())
  expect_false(ds.assign.value(NULL, "new.var", value ="D$INTEGER"))
}

.test.no.variable.names <- function(connection)
{
  expect_error(.assign(connection))
  expect_error(.assign(connection, NULL, value ="D$INTEGER"))
  expect_error(.assign(connection, "", value ="D$INTEGER"))
  expect_false(ds.assign.value(connection))
  expect_false(ds.assign.value(connection, NULL, value ="D$INTEGER"))
  expect_false(ds.assign.value(connection, "", value ="D$INTEGER"))
}

.test.no.value <- function(connection)
{
  expect_error(.assign(connection))
  expect_error(.assign(connection, "test.var", ""))
  expect_false(ds.assign.value(connection))
  expect_false(ds.assign.value(connection, "test.var", ""))
}

