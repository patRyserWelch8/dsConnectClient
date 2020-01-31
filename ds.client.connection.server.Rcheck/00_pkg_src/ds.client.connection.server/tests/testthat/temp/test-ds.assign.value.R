library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.assign.value.R")
source("connection_to_datasets/init_all_datasets.R")

context('ds.assign.value():no_connection')
test_that('no_connection',
{
  .test.no.connection()
})

connect.all.datasets()
context('ds.assign.value():more_incorrect_parameters:multiple')
test_that('more_incorrect_parameters',
{
  .test.no.variable.names(ds.test_env$connection.DSI)
  .test.no.value(ds.test_env$connection.DSI)
  .test.values.from.assign.incorrect.function(ds.test_env$connection.DSI)
})

context('ds.assign.value():correct_parameters:multiple')
test_that('correct_parameters',
{
  .test.all.parameters.correct(ds.test_env$connection.DSI)
  .test.values.from.assign.function(ds.test_env$connection.DSI)
})


log.out.data.server()

connect.dataset.1()
context('ds.assign.value():more_incorrect_parameters:single')
test_that('correct_parameters',
{
  .test.no.variable.names(ds.test_env$connection.DSI)
  .test.no.value(ds.test_env$connection.DSI)
  .test.values.from.assign.function(ds.test_env$connection.DSI)
})

context('ds.assign.value():correct_parameters:single')
test_that('more_incorrect_parameters',
{
  .test.all.parameters.correct(ds.test_env$connection.DSI)
  .test.twice.created.variable(ds.test_env$connection.DSI)
  .test.values.from.assign.incorrect.function(ds.test_env$connection.DSI)
})
log.out.data.server()



