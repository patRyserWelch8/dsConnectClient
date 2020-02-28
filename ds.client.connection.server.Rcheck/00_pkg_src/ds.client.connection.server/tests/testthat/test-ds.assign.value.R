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

connections <- connect.all.datasets()
context('ds.assign.value():more_incorrect_parameters:multiple')
test_that('more_incorrect_parameters',
{
  .test.no.variable.names(connections)
  .test.no.value(connections)
  .test.values.from.assign.incorrect.function(connections)
})

context('ds.assign.value():correct_parameters:multiple')
test_that('correct_parameters',
{
  .test.all.parameters.correct(connections)
  .test.values.from.assign.function(connections)
})


log.out.data.server()

connect.dataset.1()
context('ds.assign.value():more_incorrect_parameters:single')
test_that('correct_parameters',
{
  .test.no.variable.names(connections)
  .test.no.value(connections)
  .test.values.from.assign.function(connections)
})

context('ds.assign.value():correct_parameters:single')
test_that('more_incorrect_parameters',
{
  .test.all.parameters.correct(connections)
  .test.twice.created.variable(connections)
  .test.values.from.assign.incorrect.function(connections)
})
log.out.data.server()



