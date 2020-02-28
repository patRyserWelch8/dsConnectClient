library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.find.variable.R")
source("connection_to_datasets/init_all_datasets.R")


connections <- connect.all.datasets()
context('ds.find.variable()::multiple::correct_parameters')
test_that('correct_parameters',
{
  .test.all.parameters.correct(connections, "1")
})

context('ds.find.variable()::multiple::more_incorrect_parameters')
test_that('more_incorrect_parameters',
{
  .test.no.variable.names(connections)
})

context('ds.find.variable()::no_connection')
test_that('no_connection',
{
   .test.no.connection()
})
disconnect.all.datasets(connections)


connections <- connect.dataset.1()
print(ds.test_env$connections)
context('ds.find.variable():more_incorrect_parameters:single')
test_that('correct_parameters',
{
  .test.no.variable.names(connections)
})

context('ds.find.variable():correct_parameters:single')
test_that('more_correct_parameters',
{
  .test.all.parameters.correct(connections, "2")
})

disconnect.dataset.1(connections)

