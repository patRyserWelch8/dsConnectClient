library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.remove.variable.R")
source("connection_to_datasets/init_all_datasets.R")


connections <- connect.dataset.1(ds.test_env)

context('ds.remove.variable():more_incorrect_parameters:single')
test_that('correct_parameters',
{
  .test.no.variable.names(connections)
})

context('ds.remove.variable():correct_parameters:single')
test_that('correct_parameters',
{
  .test.all.parameters.correct(connections, "new_var", "integer", "D$INTEGER")
})
log.out.data.server()


connections <- connect.all.datasets(ds.test_env)

context('ds.remove.variable():more_incorrect_parameters:multiple')
test_that('more_incorrect_parameters',
{
  .test.no.variable.names(connections)
})


context('ds.remove.variable():correct_parameters:multiple')
test_that('correct_parameters',
{
  .test.all.parameters.correct(connections, "new_var_2", "integer", "D$INTEGER")
})

log.out.data.server()

context('ds.remove.variable():no_connection')
test_that('no_connection',
{
  .test.no.connection()
})
