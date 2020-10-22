library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.assign.value.R")
source("connection_to_datasets/init_all_datasets.R")



context('ds.assign.value():no_connection')
test_that('no_connection',
{
  .test.no.connection.assign()
})


connections <- connect.all.datasets(ds.test_env)
context('ds.assign.value():correct_parameters:multiple')
test_that('correct_parameters',
{
  .test.all.parameters.correct(connection = connections, "new_var_1", "D$INTEGER",'integer')
  .test.all.parameters.correct(connections, "new_var_2", "D$CHARACTER",'character')
  .test.all.parameters.correct(connections, "new_var_3", "D$NUMERIC",'numeric')
})



context('ds.assign.value():server_error:multiple')
test_that('server_error',
{
  .test.assign.server.error(connections)
})


context('ds.assign.value():more_incorrect_parameters:multiple')
test_that('more_incorrect_parameters',
{
  .test.no.connection.assign()
  .test.no.variable.names(connections)
  .test.no.value(connections)
  .test.values.from.assign.incorrect.function(connections)
})

log.out.data.server()


connect.dataset.1(ds.test_env)

context('ds.assign.value():no_connection')
test_that('no_connection',
{
  .test.no.connection.assign()
})

context('ds.assign.value():correct_parameters:single')
test_that('correct_parameters',
{
  .test.all.parameters.correct(connections, "new_var_1", "D$INTEGER",'integer')
  .test.all.parameters.correct(connections, "new_var_1", "D$CHARACTER",'character')
  .test.all.parameters.correct(connections, "new_var_1", "D$NUMERIC",'numeric')
})

context('ds.assign.value():incorrect_parameters:single')
test_that('incorrect_parameters',
{
  .test.no.variable.names(connections)
  .test.no.value(connections)
  .test.values.from.assign.incorrect.function(connections)
})

context('ds.assign.value():correct_parameters:single')
test_that('correct_parameters',
{
  .test.all.parameters.correct(connections, "new.var.1", "D$INTEGER",'integer')
  .test.twice.created.variable(connections)
  .test.values.from.assign.function(connections)
})

log.out.data.server()



