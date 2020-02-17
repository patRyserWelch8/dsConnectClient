library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.remove.variable.R")
source("connection_to_datasets/init_all_datasets.R")


context('ds.remove.variable():no_connection')
test_that('no_connection',
{
  .test.no.connection()
})

init.all.datasets()
log.in.data.server()


context('ds.remove.variable():more_incorrect_parameters:multiple')
test_that('more_incorrect_parameters',
{
  .test.no.variable.names(ds.test_env$connection.DSI)
})


context('ds.remove.variable():correct_parameters:multiple')
test_that('correct_parameters',
{
  .test.all.parameters.correct(ds.test_env$connection.DSI, "new.var")
})

log.out.data.server()

if(FALSE)
{
init.dataset.1()
log.in.data.server()
context('ds.remove.variable():more_incorrect_parameters:single')
test_that('correct_parameters',
          {
            .test.no.variable.names(ds.test_env$connection.DSI)
            .test.no.value(ds.test_env$connection.DSI)
          })

context('ds.remove.variable():correct_parameters:single')
test_that('more_incorrect_parameters',
{
  .test.all.parameters.correct(ds.test_env$connection.DSI)
})
log.out.data.server()

}

