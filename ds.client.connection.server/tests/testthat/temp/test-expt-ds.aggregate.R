library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.aggregate.R")
source("connection_to_datasets/init_all_datasets.R")

connections <- connect.all.datasets(ds.test_env)
context('ds.aggregate():correct parameters:multiple')
test_that('correct parameters',
{
  .test.all.parameters.correct(connections)
})

context('ds.aggregate():correct parameters:multiple')
test_that('incorrect parameters',
{
  .test.no.expression(connections)
  .test.incorrect.expression(connections)
})

disconnect.all.datasets(connections)


connections <- connect.dataset.2(ds.test_env)
context('ds.aggregate():correct parameters:single')
test_that('correct parameters',
{
  
  .test.all.parameters.correct(connections)
  
})

context('ds.aggregate():correct parameters:single')
test_that('incorrect parameters',
{
  .test.no.expression(connections)
  .test.incorrect.expression(connections)
})

disconnect.dataset.1(connections)

connections <- connect.dataset.1(ds.test_env)
context('ds.aggregate():correct parameters:single')
test_that('correct parameters',
{
  
  .test.all.parameters.correct(connections)
  
})

context('ds.aggregate():correct parameters:single')
test_that('incorrect parameters',
{
  .test.no.expression(connections)
  .test.incorrect.expression(connections)
})

disconnect.dataset.3(connections)

connections <- connect.dataset.1(ds.test_env)
context('ds.aggregate():correct parameters:single')
test_that('correct parameters',
{
  
  .test.all.parameters.correct(connections)
  
})

context('ds.aggregate():correct parameters:single')
test_that('incorrect parameters',
{
  .test.no.expression(connections)
  .test.incorrect.expression(connections)
})

disconnect.dataset.3(connections)

context('ds.aggregate():no_connection')
test_that('incorrect parameters',
{
  .test.no.connection(connections <- 1)
  .test.no.connection(connection <- "HELLO")
  .test.no.connection(connection <- NULL)
  
})
