library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.aggregate.R")
source("connection_to_datasets/init_all_datasets.R")


context('ds.aggregate():correct parameters:multiple')
test_that('correct parameters',
{
  print(ds.test_env$connection.DSI)
  connect.all.datasets()
  print(ds.test_env$connection.DSI)
  print("in test")
  #connections <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  print(ds.test_env$connection.DSI)
  .test.all.parameters.correct(ds.test_env$connection.DSI)
})

log.out.data.server()



context('ds.aggregate():correct parameters:single')
test_that('correct parameters',
{
  init.dataset.1()
  connections <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  .test.all.parameters.correct(connections)
  log.out.data.server()
})

context('ds.aggregate():incorrect parameters')
test_that('incorrect parameters',
{
  .test.no.connection(connections <- 1)
  .test.no.connection(connection <- "HELLO")
  .test.no.connection(connection <- NULL)
  init.all.datasets()
  connections <- ds.login(ds.test_env$login.data,assign = FALSE,ds.test_env$stat.vars, "D")
  .test.no.expression(connections)
  .test.incorrect.expression(connections)
  log.out.data.server()
})





