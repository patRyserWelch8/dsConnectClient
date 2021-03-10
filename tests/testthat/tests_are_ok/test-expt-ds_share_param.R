library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")

connection <- connect.dataset.3(ds.test_env)
context('ds.share_param()::smk::single')
test_that('single connections',
{
  .test_single_connection(connection)
})


context('ds.share_param()::expt::assignSettings::single')
test_that('.assignSettings',
{
  .create.server.var(connection)
  expect_true(dssp.assign.settings(connections))
})

log.out.data.server()



connections <- connect.all.datasets(ds.test_env)

context('ds.share_param()::expt::encrypt.data::multiple')
test_that("encrypt_data",
{
  expect_true(dssp.assign.settings(connections))
  expect_true(dssp.encrypt.data(connections[[1]],master_mode = TRUE, preserve_mode = FALSE))
})

context('ds.share_param()::smk::multiple')
test_that('multiple connections',
{
  outcome <- ds.aggregate(datasources = connections[[1]], expression = call("setPiDS",'pi_value'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  outcome <- ds.aggregate(datasources = connections[[1]], expression = call("setPiDS",'pi_value_B'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  expect_true(dssp.share.parameter(connections, param.names = 'pi_value;pi_value_B',tolerance = 15))
 
  
  outcome <- ds.remove.variable(datasources = connections,variable.name = "pi_value", class.type = "numeric")
  expect_equal(outcome, TRUE)
  outcome <- ds.remove.variable(datasources = connections, variable.name = "pi_value_B",class.type = "numeric")
  expect_equal(outcome, TRUE)
  
  outcome <- ds.aggregate(datasources = connections[[1]], expression = call("setPiDS",'pi_value'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  outcome <- ds.aggregate(datasources = connections[[1]], expression =  call("setPiDS",'pi_value_B'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  
  expect_equal(ds.share.param(datasources = connections),FALSE)
  
  # correct parameters
  outcome <- ds.share.param(param.name = c('pi_value', 'pi_value_B'),tolerance = 15, datasources = connections)
  expect_true(outcome)
})






log.out.data.server()




