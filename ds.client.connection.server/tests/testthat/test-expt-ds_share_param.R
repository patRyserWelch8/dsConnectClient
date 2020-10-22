library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")



context('ds.share_param()::expt::no_connections')
test_that("no_connection_all_function",
{
  connections    <- NULL
  param.names    <-  c('pi_value','pi_value_b')
  expect_equal (ds.share.param(connections,param.names), FALSE)
  expect_error(.share.parameter(connections))
  expect_equal(.assign.settings(connections), FALSE)
  expect_equal(.complete.exchange(connections, param.names), FALSE)
  expect_equal(.exchange(connections, connections, param.names), FALSE)
  expect_equal(.assign.param.settings(connections, param.names), FALSE)
  expect_equal(.encrypt.data(connections,TRUE,FALSE), FALSE)
  expect_equal(.encrypt.param(connections),FALSE)
  expect_equal(.decrypt.data(connections), FALSE)
  expect_equal(.transfer.coordinates(connections, connections), FALSE)
  expect_equal(.transfer.encrypted.matrix(connections, connections, TRUE),FALSE)
  expect_equal(.remove.encryption.data(connections,TRUE), FALSE)
})


connections <- connect.all.datasets(ds.test_env)
#.assignSettings(connections)


context('ds.share_param()::expt::error_servers')
test_that(".encrypt_data",
{
 
  master_mode = 'illogical'
  preserve_mode = 'illogical'
  expect_false(.encrypt.data(connections, master_mode = master_mode , preserve_mode =  preserve_mode))
  results <- evaluate_promise(.encrypt.data(connections, master_mode = master_mode , preserve_mode =  preserve_mode), print = FALSE)
  expect_true(length(results$messages) > 0)
  master_mode = 124.9
  preserve_mode = -1235
  expect_false(.encrypt.data(connections, master_mode = master_mode , preserve_mode =  preserve_mode))
  results <- evaluate_promise(.encrypt.data(connections, master_mode = master_mode , preserve_mode =  preserve_mode), print = FALSE)
  expect_true(length(results$messages) > 0)
})

test_that(".transfer.encrypted.matrix",
{
  expect_false(.transfer.encrypted.matrix())
  results <- evaluate_promise(.transfer.encrypted.matrix(sender = connections[[1]], receiver = connections[[2]], master_mode = TRUE), print = FALSE)
  expect_true(length(results$messages) > 0)
})

test_that(".decrypt.data",
{
  expect_false(.decrypt.data(connections[[1]]))
  results <- evaluate_promise(.decrypt.data(connections[[1]]), print = FALSE)
  expect_true(length(results$messages) > 0)
})

test_that(".assign.param.settings",
{
  expect_false(.assign.param.settings(connections[[1]],c("var_1","var_2")))
  results <- evaluate_promise(.assign.param.settings(connections[[1]],c("var_1","var_2")), print = FALSE)
  expect_true(length(results$messages) > 0)
})

test_that(".transfer.coordinates",
{
  expect_false(.transfer.coordinates())
  results <- evaluate_promise(.transfer.coordinates(sender = connections[[1]], receiver = connections[[2]]), print = FALSE)
  expect_true(length(results$messages) > 0)
})

test_that(".encrypt.param",
{
  expect_false(.encrypt.param())
  results <- evaluate_promise(.encrypt.param(), print = FALSE)
  expect_true(length(results$messages) > 0)
})


test_that(".encrypt.param",
{
  expect_false(.encrypt.param(connections[[1]]))
  results <- evaluate_promise(.encrypt.param(connections[[1]]), print = FALSE)
  expect_true(length(results$messages) > 0)
})

test_that(".decrypt.param",
{
  expect_false(.decrypt.param(connections[[2]], c("var_1","var_2"), 15 ))
  results <- evaluate_promise(.decrypt.param(connections[[2]], c("var_1","var_2"), 15 ), print = FALSE)
  expect_true(length(results$messages) > 0)
})

log.out.data.server()



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
  .test_assign_settings(connection)
})

log.out.data.server()

connections <- connect.all.datasets(ds.test_env)

context('ds.share_param()::smk::multiple')
test_that('multiple connections',
{
  outcome <- ds.aggregate(connections[[1]], call("setPiDS",'pi_value'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  outcome <- ds.aggregate(connections[[1]], call("setPiDS",'pi_value_B'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  expect_true(.share.parameter(connections,'pi_value;pi_value_B',15))
 
  
  outcome <- ds.remove.variable(connections,"pi_value","numeric")
  expect_equal(outcome, TRUE)
  outcome <- ds.remove.variable(connections,"pi_value_B","numeric")
  expect_equal(outcome, TRUE)
  
  outcome <- ds.aggregate(connections[[1]], call("setPiDS",'pi_value'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  outcome <- ds.aggregate(connections[[1]], call("setPiDS",'pi_value_B'))
  expect_equal(as.logical(outcome[[1]][1]),TRUE)
  
  expect_equal(ds.share.param(connections),FALSE)
  
  # correct parameters
  expect_true(ds.share.param(connections, c('pi_value', 'pi_value_B'),15))
  
  
})

.create.server.var(connections)
context('ds.share_param()::expt::assignSettings::multiple')
test_that('.assignSettings',
{
  .test_assign_settings(connections)
})


log.out.data.server()




#print("")
#print("=======================")
#lsDS<-function(search.filter=NULL,env.to.search)
#expression <- call("lsDS", NULL ,".GlobalEnv")
#print(expression)
#print(DSI::datashield.aggregate(connections, expression))

#print("=======================")
#expression <- call("lsDS", search.filter = NULL, env.to.search = ".GlobalEnv")
#print(expression)
#print(DSI::datashield.aggregate(connections, expression))

#master_mode = TRUE
#preserve_mode = FALSE
#results <- evaluate_promise(.encrypt_data(connections, master_mode = master_mode , preserve_mode =  preserve_mode), print = FALSE)
#expect_true(length(results$messages) > 0)
#print(ds.aggregate(connections, call("lsDS", NULL ,".GlobalEnv")))
