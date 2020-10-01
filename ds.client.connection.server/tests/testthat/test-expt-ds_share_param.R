library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")





connections <- connect.all.datasets(ds.test_env)
#.assignSettings(connections)
if (FALSE)
{
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
  print(results)
})

test_that(".decrypt.data",
{
  expect_false(.decrypt.data(connections[[1]]))
  results <- evaluate_promise(.decrypt.data(connections[[1]]), print = FALSE)
  print(results)
})

test_that(".assign.param.settings",
{
  expect_false(.assign.param.settings(connections[[1]],c("var_1","var_2")))
  results <- evaluate_promise(.assign.param.settings(connections[[1]],c("var_1","var_2")), print = FALSE)
  print(results)
})

test_that(".transfer.coordinates",
{
  expect_false(.transfer.coordinates())
  results <- evaluate_promise(.transfer.coordinates(sender = connections[[1]], receiver = connections[[2]]), print = FALSE)
  print(results)
})

test_that(".encrypt.param",
{
  expect_false(.encrypt.param())
  results <- evaluate_promise(.encrypt.param(), print = FALSE)
  print(results)
})


test_that(".encrypt.param",
{
  expect_false(.encrypt.param(connections[[1]]))
  results <- evaluate_promise(.encrypt.param(connections[[1]]), print = FALSE)
  print(results)
})

test_that(".decrypt.param",
{
  expect_false(.decrypt.param(connections[[2]], c("var_1","var_2"), 15 ))
  results <- evaluate_promise(.decrypt.param(connections[[2]], c("var_1","var_2"), 15 ), print = FALSE)
  print(results)
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
}
connections <- connect.all.datasets(ds.test_env)

context('ds.share_param()::smk::multiple')
test_that('multiple connections',
{
  .test_multiple_connections(connections)
})

.create.server.var(connections)
context('ds.share_param()::expt::assignSettings::multiple')
test_that('.assignSettings',
{
  .test_assign_settings(connections)
})


log.out.data.server()

context('ds.share_param()::expt::no_connections')
test_that("no_connection_all_function",
{
  .test_no_connection() 
})



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
#print(results)
#print(ds.aggregate(connections, call("lsDS", NULL ,".GlobalEnv")))
