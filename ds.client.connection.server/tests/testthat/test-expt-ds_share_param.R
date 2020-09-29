library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")





connections <- connect.dataset.1(ds.test_env)
.assignSettings(connections)
context('ds.share_param()::expt::error_servers')
test_that(".encrypt_data",
{
  master_mode = 'illogical'
  preserve_mode = 'illogical'
  expect_false(.encrypt_data(connections, master_mode = master_mode , preserve_mode =  preserve_mode))
  results <- evaluate_promise(.encrypt_data(connections, master_mode = master_mode , preserve_mode =  preserve_mode), print = FALSE)
  expect_true(length(results$messages) > 0)
  master_mode = 124.9
  preserve_mode = -1235
  expect_false(.encrypt_data(connections, master_mode = master_mode , preserve_mode =  preserve_mode))
  results <- evaluate_promise(.encrypt_data(connections, master_mode = master_mode , preserve_mode =  preserve_mode), print = FALSE)
  expect_true(length(results$messages) > 0)
  
  #print(ds.aggregate(connections, call("lsDS", NULL ,".GlobalEnv")))
  #master_mode = TRUE
  #preserve_mode = FALSE
  #results <- evaluate_promise(.encrypt_data(connections, master_mode = master_mode , preserve_mode =  preserve_mode), print = FALSE)
  #expect_true(length(results$messages) > 0)
  #print(results)
  #print(ds.aggregate(connections, call("lsDS", NULL ,".GlobalEnv")))
})


if (FALSE)
{

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
}
log.out.data.server()

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


