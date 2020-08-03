library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")


context('ds.share_param()::expt::no_connections')
test_that("no_connection_all_function",
{
  .test_no_connection() 
})

connection <- connect.dataset.1()

context('ds.share_param()::smk::single')
test_that('single connections',
{
  .test_single_connection(connection)
})

.create.server.var(connections)
context('ds.share_param()::expt::assignSettings::single')
test_that('.assignSettings',
{
  .test_assign_settings(connections)
})


log.out.data.server()


connections <- connect.all.datasets()

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



