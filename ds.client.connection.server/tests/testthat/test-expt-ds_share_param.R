library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")

connections <- connect.all.datasets()
connection <- connect.dataset.1()


context('ds.share_param()::expt::.assignSettings::single')
test_that('single connections',
{
  .test_assign_testing(connections)
})

context('ds.share_param()::smk::single')
test_that('single connections',
{
  .test_single_connection(connection)
}
)


log.out.data.server()


connections <- connect.all.datasets()
context('ds.share_param()::smk::multiple')
test_that('multiple connections',
{
  .test_multiple_connections(connections)
}
)


log.out.data.server()

