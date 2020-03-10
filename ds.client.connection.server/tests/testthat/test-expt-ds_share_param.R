library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")

connection <- connect.dataset.1()
context('ds.share_param()::smk::single')
test_that('single connections',
{
  .test_param(connection)
}
)


log.out.data.server()


connections <- connect.all.datasets()
context('ds.share_param()::smk::multiple')
test_that('multiple connections',
{
  .test_param(connections)
}
)


log.out.data.server()
