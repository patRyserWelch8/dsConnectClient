library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")
source("connection_to_datasets/init_all_datasets.R")

connections <- connect.all.datasets()
context('ds.share_param():connection')
test_that('',
{
  .test_param(connections)
}
)

disconnect.all.datasets(connections)
