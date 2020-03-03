library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")

context('ds.share_param():connection')
connections <- connect.dataset.1()
test_that('',
{
  .test_param(connections)
}
)


log.out.data.server()
