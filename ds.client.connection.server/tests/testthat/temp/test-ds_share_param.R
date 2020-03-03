library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.share_param.R")

context('ds.share_param():connection')
connect.all.datasets ()
test_that('',
{
  .test_param(ds.test_env$connections)
}
)


log.out.data.server()
