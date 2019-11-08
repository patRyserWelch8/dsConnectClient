library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.logout.R")

context('ds.logout():no_connection')
test_that('no_connection',
{
  .test.no.connection()
})

context('ds.logout():valid_connection')
test_that('valid_connection',
{
  .test.valid.connection()
})