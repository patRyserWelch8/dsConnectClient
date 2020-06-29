library(DSI)
library(DSOpal)
library(testthat)
library(httr)
source("definition_tests/def-ds.logout.R")
source("connection_to_datasets/init_all_datasets.R")

context('ds.logout():no_connection')
test_that('no_connection',
{
  .test.no.connection()
})

context('ds.logout():valid_connection:multiple')
test_that('valid_connection',
{
  connections <- connect.all.datasets()
  .test.valid.connection(connections)
  
})

context('ds.logout():valid_connection:multiple')
test_that('valid_connection',
{
  connections <- connect.dataset.3()
  .test.valid.connection(connections)
  connections <- connect.dataset.2()
  .test.valid.connection(connections)
  connections <- connect.dataset.1()
  .test.valid.connection(connections)
})