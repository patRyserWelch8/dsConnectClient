library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.find.variable.R")
source("connection_to_datasets/init_all_datasets.R")

print("----------test file ---------------")
print(ds.test_env$login.data)
print(ds.test_env$connections)
print(datashield.env$login.data)
print(datashield.env$connections)
print("----------test file ---------------")


connect.all.datasets()
print("----------test file ---------------")
print(ds.test_env$login.data)
print(ds.test_env$connections)

print("----------test file ---------------")


context('ds.find.variable()::multiple::correct_parameters')
print(ds.test_env$connections)
test_that('correct_parameters',
{
  .test.all.parameters.correct(ds.test_env$connections, "1")
})

context('ds.find.variable()::multiple::more_incorrect_parameters')
test_that('more_incorrect_parameters',
{
  .test.no.variable.names(ds.test_env$connections)
})

context('ds.find.variable()::no_connection')
test_that('no_connection',
{
            .test.no.connection()
})
log.out.data.server()

connect.dataset.1()
print(ds.test_env$connections)
context('ds.find.variable():more_incorrect_parameters:single')
test_that('correct_parameters',
{
  .test.no.variable.names(ds.test_env$connections)
  
})

context('ds.find.variable():correct_parameters:single')
test_that('more_correct_parameters',
{
  .test.all.parameters.correct(ds.test_env$connections, "2")
})
log.out.data.server()



