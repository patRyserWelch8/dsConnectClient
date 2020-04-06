library(DSI)
library(DSOpal)
library(testthat)
library(httr)

source("definition_tests/def-ds.exists.R")
source("connection_to_datasets/init_all_datasets.R")


connections <- connect.all.datasets()
context('ds.exists()::multiple::correct_parameters')
test_that('correct_parameters',
{
  .test.variables.does.not.exists(connections, "123")
  .test.variables.exists(connections,"number.int","D$INTEGER","integer")
 # .test.variables.exists(connections,"number.int.negative","D$NEGATIVE_INTEGER","integer")
  #.test.variables.exists(connections,"number.int.positive","D$POSITIVE_INTEGER","integer")
  #.test.variables.exists(connections,"number.num.positive","D$POSITIVE_NUMERIC","numeric")
  #.test.variables.exists(connections,"number.num.negative","D$NEGATIVE_NUMERIC","numeric")
  #.test.variables.exists(connections,"number.factor","D$FACTOR_INTEGER","integer")
  
  #.test.variables.exists(connections,"var.char","D$CHARACTER","character")
  #.test.variables.exists(connections,"var.logical","D$LOGICAL","logical")
  #.test.variables.exists(connections,"var.logical","D$LOGICAL","logical")
  
})

context('ds.exists()::multiple::more_incorrect_parameters')
test_that('more_incorrect_parameters',
{
  .test.incorrect.parameters(connections)
})

context('ds.exists()::no_connection')
test_that('no_connection',
{
   .test.no.connection()
})
disconnect.all.datasets(connections)


connections <- connect.dataset.1()
print(ds.test_env$connections)
context('ds.exists():more_incorrect_parameters:single')
test_that('correct_parameters',
{
  .test.incorrect.parameters(connections)
})

context('ds.exists():correct_parameters:single')
test_that('more_correct_parameters',
{
  .test.variables.does.not.exists(connections, "229")
  .test.variables.exists(connections,"number.int_1","D$INTEGER","integer")
  #.test.variables.exists(connections,"number.int.negative_2","D$NEGATIVE_INTEGER","integer")
  #.test.variables.exists(connections,"number.int.positive_3","D$POSITIVE_INTEGER","integer")
  #.test.variables.exists(connections,"number.num.positive_4","D$POSITIVE_NUMERIC","numeric")
  #.test.variables.exists(connections,"number.num.negative_1","D$NEGATIVE_NUMERIC","numeric")
  #.test.variables.exists(connections,"number.factor_2","D$FACTOR_INTEGER","integer")
  
  #.test.variables.exists(connections,"var.char","D$CHARACTER","character")
  #.test.variables.exists(connections,"var.logical","D$LOGICAL","logical")
  #ß.test.variables.exists(connections,"var.logical","D$LOGICAL","logical")
})

disconnect.dataset.1(connections)

