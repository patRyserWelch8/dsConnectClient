
source("definition_tests/def-ds.create_environment.R")
source("connection_to_datasets/init_all_datasets.R")

connections <- connect.dataset.1()
context('ds.create_environment()::expt::success::single')
test_that('success',
{
  .test_success(connections)
}
)

context('ds.create_environment()::expt::failure::single')
test_that('failure',
{
  .test_failure(connections)
}
)

disconnect.dataset.1(connections)


connections <- connect.all.datasets()
context('ds.create_environment()::smk::success::multiple')
test_that('success',
{
  .test_success(connections)
}
)

context('ds.create_environment()::smk::failure::single')
test_that('failure',
{
  .test_failure(connections)
}
)


disconnect.all.datasets(connections)
