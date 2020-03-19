
source("definition_tests/def-ds.create_environment.R")
source("connection_to_datasets/init_all_datasets.R")

.test.all.parameters.correct

connections <- connect.dataset.1()
context('ds.create_environment()::smk::success::single')
test_that('success',
{
  .test_success(connections)
})


context('ds.create_environment()::smk::failure::single')
test_that('failure',
{
  .test_failure(connections)
}
)

context('ds.create_environment()::expt::failure::multiple')
test_that('failure',
{
  .test.no.environment.name(connections)
})

context('ds.create_environment()::expt::success::multiple')
test_that('success',
{
  .test.all.parameters.correct(connections)
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

context('ds.create_environment()::expt::no_environment_name::single')
test_that('no_environemnt_name',
{
  .test.no.environment.name(connections)
}
)

context('ds.create_environment()::expt::failure::multiple')
test_that('failure',
{
  .test.no.environment.name(connections)
}
)

disconnect.all.datasets(connections)


context('ds.create_environment()::expt::no_connection')
test_that('no_connection',
{
  .test.no.connection()
})

