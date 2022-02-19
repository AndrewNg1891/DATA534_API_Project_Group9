library(testthat)
library(wrappedgithubactivityapi)

test_get_public <- github_get_public_events()
testthat("test_get_public returns list.",{
  expect_equal(typeof(test_get_public), "list")
})

test_get_organization_events <- github_get_organization_events()
testthat("test_get_organization_events returns list.",{
  expect_equal(typeof(test_get_organization_events), "list")
})

test_get_network_events <- github_get_network_events()
testthat("test_get_network_events returns list.",{
  expect_equal(typeof(test_get_network_events), "list")
})

test_get_repo_events <- github_get_repo_events()
testthat("test_get_repo_events returns dataframe.",{
  expect_equal(typeof(test_get_repo_events), "dataframe")
})

test_count_events_bytype <- github_count_events_bytype()
testthat("test_count_events_bytype returns list.",{
  expect_equal(typeof(test_get_repo_events), "list")
})

test_lastn_events_df <- github_lastn_events_df()
testthat("test_lastn_events_df returns list.",{
  expect_equal(typeof(test_lastn_events_df), "list")
})

test_count_events_byweekday <- github_count_events_byweekday()
testthat("test_count_events_byweekday returns list.",{
  expect_equal(typeof(test_count_events_byweekday), "list")
})

test_count_events_bydate <- github_count_events_bydate()
testthat("ttest_count_events_bydate returns list.",{
  expect_equal(typeof(test_count_events_bydate), "list")
})