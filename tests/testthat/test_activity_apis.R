library(wrappedgithubactivityapi)
library(testthat)

token <- Sys.getenv('GITHUB_PAT')
if(token == ""){
  username <- ""
} else {
  username <- "test"
}


test_that("test_github_api returns list.",{
  expect_equal(typeof(github_api("/events",30,1,username=username,token=token)), "list")
})

test_that("test_github_api error value.",{
  expect_error(github_api("/events",username=username,token=token))
})

response_data <- github_api("/events",30,1,username=username,token=token)

test_that("test_github_api returns list.",{
  expect_equal(typeof(format_response(response_data,"list")), "list")
})


test_that("test_github_api returns character.",{
  expect_equal(typeof(format_response(response_data,"character")), "character")
})


test_that("test_get_public returns list.",{
  expect_equal(typeof(github_get_public_events(username=username,token=token)), "list")
})

test_that("test_get_public detect invaild input.",{
  expect_error(github_api("/events", "aaa",username=username,token=token), "The per_page parameter should not be non-numerical value")
})

test_that("test_get_public returns correct length list.",{
  expect_length(github_get_public_events(username=username,token=token), 30)
})

sum1 <- github_count_events_bydate("repository",102,"WestHealth","pyvis",event_type = "WatchEvent",username=username,token=token)
sum2 <- github_count_events_byweekday("repository",102,"WestHealth","pyvis",event_type = "WatchEvent",username=username,token=token)

test_that("test_get_public returns correct length list.",{
  expect_equal(sum(sum1[,"count"]),sum(sum2[,"count"]))
})

test_that('test_get_organization_events returns correct length list.', {
  expect_length(github_get_organization_events("ubco-mds-2021",username=username,token=token), 8)
})

test_that('test_count_events_bytype returns list.',{
  expect_equal(typeof(github_count_events_bytype("repository",102,"AndrewNg1891","DATA534_API_Project_Group9",username=username,token=token)), "list")
})

test_that('test_count_events_bytype returns correct column.', {
  expect_equal(names(github_count_events_bytype("repository",102,"AndrewNg1891","DATA534_API_Project_Group9",username=username,token=token)[1]), 'type')
  expect_equal(names(github_count_events_bytype("repository",102,"AndrewNg1891","DATA534_API_Project_Group9",username=username,token=token)[2]), 'count')
})

test_that('test_github_lastn_events_df returns correct number of colmun.', {
  expect_length(github_lastn_events_df("organization",102,organization="ubco-mds-2021",username=username,token=token), 5)
})

test_that('test_github_lastn_events_df returns list.',{
  expect_equal(typeof(github_lastn_events_df("organization",102,organization="ubco-mds-2021",username=username,token=token)), "list")
})

test_that('test_count_events_byweekday correct columns.', {
  expect_equal(names(github_count_events_byweekday("repository",102,"WestHealth","pyvis",username=username,token=token)[1]), 'weekdays')
  expect_equal(names(github_count_events_byweekday("repository",102,"WestHealth","pyvis",username=username,token=token)[2]), 'count')
})

test_that('test_count_events_bydate correct columns.', {
  expect_equal(names(github_count_events_bydate("repository",102,"WestHealth","pyvis",username=username,token=token)[1]), 'date')
  expect_equal(names(github_count_events_bydate("repository",102,"WestHealth","pyvis",username=username,token=token)[2]), 'count')
})



