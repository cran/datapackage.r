library(datapackage.r)
library(testthat)
library(foreach)
library(stringr)
library(httr)
library(httptest)

# Constants

PROFILES <- list(
  'data-package',
  'tabular-data-package',
  'fiscal-data-package',
  'data-resource',
  'tabular-data-resource'
)

# Tests

testthat::context("Profile")

########################################
testthat::context('Profile #load')
########################################

foreach(name = 1:length(PROFILES) ) %do% {
  
  test_that(stringr::str_interp('load registry "${PROFILES[[name]]}" profile'), {
    
    jsonschema <- helpers.from.json.to.list(stringr::str_interp('inst/profiles/${PROFILES[[name]]}.json'))
    
    profile <- Profile.load(PROFILES[[name]])
    
    expect_equal(profile$jsonschema, jsonschema)
  })
}

test_that('load remote profile 1', {
  url <- 'https://specs.frictionlessdata.io/schemas/data-package.json'
  jsonschema <- helpers.from.json.to.list('inst/profiles/data-package.json')
  profile <- Profile.load(url)
  expect_equal(profile$name, "data-package")
  expect_equal(profile$jsonschema, jsonschema)
})

test_that('load remote profile', {
  url <- 'https://example.com/data-package.json'
  jsonschema <- helpers.from.json.to.list('inst/profiles/data-package.json')
  httptest::with_mock_API({
    profile <- Profile.load(url)
  })
  expect_equal(profile$name, "data-package")
  expect_equal(profile$jsonschema, jsonschema)
})

test_that('throw loading bad registry profile', {
  name <- 'bad-data-package'
  expect_error(Profile.load(name))
})



test_that('throw loading bad remote profile', {
  name <- 'https://example.com/profile.json'
  
  
  expect_error(
    with_mock(
      `httr:::request_perform` = function()
        httptest::fake_response(httr::GET(name), status_code = 400) ,
      `httptest::request_happened` = expect_message,
      eval.parent(Profile.load(name)),
      "Can not retrieve remote"
    )
  )
})


########################################
testthat::context('Profile #validate')
########################################

test_that('returns true for valid descriptor', {
  descriptor <- '{"resources": [{"name": "name", "data": ["data"]}]}'
  profile <- Profile.load('data-package')
  expect_true(profile$validate(descriptor)$valid)
})

test_that('errors for invalid descriptor', {
  descriptor <- helpers.from.json.to.list("{}")
  profile <- Profile.load('data-package')
  valid_errors <- profile$validate(descriptor)
  expect_false(valid_errors$valid)
})
