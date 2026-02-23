library(testthat)
library(PubMatrixR)

test_that("internal XML parser extracts count from fixture", {
  fixture <- test_path("fixtures", "esearch_count.xml")
  doc <- xml2::read_xml(fixture)

  expect_equal(PubMatrixR:::.pubmatrix_extract_count(doc), 42)
})

test_that("internal XML parser errors when count is missing", {
  doc <- xml2::read_xml("<eSearchResult><RetMax>0</RetMax></eSearchResult>")

  expect_error(
    PubMatrixR:::.pubmatrix_extract_count(doc),
    "did not contain a <Count> field"
  )
})
