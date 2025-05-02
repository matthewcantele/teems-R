testhat::test_that("teems_model validates input files correctly", {
  # Test with non-existent file
  testhat::expect_snapshot(
    x = teems_model(tab_file = "nonexistent.tab"),
    error = TRUE
  )

  # Test user query of an ostensible internal tab file which does not exist
  testhat::expect_snapshot(
    x = teems_model(tab_file = "internal_tab"),
    error = TRUE
  )

  # Test existing file with wrong file extension
  mock_tab <- file.path(tempdir(), "not_a_tab.txt")
  file.create(mock_tab)

  testhat::expect_snapshot(teems_model(tab_file = mock_tab),
    error = TRUE
  )

  # Test with directory instead of file
  mock_tab <- dirname(tempdir())
  testhat::expect_snapshot(teems_model(tab_file = mock_tab),
    error = TRUE
  )
})

test_that("teems_model validates numeric arguments", {
  # Create mock tab file for testing
  mock_tab <- file.path(dirname(tempdir()), "mock.tab")
  writeLines("dummy content", mock_tab)

  # Mock .check_model_version and .inform_temp_dyn
  with_mock(
    `.check_model_version` = function(...) "1.0.0",
    `.inform_temp_dyn` = function(...) TRUE,
    `.check_input` = function(...) mock_tab,
    {
      # Test with non-numeric ndigits
      expect_error(teems_model(mock_tab, ndigits = "six"), "is.numeric")

      # Test with decimal ndigits (should be floored)
      result <- teems_model(mock_tab, ndigits = 6.7, quiet = TRUE)
      expect_equal(result$ndigits, 6L)
    }
  )

  # Clean up
  unlink(mock_tab)
})
# 
# test_that("teems_model handles model_version correctly", {
#   # Create mock tab file
#   mock_tab <- tempfile(fileext = ".tab")
#   writeLines("dummy content", mock_tab)
#   
#   # Test model version validation
#   with_mock(
#     `.check_input` = function(...) mock_tab,
#     `.inform_temp_dyn` = function(...) TRUE,
#     `.check_model_version` = function(tab_file, model_version, call, quiet) {
#       if (is.null(model_version)) return("default_version")
#       if (model_version == "invalid") stop("Invalid model version")
#       return(model_version)
#     },
#     {
#       # Test with NULL model_version (should use default)
#       result1 <- teems_model(mock_tab, quiet = TRUE)
#       expect_equal(result1$model_version, "default_version")
#       
#       # Test with valid model_version
#       result2 <- teems_model(mock_tab, model_version = "1.2.3", quiet = TRUE)
#       expect_equal(result2$model_version, "1.2.3")
#       
#       # Test with invalid model_version
#       expect_error(teems_model(mock_tab, model_version = "invalid"), "Invalid model")
#     }
#   )
#   
#   # Clean up
#   unlink(mock_tab)
# })
# 
# test_that("teems_model sets intertemporal flag correctly", {
#   mock_tab <- tempfile(fileext = ".tab")
#   writeLines("dummy content", mock_tab)
#   
#   # Test intertemporal flag
#   with_mock(
#     `.check_input` = function(...) mock_tab,
#     `.check_model_version` = function(...) "1.0.0",
#     `.inform_temp_dyn` = function(tab_file, quiet) {
#       if (grepl("dynamic", tab_file)) return(TRUE)
#       return(FALSE)
#     },
#     {
#       # Should be FALSE for regular file
#       result1 <- teems_model(mock_tab, quiet = TRUE)
#       expect_false(result1$intertemporal)
#       
#       # Should be TRUE for "dynamic" file
#       mock_dynamic <- tempfile(pattern = "dynamic", fileext = ".tab")
#       writeLines("dummy content", mock_dynamic)
#       with_mock(
#         `.check_input` = function(...) mock_dynamic,
#         {
#           result2 <- teems_model(mock_dynamic, quiet = TRUE)
#           expect_true(result2$intertemporal)
#         }
#       )
#       unlink(mock_dynamic)
#     }
#   )
#   
#   # Clean up
#   unlink(mock_tab)
# })
# 
# test_that("teems_model preserves call information", {
#   mock_tab <- tempfile(fileext = ".tab")
#   writeLines("dummy content", mock_tab)
#   
#   # Test that the call is preserved
#   with_mock(
#     `.check_input` = function(...) mock_tab,
#     `.check_model_version` = function(...) "1.0.0",
#     `.inform_temp_dyn` = function(...) FALSE,
#     {
#       result <- teems_model(mock_tab, ndigits = 8, quiet = TRUE)
#       expect_true("call" %in% names(result))
#       expect_equal(deparse(result$call)[1], "teems_model(tab_file = mock_tab, ndigits = 8, quiet = TRUE)")
#     }
#   )
#   
#   # Clean up
#   unlink(mock_tab)
#})