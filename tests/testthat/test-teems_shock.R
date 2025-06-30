test_that("req inputs", {
  expect_snapshot_error(teems_shock())
  expect_snapshot_error(teems_shock(var = "pfactwld"))
  expect_snapshot_error(teems_shock(type = "uniform"))
  expect_snapshot_error(teems_shock(var = "pfactwld", type = "custom"))
})

test_that("gen correct inputs", {
  expect_snapshot_error(teems_shock(var = 1, type = "uniform", input = 1))
  expect_snapshot_error(teems_shock(var = "pfactwld", input = 1, type = 1))
  expect_snapshot_error(teems_shock(var = "pfactwld", input = 1, type = "error"))
})

test_that("uni_shk correct inputs", {
  expect_snapshot_error(teems_shock(var = "pfactwld", type = "uniform", input = "string"))
})

test_that("cust_scen correct inputs", {
  expect_snapshot_error(teems_shock(var = "pfactwld", type = "custom", input = 1))
  expect_snapshot_error(teems_shock(var = "pfactwld", type = "custom", input = "/tmp/no_file.csv"))
  file.create("/tmp/wrong_ext.pdf")
  expect_snapshot_error(teems_shock(var = "pfactwld", type = "custom", input = "/tmp/wrong_ext.pdf"))
  unlink("/tmp/wrong_ext.pdf")
  write.csv(data.frame(no_value = 1), "/tmp/no_value.csv")
  expect_snapshot_error(teems_shock(var = "pfactwld", type = "custom", input = "/tmp/no_value.csv"))
  unlink("/tmp/no_value.csv")
  write.csv(data.frame(Value = 1), "/tmp/no_year.csv")
  expect_snapshot_error(teems_shock(var = "pfactwld", type = "scenario", input = "/tmp/no_year.csv"))
  unlink("/tmp/no_year.csv")
  df <- data.frame(Value = 1, Year = 2017)
  expect_type(teems_shock(var = "pfactwld", type = "scenario", input = df)[[1]]$input,
              "character")
})

test_that("gen correct outputs", {
  expect_type(teems_shock(var = "pop",
                          type = "uniform",
                          input = 1,
                          REGr = "lam"),
                  "list")
  expect_s3_class(teems_shock(var = "pop",
                              type = "uniform",
                              input = 1,
                              REGr = "lam")[[1]],
                  "shock")
})
