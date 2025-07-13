test_that("req inputs", {
  expect_snapshot_error(teems_model())
})

test_that("valid inputs", {
  expect_snapshot_error(teems_model(tab_file = 1))
  expect_snapshot_error(teems_model(tab_file = "not_internal_tab"))
  expect_snapshot_error(teems_model(tab_file = "not_internal_tab.tab"))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", closure_file = "nocls.cls"))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", closure_file = "noshk.shf"))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", var_omit = 1))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", closure_file = 1))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", swap_in = 1))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", swap_out = 1))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", shock_file = 1))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", shock = 1))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", full_exclude = 1))
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", notes = 1))
})

test_that("swap err", {
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", swap_in = "not_a_var"))
  wrong_set <- teems_swap(var = "pop", REG = "lam")
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", swap_in = wrong_set))
  swap <- teems_swap(var = "tfd")
  expect_snapshot(teems_model(tab_file = "GTAPv6.2", swap_in = swap))
  expect_snapshot(teems_model(tab_file = "GTAPv6.2", swap_in = "qgdp"))
  expect_snapshot(teems_model(tab_file = "GTAPv6.2", swap_in = list("qgdp", swap)))
})

test_that("cls err", {
  cls_file <- readLines("data-raw/closures/GTAPv6.2/GTAPv6.2.cls")
  cls_file[2] <- "not_a_var"
  writeLines(cls_file, "/tmp/cls_file.cls")
  teems_model(tab_file = "GTAPv6.2", closure_file = "/tmp/cls_file.cls")
  unlink("/tmp/cls_file.cls")
})

test_that("shk err", {
  rndm_lst <- list(a = "rndm")
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", shock = rndm_lst))
  not_a_var <- teems_shock(var = "not_a_var", type = "uniform", input = 1)
  expect_snapshot_error(teems_model(tab_file = "GTAPv6.2", shock = not_a_var))
  df <- data.frame(REGr = "lam", ALLTIMEt = 0, Year = 2014, Value = 1)
  extra_col <- teems_shock(var = "pop", type = "custom", input = df)
  expect_snapshot_error(teems_model(tab_file = "GTAP-INTv1", shock = extra_col))
  not_a_set_uni <- teems_shock(var = "afeall", type = "uniform", input = 1, REG = "lam", Year = 2014, PROD_COMMj = "zcgds")
  expect_snapshot_error(teems_model(tab_file = "GTAP-INTv1", shock = not_a_set_uni))
  df <- data.frame(REG = "lam", PROD_COMMj = "zcgds", Year = 2014, Value = 1)
  not_a_set_cust <- teems_shock(var = "afeall", type = "custom", input = df)
  expect_snapshot_error(teems_model(tab_file = "GTAP-INTv1", shock = not_a_set_cust))
  uni_shk <- teems_shock(var = "afeall", type = "uniform", input = 1, REGr = "lam", PROD_COMMj = "zcgds")
})