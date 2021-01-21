context("Retrocompatibility with mlxR")

skip_on_cran()
skip_if_not_installed("lixoftConnectors")
initRssimulx(software = "simulx", warnings = FALSE, info = FALSE)
demo_path <- file.path(path.expand("~"), "lixoft", "monolix", paste0("monolix", smlx.getLixoftConnectorsState()$version), "demos")
skip_if(!dir.exists(demo_path), message = NULL)

test_that("Case studies and demos run with no fail", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  expect_error(simulx(project = project), NA)
  expect_error(simulx(project = project, 
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1))),
               NA)
  expect_error(simulx(project = project, 
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1)),
                      group = list(size = 5)),
               NA)
  expect_error(simulx(project = project, 
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1)),
                      treatment =  list(amount = 10, time = seq(from = 0, to = 5, by = 1)),
                      group = list(size = 5)),
               NA)
  g1 = list(size = 5, treatment = list(amount = 10, time = 0), parameter = c(omega_V = 0, omega_ka = 0, omega_Cl = 0))
  g2 = list(size = 4, treatment = list(amount = 20, time = 2))
  expect_error(simulx(project = project, 
                      output = list(name = 'Cc', time = seq(from = 1, to = 25, by = 12)),
                      group = list(g1,g2)),
               NA)
})