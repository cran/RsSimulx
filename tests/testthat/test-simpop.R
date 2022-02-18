context("Sim pop")

skip_on_cran()
skip_if_not_installed("lixoftConnectors")

initRsSimulx()
demo_path <- file.path(path.expand("~"), "lixoft", "monolix",
                       paste0("monolix", .lixoftCall("getLixoftConnectorsState")$version), "demos")
skip_if(!dir.exists(demo_path), message = NULL)

# test_that("simpopmlx returns an error when parameter & project defined together") {
#   expect_error()
# }
# test_that("simpopmlx returns an error when project & correlation defined together")

# Test that arguments are well checked -----------------------------------------
test_that("simpopmlx returns an error when n/kw.max/seed is not an integer", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  expect_error(simpopmlx(project = project, n = "toto"))
  expect_error(simpopmlx(project = project, n = -3))
  expect_error(simpopmlx(project = project, n = 1.2))
  expect_error(simpopmlx(project = project, kw.max = "toto"))
  expect_error(simpopmlx(project = project, kw.max = -3))
  expect_error(simpopmlx(project = project, kw.max = 1.2))
  expect_error(simpopmlx(project = project, seed = "toto"))
  expect_error(simpopmlx(project = project, seed = -3))
  expect_error(simpopmlx(project = project, seed = 1.2))
  
})

test_that("simpopmlx returns an error when fim parameter is different from sa / lin", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  expect_error(simpopmlx(project = project, fim = "toto"))
  expect_error(simpopmlx(project = project, fim = -3))
  expect_error(simpopmlx(project = project, fim = 3))
})

test_that("simpopmlx returns an error when input project is invalid", {
  expect_error(simpopmlx(project = "thisprojectdoesnorexist.mlxtran"))
  smlxpath <- file.path(path.expand("~"), "lixoft", "simulx",
                        paste0("simulx", .lixoftCall("getLixoftConnectorsState")$version), "demos")
  smlxproject <- file.path(smlxpath, "/2.models/longitudinal.smlx")
  expect_error(simpopmlx(project = smlxproject))
})

test_that("simpopmlx returns an error when neither parameter nor project are defined", {
  expect_error(simpopmlx(n = 2))
})

test_that("simpopmlx returns an error when parameter is not a dataframe", {
  expect_error(simpopmlx(parameter = 2))
  expect_error(simpopmlx(parameter = c(1, 2, 3)))
})

test_that("simpopmlx returns an error when wrong columns defined in parameter", {
  expect_error(simpopmlx(parameter = data.frame(sd = c(12, 2), pop.param = c(5, 6), toto = c(3, 3))))
})

test_that("simpopmlx returns an error when pop.param or sd are not defined in parameter", {
  expect_error(simpopmlx(parameter = data.frame(trans = c("N", "N"), pop.param = c(5, 6))))
  expect_error(simpopmlx(parameter = data.frame(trans = c("N", "N"), sd = c(5, 6))))
})

test_that("simpopmlx returns an error when trans are not in N L G R P", {
  expect_error(simpopmlx(parameter = data.frame(trans = c(1, "N"), pop.param = c(5, 6), sd = c(2, 3))))
  expect_error(simpopmlx(parameter = data.frame(trans = c("S", "N"), sd = c(5, 6))))
})

test_that("simpopmlx returns an error when lim.a & lim.b are defined and lim.a >= lim.b", {
  expect_error(simpopmlx(parameter = data.frame(lim.a = c(0, 1), lim.b = c(1, 0),
                                                trans = c("G", "G"), pop.param = c(5, 6), sd = c(2, 3))))
})

test_that("simpopmlx returns an error when lim.a & lim.b are defined for trans != G", {
  expect_error(simpopmlx(parameter = data.frame(lim.a = c(0, 1), lim.b = c(1, 0),
                                                trans = c("N", "G"), pop.param = c(5, 6), sd = c(2, 3))))
})

test_that("simpopmlx returns an error when correlation matrix is not in -1 1", {
  corr <- diag(2)
  corr[1, 2] <- 1.5
  expect_error(simpopmlx(parameter = data.frame(pop.param = c(5, 6), sd = c(2, 3)),
                         corr = corr))
})

test_that("simpopmlx returns an error when invalid sep (not in '\t', ',', ';', ' ')", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  expect_error(simpopmlx(project, outputFilename = "/tmp/output.csv", sep = "toto"))
  expect_error(simpopmlx(project, outputFilename = "/tmp/output.csv", sep = 1))
  expect_error(simpopmlx(project, outputFilename = "/tmp/output.csv", sep = ":"))
})

test_that("simpopmlx returns an error when outputFilename is specified with the wrong extension", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  expect_error(simpopmlx(project=project, outputFilename = "/tmp/output.docx"))
  expect_error(simpopmlx(project=project, outputFilename = "/tmp/output.smlx"))
})

# Check outputs ----------------------------------------------------------------
test_that("If only one pop, no column pop in output dataframe", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res <- simpopmlx(project=project, n = 1)
  expect_false("pop" %in% names(res))
  res <- simpopmlx(project=project, n = 50)
  expect_true("pop" %in% names(res))
})

test_that("If n pop, output is a dataframe with n rows and a column pop", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res <- simpopmlx(project=project, n = 2)
  expect_equal(nrow(res), 2)
  res <- simpopmlx(project=project, n = 1)
  expect_equal(nrow(res), 1)
  res <- simpopmlx(project=project, n = 100)
  expect_equal(nrow(res), 100)
})

test_that("pop column is a factor", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res <- simpopmlx(project=project, n = 50)
  expect_true(is.factor(res$pop))
  
})

test_that("If outputFilename specified, results is saved in a file with correct sep", {
  if (file.exists("/tmp/output.csv")) file.remove("/tmp/output.csv")
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res <- simpopmlx(project=project, outputFilename = "/tmp/output.csv", sep = ";")
  expect_true(file.exists("/tmp/output.csv"))
  expect_true(.getDelimiter("/tmp/output.csv") == ";")
})

test_that("If outputFilename specified, results is saved in a file and file content is equal to output", {
  if (file.exists("/tmp/output.csv")) file.remove("/tmp/output.csv")
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res1 <- simpopmlx(project=project, outputFilename = "/tmp/output.csv", sep = ";")
  res2 <- utils::read.csv("/tmp/output.csv", sep=";")
  expect_equal(res1, res2)
})

test_that("If no seed specified, 2 iterations of the function give different results", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res1 <- simpopmlx(project=project)
  res2 <- simpopmlx(project=project)
  expect_false(isTRUE(all.equal(res1, res2)))
})

test_that("If two seeds specified, 2 iterations of the function give different results", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res1 <- simpopmlx(project=project, seed = 123)
  res2 <- simpopmlx(project=project, seed = 1234)
  expect_false(isTRUE(all.equal(res1, res2)))
})


test_that("If one seed specified, 2 iterations of the function give the same results", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  res1 <- simpopmlx(project=project, seed = 123)
  res2 <- simpopmlx(project=project, seed = 123)
  expect_equal(res1, res2)
})


test_that("n = 1", {
  project <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  expect_error(simpopmlx(project=project, n = 10), NA)
  expect_error(simpopmlx(project=project, n = 1), NA)
})
