# context("Sim pop")
# 
# skip_on_cran()
# skip_if_not_installed("lixoftConnectors")
# 
# # test_that("simpopmlx returns an error when parameter & project defined together") {
# #   expect_error()
# # }
# # test_that("simpopmlx returns an error when project & correlation defined together")
# 
# test_that("simpopmlx returns an error when neither parameter nor project are defined", {
#   
# })
# test_that("simpopmlx returns an error when parameter is not a dataframe", {
#   expect_error(simpopmlx(parameter = 2))
#   expect_error(simpopmlx(parameter = c(1, 2, 3)))
# })
# test_that("simpopmlx returns an error when wrong columns defined in parameter", {
#   expect_error(simpopmlx(parameter = data.frame(sd = c(12, 2), pop.param = c(5, 6), toto = c(3, 3))))
# })
# test_that("simpopmlx returns an error when pop.param or sd are not defined in parameter", {
#   expect_error(simpopmlx(parameter = data.frame(trans = c("N", "N"), pop.param = c(5, 6))))
#   expect_error(simpopmlx(parameter = data.frame(trans = c("N", "N"), sd = c(5, 6))))
# })
# test_that("simpopmlx returns an error when trans are not in N L G R P", {
#   expect_error(simpopmlx(parameter = data.frame(trans = c(1, "N"), pop.param = c(5, 6), sd = c(2, 3))))
#   expect_error(simpopmlx(parameter = data.frame(trans = c("S", "N"), sd = c(5, 6))))
# })
# test_that("simpopmlx returns an error when lim.a & lim.b are defined and lim.a >= lim.b", {
#   expect_error(simpopmlx(parameter = data.frame(trans = c(1, "N"), pop.param = c(5, 6), sd = c(2, 3))))
#   expect_error(simpopmlx(parameter = data.frame(trans = c("S", "N"), sd = c(5, 6))))
# })
# test_that("simpopmlx returns an error when lim.a & lim.b are defined and lim.a >= lim.b")
# test_that("simpopmlx returns an error when lim.a & lim.b are defined for trans != G")
# test_that("simpopmlx returns an error when kw.max is not a strictly positive integer")
# test_that("if no trans defined trans is set to N")
# test_that("if no lim.a or lim.b defined for trans G there are set to 0, 1")
# test_that("simpopmlx returns an error when correlation matrix is not in -1 1")
# test_that("simpopmlx returns a dataframe with nrow = input parameter n")
# 
# unction(n = 1, project = NULL, fim = NULL, parameter = NULL,
#         corr = NULL, kw.max = 100, outputFilename = NULL, sep = ",")
# 
# test_that("writeData runs on simulx demo with no fail", {
#   demo_path <- file.path(path.expand("~"), "lixoft", "simulx", paste0("simulx", smlx.getLixoftConnectorsState()$version), "demos")
#   files <- list.files(path = demo_path , pattern = '[.]smlx$', full.names = T, include.dirs = F, recursive = T)
# 
#   for (f in files) {
#     print(f)
#     project <- get_project(f)
#     possibleError <- tryCatch(
#       smlx.getSimulationResults(),
#       error=function(e) e
#     )
#     if(inherits(possibleError, "error")) next
# 
#     expect_error(suppressMessages(writeData(project = project), classes = "message"), NA)
#     expect_error(suppressMessages(writeData(project = project, outputDirectory = "/tmp"), classes = "message"), NA)
#     expect_error(suppressMessages(writeData(project = project, sep = ","), classes = "message"), NA)
#     expect_error(suppressMessages(writeData(project = project, sep = ";", ext = "csv"), classes = "message"), NA)
#   }
# })