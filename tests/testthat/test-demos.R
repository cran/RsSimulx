context("Retrocompatibility with mlxR")

skip_on_cran()
skip_if_offline()
skip_if_not_installed("lixoftConnectors")

# test_that("mlxR Case studies and demos run with no fail", {
#   # download demos
#   demos_url <- "http://simulx.webpopix.org/simulx/mlxR410_demos.zip"
#   temp <- paste0(tempfile(), ".zip")
#   download.file(demos_url, temp)
#   unzip(temp, exdir = dirname(temp))
#   demoPaths <- c("userGuide_simulx", "userGuide_mlxR", "caseStudies")
#   demoPaths <- sapply(demoPaths, function(p) file.path(dirname(temp), p))
#   files <- c()
#   for (p in demoPaths) files <- c(files, list.files(path = p , pattern = '[.]R', full.names = T, include.dirs = F))
#   demos <- files
#   wd <- getwd()
#   for (demo in demos[54:74]) {
#     setwd(dirname(demo))
#     print(demo)
#     tryCatch(
#       eval(parse(demo)),
#       error = function(e) {
#         print(e)
#       }
#     )
#     # expect_error(eval(parse(demo)), NA)
#     graphics.off()
#     setwd(wd)
#   }
# })
