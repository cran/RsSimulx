context("Write data")

skip_on_cran()
skip_if_not_installed("lixoftConnectors")
skip_if(!dir.exists(test_path("demos/")) | length(list.files(test_path("demos/"))) == 0, message = NULL)

get_project <- function(project) {
  .loadProject(project, software = "simulx")
  tmpProject <- file.path(tempdir(), basename(project))
  smlx.saveProject(projectFile = tmpProject)
  return(tmpProject)
}

test_that("writeData runs on simulx demo with no fail", {
  demo_path <- file.path(path.expand("~"), "lixoft", "simulx", paste0("simulx", smlx.getLixoftConnectorsState()$version), "demos")
  files <- list.files(path = demo_path , pattern = '[.]smlx$', full.names = T, include.dirs = F, recursive = T)

  for (f in files) {
    print(f)
    project <- get_project(f)
    possibleError <- tryCatch(
      smlx.getSimulationResults(),
      error=function(e) e
    )
    if(inherits(possibleError, "error")) next

    expect_error(suppressMessages(writeData(project = project), classes = "message"), NA)
    expect_error(suppressMessages(writeData(project = project, outputDirectory = "/tmp"), classes = "message"), NA)
    expect_error(suppressMessages(writeData(project = project, sep = ","), classes = "message"), NA)
    expect_error(suppressMessages(writeData(project = project, sep = ";", ext = "csv"), classes = "message"), NA)
  }
})