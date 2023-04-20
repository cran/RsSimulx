skip_on_cran()
skip_if_not_installed("lixoftConnectors")

skip("Takes a long time")

initRsSimulx()

get_project <- function(project) {
  .loadProject(project, software = "simulx")
  tmpProject <- file.path(tempdir(), basename(project))
  .lixoftCall("saveProject", list(projectFile = tmpProject))
  return(tmpProject)
}
demo_path <- file.path(path.expand("~"), "lixoft", "simulx",
                           paste0("simulx", .lixoftCall("getLixoftConnectorsState")$version), "demos")
mlx_demo_path <- file.path(path.expand("~"), "lixoft", "monolix",
                           paste0("monolix", .lixoftCall("getLixoftConnectorsState")$version), "demos")

skip_if(!dir.exists(demo_path), message = NULL)

test_that("writeData runs on simulx demo with no fail", {
  files <- list.files(path = demo_path , pattern = '[.]smlx$', full.names = T, include.dirs = F, recursive = T)

  for (f in files) {
    print(f)
    project <- get_project(f)
    possibleError <- tryCatch(
      .lixoftCall("getSimulationResults"),
      error=function(e) e
    )
    if(inherits(possibleError, "error")) {
      next
    }

    if (is.null(.lixoftCall("getSimulationResults"))) {
      .lixoftCall("runSimulation")
    }

    # skip when no result computed
    if (is.null(.lixoftCall("getSimulationResults")$res)) {
      next
    }

    # skip project with regressors
    regfile <- file.path(.lixoftCall("getProjectSettings")$directory, "Simulation", "regressors.txt")
    if (file.exists(regfile)) {
      next
    }

    expect_error(suppressMessages(writeData(project = project, filename = "/tmp/data.csv"), classes = "message"), NA)
  }
})

# Test that arguments are well checked -----------------------------------------
# test_that("Writedata returns an error when no project specified and no project loaded", {
#
# })
#
# test_that("Writedata consider loaded project when no project specified", {
#
# })

test_that("Writedata returns an error when invalid project", {
  # project does not exist
  expect_error(writeData("thisprojectdoesnotexist.smlx", filename = "/tmp/output"))
  # project is no a simulx project
  mlxproject <- file.path(mlx_demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")

  expect_error(writeData(mlxproject, filename = "/tmp/output"))
})

test_that("Function stops when invalid sep (not in '\t', ',', ';', ' ')", {
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  expect_error(writeData(project, filename = "/tmp/output", sep = "toto"))
  expect_error(writeData(project, filename = "/tmp/output", sep = 1))
  expect_error(writeData(project, filename = "/tmp/output", sep = ":"))
})

test_that("Function stops when invalid ext (not in 'csv' or 'txt')", {
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  expect_error(writeData(project, filename = "/tmp/output", ext = "xml"))
  expect_error(writeData(project, filename = "/tmp/output", ext = "docx"))
  expect_error(writeData(project, filename = "/tmp/output", ext = "smlx"))
})

test_that("Function stops when nbdigits is not an integer", {
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  expect_error(writeData(project, filename = "/tmp/output", nbdigits = "toto"))
  expect_error(writeData(project, filename = "/tmp/output", nbdigits = -3))
  expect_error(writeData(project, filename = "/tmp/output", nbdigits = 1.2))
})

test_that("Function stops when filename is specified with the wrong extension", {
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  expect_error(writeData(project, filename = "/tmp/output.docx"))
  expect_error(writeData(project, filename = "/tmp/output.smlx"))
})

# Test outputs -----------------------------------------------------------------
test_that("It is posisble to create a pkanalix project with the output data file", {
  if (file.exists("/tmp/output.txt")) file.remove("/tmp/output.txt")
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)

  res <- writeData(project, filename = "/tmp/output.txt", sep = ",")
  .lixoftCall("initializeLixoftConnectors", list(software = "pkanalix", force=TRUE))
  expect_error(.lixoftCall(
    "newProject", list(data = list(
    dataFile = "/tmp/output.txt",
    headerTypes = c("id", "time", "observation", "ignore", "amount", "tinf", "admid"),
    observationTypes = "continuous"))),
  NA
  )
  suppressMessages(initRsSimulx(force=TRUE))

})

test_that("Output file is written with the correct input filename", {
  if (file.exists("/tmp/output.txt")) file.remove("/tmp/output.txt")
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  res <- writeData(project, filename = "/tmp/output.txt")
  expect_true(file.exists("/tmp/output.txt"))
})

test_that("dataframe outputed by the function and files saved by the function are equal", {
  if (file.exists("/tmp/output.txt")) file.remove("/tmp/output.txt")
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)

  res1 <- writeData(project, filename = "/tmp/output.txt", sep = ",")
  res2 <- read.csv("/tmp/output.txt", sep = ",")

  expect_equal(res1, res2)
})

test_that("Output file as the correct extension", {
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  if (file.exists("/tmp/output.txt")) file.remove("/tmp/output.txt")
  res <- writeData(project, filename = "/tmp/output", ext = "txt")
  expect_true(file.exists("/tmp/output.txt"))
  if (file.exists("/tmp/output.csv")) file.remove("/tmp/output.csv")
  res <- writeData(project, filename = "/tmp/output", ext = "csv")
  expect_true(file.exists("/tmp/output.csv"))

})

test_that("Output file as the correct separator", {
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  res <- writeData(project, filename = "/tmp/output.txt", sep = ";")
  expect_equal(.getDelimiter("/tmp/output.txt"), ";")

  res <- writeData(project, filename = "/tmp/output.csv", sep = ";")
  expect_equal(.getDelimiter("/tmp/output.csv"), ";")

  res <- writeData(project, filename = "/tmp/output.txt", sep = ",")
  expect_equal(.getDelimiter("/tmp/output.txt"), ",")

  res <- writeData(project, filename = "/tmp/output.txt", sep = "\t")
  expect_equal(.getDelimiter("/tmp/output.txt"), "\t")

  res <- writeData(project, filename = "/tmp/output.txt", sep = " ")
  expect_equal(.getDelimiter("/tmp/output.txt"), " ")
})

test_that("When only one replicate, one file is saved and function output a dataframe", {
  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  if (file.exists("/tmp/output.txt")) file.remove("/tmp/output.txt")
  res <- writeData(project, filename = "/tmp/output.txt")

  expect_true(is.data.frame(res))
  expect_true(file.exists("/tmp/output.txt"))
})

test_that("When 2 replicates, 2 files are saved and function output a list of 2 dataframes", {
  if (file.exists("/tmp/output_rep1.txt")) file.remove("/tmp/output_rep1.txt")
  if (file.exists("/tmp/output_rep2.txt")) file.remove("/tmp/output_rep2.txt")

  project <- file.path(mlx_demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  simulx(project = project, nrep = 2)
  .lixoftCall("saveProject", list("/tmp/smlx_project.smlx"))
  res <- writeData("/tmp/smlx_project.smlx", filename = "/tmp/output.txt")
  expect_false(is.data.frame(res))
  expect_true(is.list(res))
  for (rep in res) expect_true(is.data.frame(rep))
  expect_true(file.exists("/tmp/output_rep1.txt"))
  expect_true(file.exists("/tmp/output_rep2.txt"))
})

test_that("Output file as the correct number of digits", {
  decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }

  file <- file.path(demo_path, "3.definition", "3.1.treatments", "treatment_bolus_infusion_oral.smlx")
  project <- get_project(file)
  res1 <- writeData(project, filename = "/tmp/output.txt", nbdigits = 5)
  res2 <- writeData(project, filename = "/tmp/output.txt", nbdigits = 2)
  res3 <- writeData(project, filename = "/tmp/output.txt", nbdigits = 6)

  maxdigit1 <- 0
  maxdigit2 <- 0
  maxdigit3 <- 0
  colnames <- names(res1)
  for (col in names(res1)) {
    max1 <- max(sapply(res1[[col]], function(d) suppressWarnings(ifelse(is.na(as.double(d)), 0, decimalplaces(as.double(d))))))
    max2 <- max(sapply(res2[[col]], function(d) suppressWarnings(ifelse(is.na(as.double(d)), 0, decimalplaces(as.double(d))))))
    max3 <- max(sapply(res3[[col]], function(d) suppressWarnings(ifelse(is.na(as.double(d)), 0, decimalplaces(as.double(d))))))
    expect_true(max1 <= 5)
    expect_true(max2 <= 2)
    expect_true(max3 <= 6)
    maxdigit1 <- max(maxdigit1, max1)
    maxdigit2 <- max(maxdigit2, max2)
    maxdigit3 <- max(maxdigit3, max3)
  }

  expect_equal(maxdigit1, 5)
  expect_equal(maxdigit2, 2)
  expect_equal(maxdigit3, 6)
})

test_that("Simulations with occasions return a dataframe with occ colums", {
  file <- file.path(demo_path, "3.definition", "3.7.occasions", "occasions_common.smlx")
  project <- get_project(file)
  res <- writeData(project, filename = "/tmp/output.txt")
  expect_true("occ" %in% names(res))

  file <- file.path(demo_path, "3.definition", "3.7.occasions", "occasions_external.smlx")
  project <- get_project(file)
  res <- writeData(project, filename = "/tmp/output.txt")
  expect_true("OCC" %in% names(res))

  file <- file.path(demo_path, "3.definition", "3.7.occasions", "occasions_two_levels.smlx")
  project <- get_project(file)
  res <- writeData(project, filename = "/tmp/output.txt")
  expect_true("occ1" %in% names(res))
  expect_true("occ2" %in% names(res))
})

test_that("Id columns is ordered by numerical value", {
  file <- file.path(demo_path, "3.definition", "3.7.occasions", "occasions_common.smlx")
  project <- get_project(file)
  res <- writeData(project, filename = "/tmp/output.txt")
  expect_equal(sort(res$id), res$id)
})

test_that("When several groups, group column in output dataframe", {
  file <- file.path(demo_path, "5.simulation", "simulationGroups_ethnicGroups.smlx")
  project <- get_project(file)
  res <- writeData(project, filename = "/tmp/output.txt")
  expect_true("group" %in% names(res))
  expect_equal(length(unique(res$group)), 2)
})