# helper functions for tests, will automatically be sourced by testthat before tests

get_project <- function(project, runPopEstimationIfNeed = TRUE, runStdErrorsIfNeed = FALSE) {
  .loadProject(project, software = "monolix")
  tmpProject <- file.path(tempdir(), basename(project))
  .lixoftCall("saveProject", list(projectFile = tmpProject))
  
  if (runPopEstimationIfNeed | runStdErrorsIfNeed) {
    if (! .lixoftCall("getLaunchedTasks")[["populationParameterEstimation"]] | 
        ! .lixoftCall("getLaunchedTasks")[["standardErrorEstimation"]]) {
      if (runStdErrorsIfNeed &
          ! .lixoftCall("getScenario")[["tasks"]][["standardErrorEstimation"]]) {
        scenario = .lixoftCall("getScenario")
        scenario$tasks["standardErrorEstimation"] = TRUE
        .lixoftCall("setScenario", scenario)
      }
      .lixoftCall("runScenario")
    } 
  }
  
  return(tmpProject)
}