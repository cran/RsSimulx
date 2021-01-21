.hiddenCall <- function(command){
  eval.parent(parse(text = command))
}

smlx.getLixoftConnectorsState <- function(quietly = TRUE) {
  r <- NULL
  .hiddenCall(paste0('r <- lixoftConnectors::getLixoftConnectorsState(quietly = ',quietly,')'))
  return(r)
}

smlx.initializeLixoftConnectors <- function(software = "simulx", path = NULL) {
  r <- NULL
  if (!is.null(path)){
    .hiddenCall(paste0('r <- lixoftConnectors::initializeLixoftConnectors(software = software, force = TRUE, path = path)'))
  } else{
    .hiddenCall(paste0('r <- lixoftConnectors::initializeLixoftConnectors(software = software, force = TRUE)'))
  }
  return(r)
}

smlx.loadProject <- function(projectFile=NULL) {
  .hiddenCall(paste0('r <- lixoftConnectors::loadProject(projectFile = "',projectFile,'")'))
}

smlx.saveProject <- function(projectFile = "") {
  .hiddenCall(paste0('r <- lixoftConnectors::saveProject(projectFile = "',projectFile,'")'))
}

smlx.newProject <-  function(modelFile = NULL, data = NULL) {
  .hiddenCall(paste0('r <- lixoftConnectors::newProject(modelFile = modelFile, data = data)'))
}

smlx.getProjectSettings <-  function(data) {
  .hiddenCall(paste0('r <- lixoftConnectors::getProjectSettings()'))
}

smlx.importMonolixProject <- function(projectFile) {
  .hiddenCall(paste0('r <- lixoftConnectors::importMonolixProject(projectFile = projectFile)'))
}

smlx.runSimulation <-  function() {
  .hiddenCall(paste0('r <- lixoftConnectors::runSimulation()'))
}

smlx.getGroups <-  function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getGroups()'))
}

smlx.getTreatmentElements <-  function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getTreatmentElements()'))
}

smlx.getSimulationResults <-  function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getSimulationResults()'))
}

smlx.getCovariateElements <-  function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getCovariateElements()'))
}

smlx.getNbReplicates <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getNbReplicates()'))
}

smlx.getOccasionElements <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getOccasionElements()'))
}

smlx.getRegressorElements <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getRegressorElements()'))
}

smlx.getGroupRemaining <- function(group) {
  .hiddenCall(paste0('r <- lixoftConnectors::getGroupRemaining(group = group)'))
}

smlx.getPopulationElements <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getPopulationElements()'))
}

smlx.getIndividualElements <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getIndividualElements()'))
}

smlx.getOutputElements <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getOutputElements()'))
}

smlx.setGroupElement <- function(group, elements) {
  .hiddenCall(paste0('r <- lixoftConnectors::setGroupElement(group = group, elements = elements)'))
}

smlx.setGroupRemaining <- function(group, remaining) {
  .hiddenCall(paste0('r <- lixoftConnectors::setGroupRemaining(group = group, remaining = remaining)'))
}

smlx.setGroupSize <- function(group, size) {
  .hiddenCall(paste0('r <- lixoftConnectors::setGroupSize(group = group, size = size)'))
}

smlx.setAddLines <- function(lines) {
  .hiddenCall(paste0('r <- lixoftConnectors::setAddLines(lines = lines)'))
}

smlx.setNbReplicates <- function(nb) {
  .hiddenCall(paste0('r <- lixoftConnectors::setNbReplicates(nb = nb)'))
}

smlx.setSamplingMethod <- function(method) {
  .hiddenCall(paste0('r <- lixoftConnectors::setSamplingMethod(method = method)'))
}

smlx.setProjectSettings <- function(...) {
  .hiddenCall(paste0('r <- lixoftConnectors::setProjectSettings(...)'))
}

smlx.defineOutputElement <- function(name, element) {
  .hiddenCall(paste0('r <- lixoftConnectors::defineOutputElement(name = name, element = element)'))
}

smlx.defineCovariateElement <- function(name, element) {
  .hiddenCall(paste0('r <- lixoftConnectors::defineCovariateElement(name = name, element = element)'))
}

smlx.definePopulationElement <- function(name, element) {
  .hiddenCall(paste0('r <- lixoftConnectors::definePopulationElement(name = name, element = element)'))
}

smlx.defineIndividualElement <- function(name, element) {
  .hiddenCall(paste0('r <- lixoftConnectors::defineIndividualElement(name = name, element = element)'))
}

smlx.defineTreatmentElement <- function(name, element) {
  .hiddenCall(paste0('r <- lixoftConnectors::defineTreatmentElement(name = name, element = element)'))
}

smlx.defineRegressorElement <- function(name, element) {
  .hiddenCall(paste0('r <- lixoftConnectors::defineRegressorElement(name = name, element = element)'))
}

smlx.defineOccasionElement <- function(element) {
  .hiddenCall(paste0('r <- lixoftConnectors::defineOccasionElement(element = element)'))
}

smlx.addGroup <- function(group) {
  .hiddenCall(paste0('r <- lixoftConnectors::addGroup(group = group)'))
}

smlx.renameGroup <- function(currentGroupName, newGroupName) {
  .hiddenCall(paste0('r <- lixoftConnectors::renameGroup(currentGroupName = currentGroupName, newGroupName = newGroupName)'))
}

mlx.getData <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getData()'))
}

mlx.getPopulationParameterInformation <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getPopulationParameterInformation()'))
}

mlx.getEstimatedPopulationParameters <- function(...) {
  .hiddenCall(paste0('r <- lixoftConnectors::getEstimatedPopulationParameters(...)'))
}

mlx.getEstimatedStandardErrors <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getEstimatedStandardErrors()'))
}

mlx.getIndividualParameterModel <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getIndividualParameterModel()'))
}

mlx.getContinuousObservationModel <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getContinuousObservationModel()'))
}

mlx.runScenario <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::runScenario()'))
}

mlx.getLaunchedTasks <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getLaunchedTasks()'))
}

mlx.getCovariateInformation <- function() {
  .hiddenCall(paste0('r <- lixoftConnectors::getCovariateInformation()'))
}

mlx.saveProject <- function(projectFile = "") {
  .hiddenCall(paste0('r <- lixoftConnectors::saveProject(projectFile = "',projectFile,'")'))
}