context("Simulx")

# skip_on_cran()
skip_if_not_installed("lixoftConnectors")

initRsSimulx()
demo_path <- file.path(path.expand("~"), "lixoft", "monolix",
                       paste0("monolix", .lixoftCall("getLixoftConnectorsState")$version), "demos")
skip_if(!dir.exists(demo_path), message = NULL)

get_project <- function(project) {
  .loadProject(project, software = "monolix")
  tmpProject <- file.path(tempdir(), basename(project))
  .lixoftCall("saveProject", list(projectFile = tmpProject))
  return(tmpProject)
}

test_that("simulxR runs on simulx demo with no fail", {
  files <- list.files(path = demo_path , pattern = '[.]mlxtran$', full.names = T, include.dirs = F, recursive = T)

  for (f in files) {
    print(f)
    project <- get_project(f)
    
    if (! .lixoftCall("getLaunchedTasks")[["populationParameterEstimation"]]) {
      .lixoftCall("runPopulationParameterEstimation")
    }
    initRsSimulx(force = TRUE)
    expect_error(simulx(project = project), NA)
  }
})

# test_that("Case studies and demos run with no fail", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   expect_error(simulx(project = project), NA)
#   expect_error(simulx(project = project, 
#                       output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1))),
#                NA)
#   expect_error(simulx(project = project, 
#                       output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1)),
#                       group = list(size = 5)),
#                NA)
#   expect_error(simulx(project = project, 
#                       output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1)),
#                       treatment =  list(amount = 10, time = seq(from = 0, to = 5, by = 1)),
#                       group = list(size = 5)),
#                NA)
#   g1 = list(size = 5, treatment = list(amount = 10, time = 0), parameter = c(omega_V = 0, omega_ka = 0, omega_Cl = 0))
#   g2 = list(size = 4, treatment = list(amount = 20, time = 2))
#   expect_error(suppressWarnings(simulx(project = project, 
#                       output = list(name = 'Cc', time = seq(from = 1, to = 25, by = 12)),
#                       group = list(g1,g2)),
#                NA))
# })
# 
# # Test that arguments are well checked -----------------------------------------
# test_that("Function stops when Invalid project", {
#   expect_error(simulx(project = "thisprojectdoesnorexist.mlxtran"))
#   smlxpath <- file.path(path.expand("~"), "lixoft", "simulx",
#                         paste0("simulx", .lixoftCall("getLixoftConnectorsState")$version), "demos")
#   smlxproject <- file.path(smlxpath, "/2.models/longitudinal.smlx")
#   expect_error(simulx(project = smlxproject))
# })
# 
# # test_that("Function stops when Invalid model", {
# #   
# # })
# 
# test_that("Function stops when a project and a modele are defined together", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm <- list(time=seq(0,to=66,by=6), amount=50)
#   expect_error(simulx(model = pk.model, parameter = p, output = C, treatment = adm, project = project))
#   
# })
# 
# test_that("Function stops when neither project nor modele is defined", {
#   expect_error(simulx())
# })
# 
# test_that("Function stops when npop and nrep are not integers", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   expect_error(simulx(project = project, nrep = 1.2))
#   expect_error(simulx(project = project, nrep = -3))
#   expect_error(simulx(project = project, nrep = "toto"))
#   expect_error(simulx(project = project, npop = 1.2))
#   expect_error(simulx(project = project, npop = -3))
#   expect_error(simulx(project = project, npop = "toto"))
#   
# }) 
# 
# test_that("Deprecated arguments print message", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   expect_message(simulx(project = project, stat.f = "mean"))
#   expect_message(simulx(project = project, result.folder = "/tmp"))
#   expect_message(simulx(project = project, data = TRUE))
# })
# 
# test_that("Models for which distribution of covariates is defined returns an error", {
#   myModel <- inlineModel("
# [COVARIATE]
# input={p_F}
# 
# DEFINITION:
# gender = { type=categorical, 
#            categories={F,M},
#            P(gender=F)=p_F }
# 
# ;---------------------------------------
# [INDIVIDUAL]
# input={k_pop, omega_k, gender, beta_F}
# gender={type=categorical,categories={F,M}}
# 
# DEFINITION:
# k = { distribution=lognormal,
#       reference=k_pop,
#       covariate=gender,
#       coefficient={beta_F,0},
#       sd=omega_k }
# 
# ;---------------------------------------
# [LONGITUDINAL]
# input =  {k}
# 
# EQUATION:
# f = exp(-k*t)
#   ")
#   p <- c(p_F=0.4,k_pop=0.2, omega_k=0.1, beta_F=0.6)
#   f <- list(name='f', time=seq(0, 30, by=0.1))
#   ind <- list(name=c("gender","k"))
#   
#   expect_error(simulx(model=myModel, parameter=p, output=list(ind, f)))
#   
#   model <- inlineModel(
#     "[LONGITUDINAL]
# input = {V, k, a}
# 
# EQUATION:
# f = 100/V*exp(-k*t)
# 
# DEFINITION:
# y = {distribution = normal, prediction = f, sd = a}
# 
# ;----------------------------------------------
# [INDIVIDUAL]
# input = {V_pop, omega_V, w, w_pop}
# 
# EQUATION:
# V_pred = V_pop*(w/w_pop)
# 
# DEFINITION:
# V = {distribution = lognormal, prediction = V_pred, sd = omega_V}
# 
# ;----------------------------------------------
# [COVARIATE]
# input = {w_pop, omega_w}
# 
# DEFINITION:
# w = {distribution = normal, mean = w_pop, sd = omega_w}"
#   )
#   
#   p <- c(V_pop=10, omega_V=0.1, beta=1, w_pop=70, omega_w=12, k=0.15, a=0.5)
#   
#   f   <- list(name='f', time=seq(0, 30, by=0.1))
#   y   <- list(name='y', time=seq(1, 30, by=3))
#   ind <- list(name=c('w','V'))
#   out <- list(ind, f, y)
#   expect_error(simulx(model=model, parameter=p, output=out))
#   
# })
# 
# # Test settings ----------------------------------------------------------------
# test_that("When no seed specified: 2 simulations gives different results", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res1 <- simulx(project = project)
#   res2 <- simulx(project = project)
#   expect_equal(names(res1), names(res2))
#   expect_false(isTRUE(all.equal(res1, res2)))
# })
# 
# test_that("When a seed is specified: 2 simulations gives same results", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res1 <- simulx(project = project, settings = list(seed = 123))
#   res2 <- simulx(project = project, settings = list(seed = 123))
#   res3 <- simulx(project = project, npop = 2, settings = list(seed = 123))
#   res4 <- simulx(project = project, npop = 2, settings = list(seed = 123))
#   expect_equal(names(res1), names(res2))
#   expect_equal(names(res3), names(res4))
#   expect_equal(res1, res2)
#   expect_equal(res3, res4)
# })
# 
# test_that("When id.out is FALSE, when only one group, group columns are removed", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm1 <- list(time=seq(0,to=66,by=6), amount=50)
#   adm2 <- list(time=seq(0,to=66,by=12), amount=100)
#   adm3 <- list(time=seq(0,to=66,by=18), amount=150)
#   g1 <- list(treatment=adm1);
#   g2 <- list(treatment=adm2);
#   g3 <- list(treatment=adm3);
#   res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = FALSE))
#   res2 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1, g2, g3), settings = list(id.out = FALSE))
#   
#   for (n in setdiff(names(res1), "parameter")) {
#     expect_false("group" %in% names(res1[[n]]))
#     expect_true("group" %in% names(res2[[n]]))
#   }
# })
# 
# test_that("When id.out is FALSE, when only one id, id columns are removed", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm1 <- list(time=seq(0,to=66,by=6), amount=50)
#   adm21 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
#   adm22 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
#   adm2 <- rbind(adm21, adm22)
#   res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = FALSE))
#   res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm2, settings = list(id.out = FALSE))
#   
#   for (n in setdiff(names(res1), "parameter")) {
#     expect_false("id" %in% names(res1[[n]]))
#     expect_true("id" %in% names(res2[[n]]))
#   }
# })
# 
# 
# test_that("When id.out is TRUE, when only one id, id columns are removed", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm1 <- list(time=seq(0,to=66,by=6), amount=50)
#   adm21 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
#   adm22 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
#   adm2 <- rbind(adm21, adm22)
#   res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = TRUE))
#   res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm2, settings = list(id.out = TRUE))
#   
#   for (n in setdiff(names(res1), "parameter")) {
#     expect_true("id" %in% names(res1[[n]]))
#     expect_true("id" %in% names(res2[[n]]))
#   }
# })
# 
# test_that("When out.trt is FALSE, treatment is not included in ourput", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm <- list(time=seq(0,to=66,by=6), amount=50)
#   res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm, settings = list(out.trt = TRUE))
#   res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm, settings = list(out.trt = FALSE))
#   
#   expect_true("treatment" %in% names(res1))
#   expect_false("treatment" %in% names(res2))
# })
# 
# test_that("When replacement is FALSE, simulx sampling method is keepOrder", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res <- simulx(project = project)
#   expect_equal(lixoftConnectors::getSamplingMethod(), "keepOrder")
#   
#   res <- simulx(project = project, settings = list(replacement = FALSE))
#   expect_equal(lixoftConnectors::getSamplingMethod(), "keepOrder")
# })
# 
# test_that("When replacement is TRUE, simulx sampling method is withReplacement", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res <- simulx(project = project, settings = list(replacement = TRUE))
#   expect_equal(lixoftConnectors::getSamplingMethod(), "withReplacement")
# })
# 
# # Test Outputs -----------------------------------------------------------------
# test_that("cens, pop, rep, id, group columns are factors", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1), lloq = 0.8)
#   p <- c(V=10, k=0.2)
#   adm1 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
#   adm2 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
#   adm <- rbind(adm1, adm2)
#   g1 <- list(treatment=as.list(adm1[names(adm1) != "id"]))
#   g2 <- list(treatment=as.list(adm2[names(adm2) != "id"]))
#   res <- simulx(model = pk.model, parameter = p, output = C, treatment = adm, group = list(g1, g2), nrep = 3, npop = 5)
# 
#   for (n in names(res)) {
#     for (col in intersect(names(res[[n]]), c("cens", "pop", "rep", "id", "group"))) {
#       expect_true(is.factor(res[[n]][[col]]))
#     }
#   }
# })
# 
# test_that("Output results contains the correct number of ids", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm1 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
#   adm2 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
#   adm <- rbind(adm1, adm2)
#   res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm)
#   res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = TRUE))
#   for (n in names(res1)) {
#     if ("id" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$id)) == 2)
#     if ("id" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$id)) == 1)
#   }
# })
# 
# test_that("Output results contains the correct number of groups", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm1 <- list(time=seq(0,to=66,by=6), amount=50)
#   adm2 <- list(time=seq(0,to=66,by=12), amount=100)
#   adm3 <- list(time=seq(0,to=66,by=18), amount=150)
#   g1 <- list(treatment=adm1);
#   g2 <- list(treatment=adm2);
#   g3 <- list(treatment=adm3);
#   res0 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = TRUE))
#   res1 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1), settings = list(id.out = TRUE))
#   res2 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1, g2), settings = list(id.out = TRUE))
#   res3 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1, g2, g3), settings = list(id.out = TRUE))
#   
#   for (n in names(res1)) {
#     if ("group" %in% names(res0[[n]])) expect_true(length(unique(res0[[n]]$group)) == 1)
#     if ("group" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$group)) == 1)
#     if ("group" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$group)) == 2)
#     if ("group" %in% names(res3[[n]])) expect_true(length(unique(res3[[n]]$group)) == 3)
#   }
# })
# 
# test_that("Output results contains the correct number of rep", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res0 <- simulx(project = project)
#   res1 <- simulx(project = project, nrep = 1)
#   res2 <- simulx(project = project, nrep = 5)
#   res3 <- simulx(project = project, nrep = 42)
#   expect_false("rep" %in% names(res0$CONC))
#   expect_false("rep" %in% names(res1$CONC))
#   expect_true("rep" %in% names(res2$CONC))
#   expect_true("rep" %in% names(res3$CONC))
#   for (n in names(res1)) {
#     if ("rep" %in% names(res0[[n]])) expect_true(length(unique(res0[[n]]$rep)) == 1)
#     if ("rep" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$rep)) == 1)
#     if ("rep" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$rep)) == 5)
#     if ("rep" %in% names(res3[[n]])) expect_true(length(unique(res3[[n]]$rep)) == 42)
#   }
#   
# })
# 
# test_that("Output results contains the correct number of pop parameters", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res0 <- simulx(project = project)
#   res1 <- simulx(project = project, npop = 1)
#   res2 <- simulx(project = project, npop = 5)
#   res3 <- simulx(project = project, npop = 42)
#   expect_false("pop" %in% names(res0$CONC))
#   expect_false("pop" %in% names(res1$CONC))
#   expect_true("pop" %in% names(res2$CONC))
#   expect_true("pop" %in% names(res3$CONC))
#   for (n in names(res1)) {
#     if ("pop" %in% names(res0[[n]])) expect_true(length(unique(res0[[n]]$pop)) == 1)
#     if ("pop" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$pop)) == 1)
#     if ("pop" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$pop)) == 5)
#     if ("pop" %in% names(res3[[n]])) expect_true(length(unique(res3[[n]]$pop)) == 42)
#   }
#   
# })
# 
# test_that("When pop and rep are specified, Output results contains both columns", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res <- simulx(project = project, npop = 2, nrep = 3)
#   expect_true("pop" %in% names(res$CONC))
#   expect_true("rep" %in% names(res$CONC))
#   for (n in names(res)) {
#     if ("pop" %in% names(res[[n]])) expect_true(length(unique(res[[n]]$pop)) == 2)
#     if ("rep" %in% names(res[[n]])) expect_true(length(unique(res[[n]]$rep)) == 3)
#   }
#   
# })
# 
# test_that("When only one set of population parameters, a named vector is returned", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
#   res1 <- simulx(project = project, npop = 1)
#   res2 <- simulx(project = project, npop = 5)
#   res3 <- simulx(project = project, npop = 42)
#   expect_true(is.vector(res1$population))
#   expect_false(is.list(res1$population))
#   expect_true(is.list(res2$population))
#   expect_false(is.vector(res2$population))
#   expect_true(is.list(res3$population))
#   expect_false(is.vector(res3$population))
#   expect_equal(nrow(res2$population), 5)
#   expect_equal(nrow(res3$population), 42)
# })
# 
# test_that("If treatment in output, amt column is named 'amount'", {
#   pk.model <- inlineModel("
#   [LONGITUDINAL]
#   input = {V, k}
#   EQUATION:
#   C = pkmodel(V,k)
#   ")
#   C <- list(name='C', time=seq(0, 100, by=1))
#   p <- c(V=10, k=0.2)
#   adm <- list(time=seq(0,to=66,by=6), amount=50)
#   res <- simulx(model = pk.model, parameter = p, output = C, treatment = adm)
#   expect_true("amount" %in% names(res$treatment))
#   
# })
# 
# test_that("Function print warnings when missing parameters.", {
#   model <- inlineModel(
#     ";----------------------------------------------
# [COVARIATE]
# input = {w}
# 
# ;----------------------------------------------
# [INDIVIDUAL]
# input = {V_pop, omega_V, w, w_pop}
# 
# EQUATION:
# V_pred = V_pop*(w/w_pop)
# 
# DEFINITION:
# V = {distribution = lognormal, prediction = V_pred, sd = omega_V}
# 
# 
# [LONGITUDINAL]
# input = {V, k, a}
# 
# EQUATION:
# f = 100/V*exp(-k*t)
# 
# DEFINITION:
# y = {distribution = normal, prediction = f, sd = a}
# ")
#   
#   p <- c(V_pop=10, omega_V=0.1, w=70, k=0.15, a=0.5)
#   
#   f   <- list(name='f', time=seq(0, 30, by=0.1))
#   y   <- list(name='y', time=seq(1, 30, by=3))
#   out <- list(f, y)
#   
#   expect_warning(simulx(model=model,parameter=p, output=out))
# })
# 
# test_that("Function print warnings when useless parameters are given.", {
#   model <- inlineModel(
#     ";----------------------------------------------
# [COVARIATE]
# input = {w}
# 
# ;----------------------------------------------
# [INDIVIDUAL]
# input = {V_pop, omega_V, w, w_pop}
# 
# EQUATION:
# V_pred = V_pop*(w/w_pop)
# 
# DEFINITION:
# V = {distribution = lognormal, prediction = V_pred, sd = omega_V}
# 
# 
# [LONGITUDINAL]
# input = {V, k, a}
# 
# EQUATION:
# f = 100/V*exp(-k*t)
# 
# DEFINITION:
# y = {distribution = normal, prediction = f, sd = a}
# ")
#   
#   p <- c(V_pop=10, omega_V=0.1, w_pop=0.1, toto = 42, w=70, k=0.15, a=0.5)
#   
#   f   <- list(name='f', time=seq(0, 30, by=0.1))
#   y   <- list(name='y', time=seq(1, 30, by=3))
#   out <- list(f, y)
#   
#   expect_warning(simulx(model=model,parameter=p, output=out))
# })
# 
# test_that("Output defined with time = 'none' are removed from the results", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "warfarinPKPD_project.mlxtran")
#   res1 <- simulx(project = project)
#   res2 <- simulx(project = project, output = list(name="y2", time="none"))
#   expect_true("y1" %in% names(res1))
#   expect_false("y2" %in% names(res2))
# })
# 
# test_that("No error when cat cov defined as T / F", {
#   project <- file.path(demo_path, "1.creating_and_using_models",
#                        "1.1.libraries_of_models", "theophylline_project.mlxtran")
# 
#   expect_error(suppressWarnings(simulx(project = project,
#                 treatment = list(amount=80, time=0),
#                 output = list(name="Cc",time=0),
#                 parameter = data.frame(id=1:4, SEX=c("F"))), NA))
# })

