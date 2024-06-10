skip_on_cran()
skip_if_not_installed("lixoftConnectors")


initRsSimulx()
demo_path <- gsub("simulx", "monolix", .lixoftCall("getDemoPath"), fixed = TRUE)
skip_if(!dir.exists(demo_path), message = NULL)

project <- file.path(demo_path, "1.creating_and_using_models",
                     "1.1.libraries_of_models", "theophylline_project.mlxtran")
project <- get_project(project, runStdErrorsIfNeed = TRUE)
initRsSimulx(force = TRUE)

# test_that("simulxR runs on simulx demo with no fail", {
#   files <- list.files(path = demo_path , pattern = '[.]mlxtran$', full.names = T, include.dirs = F, recursive = T)
# 
#   for (f in files[81:91]) {
#     print(f)
#     project <- get_project(f)
# 
#     initRsSimulx(force = TRUE)
#     expect_error(simulx(project = project), NA)
#   }
# })

test_that("Case studies and demos run with no fail", {
  expect_error(simulx(project = project), NA)
  expect_error(simulx(project = project,
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1))),
               NA)
  expect_error(simulx(project = project,
                      parameter = "mlx_PopUncertainSA",
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1))),
               "^Invalid parameter.")
  expect_error(simulx(project = project,
                      parameter = "mlx_Pop",
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1))),
               NA)
  expect_error(simulx(project = project,
                      parameter = "mlx_PopIndiv",
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
  expect_error(simulx(project = project,
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1)),
                      treatment =  "mlx_Adm1",
                      group = list(size = 5)),
               NA)
  expect_error(simulx(project = project,
                      output = list(name = 'Cc', time = seq(from = 0, to = 24, by = 1)),
                      treatment =  "mlx_Adm2",
                      group = list(size = 5)),
               "^Invalid treatment.")
  
  g1 = list(size = 5, treatment = list(amount = 10, time = 0), parameter = c(omega_V = 0, omega_ka = 0, omega_Cl = 0))
  g2 = list(size = 4, treatment = list(amount = 20, time = 2))
  
  g3 = list(size = 5, treatment = "mlx_Adm1", parameter = "mlx_Pop")

  expect_error(suppressWarnings(simulx(project = project,
                      output = list(name = 'Cc', time = seq(from = 1, to = 25, by = 12)),
                      group = list(g1,g2)),
               NA))
  expect_error(simulx(project = project,
                      output = list(name = 'Cc', time = seq(from = 1, to = 25, by = 12)),
                      group = list(g3,g2)),
               NA)
  
})

# Test that arguments are well checked -----------------------------------------
test_that("Function stops when Invalid project", {
  expect_error(simulx(project = "thisprojectdoesnorexist.mlxtran"),
               "Project thisprojectdoesnorexist.mlxtran does not exist.")
  smlxpath <- file.path(path.expand("~"), "lixoft", "simulx",
                        paste0("simulx", .lixoftCall("getLixoftConnectorsState")$version), "demos")
  smlxproject <- file.path(smlxpath, "2.models/longitudinal.smlx")
  expect_error(simulx(project = smlxproject),
               "does not exist.")
})

test_that("Function stops when Invalid model", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V1, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm <- list(time=seq(0,to=66,by=6), amount=50)
  expect_error(res <- simulx(model = pk.model, parameter = p, output = C, treatment = adm),
               "Parse error")
})

test_that("Function stops when a project and a modele are defined together", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm <- list(time=seq(0,to=66,by=6), amount=50)
  expect_error(simulx(model = pk.model, parameter = p, output = C, treatment = adm, project = project),
               "You must define either the model or the Monolix project file, not both of them.")

})

test_that("Function stops when neither project nor modele is defined", {
  expect_error(simulx(),
               "You must define either the model or the Monolix project file.")
})

test_that("Function stops when npop and nrep are not integers", {
  expect_error(simulx(project = project, nrep = 1.2),
               "^Invalid nrep.")
  expect_error(simulx(project =project, nrep = -3),
               "^Invalid nrep.")
  expect_error(simulx(project = project, nrep = "toto"),
               "^Invalid nrep.")
})

test_that("Deprecated arguments print message", {
  expect_message(simulx(project = project, npop = 10))
  expect_message(simulx(project = project, fim = "SA"))
})

test_that("Models for which distribution of covariates is defined returns an error", {
  myModel <- inlineModel("
[COVARIATE]
input={p_F}

DEFINITION:
gender = { type=categorical,
           categories={F,M},
           P(gender=F)=p_F }

;---------------------------------------
[INDIVIDUAL]
input={k_pop, omega_k, gender, beta_F}
gender={type=categorical,categories={F,M}}

DEFINITION:
k = { distribution=lognormal,
      reference=k_pop,
      covariate=gender,
      coefficient={beta_F,0},
      sd=omega_k }

;---------------------------------------
[LONGITUDINAL]
input =  {k}

EQUATION:
f = exp(-k*t)
  ")
  p <- c(p_F=0.4,k_pop=0.2, omega_k=0.1, beta_F=0.6)
  f <- list(name='f', time=seq(0, 30, by=0.1))

  expect_error(simulx(model=myModel, parameter=p, output=f),
               "^Invalid model file.")

  model <- inlineModel(
    "[LONGITUDINAL]
input = {V, k, a}

EQUATION:
f = 100/V*exp(-k*t)

DEFINITION:
y = {distribution = normal, prediction = f, sd = a}

;----------------------------------------------
[INDIVIDUAL]
input = {V_pop, omega_V, w, w_pop}

EQUATION:
V_pred = V_pop*(w/w_pop)

DEFINITION:
V = {distribution = lognormal, prediction = V_pred, sd = omega_V}

;----------------------------------------------
[COVARIATE]
input = {w_pop, omega_w}

DEFINITION:
w = {distribution = normal, mean = w_pop, sd = omega_w}"
  )

  p <- c(V_pop=10, omega_V=0.1, beta=1, w_pop=70, omega_w=12, k=0.15, a=0.5)

  f   <- list(name='f', time=seq(0, 30, by=0.1))
  y   <- list(name='y', time=seq(1, 30, by=3))
  out <- list(f, y)
  expect_error(simulx(model=model, parameter=p, output=out),
               "^Invalid model file.")

})

# Test settings ----------------------------------------------------------------
test_that("When no seed specified: 2 simulations gives different results", {
  res1 <- simulx(project = project)
  res2 <- simulx(project = project)
  expect_equal(names(res1), names(res2))
  expect_false(isTRUE(all.equal(res1, res2)))
})

test_that("When a seed is specified: 2 simulations gives same results", {
  res1 <- simulx(project = project, settings = list(seed = 123))
  res2 <- simulx(project = project, settings = list(seed = 123))
  res3 <- simulx(project = project, npop = 2, settings = list(seed = 123))
  res4 <- simulx(project = project, npop = 2, settings = list(seed = 123))
  expect_equal(names(res1), names(res2))
  expect_equal(names(res3), names(res4))
  expect_equal(res1, res2)
  expect_equal(res3, res4)
})

test_that("When id.out is FALSE, when only one group, group columns are removed", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm1 <- list(time=seq(0,to=66,by=6), amount=50)
  adm2 <- list(time=seq(0,to=66,by=12), amount=100)
  adm3 <- list(time=seq(0,to=66,by=18), amount=150)
  g1 <- list(treatment=adm1);
  g2 <- list(treatment=adm2);
  g3 <- list(treatment=adm3);
  res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = FALSE))
  res2 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1, g2, g3), settings = list(id.out = FALSE))

  for (n in setdiff(names(res1), "parameter")) {
    expect_false("group" %in% names(res1[[n]]))
    expect_true("group" %in% names(res2[[n]]))
  }
})

test_that("When id.out is FALSE, when only one id, id columns are removed", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm1 <- list(time=seq(0,to=66,by=6), amount=50)
  adm21 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
  adm22 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
  adm2 <- rbind(adm21, adm22)
  res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = FALSE))
  res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm2, settings = list(id.out = FALSE))

  for (n in setdiff(names(res1), "parameter")) {
    expect_false("id" %in% names(res1[[n]]))
    expect_true("id" %in% names(res2[[n]]))
  }
})

test_that("When id.out is TRUE, when only one id, id columns are removed", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm1 <- list(time=seq(0,to=66,by=6), amount=50)
  adm21 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
  adm22 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
  adm2 <- rbind(adm21, adm22)
  res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = TRUE))
  res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm2, settings = list(id.out = TRUE))

  for (n in setdiff(names(res1), "parameter")) {
    expect_true("id" %in% names(res1[[n]]))
    expect_true("id" %in% names(res2[[n]]))
  }
})

test_that("When out.trt is FALSE, treatment is not included in ourput", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm <- list(time=seq(0,to=66,by=6), amount=50)
  res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm, settings = list(out.trt = TRUE))
  res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm, settings = list(out.trt = FALSE))

  expect_true("treatment" %in% names(res1))
  expect_false("treatment" %in% names(res2))
})

test_that("When replacement is FALSE, simulx sampling method is keepOrder", {
  res_keepOrder1 <- simulx(project = project, settings = list(seed=1234))
  expect_equal(lixoftConnectors::getSamplingMethod(), "keepOrder")

  res_keepOrder2 <- simulx(project = project, settings = list(samplingMethod="keepOrder", seed=1234))
  expect_equal(lixoftConnectors::getSamplingMethod(), "keepOrder")

  expect_message(res_keepOrder3 <- simulx(project = project, settings = list(replacement = FALSE, seed=1234)))
  expect_equal(lixoftConnectors::getSamplingMethod(), "keepOrder")
  
  expect_equal(res_keepOrder1, res_keepOrder2)
  expect_equal(res_keepOrder1, res_keepOrder3)
  
})

test_that("When replacement is TRUE, simulx sampling method is withReplacement", {
  expect_message(res_replacement1 <- simulx(project = project, settings = list(replacement = TRUE, seed=1234)))
  expect_equal(lixoftConnectors::getSamplingMethod(), "withReplacement")
  
  res_replacement2 <- simulx(project = project, settings = list(samplingMethod="withReplacement", seed=1234))
  expect_equal(lixoftConnectors::getSamplingMethod(), "withReplacement")
  
  expect_equal(res_replacement1, res_replacement2)
  
  res_noreplacement <- simulx(project = project, settings = list(samplingMethod="withoutReplacement", seed=1234))
  expect_equal(lixoftConnectors::getSamplingMethod(), "withoutReplacement")
  
})

# Test Outputs -----------------------------------------------------------------
test_that("cens, rep, id, group columns are factors", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1), lloq = 0.8)
  p <- c(V=10, k=0.2)
  adm1 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
  adm2 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
  adm <- rbind(adm1, adm2)
  g1 <- list(treatment=as.list(adm1[names(adm1) != "id"]))
  g2 <- list(treatment=as.list(adm2[names(adm2) != "id"]))
  res <- simulx(model = pk.model, parameter = p, output = C, group = list(g1, g2), nrep = 3)

  for (n in names(res)) {
    for (col in intersect(names(res[[n]]), c("cens", "rep", "id", "group"))) {
      expect_true(is.factor(res[[n]][[col]]))
    }
  }
})

test_that("Output results contains the correct number of ids", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm1 <- data.frame(id = 1, list(time=seq(0,to=66,by=12), amount=100))
  adm2 <- data.frame(id = 2, list(time=seq(0,to=66,by=12), amount=50))
  adm <- rbind(adm1, adm2)
  res1 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm)
  res2 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = TRUE))
  for (n in names(res1)) {
    if ("id" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$id)) == 2)
    if ("id" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$id)) == 1)
  }
})

test_that("Output results contains the correct number of groups", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm1 <- list(time=seq(0,to=66,by=6), amount=50)
  adm2 <- list(time=seq(0,to=66,by=12), amount=100)
  adm3 <- list(time=seq(0,to=66,by=18), amount=150)
  g1 <- list(treatment=adm1)
  g2 <- list(treatment=adm2)
  g3 <- list(treatment=adm3)
  res0 <- simulx(model = pk.model, parameter = p, output = C, treatment = adm1, settings = list(id.out = TRUE))
  res1 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1), settings = list(id.out = TRUE))
  res2 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1, g2), settings = list(id.out = TRUE))
  res3 <- simulx(model = pk.model, parameter = p, output = C, group = list(g1, g2, g3), settings = list(id.out = TRUE))

  for (n in names(res1)) {
    if ("group" %in% names(res0[[n]])) expect_true(length(unique(res0[[n]]$group)) == 1)
    if ("group" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$group)) == 1)
    if ("group" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$group)) == 2)
    if ("group" %in% names(res3[[n]])) expect_true(length(unique(res3[[n]]$group)) == 3)
  }
})

test_that("Output results contains the correct number of rep", {
  res0 <- simulx(project = project)
  res1 <- simulx(project = project, nrep = 1)
  res2 <- simulx(project = project, nrep = 5)
  res3 <- simulx(project = project, nrep = 42)
  expect_false("rep" %in% names(res0$CONC))
  expect_false("rep" %in% names(res1$CONC))
  expect_true("rep" %in% names(res2$CONC))
  expect_true("rep" %in% names(res3$CONC))
  for (n in names(res1)) {
    if ("rep" %in% names(res0[[n]])) expect_true(length(unique(res0[[n]]$rep)) == 1)
    if ("rep" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$rep)) == 1)
    if ("rep" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$rep)) == 5)
    if ("rep" %in% names(res3[[n]])) expect_true(length(unique(res3[[n]]$rep)) == 42)
  }

  res0 <- simulx(project=project, parameter="mlx_PopUncertainLin")
  res1 <- simulx(project=project, parameter="mlx_PopUncertainLin", nrep=1)
  res2 <- simulx(project=project, parameter="mlx_PopUncertainLin", nrep=5)
  res3 <- simulx(project=project, parameter="mlx_PopUncertainLin", nrep=42)
  expect_false("rep" %in% names(res0$population))
  expect_false("rep" %in% names(res1$population))
  expect_true("rep" %in% names(res2$population))
  expect_true("rep" %in% names(res3$population))
  expect_true(length(unique(res2$population$rep)) == 5)
  expect_true(length(unique(res3$population$rep)) == 42)
  expect_true(length(unique(res2$CONC$rep)) == 5)
  expect_true(length(unique(res3$CONC$rep)) == 42)
  
  res0 <- simulx(project = project)
  res1 <- simulx(project = project, npop = 1)
  res2 <- simulx(project = project, npop = 5)
  res3 <- simulx(project = project, npop = 42)
  expect_false("rep" %in% names(res0$CONC))
  expect_false("rep" %in% names(res1$CONC))
  expect_true("rep" %in% names(res2$CONC))
  expect_true("rep" %in% names(res3$CONC))
  for (n in names(res1)) {
    if ("rep" %in% names(res0[[n]])) expect_true(length(unique(res0[[n]]$rep)) == 1)
    if ("rep" %in% names(res1[[n]])) expect_true(length(unique(res1[[n]]$rep)) == 1)
    if ("rep" %in% names(res2[[n]])) expect_true(length(unique(res2[[n]]$rep)) == 5)
    if ("rep" %in% names(res3[[n]])) expect_true(length(unique(res3[[n]]$rep)) == 42)
  }
  

})

test_that("When only one set of population parameters, a named vector is returned", {
  res1 <- simulx(project = project, npop = 1)
  res2 <- simulx(project = project, npop = 5)
  res3 <- simulx(project = project, npop = 42)
  expect_true(is.vector(res1$population))
  expect_false(is.list(res1$population))
  expect_true(is.list(res2$population))
  expect_false(is.vector(res2$population))
  expect_true(is.list(res3$population))
  expect_false(is.vector(res3$population))
  expect_equal(nrow(res2$population), 5)
  expect_equal(nrow(res3$population), 42)
  
  res1 <- simulx(project = project, parameter = "mlx_PopUncertainLin", nrep = 1)
  res2 <- simulx(project = project, parameter = "mlx_PopUncertainLin", nrep = 5)
  res3 <- simulx(project = project, parameter = "mlx_PopUncertainLin", nrep = 42)
  expect_true(is.vector(res1$population))
  expect_false(is.list(res1$population))
  expect_true(is.list(res2$population))
  expect_false(is.vector(res2$population))
  expect_true(is.list(res3$population))
  expect_false(is.vector(res3$population))
  expect_equal(nrow(res2$population), 5)
  expect_equal(nrow(res3$population), 42)
  
})

test_that("If treatment in output, amt column is named 'amount'", {
  pk.model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  C <- list(name='C', time=seq(0, 100, by=1))
  p <- c(V=10, k=0.2)
  adm <- list(time=seq(0,to=66,by=6), amount=50)
  res <- simulx(model = pk.model, parameter = p, output = C, treatment = adm)
  expect_true("amount" %in% names(res$treatment))

})

test_that("Function print warnings when missing parameters.", {
  model <- inlineModel(
    ";----------------------------------------------
[COVARIATE]
input = {w}

;----------------------------------------------
[INDIVIDUAL]
input = {V_pop, omega_V, w, w_pop}

EQUATION:
V_pred = V_pop*(w/w_pop)

DEFINITION:
V = {distribution = lognormal, prediction = V_pred, sd = omega_V}


[LONGITUDINAL]
input = {V, k, a}

EQUATION:
f = 100/V*exp(-k*t)

DEFINITION:
y = {distribution = normal, prediction = f, sd = a}
")

  p <- c(V_pop=10, omega_V=0.1, w=70, k=0.15, a=0.5)

  f   <- list(name='f', time=seq(0, 30, by=0.1))
  y   <- list(name='y', time=seq(1, 30, by=3))
  out <- list(f, y)

  expect_warning(simulx(model=model,parameter=p, output=out),
                 "'w_pop' has not been specified. It will be set to 1.")
})

test_that("Function print warnings when useless parameters are given.", {
  model <- inlineModel(
    ";----------------------------------------------
[COVARIATE]
input = {w}

;----------------------------------------------
[INDIVIDUAL]
input = {V_pop, omega_V, w, w_pop}

EQUATION:
V_pred = V_pop*(w/w_pop)

DEFINITION:
V = {distribution = lognormal, prediction = V_pred, sd = omega_V}


[LONGITUDINAL]
input = {V, k, a}

EQUATION:
f = 100/V*exp(-k*t)

DEFINITION:
y = {distribution = normal, prediction = f, sd = a}
")

  p <- c(V_pop=10, omega_V=0.1, w_pop=0.1, toto = 42, w=70, k=0.15, a=0.5)

  f   <- list(name='f', time=seq(0, 30, by=0.1))
  y   <- list(name='y', time=seq(1, 30, by=3))
  out <- list(f, y)

  expect_warning(simulx(model=model, parameter=p, output=out),
                 "Found extra parameters. 'toto' is not in the model.")
})

# test_that("Output defined with time = 'none' are removed from the results", {
#   res1 <- simulx(project = project)
#   res2 <- simulx(project = project, output = list(name="y2", time="none"))
#   expect_true("y1" %in% names(res1))
#   expect_false("y2" %in% names(res2))
# })

test_that("No error when cat cov defined as T / F", {
  project_cov <- file.path(demo_path, "1.creating_and_using_models",
                       "1.1.libraries_of_models", "theophylline_project.mlxtran")
  project_cov <- get_project(project_cov)
  .lixoftCall("setCovariateModel", list(list(V=c(SEX=T))))
  .lixoftCall("saveProject", list(project_cov))
  .lixoftCall("runPopulationParameterEstimation")

  initRsSimulx(force = TRUE)
  
  expect_no_error(
    res <- simulx(project = project_cov,
           treatment = list(amount=80, time=0),
           output = list(name="Cc",time=0),
           covariate = data.frame(id=1:4, SEX="F", WEIGHT = 70)) )
  expect_equal(unique(res$parameter$SEX), "F")

  expect_error(
    res <- simulx(project = project_cov,
           treatment = list(amount=80, time=0),
           output = list(name="Cc",time=0),
           covariate = data.frame(id=1:4, SEX="F", WEIGHT = 70)),
    NA)
  expect_equal(unique(res$parameter$SEX), "F")
})

test_that("Model from library", {
  model <- "lib:oral1_1cpt_TlagkaVCl.txt"
  expect_no_error(expect_warning(expect_warning(simulx(model))))
  
  model <- "lib:oral1_1cpt_Tlagka.txt"
  expect_error(expect_warning(simulx(model)), "is not a model from the libraries\n$")
  
})

test_that("sharedIds argument works correctly", {
  model <- inlineModel(
"[COVARIATE]
input = {WT}

EQUATION:
  logtWT = log(WT/55)

[INDIVIDUAL]
input = {Tlag_pop, omega_Tlag, ka_pop, omega_ka, V_pop, omega_V, Cl_pop, omega_Cl, logtWT, beta_V_logtWT, corr_V_Cl}

DEFINITION:
  Tlag = {distribution=logNormal, typical=Tlag_pop, sd=omega_Tlag}
ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}
V = {distribution=logNormal, typical=V_pop, covariate=logtWT, coefficient=beta_V_logtWT, sd=omega_V}
Cl = {distribution=logNormal, typical=Cl_pop, sd=omega_Cl}
correlation = {level=id, r(V, Cl)=corr_V_Cl}

[LONGITUDINAL]
input = {a, b, Tlag, ka, V, Cl}

EQUATION:
  
  ; PK model definition
Cc = pkmodel(Tlag, ka, V, Cl)

OUTPUT:
output = Cc"
  )
  popparams1 <- c(Tlag_pop=2, omega_Tlag=0.5, ka_pop=1.5, omega_ka=0.7,
                  V_pop=10, omega_V=1, Cl_pop=5, omega_Cl=1, 
                  beta_V_logtWT=4, corr_V_Cl=0.2, a=0.3, b=0.4)
  indparams1 <- c(Tlag=1, ka=1, V=1, Cl=0.5)
  cov1 <- c(WT=75)
  cov2 <- data.frame(id=1:60, WT=runif(n=60, min=45, max=90))
  trt1 <- list(amount=c(10, 20), time=c(0, 24))
  trt2 <- data.frame(id=1:60, time=0, amount=runif(n=60, min=300, max=600))
  trt3 <- data.frame(id=1:65, time=0, amount=runif(n=65, min=300, max=600))
  output1 <- list(name="Cc", time=seq(0, 72, by=12))
  output2 <- list(name="Cc", time=data.frame(id=c(1, 1, 1, rep(2:60, each=7)), time=c(0, 36, 72, rep(seq(0, 72, by=12), times=59))))
  
  # check sharedId format
  expect_error(
    simulx(
      model=model,
      parameter=popparams1,
      covariate=cov1,
      treatment=trt1,
      output=output1,
      settings=list(sharedIds=c("population", "jh"))
    ),
    "setting sharedIds must be in \\{'covariate', 'output', 'treatment', 'regressor', 'population', 'individual'\\}."
  )

  res <- simulx(
    model=model,
    parameter=popparams1,
    covariate=cov1,
    treatment=trt2,
    output=output2,
    settings=list(sharedIds=c("output", "treatment"))
  )
  expect_equal(.lixoftCall("getSharedIds"), c("treatment", "output"))
  
  res <- simulx(
    model=model,
    parameter=popparams1,
    covariate=cov2,
    treatment=trt2,
    output=output2,
    settings=list(sharedIds=c("output", "treatment", "covariate"))
  )
  expect_equal(.lixoftCall("getSharedIds"), c("covariate", "treatment", "output"))
  
  
})

test_that("sameIdsAmongGroups argument works correctly", {
  model <- inlineModel(
    "[COVARIATE]
input = {WT}

EQUATION:
  logtWT = log(WT/55)

[INDIVIDUAL]
input = {Tlag_pop, omega_Tlag, ka_pop, omega_ka, V_pop, omega_V, Cl_pop, omega_Cl, logtWT, beta_V_logtWT, corr_V_Cl}

DEFINITION:
  Tlag = {distribution=logNormal, typical=Tlag_pop, sd=omega_Tlag}
ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}
V = {distribution=logNormal, typical=V_pop, covariate=logtWT, coefficient=beta_V_logtWT, sd=omega_V}
Cl = {distribution=logNormal, typical=Cl_pop, sd=omega_Cl}
correlation = {level=id, r(V, Cl)=corr_V_Cl}

[LONGITUDINAL]
input = {a, b, Tlag, ka, V, Cl}

EQUATION:
  
  ; PK model definition
Cc = pkmodel(Tlag, ka, V, Cl)

OUTPUT:
output = Cc"
  )
  popparams1 <- c(Tlag_pop=2, omega_Tlag=0.5, ka_pop=1.5, omega_ka=0.7,
                  V_pop=10, omega_V=0.1, Cl_pop=5, omega_Cl=0.9, beta_V_logtWT=0.8,
                  corr_V_Cl=0.7, a=0.15, b=0.23)
  indparams1 <- c(Tlag=1, ka=1, V=1, Cl=0.5)
  cov1 <- c(WT=75)
  cov2 <- data.frame(id=1:60, WT=runif(n=60, min=45, max=90))
  trt1 <- list(amount=c(10, 20), time=c(0, 24))
  trt2 <- data.frame(id=1:60, time=0, amount=runif(n=60, min=300, max=600))
  trt3 <- data.frame(id=1:65, time=0, amount=runif(n=65, min=300, max=600))
  output1 <- list(name="Cc", time=seq(0, 72, by=12))
  output2 <- list(name="Cc", time=data.frame(id=c(1, 1, 1, rep(2:60, each=7)), time=c(0, 36, 72, rep(seq(0, 72, by=12), times=59))))
  
  # check sharedId format
  expect_error(
    simulx(
      model=model,
      parameter=popparams1,
      covariate=cov1,
      treatment=trt1,
      output=output1,
      settings=list(sharedIds=c("population", "jh"))
    ),
    "^setting sharedIds must be in"
  )
  
  # expect_warning(
  #   simulx(
  #     model=model,
  #     parameter=popparams1,
  #     covariate=cov1,
  #     treatment=trt1,
  #     output=output1,
  #     settings=list(sharedIds=c("population", "treatment"))),
  #   "^No id to share"
  # )
  
  # expect_warning(
  #   simulx(
  #     model=model,
  #     parameter=popparams1,
  #     covariate=cov1,
  #     treatment=trt1,
  #     output=output2,
  #     settings=list(sharedIds=c("output", "treatment"))),
  #   "^No id to share"
  # )
  
  # expect_warning(
  #   simulx(
  #     model=model,
  #     parameter=popparams1,
  #     covariate=cov1,
  #     treatment=trt2,
  #     output=output2,
  #     settings=list(sharedIds=c("covariate", "output", "treatment"))),
  #   "^No id to share"
  # )
  # expect_equal(.lixoftCall("getSharedIds"), c("treatment", "output"))
  
  res <- simulx(
    model=model,
    parameter=popparams1,
    covariate=cov1,
    treatment=trt2,
    output=output2,
    settings=list(sharedIds=c("output", "treatment"))
  )
  expect_equal(.lixoftCall("getSharedIds"), c("treatment", "output"))
  
  res <- simulx(
    model=model,
    parameter=popparams1,
    covariate=cov2,
    treatment=trt2,
    output=output2,
    settings=list(sharedIds=c("output", "treatment", "covariate"))
  )
  expect_equal(.lixoftCall("getSharedIds"), c("covariate", "treatment", "output"))
  
  
})

test_that("simulx save smlx project when saveSmlxProject is specified", {
  res <- simulx(project=project, saveSmlxProject=NULL)
  
  f1 <- paste0(tempfile(), ".smlx")
  res <- simulx(project=project, saveSmlxProject=f1)
  expect_true(file.exists(f1))
  
  f2 <- paste0(tempfile())
  res <- simulx(project=project, saveSmlxProject=f2)
  expect_true(file.exists(paste0(f2, ".smlx")))
  
  f3 <- "/tmp/a/b/c/temp_project.smlx"
  res <- simulx(project=project, saveSmlxProject=f3)
  expect_true(file.exists(f3))
  
  f4 <- paste0(tempfile(), ".wrongextension")
  expect_error(simulx(project=project, saveSmlxProject=f4), "Invalid saveSmlxProject")
  
  expect_warning(res <- simulx(project=project, saveSmlxProject=NULL, settings=list(exportData=T)),
                 "You first need to specify a path to save smlx project in order to export data.")
  
  f5 <- paste0(tempfile(), ".smlx")
  res <- simulx(project=project, saveSmlxProject=f5, settings=list(exportData=T))
  expect_true(file.exists(file.path(.lixoftCall("getProjectSettings")$directory, "Simulation", "simulatedData.csv")))

})

test_that("Parameter argument", {
  model <- "lib:oral1_1cpt_TlagkaVCl.txt"

  # individual parameter
  parameter <- list(Tlag=23, ka=4, V=20, Cl=3)
  output <- list(name="Cc", time=seq(0, 24, 1))
  res <- simulx(model=model, parameter=parameter, output=output)
  expect_equal(res$parameter, unlist(parameter[names(res$parameter)]))

  parameter=data.frame(id=c(1, 2, 3), Tlag=c(23, 24, 30), ka=c(4, 3, 2), V=c(30, 40, 50), Cl=c(3, 2, 1))
  parameter$id <- factor(parameter$id)
  res <- simulx(model=model, parameter=parameter, output=output)
  res$parameter$original_id <- NULL
  expect_equal(res$parameter, parameter[names(res$parameter)])

  parameter_file=.addDataFrameTemp(parameter)
  res <- simulx(model=model, parameter=parameter_file, output=output)
  res$parameter$original_id <- NULL
  expect_equal(res$parameter, parameter[names(res$parameter)])

})

test_that("Covariate argument", {
  model <- inlineModel(
    "[COVARIATE]
input = {WT}

EQUATION:
  logtWT = log(WT/55)

[INDIVIDUAL]
input = {Tlag_pop, omega_Tlag, ka_pop, omega_ka, V_pop, omega_V, Cl_pop, omega_Cl, logtWT, beta_V_logtWT, corr_V_Cl}

DEFINITION:
  Tlag = {distribution=logNormal, typical=Tlag_pop, sd=omega_Tlag}
ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}
V = {distribution=logNormal, typical=V_pop, covariate=logtWT, coefficient=beta_V_logtWT, sd=omega_V}
Cl = {distribution=logNormal, typical=Cl_pop, sd=omega_Cl}
correlation = {level=id, r(V, Cl)=corr_V_Cl}

[LONGITUDINAL]
input = {a, b, Tlag, ka, V, Cl}

EQUATION:
  
  ; PK model definition
Cc = pkmodel(Tlag, ka, V, Cl)

OUTPUT:
output = Cc"
  )
  output <- list(name="Cc", time=seq(0, 24, 1))
  p <- list(Tlag_pop=3, omega_Tlag=0.2, ka_pop=4, omega_ka=0.5,
            V_pop=30, omega_V=0.9, Cl_pop=6, omega_Cl=0.6, beta_V_logtWT=19,
            corr_V_Cl=0.8, a=1, b=1)
  
  covariate <- list(WT=65)
  res <- simulx(model=model, covariate=covariate, output=output, parameter=p)
  expect_equal(res$covariate, unlist(covariate[names(res$covariate)]))
  
  covariate <- data.frame(id=c(1, 2, 3), WT=c(75, 83, 65))
  covariate$id <- factor(covariate$id)
  res <- simulx(model=model, covariate=covariate, output=output, parameter=p)
  expect_equal(res$parameter[names(covariate)], covariate)
  
  covariate_file <- .addDataFrameTemp(covariate)
  res <- simulx(model=model, covariate=covariate_file, output=output, parameter=p)
  expect_equal(res$parameter[names(covariate)], covariate)
  
})

test_that("Treatment argument", {
  model <- "lib:oral1_1cpt_TlagkaVCl.txt"
  output <- list(name="Cc", time=seq(0, 24, 1))
  p <- list(Tlag=3, ka=12, V=30, Cl=0.5)

  treatment1=list(time=c(0, 24), amount=200, type=c(1, 2))
  treatment2=list(time=48, amount=100, type=1)

  out_treatment1 <- as.data.frame(treatment1)
  out_treatment1$washout <- 0
  out_treatment1 <- .renameColumns(out_treatment1, "type", "admtype")

  out_treatment2 <- as.data.frame(treatment2)
  out_treatment2$washout <- 0
  out_treatment2 <- .renameColumns(out_treatment2, "type", "admtype")
  
  res1 <- simulx(model=model, treatment=treatment1, output=output, parameter=p)
  res2 <- simulx(model=model, treatment=list(treatment1, treatment2), output=output, parameter=p)
  expect_equal(res1$treatment, out_treatment1)
  expect_equal(res2$treatment, rbind(out_treatment1, out_treatment2))

  treatment1 <- data.frame(id=factor(c(1, 1, 2, 2, 3, 3)), time=c(0, 24, 0, 24, 0, 48),
                           amount=c(100, 200, 100, 200, 150, 300), type=c(1, 2, 1, 2, 1, 2))
  treatment2 <- data.frame(id=factor(c(1, 2, 3)), time=c(72, 72, 72),
                           amount=c(50, 55, 60), type=c(2, 2, 2))
  out_treatment1 <- treatment1
  out_treatment1$washout <- 0
  out_treatment1 <- .renameColumns(out_treatment1, "type", "admtype")
  
  out_treatment2 <- treatment2
  out_treatment2$washout <- 0
  out_treatment2 <- .renameColumns(out_treatment2, "type", "admtype")
  
  out_2 <- rbind(out_treatment1, out_treatment2)
  out_2 <- out_2[order(out_2$id, out_2$time),]
  row.names(out_2) <- NULL
  res1 <- simulx(model=model, treatment=treatment1, output=output, parameter=p)
  res2 <- simulx(model=model, treatment=list(treatment1, treatment2), output=output, parameter=p)
  res1$treatment$original_id <- NULL
  expect_equal(res1$treatment, out_treatment1)
  res2$treatment$original_id <- NULL
  expect_equal(res2$treatment, out_2)

  treatment1_file <- .addDataFrameTemp(treatment1)
  treatment2_file <- .addDataFrameTemp(treatment2)
  res1 <- simulx(model=model, treatment=treatment1_file, output=output, parameter=p)
  res1$treatment$original_id <- NULL
  res2 <- simulx(model=model, treatment=list(treatment1_file, treatment2_file), output=output, parameter=p)
  res2$treatment$original_id <- NULL
  expect_equal(res1$treatment, out_treatment1)
  expect_equal(res2$treatment, out_2)
  
})

test_that("Output argument", {
  model <- inlineModel(
    ";----------------------------------------------
[COVARIATE]
input = {w}

;----------------------------------------------
[INDIVIDUAL]
input = {V_pop, omega_V, w, w_pop}

EQUATION:
V_pred = V_pop*(w/w_pop)

DEFINITION:
V = {distribution = lognormal, prediction = V_pred, sd = omega_V}


[LONGITUDINAL]
input = {V, k, a}

EQUATION:
f = 100/V*exp(-k*t)

DEFINITION:
y = {distribution = normal, prediction = f, sd = a}
")
  
  p <- c(V_pop=10, omega_V=0.1, w_pop=70, k=0.15, a=0.5)
  cov <- c(w=75)
  
  # No output
  expect_error(
    simulx(model=model, parameter=p, covariate=cov),
    "When a model is defined without an OUTPUT section, you must define an output element"
  )
  model2 <- .addOutputToFile(model, "f")
  expect_warning(
    simulx(model=model2, parameter=p, covariate=cov),
    "Output has not been defined."
  )

  output1 <- list(name='f', time=seq(0, 30, by=0.1))
  output2 <- list(name='y', time=seq(1, 30, by=3))
  res1 <- simulx(model=model, output=output1, parameter=p, covariate=cov)
  res2 <- simulx(model=model, output=list(output1, output2), parameter=p, covariate=cov)
  expect_true("f" %in% names(res1))
  expect_equal(unique(res1$f$time), output1$time)
  expect_true(all(c("f", "y") %in% names(res2)))
  expect_equal(unique(res2$f$time), output1$time)
  expect_equal(unique(res2$y$time), output2$time)
  
  output1 <- list(name="f",
                  time=data.frame(id=factor(rep(1:3, each=301)), time=rep(seq(0, 30, by=0.1), 3)))
  output2 <- list(name="y",
                  time=data.frame(id=factor(rep(1:3, each=31)), time=rep(seq(0, 30, by=1), 3)))
  res1 <- simulx(model=model, output=output1, parameter=p, covariate=cov)
  res2 <- simulx(model=model, output=list(output1, output2), parameter=p, covariate=cov)
  expect_true("f" %in% names(res1))
  expect_equal(res1$f[c("id", "time")], output1$time)
  expect_true(all(c("f", "y") %in% names(res2)))
  expect_equal(res2$f[c("id", "time")], output1$time)
  expect_equal(res2$y[c("id", "time")], output2$time)
  
  output1_file <- list(name="f", time=.addDataFrameTemp(output1$time))
  output2_file <- list(name="y", time=.addDataFrameTemp(output2$time))
  res1 <- simulx(model=model, output=output1_file, parameter=p, covariate=cov)
  res2 <- simulx(model=model, output=list(output1_file, output2_file), parameter=p, covariate=cov)
  expect_true("f" %in% names(res1))
  expect_equal(res1$f[c("id", "time")], output1$time)
  expect_true(all(c("f", "y") %in% names(res2)))
  expect_equal(res2$f[c("id", "time")], output1$time)
  expect_equal(res2$y[c("id", "time")], output2$time)
  
})

test_that("Occasion argument", {
  warn_txt = "Column 'EVENT ID' added new occasions"
  iov_path <- file.path(demo_path, "5.models_for_individual_parameters/5.4.inter_occasion_variability")
  project1 <- file.path(iov_path, "iov2_project.mlxtran")
  expect_warning(project1 <- get_project(project1),  warn_txt)
  .lixoftCall("runPopulationParameterEstimation")
  initRsSimulx(force = TRUE)
  
  output <- list(name="Cc", time=seq(0, 200, 5))
  
  expect_warning(res <- simulx(project=project1, output=output), warn_txt)
  expect_true("OCCevid" %in% names(res$Cc))
  
  expect_warning(res <- simulx(project=project1, output=output, occasion="none"), warn_txt)
  expect_false("OCCevid" %in% names(res$Cc))
  
  occasion <- list(name='OCCASION', time=c(0, 120))
  output <- list(name="Cc", time=seq(0, 200, 5))
  expect_warning(res <- simulx(project=project1, output=output, occasion=occasion), warn_txt)
  expect_equal(unique(res$Cc[res$Cc$time < 120, "occ"]), 1)
  expect_equal(unique(res$Cc[res$Cc$time >= 120, "occ"]), 2)
  
  occasion <- data.frame(time=c(0, 120), OCCASION=c(1, 2))
  expect_warning(res <- simulx(project=project1, output=output, occasion=occasion), warn_txt)
  expect_equal(unique(res$Cc[res$Cc$time < 120, "occ"]), 1)
  expect_equal(unique(res$Cc[res$Cc$time >= 120, "occ"]), 2)
  
  occasion1 <- data.frame(id=c(1, 1, 2, 2, 3, 3), time=c(0, 120, 0, 100, 0, 130), OCCASION=c(1, 2, 1, 2, 1, 2))
  output <- list(name="Cc", time=seq(0, 200, 5))
  expect_warning(res <- simulx(project=project1, output=output, occasion=occasion1), warn_txt)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time < 120, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time >= 120, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time < 100, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time >= 100, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time < 130, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time >= 130, "OCCASION"]), 2)
  
  occasion2 <- data.frame(ID=c(1, 1, 2, 2, 3, 3), time=c(0, 120, 0, 100, 0, 130), OCCASION=c(1, 2, 1, 2, 1, 2))
  output <- list(name="Cc", time=seq(0, 200, 5))
  expect_warning(res <- simulx(project=project1, output=output, occasion=occasion2), warn_txt)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time < 120, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time >= 120, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time < 100, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time >= 100, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time < 130, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time >= 130, "OCCASION"]), 2)
  
  occasion1_file <- .addDataFrameTemp(occasion1)
  output <- list(name="Cc", time=seq(0, 200, 5))
  expect_warning(res <- simulx(project=project1, output=output, occasion=occasion1_file), warn_txt)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time < 120, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time >= 120, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time < 100, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time >= 100, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time < 130, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time >= 130, "OCCASION"]), 2)
  
  occasion2_file <- .addDataFrameTemp(occasion2)
  output <- list(name="Cc", time=seq(0, 200, 5))
  expect_warning(res <- simulx(project=project1, output=output, occasion=occasion2_file), warn_txt)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time < 120, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 1 & res$Cc$time >= 120, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time < 100, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 2 & res$Cc$time >= 100, "OCCASION"]), 2)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time < 130, "OCCASION"]), 1)
  expect_equal(unique(res$Cc[res$Cc$id == 3 & res$Cc$time >= 130, "OCCASION"]), 2)
  
})

test_that("Treatment and outputs elements when non overlapping occasions", {
  warn_txt = "Column 'EVENT ID' added new occasions"
  iov_path <- file.path(demo_path, "5.models_for_individual_parameters/5.4.inter_occasion_variability")
  project1 <- file.path(iov_path, "iov2_project.mlxtran")
  expect_warning(project1 <- get_project(project1), warn_txt)
  .lixoftCall("runPopulationParameterEstimation")
  initRsSimulx(force = TRUE)
  
  # output
  output <- list(name="Cc", time=seq(0, 140, 5))
  expect_warning(res <- simulx(project=project1, output=output), warn_txt)
  ids <- unique(.lixoftCall("getOccasionElements")$id)
  out_output <- data.frame(id=factor(rep(1:length(ids), each=29)),
                           OCCevid=sapply(output$time, function(t) ifelse(t < 120, 1, 2)),
                           time=output$time)
  expect_equal(res$Cc[c("id", "OCCevid", "time")], out_output)
  
  output <- list(name="Cc", time=data.frame(id=rep(ids, each=29), time=rep(seq(0, 140, 5), length(ids))))
  expect_warning(res <- simulx(project=project1, output=output), warn_txt)
  expect_equal(res$Cc[c("id", "OCCevid", "time")], out_output)

  output_file <- list(name="Cc", time=.addDataFrameTemp(output$time))
  expect_warning(res <- simulx(project=project1, output=output_file), warn_txt)
  expect_equal(res$Cc[c("id", "OCCevid", "time")], out_output)
  
  output$time$OCCevid <- 1
  output$time$OCCevid[output$time$time >= 120] <- 2
  expect_warning(res <- simulx(project=project1, output=output), warn_txt)
  expect_equal(res$Cc[c("id", "OCCevid", "time")], out_output)
  
  output_file <- list(name="Cc", time=.addDataFrameTemp(output$time))
  expect_warning(res <- simulx(project=project1, output=output_file), warn_txt)
  expect_equal(res$Cc[c("id", "OCCevid", "time")], out_output)
  
  output$time <- .renameColumns(output$time, "OCCevid", "occevid")
  expect_warning(res <- simulx(project=project1, output=output), warn_txt)
  expect_equal(res$Cc[c("id", "OCCevid", "time")], out_output)
  
  output_file <- list(name="Cc", time=.addDataFrameTemp(output$time))
  expect_warning(res <- simulx(project=project1, output=output_file), warn_txt)
  expect_equal(res$Cc[c("id", "OCCevid", "time")], out_output)

  # treatment
  treatment <- list(time=c(0, 120), amount=200)
  trt_output <- data.frame(id=factor(rep(1:length(ids), each=2)),
                           OCCevid=sapply(treatment$time, function(t) ifelse(t < 120, 1, 2)),
                           time=treatment$time)

  expect_warning(res <- simulx(project=project1, treatment=treatment), warn_txt)
  expect_equal(res$treatment[c("id", "OCCevid", "time")], trt_output)
  
  treatment <- data.frame(id=rep(ids, each=2), time=rep(c(0, 120), length(ids)), amount=200)
  expect_warning(res <- simulx(project=project1, treatment=treatment), warn_txt)
  expect_equal(res$treatment[c("id", "OCCevid", "time")], trt_output)
  
  treatment_file <- .addDataFrameTemp(treatment)
  expect_warning(res <- simulx(project=project1, treatment=treatment_file), warn_txt)
  expect_equal(res$treatment[c("id", "OCCevid", "time")], trt_output)
  
  treatment$OCCevid <- 1
  treatment$OCCevid[treatment$time >= 120] <- 2
  expect_warning(res <- simulx(project=project1, treatment=treatment), warn_txt)
  expect_equal(res$treatment[c("id", "OCCevid", "time")], trt_output)
  
  treatment_file <- .addDataFrameTemp(treatment)
  expect_warning(res <- simulx(project=project1, treatment=treatment_file), warn_txt)
  expect_equal(res$treatment[c("id", "OCCevid", "time")], trt_output)
  
  treatment <- .renameColumns(treatment, "OCCevid", "occevid")
  expect_warning(res <- simulx(project=project1, treatment=treatment), warn_txt)
  expect_equal(res$treatment[c("id", "OCCevid", "time")], trt_output)
  
  treatment_file <- .addDataFrameTemp(treatment)
  expect_warning(res <- simulx(project=project1, treatment=treatment_file), warn_txt)
  expect_equal(res$treatment[c("id", "OCCevid", "time")], trt_output)
})

test_that("Treatment and outputs elements when overlapping occasions", {
  iov_path <- file.path(demo_path, "5.models_for_individual_parameters/5.4.inter_occasion_variability")
  
  project1 <- file.path(iov_path, "iov1_project.mlxtran")
  project1 <- get_project(project1)
  .lixoftCall("runPopulationParameterEstimation")
  initRsSimulx(force = TRUE)
  
  # output
  output <- list(name="Cc", time=seq(0, 24, 2))
  expect_warning(res <- simulx(project=project1, output=output))
  ids <- unique(.lixoftCall("getOccasionElements")$id)
  out_output <- data.frame(id=factor(rep(1:length(ids), each=26)),
                           OCC=rep(rep(c(1, 2), each=13), length(ids)),
                           time=rep(output$time, 2 * length(ids)))
  expect_equal(res$Cc[c("id", "OCC", "time")], out_output)
  
  output <- list(name="Cc", time=data.frame(id=rep(ids, each=13), time=rep(seq(0, 24, 2), length(ids))))
  expect_warning(res <- simulx(project=project1, output=output))
  expect_equal(res$Cc[c("id", "OCC", "time")], out_output)
  
  output_file <- list(name="Cc", time=.addDataFrameTemp(output$time))
  expect_warning(res <- simulx(project=project1, output=output_file))
  expect_equal(res$Cc[c("id", "OCC", "time")], out_output)
  
  output$time <- as.data.frame(sapply(output$time, rep.int, times=2))
  output$time$time <- as.double(output$time$time)
  output$time$OCC <- rep(c(1, 2), each = 13 * length(ids))
  res <- simulx(project=project1, output=output)
  expect_equal(res$Cc[c("id", "OCC", "time")], out_output)
  
  output_file <- list(name="Cc", time=.addDataFrameTemp(output$time))
  res <- simulx(project=project1, output=output_file)
  expect_equal(res$Cc[c("id", "OCC", "time")], out_output)
  
  output$time <- .renameColumns(output$time, "OCC", "occ")
  res <- simulx(project=project1, output=output)
  expect_equal(res$Cc[c("id", "OCC", "time")], out_output)
  
  output_file <- list(name="Cc", time=.addDataFrameTemp(output$time))
  res <- simulx(project=project1, output=output_file)
  expect_equal(res$Cc[c("id", "OCC", "time")], out_output)
  
  # treatment
  treatment <- list(time=c(0, 120), amount=200)
  trt_output <- data.frame(id=factor(rep(1:length(ids), each=4)),
                           OCC=rep(rep(c(1, 2), each=2), length(ids)),
                           time=rep(treatment$time, 2 * length(ids)))
  
  expect_warning(res <- simulx(project=project1, treatment=treatment))
  expect_equal(res$treatment[c("id", "OCC", "time")], trt_output)
  
  treatment <- data.frame(id=rep(ids, each=2), time=rep(c(0, 120), length(ids)), amount=200)
  expect_warning(res <- simulx(project=project1, treatment=treatment))
  expect_equal(res$treatment[c("id", "OCC", "time")], trt_output)
  
  treatment_file <- .addDataFrameTemp(treatment)
  expect_warning(res <- simulx(project=project1, treatment=treatment_file))
  expect_equal(res$treatment[c("id", "OCC", "time")], trt_output)
  
  treatment <- as.data.frame(sapply(treatment, rep.int, times=2))
  treatment$time <- as.double(treatment$time)
  treatment$amount <- as.double(treatment$amount)
  treatment$OCC <- rep(c(1, 2), each = 2 * length(ids))
  res <- simulx(project=project1, treatment=treatment)
  expect_equal(res$treatment[c("id", "OCC", "time")], trt_output)
  
  treatment_file <- .addDataFrameTemp(treatment)
  res <- simulx(project=project1, treatment=treatment_file)
  expect_equal(res$treatment[c("id", "OCC", "time")], trt_output)
  
  treatment <- .renameColumns(treatment, "OCC", "occ")
  res <- simulx(project=project1, treatment=treatment)
  expect_equal(res$treatment[c("id", "OCC", "time")], trt_output)
  
  treatment_file <- .addDataFrameTemp(treatment)
  res <- simulx(project=project1, treatment=treatment_file)
  expect_equal(res$treatment[c("id", "OCC", "time")], trt_output)
})

test_that("Regressor argument", {
  model <- inlineModel(
"[LONGITUDINAL]
input = {Emax, EC50, C} 
C = {use = regressor}

EQUATION:
E = Emax*C/(C+EC50)"
  )
  t   <- seq(0, 50, by=1)
  out <- list(name='E', time=t)
  
  expect_error(
    simulx(model=model,
           group=list(size=3),
           parameter=c(Emax=100, EC50=0.3),
           regressor=list(name='C', time=t, value=head(exp(-0.1*t), -1)),
           output=out),
    "regressor time vector and regressor value vector must have the same length.")

  reg <- list(name='C', time=t, value=exp(-0.1*t))
  out_reg <- data.frame(id=as.factor(rep(1:3, each=length(reg$time))), time=rep(reg$time, 3), C=rep(reg$value, 3))
  res <- simulx(model=model,
                group=list(size=3),
                parameter=c(Emax=100, EC50=0.3),
                regressor=reg,
                output=out)
  expect_equal(res$regressors, out_reg)

  reg <- rbind(
    data.frame(id=as.factor(rep(1:2, each=length(t))), time=rep(t, 2), C=rep(exp(-0.1*t), 2)),
    data.frame(id=as.factor(rep(3:4, each=length(t))), time=rep(t, 2), C=rep(exp(-0.4*t), 2))
  )
  res <- simulx(model=model,
                parameter=c(Emax=100, EC50=0.3),
                regressor=reg,
                output=out)
  expect_equal(res$regressors[-which(names(res$regressors) == "original_id")], reg)
  
  reg_file <- .addDataFrameTemp(reg)
  res <- simulx(model=model,
                parameter=c(Emax=100, EC50=0.3),
                regressor=reg_file,
                output=out)
  expect_equal(res$regressors[-which(names(res$regressors) == "original_id")], reg)
  
  model <- inlineModel(
"[LONGITUDINAL]
input = {k1, k2, f0, g0, x1, x2}
x1 = {use = regressor}
x2 = {use = regressor}

EQUATION:
t0  = 0
f_0 = f0
g_0 = g0
ddt_f = -k1*f + k2*g + x1
ddt_g =  k1*f - k2*g + x2"
  )
  fg <- list(name=c('f','g'),
             time=seq(-5, 50, by=1))

  x1 <- list(name='x1',
             time=c(0,10,20,30,40),
             value=c(1,-1,1,-1,1)*0.5)
  x2 <- list(name='x2',
             time=c(5,15,25,35),
             value=c(1,-1,1,-1)*0.3)
  t <- sort(unique(c(-5, x1$time, x2$time))) # include output time too
  out_reg <- data.frame(id=as.factor(rep(1:3, each=length(t))), time=rep(t, 3))
  out_reg[out_reg$time %in% x1$time, "x1"] <- rep(x1$value, 3)
  out_reg[out_reg$time %in% x2$time, "x2"] <- rep(x2$value, 3)
  out_reg[out_reg$time == -5, c("x1", "x2")] <- rep(c(0.5, 0.3), each = 3)
  i <- c(TRUE, !is.na(out_reg$x1[-1]))
  out_reg$x1 <- out_reg$x1[i][cumsum(i)]
  i <- c(TRUE, !is.na(out_reg$x2[-1]))
  out_reg$x2 <- out_reg$x2[i][cumsum(i)]
  
  res <- simulx(model=model,
                group=list(size=3),
                parameter=c(k1=0.2, k2=0.1, f0=0, g0=0),
                regressor=list(x1, x2),
                output=list(name=c('f','g'),time=seq(-5, 50, by=1)))
  expect_equal(res$regressors, out_reg)
  
  x1 <- data.frame(id=as.factor(rep(1:3)), time=rep(c(0,10,20,30,40), 3), x1=rep(c(1,-1,1,-1,1)*0.5, 3))
  x2 <- data.frame(id=as.factor(rep(1:3)), time=rep(c(5,15,25,35), 3), x2=rep(c(1,-1,1,-1)*0.3, 3))
  res <- simulx(model=model,
                parameter=c(k1=0.2, k2=0.1, f0=0, g0=0),
                regressor=list(x1, x2),
                output=list(name=c('f','g'),time=seq(-5, 50, by=1)))
  expect_equal(res$regressors[-which(names(res$regressors) == "original_id")], out_reg)
  
  x1_file <- .addDataFrameTemp(x1)
  x2_file <- .addDataFrameTemp(x2)
  res <- simulx(model=model,
                parameter=c(k1=0.2, k2=0.1, f0=0, g0=0),
                regressor=list(x1_file, x2_file),
                output=list(name=c('f','g'),time=seq(-5, 50, by=1)))
  expect_equal(res$regressors[-which(names(res$regressors) == "original_id")], out_reg)
  
})
