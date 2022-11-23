skip_on_cran()
skip_if_not_installed("lixoftConnectors")

initRsSimulx()


test_that("treatment input of simulx", {
  model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  param <- c(V=10, k=0.2)
  
  adm <- list(amount=10, time=seq(0,24 * 20, by=20))
  out <- list(name='C', time=seq(0, 24 * 20, by=1))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = adm)
  expect_equal(res$param, param)
  expect_equal(unique(res$treatment$amount), 10)
  expect_equal(res$treatment$time, adm$time)
  expect_equal(unique(res$treatment$admtype), 1)
  expect_equal(unique(res$treatment$washout), 0)
  expect_true(out$name %in% names(res))
  expect_equal(res[[out$name]]$time, out$time)
  
  treatment = list(list(amount=20, time=c(0, 7, 14), type=1))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(unique(res$treatment$amount), 20)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(unique(res$treatment$admtype), 1)
  
  treatment = list(list(amount=20, time=c(0, 7, 14), type=c(1, 1, 1)))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(unique(res$treatment$amount), 20)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(res$treatment$admtype, treatment[[1]]$type)
  
  # Expected error when wrong treatment list
  treatment = list(list(amount=20, time=c(0, 7, 14), type=c(1, 1)))
  expect_error(
    simulx(model=model,
           parameter = param,
           output = out,
           treatment = treatment),
    "must have the same length.$"
  )
  
  treatment = list(list(amount=20, time=c(0, 7, 14), type=c(1, 2, 1)))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(unique(res$treatment$amount), 20)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(res$treatment$admtype, treatment[[1]]$type)
  
  treatment = list(list(amount=20, time=c(0, 7, 14), type=c(1, 2, 3)))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(unique(res$treatment$amount), 20)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(res$treatment$admtype, treatment[[1]]$type)
  
  treatment = list(list(amount=c(20, 50), time=c(0, 7, 14), type=1))
  expect_error(
    simulx(model=model,
           parameter = param,
           output = out,
           treatment = treatment),
    "must have the same length.$"
  )
  
  treatment = list(list(amount=c(20, 50, 100), time=c(0, 7, 14), type=1))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment$amount, treatment[[1]]$amount)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(unique(res$treatment$admtype), 1)
  
  treatment = list(list(amount=c(20, 50, 100), time=c(0, 7, 14), type=c(1, 1, 1)))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment$amount, treatment[[1]]$amount)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(res$treatment$admtype, treatment[[1]]$type)
  
  treatment = list(list(amount=c(20, 50, 100), time=c(0, 7, 14), type=c(1, 1)))
  expect_error(
    simulx(model=model,
           parameter = param,
           output = out,
           treatment = treatment),
    "must have the same length.$"
  )
  
  treatment = list(list(amount=c(20, 50, 100), time=c(0, 7, 14), type=c(1, 2, 1)))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment$amount, treatment[[1]]$amount)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(res$treatment$admtype, treatment[[1]]$type)
  
  treatment = list(list(amount=c(20, 50, 100), time=c(0, 7, 14), type=c(1, 2, 3)))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment$amount, treatment[[1]]$amount)
  expect_equal(res$treatment$time, treatment[[1]]$time)
  expect_equal(res$treatment$admtype, treatment[[1]]$type)
  
  treatment = list(list(amount = 15, time = 12), list(amount=20, time=c(0, 7, 14), type=c(1, 2, 3)))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment$amount, c(20, 20, 15, 20))
  expect_equal(res$treatment$time, c(0, 7, 12, 14))
  expect_equal(res$treatment$admtype, c(1, 2, 1, 3))
  
  treatment = list(list(amount = 15, time = 12), as.data.frame(list(amount=c(20, 20, 20), time=c(0, 7, 14), type=c(1, 2, 3))))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment$amount, c(20, 20, 15, 20))
  expect_equal(res$treatment$time, c(0, 7, 12, 14))
  expect_equal(res$treatment$admtype, c(1, 2, 1, 3))
  
  treatment = list(list(amount = 15, time = 12), as.data.frame(list(amount=c(20, 20, 20), time=c(0, 7, 14), type=c(1, 1, 1))))
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment$amount, c(20, 20, 15, 20))
  expect_equal(res$treatment$time, c(0, 7, 12, 14))
  expect_equal(res$treatment$admtype, c(1, 1, 1, 1))
  
  treatment = list(
    list(amount = 15, time = 12),
    data.frame(id = rep(1:3, each = 3), amount = rep(c(20, 50, 100), each = 3), time=rep(c(0, 7, 14), by = 3))
  )
  out_treatment <- data.frame(
    id = rep(1:3, each = 4),
    time=rep(c(0, 7, 12, 14), by = 3),
    amount = rep(c(20, 50, 100), each = 4),
    admtype = 1,
    washout = 0)
  out_treatment$amount[seq(3, 12, 4)] <- 15
  out_treatment$id <- factor(out_treatment$id)
  res <- simulx(model=model,
                parameter = param,
                output = out,
                treatment = treatment)
  expect_equal(res$treatment, out_treatment)
  
  treatment = list(
    list(amount = 15, time = 12),
    data.frame(amount = rep(c(20, 50, 100), each = 3), time=rep(c(0, 7, 14), by = 3), id = rep(1:3, each = 3))
  )
  expect_warning(res <- simulx(model=model,
                               parameter = param,
                               output = out,
                               treatment = treatment),
                 "Some columns have been ignored in treatment data : 'id'"
  )
})

test_that("treatment input of simulx : administration type", {
  model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  param <- c(V=10, k=0.2)
  out <- list(name='C', time=seq(0, 24 * 20, by=1))
  trt <- data.frame(id=rep(1:3, each=2),
                    time=0,
                    amount=10, 
                    type=rep(1:2, times=3))
  out_treatment <- cbind(trt, washout = 0)
  out_treatment$id <- factor(out_treatment$id)
  names(out_treatment) <- c("id", "time", "amount", "admtype", "washout")
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                treatment = trt)
  expect_equal(sim$treatment, out_treatment)
  
  trt <- data.frame(id=rep(1:3, each=2),
                    time=0,
                    amount=10)
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                treatment = trt)

  out_treatment <- cbind(trt, admtype = 1, washout = 0)
  out_treatment$id <- factor(out_treatment$id)
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                treatment = trt)
  expect_equal(sim$treatment, out_treatment)
  
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                treatment = list(time=0, type=2, amount=10))
  expect_equal(unique(sim$treatment$amount), 10)
  expect_equal(unique(sim$treatment$time), 0)
  expect_equal(unique(sim$treatment$admtype), 2)
  
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                treatment = list(time=0, amount=10))
  expect_equal(unique(sim$treatment$amount), 10)
  expect_equal(unique(sim$treatment$time), 0)
  expect_equal(unique(sim$treatment$admtype), 1)
  
})

test_that("treatment input of simulx : proba miss dose", {
  model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  param <- c(V=10, k=0.2)
  out <- list(name='C', time=seq(0, 24 * 20, by=1))
  trt <- data.frame(id=rep(1:3, each=2),
                    time=0,
                    amount=10, 
                    type=rep(1:2, times=3),
                    probaMissDose=rep(0:1, times=3))
  out_treatment <- cbind(trt, washout = 0)
  out_treatment <- out_treatment[out_treatment$probaMissDose != 1,]
  out_treatment$id <- factor(out_treatment$id)
  out_treatment <- out_treatment[names(out_treatment) != "probaMissDose"]
  row.names(out_treatment) <- NULL
  names(out_treatment) <- c("id", "time", "amount", "admtype", "washout")
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                treatment = trt)
  expect_equal(sim$treatment, out_treatment)
  
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                group = list(size=100),
                treatment = list(time=0, type=2, amount=10, probaMissDose=0.5))
  expect_equal(unique(sim$treatment$amount), 10)
  expect_equal(unique(sim$treatment$time), 0)
  expect_equal(unique(sim$treatment$admtype), 2)
  expect_true(nrow(sim$treatment) < 100)
  
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                group = list(size=100),
                treatment = list(time=c(0, 24), type=2, amount=10, probaMissDose=c(0, 0.5)))
  expect_equal(unique(sim$treatment$amount), 10)
  expect_equal(unique(sim$treatment$time), c(0, 24))
  expect_equal(unique(sim$treatment$admtype), 2)
  expect_true(nrow(sim$treatment[sim$treatment$time == 0,]) == 100)
  expect_true(nrow(sim$treatment[sim$treatment$time == 24,]) < 100)

})

test_that("treatment input of simulx : repeat", {
  model <- inlineModel("
  [LONGITUDINAL]
  input = {V, k}
  EQUATION:
  C = pkmodel(V,k)
  ")
  param <- c(V=10, k=0.2)
  out <- list(name='C', time=seq(0, 24 * 20, by=1))
  trt <- data.frame(id=rep(1:3, each=2),
                    time=0,
                    amount=10, 
                    type=rep(1:2, times=3),
                    probaMissDose=rep(0:1, times=3),
                    repeats=4)
  expect_warning(
    res <- simulx(model = model,
                  parameter = param,
                  output = out,
                  treatment = trt),
    "^Some columns have been ignored in treatment data"
  )

  expect_error(
    simulx(model = model,
           parameter = param,
           output = out,
           group = list(size=100),
           treatment = list(time=0, amount=10, repeats=0.5)),
    "^Invalid treatment repeats."
  )
  
  expect_error(
    simulx(model = model,
           parameter = param,
           output = out,
           group = list(size=100),
           treatment = list(time=0, amount=10, repeats=c(cycleDuration=24))),
    "^Invalid treatment repeats."
  )
  
  sim <- simulx(model = model,
                parameter = param,
                output = out,
                group = list(size=100),
                treatment = list(time=0, amount=10, repeats=c(cycleDuration=24, NumberOfRepetitions=3)))
  expect_equal(unique(sim$treatment$amount), 10)
  expect_equal(unique(sim$treatment$time), seq(0, 24 * 3, 24))

  sim <- simulx(model = model,
                parameter = param,
                output = out,
                group = list(size=100),
                treatment = list(time=c(0, 24), amount=10, type = c(1, 2), repeats=c(cycleDuration=48, NumberOfRepetitions=3)))
  expect_equal(unique(sim$treatment$amount), 10)
  expect_equal(unique(sim$treatment$admtype), c(1, 2))
  expect_equal(unique(sim$treatment$time), seq(0, 24 * 7, 24))

})

test_that("parameter input of simulx: covariate in parameters", {
  model <- inlineModel("
[COVARIATE]
input = {C1, C2}

[INDIVIDUAL]
input = {Cl_pop, omega_Cl, Q_pop, omega_Q, V1_pop, omega_V1, V2_pop, omega_V2, 
C1, C2, beta_Cl_C1, beta_Cl_C2, beta_V1_C1, corr_V1_Cl}

DEFINITION:
Cl = {distribution=logNormal, typical=Cl_pop, covariate={C1, C2}, coefficient={beta_Cl_C1, beta_Cl_C2}, sd=omega_Cl}
Q = {distribution=logNormal, typical=Q_pop, sd=omega_Q}
V1 = {distribution=logNormal, typical=V1_pop, covariate=C1, coefficient=beta_V1_C1, sd=omega_V1}
V2 = {distribution=logNormal, typical=V2_pop, sd=omega_V2}
correlation = {level=id, r(V1, Cl)=corr_V1_Cl}

[LONGITUDINAL]
input = {Cl, Q, V1, V2, a, b}

EQUATION:
Cc = pkmodel(k=Cl/V1, V=V1, k12=Q/V1, k21=Q/V2)
g = sqrt(a^2 + (b*Cc)^2)

DEFINITION:
y = {distribution=normal, prediction=Cc, sd=g}
")
  N <- 50
  
  t.y <- c(0.5, 2, 5, 8, 12, 16, 21)
  t.Cc <- seq(0, 24, by=0.2)
  
  D <- 1000
  
  p.pop <- c(Cl_pop=1, Q_pop=4, V1_pop=10, V2_pop=20)
  p.omega <- c(omega_Cl=0.3, omega_Q=0.2, omega_V1=0.3, omega_V2=0.2, corr_V1_Cl=0.5)
  p.beta <- c(beta_Cl_C1=0.7, beta_Cl_C2=1, beta_V1_C1=0.8)
  p.res <- c(a=2, b=0.15)
  
  popparam <- c(p.pop, p.omega, p.beta, p.res)
  
  ncov <- 2
  covariate <- data.frame(id=factor(1:N), matrix(rnorm(N*ncov)*0.2, ncol=ncov))
  names(covariate)[-1] <- paste0("C",1:ncov)
  
  expect_message(
    sim1 <- simulx(model=model, 
                   parameter=list(popparam, covariate),
                   treatment=list(time=0, amount=D),
                   output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "Using 'parameter' argument to define covariates is deprecated."
  )
  expect_equal(sim1$parameter[c("id", "C1", "C2")], covariate, tolerance=1e-4)
  expect_true(all(names(sim1$population) %in% names(popparam)))
  expect_equal(sim1$population[names(popparam)], popparam)
  
  sim2 <- simulx(model=model,
                 parameter=popparam,
                 covariate=covariate,
                 treatment=list(time=0, amount=D),
                 output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y)))
  expect_equal(sim2$parameter[c("id", "C1", "C2")], covariate, tolerance=1e-4)
  expect_true(all(names(sim2$population) %in% names(popparam)))
  expect_equal(sim2$population[names(popparam)], popparam)
  
  expect_warning(expect_message(
    sim1 <- simulx(model=model, 
                   parameter=list(popparam, c(C1 = 2)),
                   treatment=list(time=0, amount=D),
                   output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "Using 'parameter' argument to define covariates is deprecated."),
    "'C2' has not been specified. It will be set to 1."
  )
  expect_equal(sim1$parameter[c("C1", "C2")], c(C1 = 2, C2 = 1))

  expect_warning(
    sim2 <- simulx(model=model, 
                   parameter=popparam,
                   covariate=c(C1 = 2),
                   treatment=list(time=0,  amount=D),
                   output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "'C2' has not been specified. It will be set to 1."
  )
  expect_equal(sim2$parameter[c("C1", "C2")], c(C1 = 2, C2 = 1))

  expect_message(
    sim1 <- simulx(model=model, 
                   parameter=list(popparam, c(C1 = 2, C2 = 3)),
                   treatment=list(time=0, amount=D),
                   output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "Using 'parameter' argument to define covariates is deprecated."
  )
  expect_equal(sim1$parameter[c("C1", "C2")], c(C1 = 2, C2 = 3))
  
  sim2 <- simulx(model=model, 
                 parameter=popparam,
                 covariate=c(C1 = 2, C2 = 3),
                 treatment=list(time=0, amount=D),
                 output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y)))
  expect_equal(sim2$parameter[c("C1", "C2")], c(C1 = 2, C2 = 3))

  expect_message(
    sim3 <- simulx(model=model,
                   parameter=list(popparam, c(C1 = 2)),
                   covariate=c(C2 = 3),
                   treatment=list(time=0, amount=D),
                   output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "Using 'parameter' argument to define covariates is deprecated."
  )
  expect_equal(sim1$parameter[c("C1", "C2")], c(C1 = 2, C2 = 3))

  expect_warning(
    expect_message(
      sim1 <- simulx(model=model,
                     parameter=covariate,
                     treatment=list(time=0, amount=D),
                     output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "Using 'parameter' argument to define covariates is deprecated."),
    "^Parameter has not been defined."
  )
  expect_equal(sim1$parameter[c("id", "C1", "C2")], covariate, tolerance=1e-4)
  expect_true(all(sim1$population == 1))

  model <- inlineModel("
[COVARIATE]
input = {WT}

[INDIVIDUAL]
input = {WT, ka_pop, omega_ka, Cl_pop, omega_Cl, beta_WT_Cl, V1_pop, omega_V1, Q_pop, omega_Q, V2_pop, omega_V2, BL_pop, omega_BL, IC50_pop, omega_IC50}

EQUATION:
Cl_pred = Cl_pop *(WT/70)^beta_WT_Cl

DEFINITION:
ka   = {distribution=lognormal, typical=ka_pop, sd=omega_ka}
Cl   = {distribution=lognormal, typical=Cl_pred,sd=omega_Cl}
V1   = {distribution=lognormal, typical=V1_pop, sd=omega_V1}
Q    = {distribution=lognormal, typical= Q_pop, sd=omega_Q}
V2   = {distribution=lognormal, typical=V2_pop, sd=omega_V2}
BL   = {distribution=lognormal, typical=BL_pop, sd=omega_BL}
IC50 = {distribution=lognormal, typical=IC50_pop, sd=omega_IC50}


[LONGITUDINAL]
input = {ka, Cl, V1, Q, V2, BL, IC50, b1, b2}

EQUATION:
V = V1
k = Cl/V1
k12 = Q/V1
k21 = Q/V2

Cc = pkmodel(ka,V, k, k12, k21)

PD = (1 - Cc/(Cc+IC50))* BL

DEFINITION:
y1 = {distribution=normal, prediction=Cc, errorModel=proportional(b1)}
y2 = {distribution=normal, prediction=PD, errorModel=proportional(b2)}")
  
  # define the parameter values
  param <- c(ka_pop=0.398, omega_ka=0.15, Cl_pop=10.8, omega_Cl=0.389,
             beta_WT_Cl=1.1, V1_pop=18.5, omega_V1=0.448, Q_pop=6.7, omega_Q=0.288,
             V2_pop=64.4, omega_V2=0.333, BL_pop=98, omega_BL=0.0893,
             IC50_pop=81.4, omega_IC50=0.278, b1=0.443, b2=0.19)
  # number of individuals to simulate 
  N <- 30
  # generating the covariates as data frame with column id and one column per covariate
  cov <- data.frame(id=1:N,
                    WT=sample(x=c(60,70,45,82), size=N, replace=T))
  # define treatmet 100 mg QD 
  trt <- list(amount=100000, time=seq(0,96,by=24))
  # define what to output
  outCc <- list(name='Cc', time=seq(0,96,by=0.5))
  outPD <- list(name='PD', time=seq(0,96,by=0.5))
  # simulate
  expect_message(
    res1 <- simulx(model = model,
                   parameter = list(param, cov),
                   group = list(size=N),
                   output = list(outCc, outPD),
                   treatment = trt),
    "Using 'parameter' argument to define covariates is deprecated."
  )
  res2 <- simulx(model = model,
                 parameter = param,
                 covariate = cov,
                 group = list(size=N),
                 output = list(outCc, outPD),
                 treatment = trt)
  out_cov <- cov
  out_cov$id <- factor(out_cov$id)
  out_trt <- data.frame(id = factor(rep(1:N, each = length(trt$time))),
                        time = rep(trt$time, by = N),
                        amount = trt$amount,
                        admtype = 1, washout = 0)

  expect_equal(res1$population, param)
  expect_equal(res1$parameter[c("id", "WT")], out_cov)
  expect_length(unique(res1$Cc$id), N)
  expect_equal(res1$treatment, out_trt)
  
  expect_equal(res2$population, param)
  expect_equal(res2$parameter[c("id", "WT")], out_cov)
  expect_length(unique(res2$Cc$id), N)
  expect_equal(res2$treatment, out_trt)
  

})

test_that("groups with covariates", {
  model <- inlineModel("
[COVARIATE]
input = {C1, C2}

[INDIVIDUAL]
input = {Cl_pop, omega_Cl, Q_pop, omega_Q, V1_pop, omega_V1, V2_pop, omega_V2, 
C1, C2, beta_Cl_C1, beta_Cl_C2, beta_V1_C1, corr_V1_Cl}

DEFINITION:
Cl = {distribution=logNormal, typical=Cl_pop, covariate={C1, C2}, coefficient={beta_Cl_C1, beta_Cl_C2}, sd=omega_Cl}
Q = {distribution=logNormal, typical=Q_pop, sd=omega_Q}
V1 = {distribution=logNormal, typical=V1_pop, covariate=C1, coefficient=beta_V1_C1, sd=omega_V1}
V2 = {distribution=logNormal, typical=V2_pop, sd=omega_V2}
correlation = {level=id, r(V1, Cl)=corr_V1_Cl}

[LONGITUDINAL]
input = {Cl, Q, V1, V2, a, b}

EQUATION:
Cc = pkmodel(k=Cl/V1, V=V1, k12=Q/V1, k21=Q/V2)
g = sqrt(a^2 + (b*Cc)^2)

DEFINITION:
y = {distribution=normal, prediction=Cc, sd=g}
")
  
  t.y <- c(0.5, 2, 5, 8, 12, 16, 21)
  t.Cc <- seq(0, 24, by=0.2)
  
  D <- 1000
  
  pop_g1 <- c(Cl_pop=1, Q_pop=4, V1_pop=10, V2_pop=20)
  pop_g2 <- c(Cl_pop=1, Q_pop=6, V1_pop=10, V2_pop=20)
  omega <- c(omega_Cl=0.3, omega_Q=0.2, omega_V1=0.3, omega_V2=0.2, corr_V1_Cl=0.5)
  beta <- c(beta_Cl_C1=0.7, beta_Cl_C2=1, beta_V1_C1=0.8)
  res <- c(a=2, b=0.15)

  popparam_g1 <- c(pop_g1, omega, beta, res)
  popparam_g2 <- c(pop_g2, omega, beta, res)
  
  ncov <- 2
  ng1 <- 50
  ng2 <- 100
  
  covariate_g1 <- data.frame(id=factor(1:ng1), matrix(rnorm(ncov*ncov)*0.2, ncol=ncov))
  names(covariate_g1)[-1] <- paste0("C",1:ncov)

  covariate_g2 <- data.frame(id=factor(1:ng2), matrix(rnorm(ncov*ncov)*0.2, ncol=ncov))
  names(covariate_g2)[-1] <- paste0("C",1:ncov)
  
  expect_warning(
    expect_message(
      sim <- simulx(model=model, 
                  parameter=list(popparam_g1),
                  group=list(list(parameter=covariate_g1), list(parameter=covariate_g2)),
                  treatment=list(time=0, amount=D),
                  output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "Using 'parameter' argument to define covariates is deprecated."
    ),
    "parameter has been defined in both shared group and groups. parameter defined in shared group will be erased."
  )
  expect_equal(sim$population, popparam_g1[names(sim$population)])
  out_param_g1 <- sim$parameter[sim$parameter$group == 1,]
  out_param_g2 <- sim$parameter[sim$parameter$group == 2,]
  row.names(out_param_g1) <- NULL
  row.names(out_param_g2) <- NULL

  expect_equal(out_param_g1[c("C1", "C2")], covariate_g1[c("C1", "C2")], tolerance = 1e-4)
  expect_equal(out_param_g2[c("C1", "C2")], covariate_g2[c("C1", "C2")], tolerance = 1e-4)
  
  expect_message(
    sim <- simulx(model=model, 
                  group=list(list(parameter=list(popparam_g1, covariate_g1)), list(parameter=list(popparam_g2, covariate_g2))),
                  treatment=list(time=0, amount=D),
                  output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y))),
    "Using 'parameter' argument to define covariates is deprecated."
  )
  out_param_g1 <- sim$parameter[sim$parameter$group == 1,]
  out_param_g2 <- sim$parameter[sim$parameter$group == 2,]
  row.names(out_param_g1) <- NULL
  row.names(out_param_g2) <- NULL

  expect_equal(unlist(sim$population[sim$population$group == 1, names(popparam_g1)]), popparam_g1)
  expect_equal(unlist(sim$population[sim$population$group == 2, names(popparam_g2)]), popparam_g2)
  expect_equal(out_param_g1[c("C1", "C2")], covariate_g1[c("C1", "C2")], tolerance = 1e-4)
  expect_equal(out_param_g2[c("C1", "C2")], covariate_g2[c("C1", "C2")], tolerance = 1e-4)
  
  sim <- simulx(model=model, 
                parameter=list(popparam_g1),
                group=list(list(covariate=covariate_g1), list(covariate=covariate_g2)),
                treatment=list(time=0, amount=D),
                output=list(list(name="Cc", time=t.Cc), list(name="y", time=t.y)))
  
  expect_equal(sim$population, popparam_g1[names(sim$population)])
  out_param_g1 <- sim$parameter[sim$parameter$group == 1,]
  out_param_g2 <- sim$parameter[sim$parameter$group == 2,]
  row.names(out_param_g1) <- NULL
  row.names(out_param_g2) <- NULL
  
  expect_equal(out_param_g1[c("C1", "C2")], covariate_g1[c("C1", "C2")], tolerance = 1e-4)
  expect_equal(out_param_g2[c("C1", "C2")], covariate_g2[c("C1", "C2")], tolerance = 1e-4)
  
})