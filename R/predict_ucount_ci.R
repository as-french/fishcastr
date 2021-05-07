#' Generate simulation based confidence intervals from glmmTMB::glmmTMB model
#'
#' @description This function generates predictions (conditional (on fixed
#'   effect) means) and associated uncertainty intervals from a fitted
#'   glmmTMB::glmmTMB model object. See Brooks et al. (2017) Appendix B and/or
#'   Bolker (2008) for details on population prediction intervals generated
#'   using mvrnorm. Predications are "unconditional", which means that the
#'   variance in random effect parameter estimate is ignored, while the variance
#'   in fixed effects parameters is taken into account.
#'
#' @param model A glmmTMB::glmmTMB object.
#' @param dataset A data.frame with names matching those in formula of object.
#' @param interval A concatenated list; e.g., c(0.025, 0.975). These percentiles
#'   are extracted using quantiles().
#' @return A list of four vectors: pred.ucount.psim, single simulation from
#'   mvrnorm; mean.count, the effective "conditional" (on fixed effects, but not
#'   random effects) mean of 999 mvrnorm simulations; ci.ucount_lwr, the lower
#'   interval percentile (e.g., 0.025); ci.ucount_upr, the upper interval
#'   percentile (e.g., 0.975) of 999 mvrnorm simulations.
#' @references
#' Brooks, M. E., Kristensen, K., van Benthem, K. J., Magnusson, A., Berg, C.
#' W., Nielsen, A., Skaug, H. J., Maechler, M., & Bolker, B. M. (2017). Modeling
#' Zero-Inflated Count Data With glmmTMB. https://doi.org/10.1101/132753
#'
#' Brooks, M. (2017). Appendix B: Salamander Example Comparing GLMMs,
#' Zero-Inflated GLMMs, and Hurdle Models. 11.
#'
#' Bolker, B. M. (2008). 7.5 Estimating confidence limits of functions of
#' parameters. In Ecological models and data in R (p. 332). Princeton Univ.
#' Press.
#' @examples
#' \dontrun{
#' mTrain_genpois <- readRDS(paste0(system.file("vignettes", package = "fishcastr"),
#'                "/vignette_data/mTrain_genpois_salmon_2019.rds"))
#' newdata <- mTrain_genpois$frame
#' newdata <- newdata[newdata$salmonid_year %in% c(2012:2013),]
#' result <- predict_ucount_ci(model = mTrain_genpois,
#'                             interval = c(0.025,0.975),
#'                             dataset = newdata)
#'                             }
#' @export
predict_ucount_ci <- function(model,
                              interval,
                              dataset){

  # extract if zero inflated or not
  zero_inf_y_n <- formula(model, component = c("zi"))
  zinf <- ifelse(zero_inf_y_n == "~0",
                 FALSE,
                 TRUE)

  # extract if dispersion parameter is modelled by (/a function of) fixed effects
  dispform_y_n <- formula(model, component = c("disp"))
  disp_form <- ifelse(dispform_y_n == "~0",
                      "zero",ifelse(dispform_y_n == "~1","one","modelled"))

  ##### edited on 14-12-2020
  if(zinf == FALSE & disp_form %in% c("one","modelled")){

    # conditional model
    cond_form <- formula(model,component = "cond",fixed.only = TRUE)
    X.cond <- model.matrix(object = lme4::nobars(formula(x = model,
                                                        component = "cond",
                                                        fixed.only = TRUE)),
                           data = model.frame(formula = cond_form,
                                              data = dataset,
                                              na.action=na.pass))
    beta.cond <- glmmTMB::fixef(model)$cond
    pred.condpar.psim <- MASS::mvrnorm(999,mu=beta.cond,
                                       Sigma=vcov(model)$cond)
    pred.cond.psim <- X.cond %*% t(pred.condpar.psim)

    # might be useful...
    # # dispersion parameter estimate (on log scale)
    # beta.cond <- glmmTMB::fixef(model)[["disp"]]
    #
    # #extract standard deviation of disp param
    # ###############
    # # check back calculation method is appropriate by looking at another model param...
    # # # check vcov
    # # p1 <- vcov(model)
    # # # variance of intercept = 0.0795074321 (back calculate from wald cis..)
    # # conf_mod <- confint(object = model,method = "wald",full = TRUE)["cond.(Intercept)",]
    # # # convert to standard deviation
    # # sd_disp = (conf_mod[["97.5 %"]] - conf_mod[["Estimate"]])/1.96
    # # sd_disp^2 # 0.07950451 (as above!)
    # ###############
    #
    # confint_disp <- confint(object = model,method = "wald",full = TRUE)["sigma",]
    # # convert to standard deviation on log scale
    # sd_disp = log((confint_disp[["97.5 %"]] - confint_disp[["Estimate"]])/1.96)
    #
    # pred.disp.psim <- rnorm(999,mean=beta.cond,
    #                                    sd=sd_disp)
    #
    # dispformula <- model$modelInfo$allForm$dispformula
    # X.disp <- model.matrix(object = lme4::nobars(dispformula),
    #                      model.frame(~ ., dataset, na.action=na.pass))
    # pred.disp.psim <- X.disp %*% t(pred.disp.psim)

    pred.ucount.psim <- exp(pred.cond.psim)
    ci.ucount_lwr <- as.vector(round(t(apply(pred.ucount.psim,1,quantile,c(interval[1]), na.rm = TRUE))))
    ci.ucount_upr <- as.vector(round(t(apply(pred.ucount.psim,1,quantile,c(interval[2]), na.rm = TRUE))))
    mean.count <- as.vector(round(t(apply(pred.ucount.psim,1,mean))))
    #ci.ucount_upr[1:365]
    # extract just the first column of simulated count means
    return(list(pred.ucount.psim = round(as.vector(pred.ucount.psim[,1])),
                mean.count = mean.count,
                ci.ucount_lwr = ci.ucount_lwr,
                ci.ucount_upr = ci.ucount_upr))
  }
  ######

  if(zinf == TRUE){
  #set.seed(12345)
  X.cond <- model.matrix(object = lme4::nobars(formula(model)[-2]),
                         data = stats::model.frame(~ ., dataset, na.action=na.pass))
  beta.cond <- glmmTMB::fixef(model)$cond
  pred.condpar.psim <- MASS::mvrnorm(999,mu=beta.cond,
                                     Sigma=vcov(model)$cond)
  pred.cond.psim <- X.cond %*% t(pred.condpar.psim)
  beta.zi <- glmmTMB::fixef(model)$zi

  pred.zipar.psim <- MASS::mvrnorm(999,mu=beta.zi,Sigma=vcov(model)$zi)
  ziformula <- model$modelInfo$allForm$ziformula
  X.zi <- model.matrix(object = lme4::nobars(ziformula),
                       model.frame(~ ., dataset, na.action=na.pass))
  pred.zi.psim <- X.zi %*% t(pred.zipar.psim)
  pred.ucount.psim <- exp(pred.cond.psim)*(1-plogis(pred.zi.psim))
  ci.ucount_lwr <- as.vector(round(t(apply(pred.ucount.psim,1,quantile,c(interval[1]), na.rm = TRUE))))
  ci.ucount_upr <- as.vector(round(t(apply(pred.ucount.psim,1,quantile,c(interval[2]), na.rm = TRUE))))
  mean.count <- as.vector(round(t(apply(pred.ucount.psim,1,mean))))
  # extract just the first column of simulated count means
  return(list(pred.ucount.psim = round(as.vector(pred.ucount.psim[,1])),
              mean.count = mean.count,
              ci.ucount_lwr = ci.ucount_lwr,
              ci.ucount_upr = ci.ucount_upr))
  }
}
