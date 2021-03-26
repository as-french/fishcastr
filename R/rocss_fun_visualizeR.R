#' @title Compute the ROC Area Skill Score and the significance of the Area
#'   under the Curve
#' @description Computes the skill score for the area under the ROC curve
#'   compared to an arbitrary reference forecast.
#' @param obs A binary observation (code: 0, 1)
#' @param pred A probability prediction on the interval (0,1)
#' @param conf.level Confidence level to compute the score significance, by
#'   default conf.level=0.95
#' @return The ROC area skill score and the significance (TRUE or FALSE)
#' @author M. D. Frias \email{mariadolores.frias@@unican.es} and J. Fernandez
#' @importFrom SpecsVerification Auc
#' @importFrom stats qnorm complete.cases
#' @export
rocss_fun_visualizeR <- function (obs, pred, conf.level = 0.95){
  no.nan <- complete.cases(obs, pred)
  if (sum(no.nan)==0){
    rval <- list(score.val = NaN, sig = NaN)
  } else{
    alfa <- 1-conf.level
    z <- qnorm(1-alfa/2)
    auc.val <- Auc(pred[no.nan], obs[no.nan], handle.na = "only.complete.pairs")
    sig <- auc.val[[1]] - z*auc.val[[2]] > 0.5
    rval <- list(score.val = auc.val[[1]]*2-1, sig = sig)
  }
  return(rval)
}
