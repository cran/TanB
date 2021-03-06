#' The cumulative function of the Tangent Burr XII probability distribution.
#' @export
#'
#' @param q vector of quantiles.
#' @param c C parameter.
#' @param k K parameter.
#' @param s S parameter.
#' @param lower Lower parameter.
#' @param log.p Log.p parameter.
#' @return A vector with n observations of the Tangent Burr XII distribution.
#' @examples
#' ptanb(0.5, 32.5, 3, 3.5, TRUE, FALSE)
#' ptanb(0.5, 2, 3, 3, TRUE, FALSE)

ptanb<-function(q,c,k,s,lower = TRUE,log.p = FALSE){
  if (log.p == TRUE) {
    if (lower == TRUE){
      log(tan((pi/4)*(1-(1+(q/s)^c)^(-k))))
    }else{
      log((1 - tan((pi/4)*(1-(1+(q/s)^c)^(-k)))))
    }
  } else {
    if (lower == TRUE){
      tan((pi/4)*(1-(1+(q/s)^c)^(-k)))
    }else{
      (1 - tan((pi/4)*(1-(1+(q/s)^c)^(-k))))
    }
  }

}
