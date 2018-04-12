#'Calculates an Genuchten Model based on parameters requires Van Genucten equation (vG) as well
#'
#' @param fit  - data frame with theta.s theta.r alpha and n
#' @param P  - is pressure head
#' @keywords water retention saturated hydraulic conductivity
#' @export predict.vG
#' @examples
#'
predict.vG = function(P,fit){
     theta.s = fit$theta.s[1]
     theta.r = fit$theta.r[1]
     alpha   = fit$alpha[1]
     n       = fit$n[1]


     theta = vG(P,alpha,n)*(theta.s-theta.r)+theta.r
     return(theta)
}
