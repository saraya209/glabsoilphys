
#
#'A Function to calculate effective saturation
#'
#' @param theta   -- volumetric water content (cm^3 cm^(-3))
#' @param theta.s -- saturated volumetric water content (cm^3 cm^{-3})
#' @param theta.r -- residual water content (cm^3 cm^(-3))
#' @param SE      -- effective saturation (unitless) plugs into water retention functions
#' @keywords water retention, saturated hydraulic conductivity
#' @export SE
#' @examples
#'
#'
SE     = function(theta,theta.s,theta.r){ # note that theta S is not equal to porosity, it is typically 85-90% of porosity unless soil was wetted under very specific conditions (Kosugi etal 2002)
     SE = (theta-theta.r)/(theta.s-theta.r)
     return(SE)
}
