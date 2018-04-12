
# Mualem unsaturated hydraulic conductivity function (Mualem 1976) calculates K(h) based on saturated conductivity
# Mualem unsaturated hydraulic conductivity parameters
# P       -- pressure head (cm)
# L       -- corresponds to the tortuosity, often set at 1/2
# Ko      -- saturated hydraulic conductivity (can be any unit)

# Van Genuchten water retention parameters
# alpha   -- corresponds to the inverse of the air entry value (1/cm)
# n       -- relates to the pore size distribution
#' A Function
#'
#' @param parameters here.
#' @keywords water retention saturated hydraulic conductivity
#' @export Mualem
#' @examples
#'

Mualem = function(P, alpha,n,Ko,L){
     m = 1-1/n
     Se = vG(P,alpha,n)

     Ko * Se^L * ( 1 - ( 1-Se^(1/m)  )^m   )^2


}
