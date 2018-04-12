# Van Genuchten Function (Van Genuchten, 1980, eq 3.) returns unitless water content
# alpha   -- units of 1/cm
# P       -- water suction in units of cm
# n       -- describes pore size distribution
# m       -- function of n

#'A Function
#'
#' @param parameters here.
#' @keywords water retention saturated hydraulic conductivity
#' @export vG
#' @examples hi

vG = function(P,alpha,n){
     m =1-1/n
     vg=  (1+(alpha*P)^n)^(-m)
     return(vg)
}
