#'Pore DF function
#'
#' @param r parameter
#' @param alpha parameter
#' @param n parameter
#' @keywords some keywords
#' @export PoreDF
#' @examples
#'
PoreDF = function(r,alpha,n){
     sigma =0.071           # (N/m)
     alpha_SI = alpha*1e-5  # (1/Pa)
     m =1-1/n
     poredf= (m*n) * ((alpha_SI*sigma/r)^n) * (r^(-1)) * (1+(alpha_SI*sigma/r)^n)^(-(1+m))
     return(poredf)
}
