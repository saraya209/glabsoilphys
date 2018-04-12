#'Fits van Genuchten Model
#'
#' @param theta moisture content cm_3
#' @param theta.s saturated moisture content cm^3^ cm^{-3}^
#' @keywords water retention saturated hydraulic conductivity
#' @export fit.vG
#' @examples
#'
fit.vG = function(theta,P,theta.s=-999,theta.r=-999,alpha=-999,n=-999){
     # REQUIRES
     # - minpack.lm | Library
     # - vG | Function

     # Initialize starting guess and bounds of free parameters
     s.par = c(theta.s=0.5,theta.r=0.01,alpha=0.01,n=5)
     l.par = c(theta.s=0.1,theta.r=0.001,alpha=0.000001,n=1.05)
     u.par = c(theta.s=0.9,theta.r=0.3,alpha=100,n=10)
     free = c()

     if(theta.s==-999) {free = append(free,"theta.s")}
     if(theta.r==-999) {free = append(free,"theta.r")}
     if(alpha  ==-999) {free = append(free,"alpha")}
     if(n      ==-999) {free = append(free,"n")}

     s.par = s.par[free]
     l.par = l.par[free]
     u.par = u.par[free]




     # If parameter is supplied, assume it is known and remove from free parameters
     # THE FOLLOWING WORKED IF ONLY PARAMETER WAS KNOWN
     #   if(theta.s!=-999){
     #     s.par = s.par[-1]
     #     l.par = l.par[-1]
     #     u.par = u.par[-1]
     #   }
     #   if(theta.r!=-999){
     #     s.par = s.par[-2]
     #     l.par = l.par[-2]
     #     u.par = u.par[-2]
     #   }
     #   if(alpha!=-999){
     #     s.par = s.par[-3]
     #     l.par = l.par[-3]
     #     u.par = u.par[-3]
     #   }
     #   if(n!=-999){
     #     s.par = s.par[-4]
     #     l.par = l.par[-4]
     #     u.par = u.par[-4]
     #   }

     # Setup Objective Function
     fit = nlsLM( theta ~ vG(P,alpha,n)*(theta.s-theta.r)+theta.r,  start=s.par,lower=l.par, upper=u.par,trace=FALSE  )

     # Summarize fitted results. Out put a list of:
     # 1. all parameters (so that predict function can use them all regardless of whether fitted or not)
     # 2. Statistics of fitted parameters
     param = coef(summary(fit))
     param = data.frame(t(param))

     if(theta.s!=-999){
          param$theta.s = c(theta.s,NA,NA,NA)
     }
     if(theta.r!=-999){
          param$theta.r = c(theta.r,NA,NA,NA)
     }
     if(alpha!=-999){
          param$alpha = c(alpha,NA,NA,NA)
     }
     if(n!=-999){
          param$n = c(n,NA,NA,NA)
     }
     # 3. Number of iterations and SSE




     return(param)
}
