
# ROXYGEN Documentation
#' PK predictions using linear equations
#' @description Collection of linear PK function for 1, 2, and 3 compartment distribution structures and various input functions. Further a collection of effect relationships feeding on concentration from the PK functions. Can be used in isolation but ideally embedded within superposition function \code{pk.pred} and plotting function \code{pkpdPredPlot}
#' @param dose vector of dose amounts (individual pk.xxx functions)
#' @param doses vector of dose amounts (with pk.pred multiple dose wrapper)
#' @param t.doses vector of dose times. Needs to have same length as doses 
#' @param tob vector of time observations  (individual pk.xxx functions)
#' @param t.obs vector of observation times relative to time of first dose (with pk.pred multiple dose wrapper)
#' @param pk.func function object predicting concentration as a function of time, given	dose and pk parameters 
# @param e.func function object predicting effects
#' @param parms vector of pk parameter values. This can but is not required to be a named vector.
#' @return PK predictions, PK plots
#' @export pk.pred pk.1comp.iv pk.1comp.0abs pk.1comp.1abs pk.1comp.1abs.ss pk.1comp.10abs pk.1comp.lag pk.2comp.iv pk.2comp.0abs pk.2comp.1abs pk.2comp.1abs.m pk.2comp.1abs.ss pk.3comp.iv pk.3comp.iv.ss pk.3comp.1abs pk.3comp.1abs.ss eff.1comp.iv eff.1comp.1abs eff.1comp.1abs.ss eff.2comp.iv eff.2comp.1abs eff.2comp.1abs.ss eff.3comp.1abs
#' @examples 
#' # 1comp elimination, 1st-order absorption
#' pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7)
#'    , pk.func = pk.1comp.1abs, parms = c(1,25, 0.1, 5,0.5)
#' )
#' 
#' # 2comp elimination, 0-order absoorption
#' pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7)
#'    , pk.func = pk.2comp.0abs, parms = c(10,30, 3, 90,10), log = TRUE
#' )
#' 
#' ## demo effect prediction after single and multiple doses
#' eff.1comp.1abs(dose = 100, tob = seq(0,24*7), parms = c(1,10, 0.25, 0.05))
#' pk.pred(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7)
#'    , pk.func = eff.1comp.iv, parms = c(1,10, 0.25)
#' )
#' pkpdPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7)
#'    , pk.func = pk.1comp.1abs, e.func = eff.1comp.1abs
#'    , parms = c(1,10, 0.25, 0.05)
#' )

pk.pred = function(doses, t.doses, t.obs, pk.func, parms){
	# Function performs superposition for series of observation times given a 
      # series of doses
	# Function inputs:
	#	doses		= vector of dose amounts
	#	t.doses	= vector of dose times. Needs to have same length as doses
	#	t.obs		= vector of observation times relative to time of first dose
	#	pk.func	= function object predicting concentration as a function of time, given
	#					dose and pk parameters
	#	parms		= vector of pk parameter values
	
	# check length of times vs. number of doses
	if(length(doses) != length(t.doses)) stop("Need same number of doses and dose times")

	# find difference between observation time and dose time for each dose
	t.star = apply(matrix(t.obs, ncol=1), 1 
						, function(tob, td) ifelse(tob >= td, tob-td, 0)
						, t.doses)
						
	# Contribution from each dose
	dose.cont = (t.star>0) * pk.func(doses, t.star, parms)
	
	# dose.cont is a matrix, columns are times, rows are individual doses
	dose.cont = matrix(dose.cont, nrow=length(t.doses))
	
	# Apply superposition -- total concentration is sum of contributions of each
	# dose at each time
	return(apply(dose.cont, 2, sum))
		
} # pk.pred

#_______________________________________________________________________________
# 1- Compartment Models
# References: Gabrielsson & Weiner 2nd ed., pp 
#             Gibaldi & Perrier, 2nd ed., pp 33-42, 128-142
#             Shargel & Wu, 4th ed., pp 434

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.1comp.iv = function(dose, tob, parms){
	# 1-compartment model with iv dosing
	cl	= parms[1]
	v	= parms[2]
	
	kel = cl / v
	conc = (tob>0)*dose/v * exp(-kel*tob)
} # pk.1comp.iv

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.1comp.0abs = function(dose, tob, parms){
	# 1 compartment model with 0-order absorprtion
	cl	= parms[1]
	v	= parms[2]
	k0	= parms[3]		# infusion rate
	
	kel = cl/v
	dur = dose/k0		# infusion duration
	
	# Concentration at end of infusion
	c.end = k0/v/kel * (1 - exp(-kel*dur))
	
	# Total concentration
	conc = (tob<=dur)* k0/v/kel *(1 - exp(-kel*tob)) +
			   (tob>dur) * c.end * exp(-kel*(tob - dur))
	
	return(conc) 
} # pk.1comp.0abs
#pk.1comp.0abs(100, tob = seq(0,140), parms=  c(1,3,1))

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.1comp.1abs = function(dose, tob, parms){
	# 1-compartment model with 1st-order absorption
	cl 		= parms[1]
	v		= parms[2]
	ka		= parms[3]

	kel = cl / v
	return((tob>0) * dose * ka/v/(ka-kel) * (exp(-kel*tob) - exp(-ka*tob)))
} # pk.1comp.1abs
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.1comp.1abs, parms = c(.5, 25, 1))
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.1comp.1abs.ss = function(dose, tob, parms){
	# Steady-state concentration during the dosing interval for 1-compartment
	# model with 1st-order absorption
	# Note that tob is time relative to start of dosing interval, and the diosing
	# inteval, tau, is one of the parameters given in parm
	
	cl		= parms[1]
	v  		= parms[2]
	ka		= parms[3]
	tau		= parms[4]
	
	k10 = cl/v
	
	# Calculate time relative to start of current time last dose time
	tst = tob - floor(tob/tau)*tau # allow
	
	# concentration
	(tst>0) * (dose*ka/v/(ka-k10)) * (exp(-k10*tst)/(1-exp(-k10*tau)) - 
            exp(-ka*tst)/(1-exp(-ka*tau)))
}	# pk.1comp.1abs.ss
pk.1comp.1abs.ss(100, seq(0.1,23.99,length=100), c(1,25,0.5,24))

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.1comp.10abs = function(dose, tob, parms){
  # 1- comparment PK, plus combination of 1st and 0-order absorption
  # Refs:
	cl	= parms[1]
	v	= parms[2]
	ka	= parms[3]		# 1st-order absorption constant
	k0	= parms[4]		# 0-order absorption rate
	f	= parms[5]		# fraction of dose absorbed by 0-order absorption
	
	kel = cl/v
	
	dur = (1-f)*dose / k0	# duration of apparent infusion
	c.end = (1-f) * k0/v/kel * (1 - exp(-kel*dur)) # Concentration at end of infusion
	conc = f*dose*ka/v/(ka-kel) * (exp(-kel*tob) - exp(-ka*tob)) +
	       (1-f)*((tob<=dur)*k0/v/kel*(1-exp(-kel*tob))) +
	       (tob>dur)*c.end*exp(-kel*(tob-dur))
               
  return(conc)
}

if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.1comp.1abs, parms = c(1,25, 0.1, 5,0.5))
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.1comp.lag = function(dose, tob, parms) {
  # 1-compartment PK with lagged first-order absorption
  # Refs:
	cl	= parms[1]
	v	= parms[2]
	ka	= parms[3]		# 1st-order absorption constant
	tlag	= parms[4]		# lag-time
	
	kel = cl/v
	
	tst = tob - tlag
	(tst > 0)*dose*ka/v/(ka-kel)*(exp(-kel*tst) - exp(-ka*tst))
} # pk.1comp.lag
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.1comp.lag, parms = c(1,25, 0.1, 2))
}

#_______________________________________________________________________________
# 2-Compartment Models
# References: Gabrielsson and Weiner, 2nd ed. 86-98
#             Gibaldi and Perrier, 2nd ed. 65-67, 132-142
#             Beal, Sheiner NONMEM Manual
#             

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.2comp.iv = function(dose, tob, parms){
  # 2-compartment model with IV bolus dosing
  cl	= parms[1]  # Central clearance
  v1	= parms[2]  # Central volume  
  q	= parms[3]  # Intercompartmental clearance
  v2	= parms[4]  # Peripheral volume
  
  # define macro-parameters
  k10		= cl / v1
  k12		= q  / v1
  k21		= q  / v2
  beta 	= 0.5 * (k12 + k21 + k10 - sqrt((k12 + k21 + k10)^2 - 4*k10*k21))
  alpha 	= k21 * k10 / beta
  
	# Return concentration
	conc = (dose/v1)*((k21 - alpha)*exp(-alpha*tob) / (beta - alpha) +
           (k21 - beta)*exp(-beta*tob) / (alpha - beta))
	return(conc)
} # pk.2comp.iv
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.2comp.iv, parms = c(10,30, 3, 90), log = TRUE)
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.2comp.0abs = function(dose, tob, parms){
  # 2-compartment model with 0-order infusion
  cl  = parms[1]  # Central clearance
  v1  = parms[2]  # Central volume  
  q   = parms[3]  # Intercompartmental clearance
  v2  = parms[4]  # Peripheral volume
  k0  = parms[5]  # Infusion rate
  
   # define macro-parameters
  k10 = cl  / v1
  k12 = q   / v1
  k21 = q   / v2
  beta = 0.5 * (k12 + k21 + k10 - sqrt((k12 + k21 + k10)^2 - 4*k10*k21))
  alpha = k21 * k10 / beta
 
  A = (k21-alpha) / (beta-alpha)
  B = (k21-beta) / (alpha-beta)
  
  dur = dose/k0   # duration of infusion
  
  # Individual concentration contributions
  t1  = (1/k10 - A/alpha * exp(-alpha*tob) - B/beta * exp(-beta*tob))
  t2  = (1 - exp(-alpha*dur)) * A/alpha * exp(-alpha*(tob-dur)) +
        (1 - exp(-beta*dur)) * B/beta * exp(-beta*(tob-dur))
  
  # Concentration is sum of contributions due to t1 and t2
  return((tob <= dur)*k0/v1*t1 + (tob > dur)*k0/v1*t2)
} ## pk.2comp.0abs
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.2comp.0abs, parms = c(10,30, 3, 90,10), log = TRUE)
}
 
# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.2comp.1abs = function(dose, tob, parms){
  # 2-compartment model with 1st-order absorption
  cl  = parms[1]  # Central clearance
  v1  = parms[2]  # Central volume  
  q   = parms[3]  # Intercompartmental clearance
  v2  = parms[4]  # Peripheral volume
  ka  = parms[5]  # Absorption rate
  
  # define macro-parameters
  k10 = cl  / v1
  k12 = q   / v1
  k21 = q   / v2
  beta = 0.5 * (k12 + k21 + k10 - sqrt((k12 + k21 + k10)^2 - 4*k10*k21))
  alpha = k21 * k10 / beta

  # coefficients
  A = (k21-alpha) / (ka-alpha) / (beta-alpha)
  B = (k21-beta) / (ka-beta) / (alpha-beta)
  C = (k21-ka) / (alpha-ka) / (beta-ka)

  # Return concentration
  (tob>0) * dose*ka/v1*(A*exp(-alpha*tob) +
                        B*exp(-beta*tob) +
                        C*exp(-ka*tob)
                        )
} # pk.2comp.1abs
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.2comp.1abs, parms = c(10,30, 3, 90,100), log = TRUE)
}
                                               c(10,30, 3, 90)
# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.2comp.1abs.m = function(dose, tob, parms){
  # 2-compartment model with 1st-order absorption, parameterized in
  # macro-constants
  AOB   = parms[1]  # A / B
  alpha = parms[2]  # Distributional rate constant
  beta  = parms[3]  # Terminal rate constant
  ka    = parms[4]  # Absorption rate constant
  v1    = parms[5]  # Central Volume (scaling constant)
  
  # define parameters
  k21 = (AOB*beta + alpha) / (AOB + 1)

  # coefficients
  A = (k21-alpha) / (ka-alpha) / (beta-alpha)
  B = (k21-beta) / (ka-beta) / (alpha-beta)
  C = (k21-ka) / (alpha-ka) / (beta-ka)

  # Return concentration
  (tob>0) * dose*ka/v1*(A*exp(-alpha*tob) +
                        B*exp(-beta*tob) +
                        C*exp(-ka*tob)
                        )
} # pk.2comp.1abs.m
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.2comp.1abs.m, parms = c(10,0.2, 0.03, 0.25, 50), log = TRUE)
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.2comp.1abs.ss = function(dose, tob, parms){
  # Steady-State 2-compartment concentration with 1st-order absorption during
  # the dosing interval
  # Note that tob is time relative to the start of the dosing interval
  # tau is the interdose time interval
  cl  = parms[1]  # Central clearance
  v1  = parms[2]  # Central volume  
  q   = parms[3]  # Intercompartmental clearance
  v2  = parms[4]  # Peripheral volume
  ka  = parms[5]  # Absorption rate
  tau = parms[6]  # interdose interval
  
  # define macro-parameters
  k10 = cl  / v1
  k12 = q   / v1
  k21 = q   / v2
  beta = 0.5 * (k12 + k21 + k10 - sqrt((k12 + k21 + k10)^2 - 4*k10*k21))
  alpha = k21 * k10 / beta
  
  # Allow for more than one time interval to be predicted
  tst = tob - floor(tob/tau) * tau
  
  # coefficients
  A = (k21-alpha) / (ka-alpha) / (beta-alpha) / (1-exp(-alpha*tau))
  B = (k21-beta) / (ka-beta) / (alpha-beta) / (1-exp(-beta*tau))
  C = (k21-ka) / (alpha-ka) / (beta-ka) / (1-exp(-ka*tau))

  # Return concentration
  (tst>0) * dose*ka/v1*(A*exp(-alpha*tob) +
                        B*exp(-beta*tob) +
                        C*exp(-ka*tob)
                        )
  } # pk.2comp.1abs.ss

# ROXYGEN Documentation
# @describeIn pk.pred 

# pk.2comp.1abs.m.ss = function(dose, tob, parms){
#   # SS 2-compartment concentration with 1st-order absorption parameterized in
#   # macro-constants
#   AOB   = parms[1]  # A / B
#   alpha = parms[2]  # Distributional rate constant
#   beta  = parms[3]  # Terminal rate constant
#   ka    = parms[4]  # Absorption rate constant
#   v1    = parms[5]  # Central Volume (scaling constant)
#   
#   # define parameters
#   k21 = (AOB*beta + alpha) / (AOB + 1)
#         
#   # Allow for more than one time interval to be predicted
#   tst = tob - floor(tob/tau) * tau
#   
#   # coefficients
#   A = (k21-alpha) / (ka-alpha) / (beta-alpha) / (1-exp(-alpha*tau))
#   B = (k21-beta) / (ka-beta) / (alpha-beta) / (1-exp(-beta*tau))
#   C = (k21-ka) / (alpha-ka) / (beta-ka) / (1-exp(-ka*tau))
# 
#   # Return concentration
#   (tst>0) * dose*ka/v1*(A*exp(-alpha*tob) +
#                         B*exp(-beta*tob) +
#                         C*exp(-ka*tob)
#                         )
# } # pk.2comp.1abs.m.ss

#_______________________________________________________________________________
# 3-compartment models
# Reference: LZ Benet, J Pharm Sci, 61:536-541 (1972)

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.3comp.iv = function(dose, tob, parms){
  # 3-compartment PK with iv bolus dosing
  cl  = parms[1]  # central clearance
  v1  = parms[2]  # central volume
  q2  = parms[3]  # intercompartmental clearance betw. 1 and 2
  v2  = parms[4]  # peripheral volume, comp 2
  q3  = parms[5]  # intercompartmental clearance betw 1 and 3
  v3  = parms[6]  # volume of compartment 3
  
  # Define parameters
  k10 = cl  / v1
  k12 = q2  / v1
  k21 = q2  / v2
  k13 = q3  / v1
  k31 = q3  / v3
  
  # Exit rate constants
  e1 = k10 + k12 + k13
  e2 = k21
  e3 = k31
  
  # coefficients for cubic eqn
  a0 = k10*k21*k31
  a1 = k31*(k12 + k21 + k10) + k21*(k13 + k10)
  a2 = k10 + k12 + k13 + k21 + k31
  
  # coefficients
  q = (a2^2 - 3*a1) / 9
  r = (2*a2^3 - 9*a1*a2 + 27*a0) / 54
  theta = acos(r / sqrt(q^3))
  alpha = 2*sqrt(q) * cos(theta/3) + a2/3
  beta  = 2*sqrt(q) * cos((theta + 2*pi)/3) + a2/3
  gamma = 2*sqrt(q) * cos((theta - 2*pi)/3) + a2/3

  # Concentration coefficients
  A = (k21-alpha) * (k31-alpha) / (beta-alpha) / (gamma-alpha)
  B = (k21-beta) * (k31-beta) / (alpha-beta) / (gamma-beta)
  C = (k21-gamma) * (k31-gamma) / (alpha-gamma) / (beta-gamma)
  
  # Return concentration
  (tob>0)* dose/v1 * (A*exp(-alpha*tob) +
                      B*exp(-beta*tob) +
                      C*exp(-gamma*tob)
                      )
} # pk.3comp.iv
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.3comp.iv, parms = c(10,3,5,40,2,100))
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.3comp.iv.ss = function(dose, tob, parms) {
  # 3-compartment PK with iv dosing at SS
  # Note that time is relative to the start of the dosing period
  cl  = parms[1]  # central clearance
  v1  = parms[2]  # central volume
  q2  = parms[3]  # intercompartmental clearance betw. 1 and 2
  v2  = parms[4]  # peripheral volume, comp 2
  q3  = parms[5]  # intercompartmental clearance betw 1 and 3
  v3  = parms[6]  # volume of compartment 3
  tau = parms[7]  # interdose interval
  
  # Define parameters
  k10 = cl  / v1
  k12 = q2  / v1
  k21 = q2  / v2
  k13 = q3  / v1
  k31 = q3  / v3
  
  # Exit rate constants
  e1 = k10 + k12 + k13
  e2 = k21
  e3 = k31
  
  # coefficients for cubic eqn
  a0 = k10*k21*k31
  a1 = k31*(k12 + k21 + k10) + k21*(k13 + k10)
  a2 = k10 + k12 + k13 + k21 + k31
  
  # coefficients
  q = (a2^2 - 3*a1) / 9
  r = (2*a2^3 - 9*a1*a2 + 27*a0) / 54
  theta = acos(r / sqrt(q^3))
  alpha = 2*sqrt(q) * cos(theta/3) + a2/3	
  beta  = 2*sqrt(q) * cos((theta + 2*pi)/3) + a2/3
  gamma = 2*sqrt(q) * cos((theta - 2*pi)/3) + a2/3
 
  # Allow more than one period to be predicted
  tst = tob - floor(tob/tau)*tau
    
  # Concentration coefficients
  A = (k21-alpha) * (k31-alpha) / (beta-alpha) / (gamma-alpha) / (1-exp(-alpha*tau))
  B = (k21-beta) * (k31-beta) / (alpha-beta) / (gamma-beta) / (1-exp(-beta*tau))
  C = (k21-gamma) * (k31-gamma) / (alpha-gamma) / (beta-gamma) / (1-exp(-gamma*tau))
  
  # Return concentration
  (tst>0)* dose/v1 * (A*exp(-alpha*tst) +
                      B*exp(-beta*tst) +
                      C*exp(-gamma*tst)
                      )
} # pk.3comp.iv.ss

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.3comp.1abs = function(dose, tob, parms){
  # 3-compartment PK with 1st-order absorption
  cl  = parms[1]  # central clearance
  v1  = parms[2]  # central volume
  q2  = parms[3]  # intercompartmental clearance betw. 1 and 2
  v2  = parms[4]  # peripheral volume, comp 2
  q3  = parms[5]  # intercompartmental clearance betw 1 and 3
  v3  = parms[6]  # volume of compartment 3
  ka  = parms[7]  # 1st-order absorption constant
  
  # Define parameters
  k10 = cl  / v1
  k12 = q2  / v1
  k21 = q2  / v2
  k13 = q3  / v1
  k31 = q3  / v3
  
  # Exit rate constants
  e1 = k10 + k12 + k13
  e2 = k21
  e3 = k31
  
  # coefficients for cubic eqn
  a0 = k10*k21*k31
  a1 = k31*(k12 + k21 + k10) + k21*(k13 + k10)
  a2 = k10 + k12 + k13 + k21 + k31
  
  # coefficients
  q = (a2^2 - 3*a1) / 9
  r = (2*a2^3 - 9*a1*a2 + 27*a0) / 54
  theta = acos(r / sqrt(q^3))
  alpha = 2*sqrt(q) * cos(theta/3) + a2/3	
  beta  = 2*sqrt(q) * cos((theta + 2*pi)/3) + a2/3
  gamma = 2*sqrt(q) * cos((theta - 2*pi)/3) + a2/3
    
  # Concentration coefficients
  A = (k21-alpha) * (k31-alpha) / (beta-alpha) / (gamma-alpha) / (ka-alpha)
  B = (k21-beta) * (k31-beta) / (alpha-beta) / (gamma-beta) / (ka-beta)
  C = (k21-gamma) * (k31-gamma) / (alpha-gamma) / (beta-gamma) / (ka-gamma)
  D = (k21-ka) * (k31-ka) / (alpha-ka) / (beta-ka) / (gamma-ka)

  # Return concentration
  (tob>0)* dose*ka/v1 * (A*exp(-alpha*tob) +
                      B*exp(-beta*tob) +
                      C*exp(-gamma*tob) +
                      D*exp(-ka*tob)
                      )
} # pk.3comp.1abs
if(F){
  pkPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
	 pk.func = pk.3comp.1abs, parms = c(3,3,5,40,2,100, 4), log=TRUE)
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
pk.3comp.1abs.ss = function(dose,tob,parms) {
  # 3-compartment pk with 1st-order absorption, at SS  
  cl  = parms[1]  # central clearance
  v1  = parms[2]  # central volume
  q2  = parms[3]  # intercompartmental clearance betw. 1 and 2
  v2  = parms[4]  # peripheral volume, comp 2
  q3  = parms[5]  # intercompartmental clearance betw 1 and 3
  v3  = parms[6]  # volume of compartment 3
  ka  = parms[7]  # 1st-order absorption constant
  tau = parms[8]  # interdose interval
    
  # Define parameters
  k10 = cl  / v1
  k12 = q2  / v1
  k21 = q2  / v2
  k13 = q3  / v1
  k31 = q3  / v3
  
  # Exit rate constants
  e1 = k10 + k12 + k13
  e2 = k21
  e3 = k31
  
  # coefficients for cubic eqn
  a0 = k10*k21*k31
  a1 = k31*(k12 + k21 + k10) + k21*(k13 + k10)
  a2 = k10 + k12 + k13 + k21 + k31
  
  # coefficients
  q = (a2^2 - 3*a1) / 9
  r = (2*a2^3 - 9*a1*a2 + 27*a0) / 54
  theta = acos(r / sqrt(q^3))
  alpha = 2*sqrt(q) * cos(theta/3) + a2/3
  beta  = 2*sqrt(q) * cos((theta + 2*pi)/3) + a2/3
  gamma = 2*sqrt(q) * cos((theta - 2*pi)/3) + a2/3
    
  # Allow more than one period to be predicted
  tst = tob - floor(tob/tau)*tau
    
  # Concentration coefficients
  A = (k21-alpha) * (k31-alpha) / (beta-alpha) / (gamma-alpha) / 
                                  (ka-alpha) / (1-exp(-alpha*tau))
  B = (k21-beta) * (k31-beta) / (alpha-beta) / (gamma-beta) / 
                                (ka-beta) / (1-exp(-beta*tau))
  C = (k21-gamma) * (k31-gamma) / (alpha-gamma) / (beta-gamma) /
                                  (ka-gamma) / (1-exp(-gamma*tau))
  D = (k21-ka) * (k31-ka) / (alpha-ka) / (beta-ka) / 
                            (gamma-ka) / (1-exp(-ka*tau))

  # Return concentration
  (tob>0)* dose*ka/v1 * (A*exp(-alpha*tob) +
                      B*exp(-beta*tob) +
                      C*exp(-gamma*tob) +
                      D*exp(-ka*tob) 
                      )
}

#_______________________________________________________________________________
# Effect-site concentrations
# References: Gibaldi & Perrier, pp 132-133, 243
#             Gabrielsson & Weiner, 2nd ed. p 214

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
eff.1comp.iv = function(dose, tob, parms){
  # Effect-site, 1-compartment, iv bolus dosing
  cl  = parms[1]  # clearance
  v   = parms[2]  # volume
  keo = parms[3]  # effect-site transfer constant

  kel = cl / v
    
  # Return effect-site concentration
  dose*keo/v /(keo-kel) * (exp(-kel*tob) - exp(-keo*tob))
}



# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
eff.1comp.1abs = function(dose, tob, parms){
  # Effect-site concentration for 1-compartment model, 1st-order absorption
  cl  = parms[1]  # clearance
  v   = parms[2]  # volume
  ka  = parms[3]  # absorption rate constant
  keo = parms[4]  # effect-site transfer constant

  kel = cl / v
  
  # Define coefficients
  A = 1/(kel-ka) / (keo-ka)
  B = 1/(ka-kel) / (keo-kel)
  C = 1/(ka-keo) / (kel-keo)
  
  # Return effect-site concentration
  dose*ka*keo/v * (A*exp(-ka*tob) + B*exp(-kel*tob) + C*exp(-keo*tob))
}
if(F){
  eff.1comp.1abs(dose = 100, tob = seq(0,24*7), parms = c(1,10, 0.25, 0.05))
  pk.pred(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
  	pk.func = eff.1comp.iv, parms = c(1,10, 0.25))
  pkpdPredPlot(doses = rep(100, 7), t.doses = seq(0,24*7,length=7), t.obs = seq(0,24*7),
  	pk.func = pk.1comp.1abs, e.func = eff.1comp.1abs,
  	parms = c(1,10, 0.25, 0.05))
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
eff.1comp.1abs.ss = function(dose, tob, parms){
  # Steady-state effect-site concentration for 1-compartment model,
  # 1st-order absorption
  cl  = parms[1]  # clearance
  v   = parms[2]  # volume
  ka  = parms[3]  # absorption rate constant
  keo = parms[4]  # effect-site transfer constant
  tau = parms[5]  # interdose-interval
  
  kel = cl / v

  # Permit prediction of more than a single dosing interval
  tst = tob - floor(tob/tau)*tau
  
  # Define coefficients
  A = 1/(kel-ka) / (keo-ka) / (1-exp(-ka*tau))
  B = 1/(ka-kel) / (keo-kel) / (1-exp(-kel*tau))
  C = 1/(ka-keo) / (kel-keo) / (1-exp(-keo*tau))
  
  # Return effect-site concentration
  dose*ka*keo/v * (A*exp(-ka*tst) + B*exp(-kel*tst) + C*exp(-keo*tst))
}

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
eff.2comp.iv = function(dose, tob, parms){
  # effect-site concentration for 2-compartment kinetics, iv dosing
  cl  = parms[1]  # clearance
  v1  = parms[2]  # central volume
  q   = parms[3]  # intercompartmental clearance
  v2  = parms[4]  # peripheral volume
  keo = parms[5]  # effect-site rate constant
  ka  = parms[6]  # meaning for iv dosing requires review
  stop('this function requires review')
  
  k10 = cl  / v1
  k12 = q   / v1
  k21 = q   / v2
  beta = 0.5*(k12 + k21 + k10 - sqrt((k12+k21+k10)^2 - 4*k10*k21))
  alpha = k21*k10 / beta
  
  # Coefficients
  A = (k21-alpha) / (ka-alpha) / (beta-alpha) / (keo-alpha)
  B = (k21-beta) / (ka-beta) / (alpha-beta) / (keo-beta)
  C = (k21-keo) / (ka-keo) / (alpha-keo) / (beta-keo)

  (tob>0)* dose*keo/v1 * (A*exp(-alpha*tob) +
                          B*exp(-beta*tob) +
                          C*exp(-keo*tob)
                          )
} # eff.2comp.iv
  
# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
eff.2comp.1abs = function(dose, tob, parms){
  # Effect site model with 2 compartments, 1st-order absorption
  cl  = parms[1]  # clearance
  v1  = parms[2]  # central volume
  q   = parms[3]  # intercompartmental clearance
  v2  = parms[4]  # peripheral volume
  ka  = parms[5]  # absorption rate constant ## CHECK
  keo = parms[6]  # effect-site rate constant
  
  # Macro parameters  
  k10 = cl  / v1
  k12 = q   / v1
  k21 = q   / v2
  beta = 0.5*(k12 + k21 + k10 - sqrt((k12+k21+k10)^2 - 4*k10*k21))
  alpha = k21*k10 / beta
  
  # Coefficients
  A = (k21-ka) / (alpha-ka) / (beta-ka) / (keo-ka)
  B = (k21-alpha) / (ka-alpha) / (beta-alpha) / (keo-alpha)
  C = (k21-beta) / (ka-beta) / (alpha-beta) / (keo-beta)
  D = (k21-keo) / (ka-keo) / (alpha-keo) / (beta-keo)
  
  (tob>0)*dose*keo*ka/v1 * ( A*exp(-ka*tob) +
                             B*exp(-alpha*tob) +
                             C*exp(-beta*tob) +
                             D*exp(-keo*tob)
                             )
} # eff.2comp.1abs

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
eff.2comp.1abs.ss = function(dose, tob, parms){
  # Steady-state effect-site concentration
  cl  = parms[1]  # clearance
  v1  = parms[2]  # central volume
  q   = parms[3]  # intercompartmental clearance
  v2  = parms[4]  # peripheral volume
  keo = parms[5]  # effect-site rate constant
  tau = parms[6]  # inter-dose interval
  ka  = parms[7]  # first order absorption rate constant ## CHECK
  
  # Macro parameters  
  k10 = cl  / v1
  k12 = q   / v1
  k21 = q   / v2
  beta = 0.5*(k12 + k21 + k10 - sqrt((k12+k21+k10)^2 - 4*k10*k21))
  alpha = k21*k10 / beta
  
  # Permit prediction of more than a single dosing interval
  tst = tob - floor(tob/tau)*tau
  
  # Coefficients
  A = (k21-ka) / (alpha-ka) / (beta-ka) / (keo-ka) / (1-exp(-ka*tau))
  B = (k21-alpha) / (ka-alpha) / (beta-alpha) / (keo-alpha) / (1-exp(-alpha*tau))
  C = (k21-beta) / (ka-beta) / (alpha-beta) / (keo-beta) / (1-exp(-beta*tau))
  D = (k21-keo) / (ka-keo) / (alpha-keo) / (beta-keo) / (1-exp(-keo*tau))
  
  (tst>0)*dose*keo*ka/v1 * ( A*exp(-ka*tst) +
                             B*exp(-alpha*tst) +
                             C*exp(-beta*tst) +
                             D*exp(-keo*tst)
                             )
} # eff.2comp.1abs.ss

# ROXYGEN Documentation
#' @describeIn pk.pred 
#' 
eff.3comp.1abs = function(dose, tob, parms){
  # 3-compartment PK with 1st-order absorption
  cl  = parms[1]  # central clearance
  v1  = parms[2]  # central volume
  q2  = parms[3]  # intercompartmental clearance betw. 1 and 2
  v2  = parms[4]  # peripheral volume, comp 2
  q3  = parms[5]  # intercompartmental clearance betw 1 and 3
  v3  = parms[6]  # volume of compartment 3
  ka  = parms[7]  # 1st-order absorption constant
  keo = parms[8]  # effect site concentration
  
  # Define parameters
  k10 = cl  / v1
  k12 = q2  / v1
  k21 = q2  / v2
  k13 = q3  / v1
  k31 = q3  / v3
  
  # Exit rate constants
  e1 = k10 + k12 + k13
  e2 = k21
  e3 = k31
  
  # coefficients for cubic eqn
  a0 = k10*k21*k31
  a1 = k31*(k12 + k21 + k10) + k21*(k13 + k10)
  a2 = k10 + k12 + k13 + k21 + k31
  
  # coefficients
  q = (a2^2 - 3*a1) / 9
  r = (2*a2^3 - 9*a1*a2 + 27*a0) / 54
  theta = acos(r / sqrt(q^3))
  alpha = 2*sqrt(q) * cos(theta/3) + a2/3
  beta  = 2*sqrt(q) * cos((theta + 2*pi)/3) + a2/3
  gamma = 2*sqrt(q) * cos((theta - 2*pi)/3) + a2/3
    
  # Concentration coefficients
  A = (k21-alpha) * (k31-alpha) / (beta-alpha) / (gamma-alpha) / (ka-alpha)
  B = (k21-beta) * (k31-beta) / (alpha-beta) / (gamma-beta) / (ka-beta)
  C = (k21-gamma) * (k31-gamma) / (alpha-gamma) / (beta-gamma) / (ka-gamma)
  D = (k21-ka) * (k31-ka) / (alpha-ka) / (beta-ka) / (gamma-ka)
  E = (k21-keo) * (k31-keo) / (alpha-keo) / (beta-keo) / (gamma-keo)

  # Return concentration
  (tob>0)* dose*ka*keo/v1 * (A*exp(-alpha*tob) +
                      B*exp(-beta*tob) +
                      C*exp(-gamma*tob) +
                      D*exp(-ka*tob) +
                      E*exp(-keo*tob)
                      )
} # eff.3comp.1abs

## ==============
##  end of code
## ==============
