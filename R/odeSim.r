
# ROXYGEN Documentation
#' ODE simulation with Multiprocessor Computation
#' @description Simulate with ordinary differential equations in R including multiprocessor computation
#' @param state  initial conditions
#' @param times  at what times should the system of differential equations be updated
#' @param func function describing the system of differential equations
#' @param parms global parameter values
#' @param simPar  names of parameters and states that should be changed from global values
#' @param simParValues values of parameters and states - each row corresponds to one scenario
#' @param method method for updating the states of the differential equations (see ?ode)
#' @param nodes number of parallel nodes to use in computations
#' @param keepTimeDT The times that return a zero when modulus (divided) by keepTimeDT are kept

#' @return Data.frame with simulated output
#' @export
#' @importFrom deSolve ode
#' @importFrom snowfall sfInit
#' @importFrom snowfall sfParallel 
#' @importFrom snowfall sfExportAll 
#' @importFrom snowfall sfStop 
#' @importFrom snowfall sfLapply
#' @importFrom snowfall sfCpus

## wrapper function to simulate across model parameters and initial conditions (e.g. doses)
odeSim <- function(state, times,  func, parms, simPar, simParValues, method = "lsoda",
  nodes = 1, keepTimeDT = 0.1)
  ## state        (vector)     : initial conditions
  ## times        (vector)     : at what times should the system of differential equations be updated
  ## func         (function)   : function describing the system of differential equations
  ## parms        (vector)     : global parameter values
  ## simPar       (vector)     : names of parameters and states that should be changed from global values
  ## simParValues (data frame) : values of parameters and states - each row corresponds to one scenario
  ## method       (character)  : method for updating the states of the differential equations (see ?ode)
  ## nodes        (number)     : number of parallel nodes to use in computations
  ## keepTimeDT   (number)     : The times that return a zero when modulus (divided) by keepTimeDT are kept
{
  cat("performing simulation of",deparse(substitute(func)),"across", simPar, "...\n")

  ## Initiate parallel computation
  sfInit( parallel=ifelse(nodes>1,TRUE,FALSE), cpus=nodes,useRscript=TRUE )
  if( sfParallel() ) {
    cat( "Running in parallel mode on", sfCpus(), "nodes.\n" )
  } else {
    cat( "Running in sequential mode.\n" )
  }
  sfExportAll()
  ## Simulation loop
  sim = sfLapply(as.list(as.data.frame(t(simParValues))), function(x, myState, myTimes, myFunc, myMethod,
    myParms, mySimPar)
  {
    #library(deSolve)
    ## Replace global parameter values with simulation specific ones
    if(sum(names(myParms)%in%mySimPar)>0){
      myParms[mySimPar[mySimPar%in%names(myParms)]] = x[mySimPar%in%names(myParms)]
    }
    ## Replace global state initial values with simulation specific ones
    if(sum(names(myState)%in%mySimPar)>0){
      myState[mySimPar[mySimPar%in%names(myState)]] = x[mySimPar%in%names(myState)]
    }
    ## Simulate
    myOut = as.data.frame(ode(y = myState, times = myTimes, func = myFunc, parms = myParms, method = myMethod))
    ## Reduce the number of simulated data points (for memory reasons)
    myOut = myOut[myOut$time%%keepTimeDT==0,]
    ## Return the output combined with the parameters and states used for this particular simulation scenario
    mySimPar[mySimPar%in%names(myState)] =
      paste(mySimPar[mySimPar%in%names(myState)],"init",sep=".")
    simVals = data.frame(matrix(x,nrow=nrow(myOut),ncol=length(x),byrow=TRUE))
    names(simVals) = mySimPar
    cbind(myOut,simVals)
  },
    myState = state,
    myTimes = times,
    myFunc = func,
    myParms = parms,
    mySimPar = simPar,
    myMethod = method)

  ## Stop parallel execution
  sfStop()

  ## put list result into indexed data frame
  out = data.frame(do.call("rbind",sim))

  cat("simulation done.\n")
  return(out)
}

