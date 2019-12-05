#' Make a list of equations
#' @describeIn nm.process.coveffects Makes a list of calls from its named arguments 
#' input formulas.  
#' @param ... A series of named formulas
#' @return A list of call objects that can be used to evaluate the input equations 
#' in a given environment. 
#' @note This function is used with \code{nm.process.coveffects}
#' @export make.eqs
#' @examples
#' eqs=make.eqs(CL=THETA1*(1 + (GNDR==0)*THETA7)*(AGE/33.72)^THETA6*exp(ETA1)
#'             ,V1=THETA2*(1 + (GNDR==0)*THETA9)*(AGE/33.72)^THETA8*exp(ETA2)
#'             )
#' eqs$label=list(CL="Clearance (L/h)", V1="Volume of Central Compartment (L)")            
#' 

# Make a list of named functions (as call objects)
make.eqs <- function(...) {
  eval(substitute(alist(...)))
}


#' Make a list of equations
#' @describeIn nm.process.coveffects Evaluate a list of calls in an environment, list, 
#' or data.frame.  
#' @param .data is data.frame, list, or environment
#' @param ... is a list of calls or expressions
#' @return .data is returned with the evaluated expressions added 
#' @note This function is used with \code{nm.process.coveffects}
#' @export eval.eqs
#' @examples
#' eqs=make.eqs(CL=THETA1*(1 + (GNDR==0)*THETA7)*(AGE/33.72)^THETA6*exp(ETA1)
#'             ,V1=THETA2*(1 + (GNDR==0)*THETA9)*(AGE/33.72)^THETA8*exp(ETA2)
#'             )
#' eqs$label=list(CL="Clearance (L/h)", V1="Volume of Central Compartment (L)")            
#' 
# Evaluate a list of calls, given a data.frame, list, or environment
# modified from plyr::mutate

eval.eqs=function(.data,  ...) 
{
  #.data is data.frame, list, or environment
  # env.aux is alternative environment (only) where we can find variables not in .data
  # ... is a list of calls or expressions
  stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
  cols <- as.list(unlist(list(...)))
  cols <- cols[unlist(lapply(cols,is.call))] #only process equations, not other stuff like labels or  units
  for (col in names(cols)) {
    .data[[col]] <- eval(cols[[col]], .data, parent.frame())
  }
  .data
}


#' Make a covInfo S3 object
#' @describeIn nm.process.coveffects
#' @description Constructs a covInfo (covariate information) object from a vector.  
#' @param cov.name The name of the covariate
#' @param cov.vals A vector of values
#' @param cov.cat A boolean indicating if the covariate is categorical
#' @param cov.center The typical value of the covariate
#' @param cov.min The minimum value of the covariate
#' @param cov.max The maximum value of the covariate
#' @param cov.label The printed name of the covariate (e.g. seen on plot axes)
#' @param cov.breaks The points at which simulations and plots would be assessed
#' @return A covInfo object 
#' @note This function is used with \code{nm.process.coveffects}
#' @export covInfo
#' @examples
#' eqs=make.eqs(CL=THETA1*(1 + (GNDR==0)*THETA7)*(AGE/33.72)^THETA6*exp(ETA1)
#'             ,V1=THETA2*(1 + (GNDR==0)*THETA9)*(AGE/33.72)^THETA8*exp(ETA2)
#'             )
#' eqs$label=list(CL="Clearance (L/h)", V1="Volume of Central Compartment (L)")            
#' 
# Evaluate a list of calls, given a data.frame, list, or environment
# modified from plyr::mutate
covInfo = function(cov.name
                   ,cov.vals
                   ,cov.cat = FALSE
                   ,cov.center=median(cov.vals)
                   ,cov.min=min(cov.vals)
                   ,cov.max=max(cov.vals)
                   ,cov.label=cov.name
                   ,cov.breaks=pretty(cov.vals))
{
  if(cov.cat){
    cov.breaks=unique(cov.vals)
    cov.center=cov.breaks[which.max(tabulate(match(cov.vals,cov.breaks)))]
  }
  structure(list(name=cov.name,categorical=cov.cat,center=cov.center,min=cov.min,
                 max=cov.max,label=cov.label,breaks=cov.breaks),class="covInfo")
}


#' Print a covInfo S3 object
#' @describeIn nm.process.coveffects
#' @description Prints a covInfo object.  
#' @param x A covInfo object
#' @return A covInfo object 
#' @note This function is used with \code{nm.process.coveffects}
#' @export
#' 
#print.covInfo
print.covInfo=function(x) cat(x$name, ": (", x$min, x$center, x$max, ")", x$label)


#' Make a list of covInfo objects from a data frame.
#' @describeIn nm.process.coveffects
#' @description Takes a data frame and creates covInfo objects based on its columns.  
#' @param df A data frame
#' @param cnames A vector of column names to create covInfo objects for
#' @param cat.covs A vector of categorical column names.  Must be a subset of cnames
#' @return A list of covInfo objects
#' @note This function is used with \code{nm.process.coveffects}
#' @export 
#' 
#make a list of covInfo objects from a dataframe.  User can override elements as desired
makeCovInfo= function(df, cnames=names(df), cat.covs="")
{
  #df is a data.frame of all the covariates (and maybe other things)
  #cnames, provide list of columns to use otherwise will assume EVERY column is a covariate
  #cat.covs, names of categorical covariates
  info=list()
  for(i in names(df)){
    info[[i]] = covInfo(i,df[[i]],i %in% cat.covs)
  }
  info
}

# bootsamp: sample an equation with random normals pulled from varcov, other variables from supplied env
#   returns mean, median, var of sample

bootsamp = function(equ, .data, varcov, N=10000)
{
  # .data should be a data.frame by this point
  # take names from varcov out of .data
  rand.vars = colnames(varcov)
  fixed.vars = names(.data)[!names(.data) %in% rand.vars]
  
  fix.df = .data[fixed.vars] #works for data.frame or list
  
  # sample MVN, centered at 
  mvrnormR <- function(n, mu, sigma)
  {
    ncols <- ncol(sigma)
    mu <- rep(mu, each = n) ## not obliged to use a matrix (recycling)
    mu + matrix(rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
  }
  ran.df=as.data.frame(mvrnormR(N,0,varcov))
  
  # merge fix and ran and eval the eq
  df=merge(fix.df,ran.df)
  df=eval.eqs(df,equ)
  df
}


# Covariate Effect Plots
# take an equation and a covInfo object and do covariate plot based on supplied (or defaulted) covariate names

# internal function to build a coveffect grid
.coveffect = function(equ, cov.info, ptab, varcov, Nboot=10000, pi.wid=0.9)
{ #ptab has parameters (THETA, OMEGA, SIGMA) and is either a 1 row data frame or a flat list
  #cov.inf has covariate center, min, max, breaks
  #equ is the equation to evaluate
  param = names(equ)
  cov = cov.info$name
  stopifnot(cov %in% all.vars(equ[[1]]))
  
  #get typical value (ETAs = 0)
  coveff.df = data.frame(eval.eqs(ptab,equ), stringsAsFactors=FALSE)
  #get cov,param,group columns if they exist
  cols =c(cov,param)
  if("group" %in% names(ptab)) cols=c(cols,"group")
  coveff.df = cbind(Cov=cov, Par=param,
                    Parameter=paste(param,cov,sep="."),coveff.df[,cols],
                    stringsAsFactors=FALSE)
  names(coveff.df)[4:5]=c("cov.val","typical")
  
  # bootsample for mean, var, sd, 95%PI
  samples.df = ddply(data.frame(ptab),cov,function(x) bootsamp(equ,x,varcov,Nboot))
  #rename cov and param so we can see them easily in ddply
  names(samples.df)[which(names(samples.df)==param)]="param"
  names(samples.df)[which(names(samples.df)==cov)]="cov.val"
  # get sample stats
  #if there is group column, use it for the stats
  grouping="cov.val"
  if("group" %in% names(samples.df)) grouping=c(grouping,"group")
  summ.df = ddply(samples.df,grouping,here(summarise)
                  ,mean=mean(param)
                  ,var=var(param)
                  ,sd=sd(param)
                  ,pi.lower=quantile(param,p=(1-pi.wid)/2)
                  ,pi.upper=quantile(param,p=1-(1-pi.wid)/2)
  )
  
  merge(coveff.df,summ.df,by=grouping)
}


#' Evaluate equations (as a list of calls) over the covariates involved.
#' @description A list of equations that define the covariate relationships is...  
#' @param eqs A list of calls
#' @param covs.info A list of covInfo objects to parameterize the subanalysis
#' @param xpose.df A dataframe containing post-hoc parameter values
#' @param omega The random effect covariance matrix from which to simulate ETA values
#' @param Nboot The number of bootstrap samples to run for each sample point
#' @param pi.wid The width of prediction intervals (e.g. 0.9 is a 90\% prediction interval)
#' @return A list including lists of: covariate effect plots, table of covariate effects,
#' table of post-hoc values -- for each covariate effect; the call list; the list of covInfo
#' @export nm.process.coveffects
#' 
nm.process.coveffects= function(eqs,covs.info,pars, xpose.df, omega, Nboot=10000, 
                             pi.wid=0.9)
{
  #eqs is a list of calls for the structural parameter equations (using THETA, ETA, COV)
  #cov is a list of covEffect objects (S3)
  #pars is the list or single-row data.frame of the parameter estimates (THETA, OMEGA)
  #xpose.df has the posthocs and covariates
  #omega is covariance matrix for random effects
  
  #reset names in omega
  n.eta = dim(omega)[1] #symetric matrix, so which dim doesn't matter
  rownames(omega)<-colnames(omega)<-paste0("ETA",1:n.eta)
  
  #return a list with list of data.frames and list of ggplots
  coveffs.l=list()
  plot.l=list()
  x.l=list()
  cov.names = names(covs.info)
  
  #add covariate center values into pars
  pars[cov.names]=lapply(covs.info,function(x) x$center)
  
  for(par in names(eqs)){
    #which covs are in the equations
    equ=eqs[par]
    vars=all.vars(equ[[1]])
    activ.covs = cov.names[cov.names %in% vars]
    
    # Which covs are categorical?
    activ.cat=character(0)
    if(length(activ.covs)>0){
      activ.cat = activ.covs[sapply(activ.covs,function(x)covs.info[[x]]$categorical)]
    }
    # data.frame of categorical covariate combinations
    if(length(activ.cat>0)){
      cats.df = expand.grid(lapply(activ.cat,function(x)covs.info[[x]]$breaks))
      names(cats.df)=activ.cat
    }
    
    for(i in activ.covs){
      
      ptab=as.list(pars) #should still be flat/one-row
      # add coveff breaks into ptab as coveff, only if not categorical
      # categorical breaks should be part of ptab
      if(!i %in% activ.cat){
        ptab[[i]] = covs.info[[i]]$breaks
      }
      #convert to data.frame for easier merging
      ptab=as.data.frame(ptab)
      cats.group=character(0)
      if(length(activ.cat>0)){
        # add in the categorical breaks and compute groups
        # have to remove activ.cat first
        ptab=merge(ptab[,names(ptab)[!names(ptab) %in% activ.cat]],cats.df)
        # create grouping column for 
        # any cats not the current covariate?
        cats.group = activ.cat[activ.cat!=i]
        if(length(cats.group)>0){
          ptab$group = interaction(ptab[,cats.group])
          xpose.df$group = interaction(xpose.df[,cats.group])
          group.df = unique(ptab[,c("group",cats.group)]) 
        } 
      }
      
      eff.name=paste0(par,".",i)
      #get the covariate effects on this parameter
      ce.df=.coveffect(equ,covs.info[[i]],ptab,omega,Nboot,pi.wid)

      #generate pairlist from xpose data
      x.df=data.frame(Par=par,Cov=i,cov.val=xpose.df[[i]], Parameter=eff.name,
                      posthoc=xpose.df[[par]],stringsAsFactors = FALSE)
      if(length(cats.group)>0){
        #add group
        x.df$group=xpose.df$group
        #add the grouping covariates, in case user may want to custom plot
        x.df[cats.group]=xpose.df[cats.group]
        #join the groups to the predicted data
        ce.df=merge(ce.df,group.df,by="group")
      }

      #plot the effect
      p = coveff.plot(x.df,ce.df, covs.info[[i]]$categorical, cats.group) + 
        labs(x=covs.info[[i]]$label,y=eqs$label[[par]])

      #save stuff
      x.l[[eff.name]]=x.df
      coveffs.l[[eff.name]]=ce.df
      plot.l[[eff.name]]=p
    }
  }
  structure(list(plot=plot.l, table=coveffs.l, posthoc=x.l,
                 eq.cov=eqs, covs.info=covs.info), class="covEffects")
  
}

#' Plot a covariate effects object
#'
#' @param a covEffects object from nm.process.coveffects 
#' @param idx Names of the plots to print. Defaults to all plots.
#'
#' @export
#'

plot.covEffects=function(obj, idx=names(obj$plot))
{
  for(i in idx) print(obj$plot[[i]])
}

coveff.plot=function(xpose.df, ppred.df, box, grouping)
{ 
  #box is T or F and is set T if x axis is categorical covariate
  cov=as.character(ppred.df$Cov[1])
  par=as.character(ppred.df$Par[1])
  p = ggplot() + theme_bw()
  if(length(grouping)==0){
    if(!box){
      p=p+geom_ribbon(data=ppred.df,aes(x=cov.val,ymin=pi.lower,ymax=pi.upper),
                      alpha=.2, fill="steelblue") +
        geom_point(data=xpose.df, aes(x=cov.val,y=posthoc)) +
        geom_line(data=ppred.df,aes(x=cov.val,y=typical), 
                  color="blue",size=1) 
    }else{
      p=p+geom_crossbar(data=ppred.df,aes(x=factor(cov.val),ymin=pi.lower, y=typical,
                                          ymax=pi.upper), alpha=.2
                        , fill="steelblue",color="steelblue") +
        geom_jitter(data=xpose.df, aes(x=factor(cov.val),y=posthoc)) 
      
    }
  }
  if(length(grouping)>0){
    if(!box){
      p=p+geom_ribbon(data=ppred.df,aes(x=cov.val,ymin=pi.lower, 
                                        ymax=pi.upper, fill=group), alpha=.2) +
        geom_point(data=xpose.df, aes(x=cov.val,y=posthoc,color=group)) +
        geom_line(data=ppred.df,aes(x=cov.val,y=typical, color=group),size=1) 
      
    }else{
      p=p+geom_crossbar(data=ppred.df,aes(x=factor(cov.val),ymin=pi.lower, y=typical,
                                          ymax=pi.upper, fill=group, color=group)
                        , alpha=.2, position="dodge") +
        geom_point(data=xpose.df, aes(x=factor(cov.val),y=posthoc,color=group, fill=group),
                   position=position_jitterdodge()) 
    }
    p=p+labs(color=paste(grouping,collapse="."),fill=paste(grouping,collapse="."))
  }
  p
}