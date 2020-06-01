#' Calculates coefficients and exponents based on microconstants for 3-compartment IV popPK (post-hoc) parameter estimates
#'
#' @param ds          # select the dataset
#' @param subjVar     # select the variable that represents the subject identifier
#' @param Dose        # select the variables that represents the dose
#' @param V1          # select the variables that represents the central volume of distribution
#' @param k10         # select the variables that represents the microconstant k10
#' @param k12         # select the variables that represents the microconstant k12
#' @param k13         # select the variables that represents the microconstant k13
#' @param k21         # select the variables that represents the microconstant k21
#' @param k31         # select the variables that represents the microconstant k31
#' @param nsig        # select the number of significant digits
#' @return            # returns a data.frame with coefficients, exponents and derived parameters
#' @importFrom        # rlang::enexprs() 
#' @importFrom        # dplyr::select()
#' @author            # Koen Jolling, Max Lagraauw
#' @references        # Dennis Fisher & Steven Shafer NONMEM Workshop - Basic Concepts (2007)
#' @references        # Upton J Pharmacol Toxicol Methods. 2004 Jan-Feb;49(1):65-8.
#'
#' @examples
#'
#' X <- data
#'
#' ConvertPK.iv.Vk.3comp(x)
#'
#' @export
#' 

ConvertPK.iv.Vk.3comp  = function(ds, subjVar="ID", Dose="DOSE", V1="V1", k10="K10", k12="K12", k21="K21", k13="K13", k31="K31", nsig=3, ...){
  
  arg = rlang::enexprs(...)
  id = (ds[[subjVar]])
  data_in = ds
  
  data_in = data_in %>%
    mutate(subjVar=ds[[subjVar]],
           Dose=ds[[Dose]],
           k10=ds[[k10]],  
           k12=ds[[k12]],   
           k21=ds[[k21]],   
           k13=ds[[k13]],   
           k31=ds[[k31]])   
  
  # Print the exponents and half-lifes per subject
  data = data_in %>% dplyr::select(subjVar,k10,k12,k13,k21,k31)
  for( i in unique(id)) {

    df = data_in %>% filter(subjVar==i) %>%
      data.frame(matrix(vector(mode = 'numeric',length = 6), nrow = 3, ncol = 4))  %>%
      mutate(one = c(- data[data$subjVar==i,]$k10 -  data[data$subjVar==i,]$k12 -  data[data$subjVar==i,]$k13,  data[data$subjVar==i,]$k21,  data[data$subjVar==i,]$k31)) %>%
      mutate(two =  c(data[data$subjVar==i,]$k12, -  data[data$subjVar==i,]$k21, 0)) %>%
      mutate(three =  c(data[data$subjVar==i,]$k13, 0, -  data[data$subjVar==i,]$k31)) %>%
      dplyr::select(one,two,three)

    dfid = data_in %>% filter(subjVar==i) %>%
      dplyr::select(subjVar)

    exponents = cbind(dfid,-eigen(df)$values)
    #print("exponents")
    #print(exponents)

    exponents_thalf = cbind(dfid,log(2)/exponents %>% dplyr::select(-subjVar))
    #print("exponents_thalf")
    #print(exponents_thalf)
  }
  
  ## Derived parameters
  # Volumes
  data_in = data_in %>%
    group_by(subjVar) %>%
    mutate(  V2 = V1*k12/k21
            ,V3 = V1*k13/k31
            ,VSS = V1+V2+V3
            
            # Clearances
            ,CL1 = V1*k10
            ,CL2 = V1*k12
            ,CL3 = V1*k13

            # Solve cubic equation
            ,a0 = k10*k21*k31
            ,a1 = k10*k31 + k21*k31 + k21*k13 + k10*k21 + k31*k12
            ,a2 = k10 + k12 + k13 + k21 + k31

            ,P = a1 - (a2^2)/3
            ,q = (2*(a2^3)/27) - (a1*a2/3) + a0
            ,r1 = sqrt(-(P^3)/27)
            ,phi = acos((-q/2)/r1)/3
            ,r2 = 2*exp(log(r1)/3)
            ,root1 = -(cos(phi)*r2-a2/3)
            ,root2 = -(cos(phi+2*pi/3)*r2-a2/3)
            ,root3 = -(cos(phi+4*pi/3)*r2-a2/3)
            
            # Sort by size
            ,I1 = max(c(root1,root2,root3))
            ,I2 = median(c(root1,root2,root3))
            ,I3 = min(c(root1,root2,root3))
            
            ,alpha = I1  # Macro rate constant associated with the alpha phase
            ,beta  = I2  # Macro rate constant associated with the beta phase
            ,gamma = I3  # Macro rate constant associated with the gamma phase
            
            ,alpha_thalf = log(2)/alpha # The half life associated with the macro constant alpha
            ,beta_thalf  = log(2)/beta  # The half life associated with the macro constant beta
            ,gamma_thalf = log(2)/gamma # The half life associated with the macro constant gamma
			
			,Vz = CL1/gamma  # Volume of distribution during the terminal phase (V area)
            
            # Calculate true coefficients
            ,C1 = (k21-I1)*(k31-I1)/(I1-I2)/(I1-I3)/V1
            ,C2 = (k21-I2)*(k31-I2)/(I2-I1)/(I2-I3)/V1
            ,C3 = (k21-I3)*(k31-I3)/(I3-I2)/(I3-I1)/V1
            
            # True coefficients
            ,Atrue = C1  # The zero time intercept (concentration) associated with the alpha phase
            ,Btrue = C2  # The zero time intercept (concentration) associated with the beta phase
            ,Ctrue = C3  # The zero time intercept (concentration) associated with the gamma phase
            
            # Fractional Coefficients (Afrac + Bfrac + Cfrac = 1)
            ,Afrac = Atrue*V1
            ,Bfrac = Btrue*V1
            ,Cfrac = Ctrue*V1

            # Partial AUCs
            ,AUC = Dose/CL1        # Total AUC
            ,AUCalpha = AUC*Afrac  # The relative portion of the total AUC related to the alpha phase
            ,AUCbeta  = AUC*Bfrac  # The relative portion of the total AUC related to the beta phase
            ,AUCgamma = AUC*Cfrac  # The relative portion of the total AUC related to the gamma phase
            ,AUCsum = AUCalpha + AUCbeta + AUCgamma) %>%
    ungroup() %>%
    dplyr::select(-c(subjVar,a0,a1,a2,P,q,r1,phi,r2,root1,root2,root3,I1,I2,I3,C1,C2,C3)) %>%
    mutate_at(vars(subjVar),funs(factor)) %>%
    mutate_if(is.numeric, signif, digits=nsig) #Set significant digits for the numeric columns

    return(data_in)
}


#' Calculates coefficients and exponents based on microconstants for 2-compartment IV popPK (post-hoc) parameter estimates
#'
#' @param ds          # select the dataset
#' @param subjVar     # select the variable that represents the subject identifier
#' @param Dose        # select the variables that represents the dose
#' @param V1          # select the variables that represents the central volume of distribution
#' @param k10         # select the variables that represents the microconstant k10
#' @param k12         # select the variables that represents the microconstant k12
#' @param k21         # select the variables that represents the microconstant k21
#' @param nsig        # select the number of significant digits
#' @return            # returns a data.frame with coefficients, exponents and derived parameters
#' @importFrom        # rlang::enexprs() 
#' @importFrom        # dplyr::select()
#' @author            # Koen Jolling, Max Lagraauw
#' @references        # Dennis Fisher & Steven Shafer NONMEM Workshop - Basic Concepts (2007)
#' @references        # Toutain, P. L., Bousquet-Melou, A. Plasma terminal half-life. J. vet. Pharmacol.Therap. 27, 427-439
#'
#' @examples
#'
#' X <- data
#'
#' ConvertPK.iv.Vk.2comp(x)
#'
#' @export
#' 

ConvertPK.iv.Vk.2comp  = function(ds, subjVar="ID", Dose="DOSE", V1="V1", k10="K10", k12="K12", k21="K21", nsig=3, ...){
  
  arg = rlang::enexprs(...)
  id = (ds[[subjVar]])
  data_in = ds
  
  data_in = data_in %>%
    mutate(subjVar=ds[[subjVar]],
           Dose=ds[[Dose]],
           k10=ds[[k10]],  
           k12=ds[[k12]],   
           k21=ds[[k21]])   
  
  # Print the exponents and half-lifes per subject
  data = data_in %>% dplyr::select(subjVar,k10,k12,k21)
  for( i in unique(id)) {

    df = data_in %>% filter(subjVar==i) %>%
      data.frame(matrix(vector(mode = 'numeric',length = 4), nrow = 2, ncol = 1))  %>%
      mutate(one = c(- data[data$subjVar==i,]$k10 -  data[data$subjVar==i,]$k12,  data[data$subjVar==i,]$k21)) %>%
      mutate(two =  c(data[data$subjVar==i,]$k12, -  data[data$subjVar==i,]$k21)) %>%
      dplyr::select(one,two)

    dfid = data_in %>% filter(subjVar==i) %>%
      dplyr::select(subjVar)

    exponents = cbind(dfid,-eigen(df)$values)
    #print("exponents")
    #print(exponents)

    exponents_thalf = cbind(dfid,log(2)/exponents %>% dplyr::select(-subjVar))
    #print("exponents_thalf")
    #print(exponents_thalf)
  }
  
  ## Derived parameters
  # Volumes
  data_in = data_in %>%
    group_by(subjVar) %>%
    mutate( V2 = V1*k12/k21
            ,VSS = V1+V2
            
            # Clearances
            ,CL1 = V1*k10
            ,CL2 = V1*k12
            
            # Solve quadratic equation
            ,a0 = k10*k21
            ,a1 = -(k10+k12+k21)
            
            ,I1 = (-a1+sqrt(a1*a1-4*a0))/2
            ,I2 = (-a1-sqrt(a1*a1-4*a0))/2
            
            ,alpha = I1  # Macro rate constant associated with the alpha phase
            ,beta  = I2  # Macro rate constant associated with the beta phase

            ,alpha_thalf = log(2)/alpha # The half life associated with the macro constant alpha.
            ,beta_thalf  = log(2)/beta  # The half life associated with the macro constant beta.
			
			,Vz = CL1/beta  # Volume of distribution during the terminal phase (V area)

            # Calculate true coefficients
            ,C1 = (k21-I1)/(I2-I1)/V1
            ,C2 = (k21-I2)/(I1-I2)/V1
            
            # True coefficients
            ,Atrue = C1  #The zero time intercept (concentration) associated with the alpha phase
            ,Btrue = C2  #The zero time intercept (concentration) associated with the beta phase
            
            # Fractional Coefficients (Afrac + Bfrac = 1)
            ,Afrac = Atrue*V1
            ,Bfrac = Btrue*V1
            
            # Partial AUCs
            ,AUC = Dose/CL1        # Total AUC
            ,AUCalpha = AUC*Afrac  # The relative portion of the total AUC related to the alpha phase
            ,AUCbeta  = AUC*Bfrac  # The relative portion of the total AUC related to the beta phase
            ,AUCsum = sum(AUCalpha + AUCbeta)
            ) %>%
    ungroup() %>%
    dplyr::select(-c(subjVar,a0,a1,I1,I2,C1,C2))%>%
    mutate_at(vars(subjVar),funs(factor)) %>%
    mutate_if(is.numeric, signif, digits=nsig) #Set significant digits for the numeric columns
  
  return(data_in)
}


#' Calculates coefficients and exponents based on microconstants for 1-compartment IV popPK (post-hoc) parameter estimates
#'
#' @param ds          # select the dataset
#' @param subjVar     # select the variable that represents the subject identifier
#' @param Dose        # select the variables that represents the dose
#' @param V1          # select the variables that represents the central volume of distribution
#' @param k10         # select the variables that represents the microconstant k10
#' @param nsig        # select the number of significant digits
#' @return            # returns a data.frame with coefficients, exponents and derived parameters
#' @importFrom        # rlang::enexprs() 
#' @importFrom        # dplyr::select()
#' @author            # Koen Jolling, Max Lagraauw
#' @references        # Dennis Fisher & Steven Shafer NONMEM Workshop - Basic Concepts (2007)
#'
#' @examples
#'
#' X <- data
#'
#' ConvertPK.iv.Vk.1comp(x)
#'
#' @export
#' 



ConvertPK.iv.Vk.1comp  = function(ds, subjVar="ID", Dose="DOSE", V1="V1", k10="K10", nsig=3, ...){
  
  arg = rlang::enexprs(...)
  id = (ds[[subjVar]])
  data_in = ds
  
  data_in = data_in %>%
    mutate(subjVar=ds[[subjVar]],
           Dose=ds[[Dose]],
           k10=ds[[k10]])   
  
  # Print the exponents and half-lifes per subject
  data = data_in %>% dplyr::select(subjVar,k10)
  for( i in unique(id)) {
    
    df = data_in %>% filter(subjVar==i) %>%
      data.frame(matrix(vector(mode = 'numeric',length = 1), nrow = 1, ncol = 1))  %>%
      mutate(one = c(- data[data$subjVar==i,]$k10)) %>%
      dplyr::select(one)
    
    dfid = data_in %>% filter(subjVar==i) %>%
      dplyr::select(subjVar)
    
    exponents = cbind(dfid,-eigen(df)$values)
    #print("exponents")
    #print(exponents)
    
    exponents_thalf = cbind(dfid,log(2)/exponents %>% dplyr::select(-subjVar))
    #print("exponents_thalf")
    #print(exponents_thalf)
  }
  
  ## Derived parameters
  # Volumes
  data_in = data_in %>%
    group_by(subjVar) %>%
    mutate( VSS = V1
            
            # Clearances
            ,CL1 = V1*k10
          
            # Just k10
            ,I1 = k10
            
            ,alpha = I1  # “Macro” rate constant associated with the alpha phase
            
            ,alpha_thalf = log(2)/alpha # The half life associated with the macro constant alpha
			
			,Vz = CL1/alpha  # Volume of distribution during the terminal phase (V area)

            # Calculate true coefficients
            ,C1 = 1/V1

            # True coefficients
            ,Atrue = C1 #The zero time intercept (concentration) associated with the alpha phase

            # Fractional Coefficients
            ,Afrac = Atrue*V1
            
            # Partial AUCs
            ,AUC = Dose/CL1
            ,AUCalpha = AUC*Afrac
            ,AUCsum = sum(AUCalpha)
    ) %>%
    ungroup() %>%
    dplyr::select(-c(subjVar,I1,C1))%>%
    mutate_at(vars(subjVar),funs(factor)) %>%
    mutate_if(is.numeric, signif, digits=nsig) #Set significant digits for the numeric columns
  
  return(data_in)
}

