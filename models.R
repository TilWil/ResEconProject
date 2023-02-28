################################################################################
################################################################################
###############################Three logit models############################### 

# Basic model

apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="model1",
  modelDescr ="Land use logit model",
  indivID    ="Respondent_ID"
)

apollo_beta=c(b_alt1 =0,  # Alternative specific constant (intercept parameters)
              b_alt2 =0,
              b_ShFor =0,
              b_FiSizHalf = 0,
              b_FiSizDouble =0,
              b_price =0,
              b_FoBio  =0,
              b_NoUse =0,
              b_FoTre =0)

### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_ShFor*wald_1 + b_FiSizHalf* DummyHalf_1 + b_FiSizDouble*     DummyDouble_1 +  b_price * prei_1 + b_FoBio * FoBio_1 + b_NoUse * NoUse_1 + b_FoTre * FoTre_1
  V[['alt2']] = b_alt2 + b_ShFor*wald_2 + b_FiSizHalf* DummyHalf_2 + b_FiSizDouble*     DummyDouble_2 +  b_price * prei_2 + b_FoBio * FoBio_2 + b_NoUse * NoUse_2 + b_FoTre * FoTre_2
  V[['alt3']] = b_ShFor*wald_3 + b_price * prei_3 + b_FoBio * FoBio_3 + b_NoUse * NoUse_3 + b_FoTre * FoTre_3
  
  # V[['alt3']] = 0   # utility of opt out, normalized to zero
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model1 = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs, 
                         estimate_settings=list(hessianRoutine="analytic"))




kable(apollo_modelOutput(model1, modelOutput_settings = list(printPVal=T)), digits = 3) %>% kable_styling() 


############################################################################### 
# No Interaction Model



apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="model2",
  modelDescr ="Land use logit model",
  indivID    ="Respondent_ID"
)

apollo_beta=c(b_alt1 =0,  # Alternative specific constant (intercept parameters)
              b_alt2 =0,
              b_ShFor =0,
              b_ShFor2 =0,
              b_FiSizHalf =0,
              b_FiSizDouble = 0,
              b_price =0,
              b_FoBio  =0,
              b_NoUse =0,
              b_FoTre =0)

### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_ShFor*SQwald_1 + b_ShFor2*SQwald2_1 + b_FiSizHalf* DummyHalf_1 + b_FiSizDouble*     DummyDouble_1 + b_price * prei_1 + b_FoBio * FoBio_1 + b_NoUse * NoUse_1 + b_FoTre * FoTre_1
  V[['alt2']] = b_alt2 + b_ShFor*SQwald_2 + b_ShFor2*SQwald2_2 + b_FiSizHalf* DummyHalf_2 + b_FiSizDouble*     DummyDouble_2 + b_price * prei_2 + b_FoBio * FoBio_2 + b_NoUse * NoUse_2 + b_FoTre * FoTre_2
  V[['alt3']] = b_ShFor*SQwald_3 + b_ShFor2*SQwald2_3 + b_price * prei_3 + b_FoBio * FoBio_3 + b_NoUse * NoUse_3 + b_FoTre * FoTre_3
  
  # V[['alt3']] = 0   # utility of opt out, normalized to zero
  
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model2 = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs, 
                         estimate_settings=list(hessianRoutine="analytic"))



############################################################################### 
#3. Full Model


apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="model3",
  modelDescr ="Land use logit model",
  indivID    ="Respondent_ID"
)

apollo_beta=c(b_alt1 =0,  # Alternative specific constant (intercept parameters)
              b_alt2 =0,
              b_ShFor =0,
              b_ShFor2 =0,
              b_FiSizHalf =0,
              b_FiSizDouble =0,
              b_price =0,
              b_FoBio  =0,
              b_NoUse =0,
              b_FoTre =0,
              b_IncXFoBio =0,
              b_NatDayXFoBio =0)

### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_ShFor*SQwald_1 + b_ShFor2*SQwald2_1 +  b_FiSizHalf* DummyHalf_1 + b_FiSizDouble*     DummyDouble_1 + b_price * prei_1 + b_FoBio * FoBio_1 + b_NoUse * NoUse_1 + b_FoTre * FoTre_1 +            b_IncXFoBio * (FoBio_1*IncomeMC) + 
    b_NatDayXFoBio * (FoBio_1*NatDayMC)
  V[['alt2']] = b_alt2 + b_ShFor*SQwald_2 + b_ShFor2*SQwald2_2 + b_FiSizHalf* DummyHalf_2 + b_FiSizDouble*     DummyDouble_2 + b_price * prei_2 + b_FoBio * FoBio_2 + b_NoUse * NoUse_2 + b_FoTre * FoTre_2 +            b_IncXFoBio * (FoBio_2*IncomeMC) + 
    b_NatDayXFoBio * (FoBio_2*NatDayMC)
  V[['alt3']] =  b_ShFor*SQwald_3 + b_ShFor2*SQwald2_3 + b_price * prei_3 + b_FoBio * FoBio_3 + b_NoUse * NoUse_3 + b_FoTre * FoTre_3 +            b_IncXFoBio * (FoBio_3*IncomeMC) + 
    b_NatDayXFoBio * (FoBio_3*NatDayMC)  # utility of opt out, normalized to zero
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model3 = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs, 
                         estimate_settings=list(hessianRoutine="analytic"))

# End
############################################################################### 
############################################################################### 