#**we clear the memory/workspace by using rm(list = ls()), before loading the Apollo library**
#-----------------------------------#
#Clear memory 
#-----------------------------------#
rm(list = ls())

#-----------------------------------#
# load packages
#-----------------------------------#
library(apollo)
library(tidyverse)

#-----------------------------------#
#Initialize code
#-----------------------------------#
apollo_initialise( )

#-----------------------------------#
#Set core controls
#-----------------------------------#
apollo_control = list(
  modelName ="output/mnl_1" ,
  modelDescr ="MNL model with ebird data " ,
  indivID = "observer_id",
  workInLogs = T)
#  nCores = 6)  

#-----------------------------------#
# load the .Rdata 
#-----------------------------------#

database = read_csv("data/processed/ApolloData.csv")

#-----------------------------------#
# Divide TC by 10 to for convergence
#-----------------------------------#
database <- database %>%
  mutate_at(vars(starts_with("tc_")), ~./ 10 ) %>%
  arrange(choice_id)

#-----------------------------------#
# Need to set to data.frame for apollo (I've renamed to database for ease)
#-----------------------------------#
database <- as.data.frame(database)

#-----------------------------------#
#Iterative coding for alternatives
#-----------------------------------#
J = length(unique(database$choice))

#-----------------------------------#
#loop-1 : creating ascs for sites (Define model parameters)
#-----------------------------------#
# No ascs for these models yet
#asc = list ()
#for ( j in 1:J ) asc[[ paste0("cg_code", j ) ]] = paste0("asc_", j)

#-----------------------------------#
#loop-2 : Set all of \ asc = 0 
#-----------------------------------#
apollo_beta = c(b_tc=0)

#-----------------------------------#
#Home is a fixed parameter 
#-----------------------------------#
apollo_fixed = c()

#-----------------------------------#
#apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"df_1418_ADV_T",overwriteFixed=FALSE)

#-----------------------------------#
#Validation and preparing inputs
#-----------------------------------#
apollo_inputs=apollo_validateInputs()

#-----------------------------------#
#Apollo_probabilities function
#-----------------------------------#

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
#  J = 3 # need this if parallel (nCores > 1 in apollo_controls)
  
  V= list( ) 
  avail = list()
  
  # set up utilities for hotspots
  for ( j in 1:J ) {
    V[[ paste0('choice', j ) ]] = b_tc * get(paste0('tc_', j)) #get(paste0('asc_', j)) + 
    avail[[paste0("choice", j)]]=get(paste0("avail_", j))
  }
  
  
  ###mnl_settings
  mnl_settings = list(
    alternatives = setNames (1:J,  names(V)) ,
    avail = avail ,
    choiceVar = choice ,
    V = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual (recognizing the repeated choice nature of our data)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#-----------------------------------#
#MWTP 
#-----------------------------------#

#-----------------------------------#
#Save the results 
#-----------------------------------#
apollo_modelOutput(model)


#apollo_saveOutput(model)
