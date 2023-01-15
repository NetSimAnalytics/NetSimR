#help functions
#set maximum number of pareto slices
max_number_of_pareto_slices = 5

#random pareto generator
rpareto <- function(n, alpha, x_m) x_m / runif(n)^(1/alpha)
#apply severity cap
apply_severity_cap <- function(claims, severity_cap_boolean, severity_cap_amount){
  if(severity_cap_boolean){
    claims <- ifelse(claims>severity_cap_amount, severity_cap_amount, claims)
  }
  return(claims)
}
#apply deductible and limit
apply_deductible_limit <- function(gross_claims_data, reinsurance_structure, deductible, limit){
  if(reinsurance_structure=='No Reinsurance Structure'){return(gross_claims_data)}
  else if (reinsurance_structure=='Unlimited Layer'){return(ifelse(gross_claims_data<deductible,0, gross_claims_data-deductible))}
  else if (reinsurance_structure=='Limited Layer'){return(ifelse(gross_claims_data<deductible, 0, ifelse(gross_claims_data-deductible>limit, limit, gross_claims_data-deductible)))}
  else if (reinsurance_structure=='Exclude Layer'){return(gross_claims_data - ifelse(gross_claims_data<deductible, 0, ifelse(gross_claims_data-deductible>limit, limit, gross_claims_data-deductible)))}
}

#reinsurance structures
reinsurance_structures_options <- c('No Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude Layer')

#set distribution object class
distributionClass <- setClass("distributionClass", slots = c(
  distrID="character"
  ,distr_label="character"
  ,paramIDs="character"
  ,param_labels="character"
  ,param_min_values="numeric"
  ,param_max_values="numeric"
  ,simulate_func = "function"
))

#set frequency distribitions
freq_dist_options <- c(
  distributionClass(
    distrID='Poisson'
    ,distr_label='Poisson'
    ,paramIDs=c("lamda")
    ,param_labels=c("lamda")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpois(n = number_of_simulations, lambda = parameters[1])
    )}
  )
  ,distributionClass(
    distrID='Negative_Binomial'
    ,distr_label='Negative Binomial'
    ,paramIDs=c("r", "beta")
    ,param_labels=c("r", "beta")
    ,param_min_values=c(0,0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpois(n = number_of_simulations, lambda = rgamma(n =number_of_simulations, shape = parameters[1], scale = parameters[2]))
    )}
  )
  ,distributionClass(
    distrID='Binomial'
    ,distr_label='Binomial'
    ,paramIDs=c("n", "p")
    ,param_labels=c("n", "p")
    ,param_min_values=c(0,0)
    ,param_max_values=c(NA,1)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rbinom(n = number_of_simulations, size = parameters[1], prob = parameters[2])
    )}
  )
  ,distributionClass(
    distrID='Fixed_number_of_Counts'
    ,distr_label='Fixed number of Counts'
    ,paramIDs=c("FixedNumberOfCounts")
    ,param_labels=c("FixedNumberOfCounts")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rep(parameters[1], number_of_simulations)
    )}
  )
)

names(freq_dist_options) <- sapply(freq_dist_options, function(x) x@distrID)
freq_dist_parameter_placeholders <- data.frame(
  param_number = 1:max(sapply(freq_dist_options, function(x) length(x@paramIDs)))
)
freq_dist_parameter_placeholders$param_id <- paste0("freq_param_", freq_dist_parameter_placeholders$param_number)

#set severity distributions
sev_dist_options <- c(
  distributionClass(
    distrID='Normal'
    ,distr_label='Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("mu", "sigma")
    ,param_min_values=c(0, 0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rnorm(n = number_of_simulations, mean = parameters[1], sd = parameters[2])
    )}
  )
  ,distributionClass(
    distrID='LogNormal'
    ,distr_label='Log-Normal'
    ,paramIDs=c("mu", "sigma")
    ,param_labels=c("mu", "sigma")
    ,param_min_values=c(0,0)
    ,param_max_values=c(NA,1)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rlnorm(n = number_of_simulations, meanlog = parameters[1], sdlog = parameters[2])
    )}
  )
  ,distributionClass(
    distrID='Gamma'
    ,distr_label='Gamma'
    ,paramIDs=c("rate", "scale")
    ,param_labels=c("rate", "scale")
    ,param_min_values=c(0,0)
    ,param_max_values=c(NA,1)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rgamma(n = number_of_simulations, rate = parameters[1], scale = parameters[2])
    )}
  )
  ,distributionClass(
    distrID='Exponential'
    ,distr_label='Exponential'
    ,paramIDs=c("rate")
    ,param_labels=c("rate")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rexp(n = number_of_simulations, rate = parameters[1])
    )}
  )
  ,distributionClass(
    distrID='Pareto'
    ,distr_label='Pareto'
    ,paramIDs=c("alpha", "x_m")
    ,param_labels=c("alpha", "x_m")
    ,param_min_values=c(0, 0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rpareto(n = number_of_simulations, alpha = parameters[1], x_m = parameters[2])
    )}
  )
  ,distributionClass(
    distrID='Fixed_Severity'
    ,distr_label='Fixed Severity'
    ,paramIDs=c("Fixed_sev_amount")
    ,param_labels=c("Fixed Severity Amount")
    ,param_min_values=c(0)
    ,simulate_func = function(number_of_simulations, parameters){return(
      rep(parameters[1], number_of_simulations)
    )}
  )
)

names(sev_dist_options) <- sapply(sev_dist_options, function(x) x@distrID)
sev_dist_parameter_placeholders <- data.frame(
  param_number = 1:max(sapply(sev_dist_options, function(x) length(x@paramIDs)))
)
sev_dist_parameter_placeholders$param_id <- paste0("sev_param_", sev_dist_parameter_placeholders$param_number)


#simulate function
simulate_function <- function(
    numOfSimulations,
    freq_params,
    sev_params,
    seedSetBinary,
    seedValue,
    freqDistr,
    sevDistr,
    paretoSlice,
    pareto_slice_times,
    slice_pareto_alphas,
    slice_pareto_x_ms,
    sevCapBinary,
    sev_cap_amount,
    reinsuranceStructureEEL,
    reinsurance_structure_eel_dedctible_amount,
    reinsurance_structure_eel_limit_amount,
    reinsuranceStructureAL,
    reinsurance_structure_al_dedctible_amount,
    reinsurance_structure_al_limit_amount,
    reinsuranceStructureLimitedReinstatements,
    reinsuranceStructureReinstatementLimit,
    multiprocessing
  ){
  #set custom seed
  if(seedSetBinary){set.seed(seedValue)}
  #initiate data and simulate counts
  data <- data.frame(claim_counts = freq_dist_options[[freqDistr]]@simulate_func(
    number_of_simulations = numOfSimulations
    ,parameters = freq_params
  ))

  #predefine parameters in a simulate severity function
  #simulate claims from the chosen distribution and parammeters
  simulate_individual_severities_parametrised <- function (claim_counts){

    claims = sev_dist_options[[sevDistr]]@simulate_func(
      number_of_simulations = claim_counts
      ,parameters = sev_params
    )
    #apply pareto slices
    if(paretoSlice){
      for(j in 1:pareto_slice_times){
        claims = ifelse(
          claims > slice_pareto_x_ms[j]
          ,rpareto(n = claim_counts, alpha = slice_pareto_alphas[j], x_m = slice_pareto_x_ms[j])
          ,claims
        )
      }
    }
    #apply severity cap
    claims = apply_severity_cap(
      claims
      ,severity_cap_boolean = sevCapBinary
      ,severity_cap_amount = sev_cap_amount
    )
    #apply EEL deductible
    claims = apply_deductible_limit(
      claims
      ,reinsurance_structure = reinsuranceStructureEEL
      ,deductible = reinsurance_structure_eel_dedctible_amount
      ,limit = reinsurance_structure_eel_limit_amount
    )
    #sum individual claims and return them
    claims = sum(claims)
    return(claims)
  }

  #simulate individual severities, apply severity cap, apply reinsurance structure EEL and take a sum of individual claims
  if(multiprocessing){plan(multisession)}
  data$total_claims <- unlist(
    if(multiprocessing){
      future_lapply(
        future.seed = T
        ,data$claim_counts
        ,function(z) {simulate_individual_severities_parametrised(z)}
      )
    } else {
      lapply(
        data$claim_counts
        ,function(z) {simulate_individual_severities_parametrised(z)}
      )
    }
  )
  if(multiprocessing){plan(sequential)}
  #apply reinstatements
  if(reinsuranceStructureEEL  %in% c('Limited Layer')){
    if(reinsuranceStructureLimitedReinstatements){
      data$total_claims <- apply_deductible_limit(
        data$total_claims
        ,'Limited Layer'
        ,0
        ,(reinsuranceStructureReinstatementLimit + 1) * reinsurance_structure_eel_limit_amount
      )
      data$number_of_reinstatements_used <- (data$total_claims / reinsurance_structure_eel_limit_amount)
      data$number_of_reinstatements_used <- ifelse(
        data$number_of_reinstatements_used>reinsuranceStructureReinstatementLimit
        ,reinsuranceStructureReinstatementLimit
        ,data$number_of_reinstatements_used
      )
      data$number_of_reinstatements_used <- round(data$number_of_reinstatements_used,2)
    }
  }
  #apply reinsurance structure AL
  data$total_claims <- apply_deductible_limit(
    data$total_claims
    ,reinsuranceStructureAL
    ,reinsurance_structure_al_dedctible_amount
    ,reinsurance_structure_al_limit_amount
  )
  data$total_claims <- round(data$total_claims,2)
  return(data)
}

#run app function
run_shiny_simulator = function(){shinyApp(ui = ui, server = server)}
