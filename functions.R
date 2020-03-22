imperial_model <- function(days = 200,
                           initial_susceptible = 10000,
                           initial_exposed = 0,
                           initial_infected = 1,
                           initial_recovered = 0,
                           R0 = 2.4,
                           latent_period = 5.1,
                           infectious_period = 4.6,
                           birth_rate = 0,
                           mortality_rate = 0,
                           service_impact = TRUE,
                           symptomatic_pct = 0,
                           hospitalised_pct = 0,
                           critical_care_pct = 0,
                           critical_care_l3_pct = 0,
                           incidence_fatatilty_rate = 0,
                           los_hopsital = 1,
                           los_critical_care = 1,
                           los_critical_care_l3 = 1,
                           mod_critical_care = 1,
                           mod_deaths = 1) {
  
  # This function calacutes a SEIR model and provides an option to apply the model
  # to estimates of how many patients may require services
  
  total_population = initial_susceptible + initial_exposed + initial_infected + initial_recovered
  effective_contact_rate = R0 / infectious_period 
  new_exposure_rate = effective_contact_rate / initial_susceptible # Beta - total pop or initial suspectible??
  new_infection_rate = 1 / latent_period # Sigma
  recovery_rate = 1 / infectious_period # Gamma
  
  seir_data <- list(c(1, initial_susceptible, initial_exposed, initial_infected, initial_recovered, total_population))
  
  for(i in 2:days){
    susceptible = seir_data[[i-1]][2] - new_exposure_rate * seir_data[[i-1]][2] *
      seir_data[[i-1]][4] + birth_rate - mortality_rate
    
    exposed = seir_data[[i-1]][3] + new_exposure_rate * susceptible *
      seir_data[[i-1]][4] - new_infection_rate * seir_data[[i-1]][3]
    
    infected = seir_data[[i-1]][4] + new_infection_rate * seir_data[[i-1]][3] -
      recovery_rate * seir_data[[i-1]][4]
    
    recovered = seir_data[[i-1]][5] + recovery_rate * seir_data[[i-1]][4]
    
    seir_data[i] <- list(c(i, susceptible, exposed, infected, recovered, total_population))
    
  }
  
  seir_data <- do.call(rbind, seir_data) %>%
    as.data.frame() %>%
    rename(days = V1, susceptible = V2, exposed = V3, infected = V4, recovered = V5, total_pop = V6)
  
  if(service_impact == FALSE){
    return(seir_data)
  } else {
    seir_data <- service_demand(seir_data, symptomatic_pct, hospitalised_pct,
                                critical_care_pct, critical_care_l3_pct,
                                incidence_fatatilty_rate, los_hopsital, 
                                los_critical_care, los_critical_care_l3,
                                mod_critical_care, mod_deaths)
    return(seir_data)
  }
}

service_demand <- function(data, symptomatic_pct, hospitalised_pct, critical_care_pct,
                           critical_care_l3_pct, incidence_fatatilty_rate, los_hopsital,
                           los_critical_care, los_critical_care_l3, mod_critical_care,
                           mod_deaths){
  
  # This function applies weights to different populations to estimate the impact
  # on services based on the SEIR model
  
  data <- data %>%
    mutate(symptomatic = infected * symptomatic_pct,
           hospitalised_admissions = symptomatic * hospitalised_pct,
           hospitalised_occupied = rollapplyr(hospitalised_admissions, los_hopsital, sum, partial = TRUE),
           critical_care_admissions = (hospitalised_admissions * critical_care_pct) * mod_critical_care,
           critical_care_occupied = rollapplyr(critical_care_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_l3_admissions = critical_care_admissions * critical_care_l3_pct,
           critical_care_l3_occupied = rollapplyr(critical_care_l3_admissions, los_critical_care_l3, sum, partial = TRUE),
           deaths = ((critical_care_admissions * 0.5) + (hospitalised_admissions - critical_care_admissions) * incidence_fatatilty_rate) * mod_deaths)
  
  return(data)
 
}
