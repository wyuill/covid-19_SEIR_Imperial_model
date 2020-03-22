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
                           critical_care_nasal = 0,
                           critical_care_ventilation_noninvasive = 0,
                           critical_care_ventilation_invasive = 0,
                           critical_care_ventilation_prone = 0,
                           critical_care_ecmo = 0,
                           critical_care_renal_replacement = 0,
                           critical_care_vasoconstrictive = 0,
                           critical_care_antiviral = 0,
                           critical_care_antibacterial = 0,
                           critical_care_glucocorticiods = 0,
                           critical_care_immunoglobulin = 0,
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
                                critical_care_pct, critical_care_nasal, 
                                critical_care_ventilation_noninvasive,
                                critical_care_ventilation_invasive,
                                critical_care_ventilation_prone, critical_care_ecmo,
                                critical_care_renal_replacement, critical_care_vasoconstrictive,
                                critical_care_antiviral, critical_care_antibacterial,
                                critical_care_glucocorticiods, critical_care_immunoglobulin,
                                incidence_fatatilty_rate,
                                los_hopsital, los_critical_care,
                                mod_critical_care, mod_deaths)
    return(seir_data)
  }
}

service_demand <- function(data, symptomatic_pct, hospitalised_pct,
                           critical_care_pct, critical_care_nasal, 
                           critical_care_ventilation_noninvasive,
                           critical_care_ventilation_invasive,
                           critical_care_ventilation_prone, critical_care_ecmo,
                           critical_care_renal_replacement, critical_care_vasoconstrictive,
                           critical_care_antiviral, critical_care_antibacterial,
                           critical_care_glucocorticiods, critical_care_immunoglobulin,
                           incidence_fatatilty_rate,
                           los_hopsital, los_critical_care,
                           mod_critical_care, mod_deaths){
  
  # This function applies weights to different populations to estimate the impact
  # on services based on the SEIR model
  
  data <- data %>%
    mutate(symptomatic = infected * symptomatic_pct,
           hospitalised_admissions = symptomatic * hospitalised_pct,
           hospitalised_occupied = rollapplyr(hospitalised_admissions, los_hopsital, sum, partial = TRUE),
           critical_care_admissions = (hospitalised_admissions * critical_care_pct) * mod_critical_care,
           critical_care_occupied = rollapplyr(critical_care_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_nasal_admissions = critical_care_admissions * critical_care_nasal,
           critical_care_nasal_occupied = rollapplyr(critical_care_nasal_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_ventilation_noninvasive_admissions = critical_care_admissions * critical_care_ventilation_noninvasive,
           critical_care_ventilation_noninvasive_occupied = rollapplyr(critical_care_ventilation_noninvasive_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_ventilation_invasive_admissions = critical_care_admissions * critical_care_ventilation_invasive,
           critical_care_ventilation_invasive_occupied = rollapplyr(critical_care_ventilation_invasive_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_ventilation_prone_admissions = critical_care_admissions * critical_care_ventilation_prone,
           critical_care_ventilation_prone_occupied = rollapplyr(critical_care_ventilation_prone_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_ecmo_admissions = critical_care_admissions * critical_care_ecmo,
           critical_care_ecmo_occupied = rollapplyr(critical_care_ecmo_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_renal_replacement_admissions = critical_care_admissions * critical_care_renal_replacement,
           critical_care_renal_replacement_occupied = rollapplyr(critical_care_renal_replacement_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_vasoconstrictive_admissions = critical_care_admissions * critical_care_vasoconstrictive,
           critical_care_vasoconstrictive_occupied = rollapplyr(critical_care_vasoconstrictive_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_antiviral_admissions = critical_care_admissions * critical_care_antiviral,
           critical_care_antiviral_occupied = rollapplyr(critical_care_antiviral_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_antibacterial_admissions = critical_care_admissions * critical_care_antibacterial,
           critical_care_antibacterial_occupied = rollapplyr(critical_care_antibacterial_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_glucocorticiods_admissions = critical_care_admissions * critical_care_glucocorticiods,
           critical_care_glucocorticiods_occupied = rollapplyr(critical_care_glucocorticiods_admissions, los_critical_care, sum, partial = TRUE),
           critical_care_immunoglobulin_admissions = critical_care_admissions * critical_care_immunoglobulin,
           critical_care_immunoglobulin_occupied = rollapplyr(critical_care_immunoglobulin_admissions, los_critical_care, sum, partial = TRUE),
           deaths = ((critical_care_admissions * 0.5) + (hospitalised_admissions - critical_care_admissions) * incidence_fatatilty_rate) * mod_deaths)
  
  return(data)
 
}
