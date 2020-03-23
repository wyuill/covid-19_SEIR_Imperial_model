## Trust catchment populations

Trust_Catchment_Populations_raw <- read_excel("populations/2019 Trust Catchment Populations Worksheet.xlsx", sheet = "Trust Analysis")

Trust_Catchment_Populations <- Trust_Catchment_Populations_raw %>%
  select(CatchmentYear, AdmissionType, TrustCode, TrustName, Age, Catchment) %>%
  filter(CatchmentYear==max(CatchmentYear)) %>%
  mutate(age = recode(Age, "00-04" = "0-9", "05-09" = "0-9", "10-14" = "10-19", "15-19" = "10-19",
                      "20-24" = "20-29", "25-29" = "20-29", "30-34" = "30-39", "35-39" = "30-39",
                      "40-44" = "40-49", "44-49" = "40-49", "50-54" = "50-59", "54-59" = "50-59",
                      "60-64" = "60-69", "65-69" = "60-69", "70-74" = "70-79", "75-79" = "70-79",
                      "80-84" = "80+", "85-89" = "80+", "90+" = "80+"),
         area = paste(TrustCode, "-", TrustName),
         type = paste("Trust Catchment -", AdmissionType),
         year = as.character(CatchmentYear)) %>%
  group_by(year, area, type, age) %>%
  summarise(population = sum(Catchment))

## LA populations

la_populations_raw <- read_excel("populations/nomis_2020_03_21_212322.xlsx", skip = 5)

la_populations <- la_populations_raw %>%
  select(-`All Ages`) %>%
  pivot_longer(-Area, names_to = "Age") %>%
  mutate(age = recode(Age, "Age 0 - 4" = "0-9", "Aged 5-9" = "0-9", "Aged 10-14" = "10-19", "Aged 15-19" = "10-19",
                "Aged 20-24" = "20-29", "Aged 25-29" = "20-29", "Aged 30-34" = "30-39", "Aged 35-39" = "30-39",
                "Aged 40-44" = "40-49", "Aged 45-49" = "40-49", "Aged 50-54" = "50-59", "Aged 55-59" = "50-59",
                "Aged 60-64" = "60-69", "Aged 65-69" = "60-69", "Aged 70-74" = "70-79", "Aged 75-79" = "70-79",
                "Aged 80-84" = "80+", "Aged 85+" = "80+", "Aged 90+" = "80+"),
         type = "Resident",
         year = "2018",
         area = Area) %>%
  group_by(year, area, type, age) %>%
  summarise(population = sum(value))

## NHS Populations

nhs_populations_raw <- read_csv("populations/gp-reg-pat-prac-sing-age-regions (1).csv", 
                                col_types = cols(PUBLICATION = col_skip(), SEX = col_skip()))
nhs_populations <- nhs_populations_raw %>%
  mutate(age = case_when(AGE >= 0 & AGE < 10 ~ "0-9",
                         AGE >= 10 & AGE < 20 ~ "10-19",
                         AGE >= 20 & AGE < 30 ~ "20-29",
                         AGE >= 30 & AGE < 40 ~ "30-39",
                         AGE >= 40 & AGE < 50 ~ "40-49",
                         AGE >= 50 & AGE < 60 ~ "50-59",
                         AGE >= 60 & AGE < 70 ~ "60-69",
                         AGE >= 70 & AGE < 80 ~ "70-79",
                         AGE >= 80 ~ "80+",
                         TRUE ~ "error"),
         area = paste(ORG_TYPE, ORG_CODE, ONS_CODE),
         type = "Registered",
         year = EXTRACT_DATE) %>%
  group_by(year, area, type, age) %>%
  summarise(population = sum(NUMBER_OF_PATIENTS))

populations <- rbind(Trust_Catchment_Populations, la_populations, nhs_populations)

write.csv(populations, file="populations/populations.csv")
