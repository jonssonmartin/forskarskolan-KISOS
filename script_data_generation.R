library(dplyr)

set.seed(999)

n <- 1000

LopNr <- 1:n

age_beta <- rbeta(n, shape1 = 5, shape2 = 2.5)  # Right-skewed
age <- round(20 + age_beta * (100 - 20))

sex <- sample(c("Female", "Male"), size = n, replace = TRUE, prob = c(0.3333, 0.6667))

treatment <- sample(c("Control", "Treatment"), size = n, replace = TRUE)

survival <- ifelse(
  treatment == "Treatment",
  rbinom(n, 1, prob = 0.18),
  rbinom(n, 1, prob = 0.12)
)

trial_data <- data.frame(
  LopNr = LopNr,
  Age = age,
  Sex = sex,
  Treatment = treatment,
  Survival = survival
)

summary(trial_data)

write.csv(trial_data, "df_trial_data.csv", row.names = FALSE)


################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################# SLUTENVÅRD

# Load necessary libraries
library(dplyr)
library(lubridate)

icd_codesSV <- c("I109", "I210", "I509", "E110", "J930", "I460", "I652", "I071", "S090", "I702",
               "I251", "I212", "I350", "E100", "I252", "J44", "I719", "J961", "I259", "I480",
               "I482", "I352", "J181", "I219", "I251", "C349", "C209", "C160")

random_date <- function(start_year = 1980, end_year = 2025) {
  start <- as.Date(paste0(start_year, "-01-01"))
  end <- as.Date(paste0(end_year, "-12-31"))
  as.Date(runif(1, min = as.numeric(start), max = as.numeric(end)), origin = "1970-01-01")
}

generate_row <- function(lopnr) {
  indatum <- random_date()
  utdatum <- indatum + sample(1:10, 1)
  year <- year(indatum)
  num_dia <- sample(1:10, 1)
  dias <- sample(icd_codesSV, num_dia, replace = TRUE)
  dias <- c(dias, rep("", 10 - num_dia))
  data.frame(LopNr = lopnr, AR = year, INDATUM = indatum, UTDATUM = utdatum,
             DIA1 = dias[1], DIA2 = dias[2], DIA3 = dias[3], DIA4 = dias[4], DIA5 = dias[5],
             DIA6 = dias[6], DIA7 = dias[7], DIA8 = dias[8], DIA9 = dias[9], DIA10 = dias[10],
             stringsAsFactors = FALSE)
}

set.seed(999)  #
n_individuals <- 500
SV <- data.frame()

for (i in 1:n_individuals) {
  n_rows <- sample(1:20, 1, prob = ifelse(1:20 == 2, 0.2, 0.8/19))  #
  for (j in 1:n_rows) {
    SV <- bind_rows(SV, generate_row(i))
  }
}

# View the first few rows
head(SV)
nrow(SV)
SV <- SV |> arrange(LopNr, INDATUM)

# Optionally save to CSV
# write.csv(dataset, "simulated_dataset.csv", row.names = FALSE)

write.csv(SV, "PAR_SV_project.csv", row.names = FALSE)

################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################## ÖPPENVÅRD


# Load necessary libraries
library(dplyr)
library(lubridate)

icd_codesOV <- c("I109", "I210", "I509", "E110", "J930", "I460", "I652", "I071", "S090", "I702",
               "I251", "I212", "I350", "E100", "I252", "J44", "I719", "J961", "I259", "I480",
               "I482", "I352", "J181", "I219", "I251", "C349", "C209", "C160")

random_date <- function(start_year = 2001, end_year = 2025) {
  start <- as.Date(paste0(start_year, "-01-01"))
  end <- as.Date(paste0(end_year, "-12-31"))
  as.Date(runif(1, min = as.numeric(start), max = as.numeric(end)), origin = "1970-01-01")
}

generate_row <- function(lopnr) {
  indatum <- random_date()
  utdatum <- indatum + sample(1:10, 1)
  year <- year(indatum)
  num_dia <- sample(1:10, 1)
  dias <- sample(icd_codesOV, num_dia, replace = TRUE)
  dias <- c(dias, rep("", 10 - num_dia))
  data.frame(LopNr = lopnr, AR = year, INDATUM = indatum, UTDATUM = utdatum,
             DIA1 = dias[1], DIA2 = dias[2], DIA3 = dias[3], DIA4 = dias[4], DIA5 = dias[5],
             DIA6 = dias[6], DIA7 = dias[7], DIA8 = dias[8], DIA9 = dias[9], DIA10 = dias[10],
             stringsAsFactors = FALSE)
}

# Generate dataset
set.seed(999)  #
n_individuals <- 500
OV <- data.frame()

for (i in 1:n_individuals) {
  n_rows <- sample(1:20, 1, prob = ifelse(1:20 == 1, 0.2, 0.8/19))  #
  for (j in 1:n_rows) {
    OV <- bind_rows(OV, generate_row(i))
  }
}

head(OV)
nrow(OV)

OV <- OV |> arrange(LopNr, INDATUM)

write.csv(OV, "PAR_OV_project.csv", row.names = FALSE)

################################################################################################################################################################################################################################################################################################################################################################## SCB


set.seed(999)

library(dplyr)
library(tidyr)

n_individuals <- 1000
years <- 2015:2025

sun2000niva_dist <- data.frame(
  Sun2000niva = c(100,106,200,204,206,310,312,313,316,317,320,322,323,326,327,
                  330,332,333,336,337,410,412,413,415,417,520,522,525,526,527,
                  530,532,535,536,537,540,545,546,547,555,556,557,600,620,640),
  Freq = c(0.0102038,0.2569920,0.0088305,0.0185867,0.0962522,0.0059737,0.0011507,
           0.0031404,0.0000375,0.0450760,0.0681436,0.0007523,0.0045676,0.0232340,
           0.1350263,0.0049801,0.0018936,0.0080478,0.0618628,0.0454345,0.0014014,
           0.0116123,0.0104523,0.0030115,0.0025967,0.0083267,0.0016780,0.0144972,
           0.0020764,0.0253127,0.0025568,0.0027701,0.0044973,0.0173142,0.0232785,
           0.0014788,0.0003656,0.0028474,0.0279446,0.0002297,0.0001781,0.0074174,
           0.0004054,0.0013452,0.0049285)
)

sun2000niva_dist$Freq <- sun2000niva_dist$Freq / sum(sun2000niva_dist$Freq)

sun2000niva_dist$Scale <- (sun2000niva_dist$Sun2000niva - min(sun2000niva_dist$Sun2000niva)) /
                          (max(sun2000niva_dist$Sun2000niva) - min(sun2000niva_dist$Sun2000niva)) + 1

initial_sun <- sample(sun2000niva_dist$Sun2000niva, size = n_individuals, replace = TRUE, prob = sun2000niva_dist$Freq)
names(initial_sun) <- 1:n_individuals

scale_map <- sun2000niva_dist$Scale[match(initial_sun, sun2000niva_dist$Sun2000niva)]

base_fam_income <- rlnorm(n_individuals, meanlog = 7.5, sdlog = 0.6) * scale_map
base_ind_income <- base_fam_income * runif(n_individuals, min = 0.4, max = 1.0)

base_data <- data.frame(
  LopNr = 1:n_individuals,
  Sun2000niva = initial_sun,
  DispInkFam04 = round(base_fam_income),
  DispInk04 = round(base_ind_income)
)

sim_data <- expand.grid(LopNr = 1:n_individuals, Year = years) %>%
  left_join(base_data, by = "LopNr") %>%
  arrange(LopNr, Year) %>%
  group_by(LopNr) %>%
  mutate(
    Sun2000niva = {
      sun_vals <- rep(Sun2000niva[1], n())
      for (i in 2:n()) {
        if (runif(1) < 0.02) {
          higher_vals <- sun2000niva_dist$Sun2000niva[sun2000niva_dist$Sun2000niva > sun_vals[i - 1]]
          if (length(higher_vals) > 0) {
            sun_vals[i] <- sample(higher_vals, 1)
          } else {
            sun_vals[i] <- sun_vals[i - 1]
          }
        } else {
          sun_vals[i] <- sun_vals[i - 1]
        }
      }
      sun_vals
    },
    DispInkFam04 = round(DispInkFam04[1] * cumprod(1 + runif(n(), -0.01, 0.03))),  
    DispInk04 = round(DispInkFam04 * DispInk04[1] / DispInkFam04[1])
  ) %>%
  ungroup()

sim_data <- sim_data |> mutate(DispInkFam04 = ifelse(DispInkFam04<DispInk04, DispInkFam04+100, DispInkFam04))


sim_data %>% filter(LopNr == 1)


write.csv(sim_data |> filter(Year==2015), "Project_LISA_2015.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2016), "Project_LISA_2016.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2017), "Project_LISA_2017.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2018), "Project_LISA_2018.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2019), "Project_LISA_2019.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2020), "Project_LISA_2020.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2021), "Project_LISA_2021.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2022), "Project_LISA_2022.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2023), "Project_LISA_2023.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2024), "Project_LISA_2024.csv", row.names = FALSE)
write.csv(sim_data |> filter(Year==2025), "Project_LISA_2025.csv", row.names = FALSE)



df <- read.csv("https://raw.githubusercontent.com/jonssonmartin/forskarskolan-KISOS/main/PAR_OV_project.csv")


