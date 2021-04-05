library(tidyverse)
library(R2jags)
library(dplyr)

args = (commandArgs(TRUE))

beta_mean = args[1]
beta_precision = args[2]

a_mean = args[3]
a_precision = args[4]

b_mean = args[5]
b_precision = args[6]

`wtd.mean` <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE) 
{
  if (!length(weights)) 
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  sum(weights * x)/sum(weights)
}

`wtd.var` <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE) 
{
  if (!length(weights)) {
    if (na.rm) 
      x <- x[!is.na(x)]
    return(var(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt) 
    weights <- weights * length(x)/sum(weights)
  xbar <- sum(weights * x)/sum(weights)
  sum(weights * ((x - xbar)^2))/(sum(weights) - 1)
}

set.seed(123)

covid.df <- read.csv("merged_surveys.csv")
covid.df[covid.df==""]<-NA

## take most recent survey answers for each unique participant, filter out anyone that immediately terminated survey but had voter registration information to populate other fields (3 participants)
covid.df <- covid.df %>% group_by(uniqueID) %>% top_n(1, Date) %>% ungroup() %>%
  filter(!is.na(Q1..Health.Quality))

# Assuming 0-3 means poor, 4-5 means fair, 6-7 means good, 8-9 means very good
covid.df <- covid.df %>% mutate(Q1..Health.Quality = case_when(
  Q1..Health.Quality == "6" ~ "Good",
  Q1..Health.Quality == "7" ~ "Good",
  Q1..Health.Quality == "5" ~ "Fair",
  Q1..Health.Quality == "9" ~ "Very Good",
  TRUE ~ Q1..Health.Quality))

## Convert health quality to numeric
covid.df <- covid.df %>%  mutate(
  health = case_when(
    Q1..Health.Quality == "Poor" ~ 0,
    Q1..Health.Quality == "Fair" ~ 1,
    Q1..Health.Quality == "Good" ~ 2,
    Q1..Health.Quality == "Very Good" ~ 3
  ))


## Weighted Mean Center Age
covid.df$age <- (covid.df$age - wtd.mean(covid.df$age, covid.df$weight))/sqrt(wtd.var(covid.df$age, covid.df$weight))

## Make gender a factor
covid.df$DEMOGRAPHICS...GENDER <- as.factor(covid.df$DEMOGRAPHICS...GENDER)

## Make race/ethnicity a factor
covid.df <- covid.df %>%  mutate(
  Q19.20..Race...Ethnicity = case_when(
    Q19.20..Race...Ethnicity == "Another race" ~ "Other",
    Q19.20..Race...Ethnicity == "Hispanic or Latino" ~ "Hispanic/Latino",
    TRUE ~ Q19.20..Race...Ethnicity
  ))
covid.df$Q19.20..Race...Ethnicity <- as.factor(covid.df$Q19.20..Race...Ethnicity)
covid.df$Q19.20..Race...Ethnicity <- relevel(covid.df$Q19.20..Race...Ethnicity, ref="White")

## Generate esda
covid.df <- covid.df %>% dplyr::mutate(esda = case_when(
  !is.na(Q6..Non.HH.Face.to.Face.Count) &  Q6..Non.HH.Face.to.Face.Count == 0 ~ TRUE,
  !is.na(Q6..Non.HH.Face.to.Face.Count) &
    !is.na(Q7..Six.Feet.Away...If.Q6...0.) &
    Q7..Six.Feet.Away...If.Q6...0. -  Q6..Non.HH.Face.to.Face.Count != 0 ~ FALSE,
  !is.na(Q6..Non.HH.Face.to.Face.Count) &
    !is.na(Q7..Six.Feet.Away...If.Q6...0.) &
    Q7..Six.Feet.Away...If.Q6...0. -  Q6..Non.HH.Face.to.Face.Count == 0 ~ TRUE,
  TRUE ~ NA))



covid.df<- covid.df %>% mutate(
  miss.esda = case_when(
    is.na(esda) ~ 1,
    TRUE ~ 0
  )
)

## Select Relevant Variables and rename columns
covid.df.demographic <- covid.df %>% dplyr::select("age",
                                                   "DEMOGRAPHICS...GENDER",
                                                   "health",
                                                   "Q19.20..Race...Ethnicity",
                                                   "esda",
                                                   "miss.esda")
colnames(covid.df.demographic) <- c("age",
                                    "gender",
                                    "health",
                                    "race.ethnicity",
                                    "esda",
                                    "miss.esda")

create_model_text_file <- function(file_name, mu_params){

  ## GENERATE MU EQUATION FROM PARAMS
  params_indexed <- paste0(mu_params, "[i]")

  betas <- paste0(rep("beta",length(mu_params)),
                  as.character(1:length(mu_params)))

  betas_params <- paste0(paste0(betas,
                                rep("*",length(mu_params))),
                         params_indexed)

  mu_eqn <- paste0(c("alpha", betas_params), collapse=" + ")

  ## GENERATE BETA PRIORS
  beta_prior_input <- paste0(" ~ dnorm(", beta_mean, ",", beta_precision, ")")
  betas_priors <- paste0(paste0(betas, beta_prior_input), collapse="\n")
  
  ## GENERATE A AND B PRIORS
  a_prior <- paste0("a ~ dnorm(", a_mean, ",", a_precision, ")")
  b_prior <- paste0("b ~ dnorm(", b_mean, ",", b_precision, ")")

  cat("model{
    for (i in 1:N) {
    mu[i] <- ",
      mu_eqn,
      "
    y[i] ~ dbern(mu[i])

    miss[i] ~ dbern(p[i])
    logit(p[i]) <- a + b*(y[i])

    }\n",
    a_prior,
    "\n",
    b_prior,
    "\n",
    "alpha ~ dnorm(0, 1)",
    "\n",
      betas_priors,
      "\n}",
      file=file_name)
}

get_model <- function(dat,
                      params_to_save,
                      model_text_file,
                      model_name,
                      n_iter=300000,
                      n_chains=3,
                      n_burn_in=8000)
{
  if (file.exists(model_name)) {
    load(file=model_name)
  }
  else {
    rjags_model <- jags(data=dat,
                        parameters.to.save=params_to_save,
                        n.iter=n_iter,
                        n.chains=n_chains,
                        n.burnin=n_burn_in,
                        model.file=model_text_file)
    save(rjags_model, file = model_name)
  }
  return(rjags_model)
}

esda_dat <- list(gender = covid.df.demographic$gender,
                 race_ethnicity_asian=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Asian")),
                 race_ethnicity_black=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Black")),
                 race_ethnicity_hispanic=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Hispanic/Latino")),
                 race_ethnicity_other=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Other")),
                 age_race_ethnicity_asian=covid.df.demographic$age*as.numeric(covid.df.demographic$race.ethnicity == "Asian"),
                 age_race_ethnicity_black=covid.df.demographic$age*as.numeric(covid.df.demographic$race.ethnicity == "Black"),
                 age_race_ethnicity_hispanic=covid.df.demographic$age*as.numeric(covid.df.demographic$race.ethnicity == "Hispanic/Latino"),
                 age_race_ethnicity_other=covid.df.demographic$age*as.numeric(covid.df.demographic$race.ethnicity == "Other"),
                 age_race_ethnicity_white=covid.df.demographic$age*as.numeric(covid.df.demographic$race.ethnicity == "White"),
                 miss = covid.df.demographic$miss.esda,
                 N = nrow(covid.df.demographic))

esda_mu_params <- head(names(esda_dat), length(esda_dat)-2)
esda_model_prefix <- paste0("beta_",
                               beta_mean,
                               "_",
                               beta_precision,
                              "-a_",
                              a_mean,
                              "_",
                              a_precision,
                              "-b_",
                              b_mean,
                              "_",
                              b_precision)
esda_model_text_file = paste0(esda_model_prefix, ".txt")

create_model_text_file(file_name=esda_model_text_file,
                       mu_params=esda_mu_params)

esda_params_to_save <-c(c("mu", "y", "alpha"),
                        paste0(rep("beta",length(esda_mu_params)),
                               paste(1:length(esda_mu_params))),
                        c("a", "b"))
esda_model_name <- paste0(esda_model_prefix, ".Rdata")

esda_model <- get_model(dat=esda_dat,
                        params_to_save=esda_params_to_save,
                        model_text_file=esda_model_text_file,
                        model_name=esda_model_name)



