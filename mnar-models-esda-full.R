library(tidyverse)
library(R2jags)
library(dplyr)

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

## Make ssda a logical vector
covid.df<- covid.df %>% mutate(
  ssda = case_when(
    is.na(Q13..Currently.Practicing.Social.Distancing.) ~ NA,
    Q13..Currently.Practicing.Social.Distancing. == "Yes" ~ TRUE,
    Q13..Currently.Practicing.Social.Distancing. == "No" ~ FALSE,
    TRUE ~ NA
  ))


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
  miss.ssda = case_when(
    is.na(Q13..Currently.Practicing.Social.Distancing.) ~ 1,
    TRUE ~ 0
  ),
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
                                                   "ssda", 
                                                   "esda",
                                                   "miss.ssda",
                                                   "miss.esda")
colnames(covid.df.demographic) <- c("age.centered", 
                                    "gender", 
                                    "health",  
                                    "race.ethnicity", 
                                    "ssda", 
                                    "esda",
                                    "miss.ssda",
                                    "miss.esda")

covid.df.president.trump <- covid.df %>% filter(!is.na(trump_approve_score))
covid.df.president.trump <- covid.df.president.trump %>% dplyr::select("age",
                                                                       "DEMOGRAPHICS...GENDER",
                                                                       "health",
                                                                       "Q19.20..Race...Ethnicity",
                                                                       "trump_approve_score",
                                                                       "ssda",
                                                                       "esda",
                                                                       "miss.ssda",
                                                                       "miss.esda")
colnames(covid.df.president.trump) <- c("age.centered", 
                                        "gender", 
                                        "health",  
                                        "race.ethnicity",
                                        "trump",
                                        "ssda", 
                                        "esda",
                                        "miss.ssda",
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
  betas_priors <- paste0(paste0(betas, " ~ dnorm(0,50)"), collapse="\n")
  
  cat("model{ 
    for (i in 1:N) {
    mu[i] <- ",
      mu_eqn,
      "
    y[i] ~ dbern(mu[i])
    
    miss[i] ~ dbern(p[i])
    logit(p[i]) <- a + b*(y[i])
    
    }
    a ~ dnorm(0.5,50)
    b ~ dnorm(-0.5,50)
    
    alpha ~ dnorm(0, 10)\n",
      betas_priors,
      "\n}", 
      file=file_name)
}

get_model <- function(dat, 
                      params_to_save, 
                      model_text_file, 
                      model_name, 
                      n_iter=200000, 
                      n_chains=3,
                      n_burn_in=5000)
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

esda_dat <- list(age = covid.df.demographic$age.centered,
                 gender = covid.df.demographic$gender,
                 health = covid.df.demographic$health,
                 race_ethnicity_asian=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Asian")),
                 race_ethnicity_black=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Black")),
                 race_ethnicity_hispanic=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Hispanic/Latino")),
                 race_ethnicity_other=as.factor(as.numeric(covid.df.demographic$race.ethnicity == "Other")),
                 miss = covid.df.demographic$miss.esda,
                 N = nrow(covid.df.demographic))

esda_mu_params <- head(names(esda_dat), length(esda_dat)-2)
esda_model_text_file <- "esda-age-gender-health-race-beta_n_0_50-a_05_50_b_-05_50.txt"

create_model_text_file(file_name=esda_model_text_file, 
                       mu_params=esda_mu_params)

esda_params_to_save <-c(c("mu", "y", "alpha"),
                        paste0(rep("beta",length(esda_mu_params)),
                               paste(1:length(esda_mu_params))),
                        c("a", "b"))
esda_model_name <- "esda-age-gender-health-race-beta_n_0_50-a_05_50_b_-05_50.Rdata"

esda_model <- get_model(dat=esda_dat,
                        params_to_save=esda_params_to_save,
                        model_text_file=esda_model_text_file,
                        model_name=esda_model_name)



