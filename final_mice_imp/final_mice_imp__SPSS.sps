DATA LIST FILE= "/Users/annadarwish/Desktop/senior/thesis/covid/final_mice_imp/final_mice_imp__SPSS.txt"  free (TAB)
   / Imputation_ age Q1..Health.Quality 
  DEMOGRAPHICS...GENDER Q4..Number.of.People.in.HH 
  Q5..Children.in.HH Q18..College.Degree 
  Q19.20..Race...Ethnicity week trump_approve_score 
  Social.Distancing.Adherence age_race_ethnicity_asian 
  age_race_ethnicity_black age_race_ethnicity_hispanic 
  age_race_ethnicity_other age_race_ethnicity_white .


VARIABLE LABELS
  Imputation_ "Imputation_" 
 age "age" 
 Q1..Health.Quality "Q1..Health.Quality" 
 DEMOGRAPHICS...GENDER "DEMOGRAPHICS...GENDER" 
 Q4..Number.of.People.in.HH "Q4..Number.of.People.in.HH" 
 Q5..Children.in.HH "Q5..Children.in.HH" 
 Q18..College.Degree "Q18..College.Degree" 
 Q19.20..Race...Ethnicity "Q19.20..Race...Ethnicity" 
 week "week" 
 trump_approve_score "trump_approve_score" 
 Social.Distancing.Adherence "Social.Distancing.Adherence" 
 age_race_ethnicity_asian "age_race_ethnicity_asian" 
 age_race_ethnicity_black "age_race_ethnicity_black" 
 age_race_ethnicity_hispanic "age_race_ethnicity_hispanic" 
 age_race_ethnicity_other "age_race_ethnicity_other" 
 age_race_ethnicity_white "age_race_ethnicity_white" 
 .

VALUE LABELS
 / DEMOGRAPHICS...GENDER 
   1 "Female"
   2 "Male"
 / Q18..College.Degree 
   1 "FALSE"
   2 "TRUE"
 / Q19.20..Race...Ethnicity 
   1 "White"
   2 "Asian"
   3 "Black"
   4 "Hispanic.Latino"
   5 "Other"
 .

EXECUTE.
SORT CASES by Imputation_.
SPLIT FILE layered by Imputation_.
