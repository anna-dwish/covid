DATA LIST FILE= "/Users/annadarwish/Desktop/senior/thesis/covid/sensitivity_imp/sensitivity_imp__SPSS.txt"  free (TAB)
   / Imputation_ Q1..Health.Quality age 
  DEMOGRAPHICS...GENDER Q4..Number.of.People.in.HH 
  Q5..Children.in.HH Q6..Non.HH.Face.to.Face.Count 
  Q7..Six.Feet.Away...If.Q6...0. Q18..College.Degree 
  Q19.20..Race...Ethnicity trump_approve_score 
  age_race_ethnicity_asian age_race_ethnicity_black 
  age_race_ethnicity_hispanic age_race_ethnicity_other 
  age_race_ethnicity_white Social.Distancing.Adherence .


VARIABLE LABELS
  Imputation_ "Imputation_" 
 Q1..Health.Quality "Q1..Health.Quality" 
 age "age" 
 DEMOGRAPHICS...GENDER "DEMOGRAPHICS...GENDER" 
 Q4..Number.of.People.in.HH "Q4..Number.of.People.in.HH" 
 Q5..Children.in.HH "Q5..Children.in.HH" 
 Q6..Non.HH.Face.to.Face.Count "Q6..Non.HH.Face.to.Face.Count" 
 Q7..Six.Feet.Away...If.Q6...0. "Q7..Six.Feet.Away...If.Q6...0." 
 Q18..College.Degree "Q18..College.Degree" 
 Q19.20..Race...Ethnicity "Q19.20..Race...Ethnicity" 
 trump_approve_score "trump_approve_score" 
 age_race_ethnicity_asian "age_race_ethnicity_asian" 
 age_race_ethnicity_black "age_race_ethnicity_black" 
 age_race_ethnicity_hispanic "age_race_ethnicity_hispanic" 
 age_race_ethnicity_other "age_race_ethnicity_other" 
 age_race_ethnicity_white "age_race_ethnicity_white" 
 Social.Distancing.Adherence "Social.Distancing.Adherence" 
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
