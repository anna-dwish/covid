DATA LIST FILE= "/Users/annadarwish/Desktop/senior/case-studies/covid/final_mice_imp/final_mice_imp__SPSS.txt"  free (TAB)
   / Imputation_ age DEMOGRAPHICS...GENDER 
  Q4..Number.of.People.in.HH Q5..Children.in.HH 
  Q18..College.Degree   Q19.20..Race...Ethnicity (A15) week 
  trump_approve_score Social.Distancing.Adherence .


VARIABLE LABELS
  Imputation_ "Imputation_" 
 age "age" 
 DEMOGRAPHICS...GENDER "DEMOGRAPHICS...GENDER" 
 Q4..Number.of.People.in.HH "Q4..Number.of.People.in.HH" 
 Q5..Children.in.HH "Q5..Children.in.HH" 
 Q18..College.Degree "Q18..College.Degree" 
 Q19.20..Race...Ethnicity "Q19.20..Race...Ethnicity" 
 week "week" 
 trump_approve_score "trump_approve_score" 
 Social.Distancing.Adherence "Social.Distancing.Adherence" 
 .

VALUE LABELS
 / DEMOGRAPHICS...GENDER 
   1 "Female"
   2 "Male"
 / Q5..Children.in.HH 
   1 "0"
   2 "1"
   3 "2"
   4 "3"
 / Q18..College.Degree 
   1 "FALSE"
   2 "TRUE"
 .

EXECUTE.
SORT CASES by Imputation_.
SPLIT FILE layered by Imputation_.
