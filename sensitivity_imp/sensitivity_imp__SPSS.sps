DATA LIST FILE= "/Users/annadarwish/Desktop/senior/thesis/covid/sensitivity_imp/sensitivity_imp__SPSS.txt"  free (TAB)
   / Imputation_ age Q5..Children.in.HH 
  Q6..Non.HH.Face.to.Face.Count 
  Q7..Six.Feet.Away...If.Q6...0. 
    Q19.20..Race...Ethnicity (A15) 
  Social.Distancing.Adherence .


VARIABLE LABELS
  Imputation_ "Imputation_" 
 age "age" 
 Q5..Children.in.HH "Q5..Children.in.HH" 
 Q6..Non.HH.Face.to.Face.Count "Q6..Non.HH.Face.to.Face.Count" 
 Q7..Six.Feet.Away...If.Q6...0. "Q7..Six.Feet.Away...If.Q6...0." 
 Q19.20..Race...Ethnicity "Q19.20..Race...Ethnicity" 
 Social.Distancing.Adherence "Social.Distancing.Adherence" 
 .

VALUE LABELS
 / Q5..Children.in.HH 
   1 "0"
   2 "1"
   3 "2"
   4 "3"
 .

EXECUTE.
SORT CASES by Imputation_.
SPLIT FILE layered by Imputation_.
