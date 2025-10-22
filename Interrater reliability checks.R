# REVISION PLAN ---------

"In response to the concerns raised here, as well as by Carlsson & Maatman, 
we will enlist the help of another independent reviewer to conduct independent double coding. 
We have identified a suitable reviewer who is willing to do so, and who has not seen the existing ratings. 

We plan to add this reviewer as a co-author at the R&R stage.

This reviewer will be trained on the same two training articles that the initial reviewer (first author) 
and second author used to calibrate responses. They will then go through the data extraction process. 

We will then compare the extracted data to the first reviewer’s data, calculate measurement error and 
report on points of discussion.  We will re-run analyses with the data extracted by the independent reviewer. 

When reporting our findings, we will retain the original codings, and will report on interrater reliability. 
Specifically, we will calculate interrater reliability for each variable rated. 
For dichotomous or other nominal ratings we will use Cohen's kappa, and for the ordinal variables we will use weighted kappa.

Additionally, we would be happy to share a supplemental table with more examples to provide greater detail as 
to how the ratings were determined.
"

# LOAD DATA AND PACKAGES ----------
rater1_df <- read.csv("PsyCap Review_R data file.csv")
rater2_df <- read.csv("Second rater_PsyCap Review Tool (Responses).csv")
library(dplyr)
library(psych)

# HOMOGENISE VARIABLE NAMES AND FORMATS --------

#Though it may arguably have been better to leave old names in place, and have processed variables as new names?
# This would make it easier to check processed data comes through well

rater2_df <- rename(rater2_df,
                    "identifier" = "Article.Identifier",
                    "abs.strength" = "Abstract..How.strongly.does.the.language.in.this.sentence.imply.that.the.authors.identified.a.causal.relationship.between.the.independent.variable.of.interest.and.the.outcome.of.interest.",
                    "abs.action" = "Abstract..Action.Recommendation.s.",
                    "abs.action.strength" = "Abstract..Action.recommendation.causal.implication",
                    "causal.model.yn" = "Introduction.methods..Formal.causal.model",
                    "control" = "Introduction.methods..Are.there.variables.controlled..adjusted..matched.or.stratified.on.",
                    "dis.strength" = "Discussion.conclusions..How.strongly.does.the.language.in.this.sentence.imply.that.the.authors.identified.a.causal.relationship.between.the.primary.independent.variable.and.the.primary.outcome.",
                    "dis.action" = "Discussion.conclusions..Action.Recommendation.s.",
                    "dis.action.strength" = "Discussion.conclusion..Action.recommendation.causal.implication",
                    "disclaimer" = "Anywhere.in.text..Causal.disclaimer.statements",
                    "causal.intent" = "Anywhere.in.text...Acknowledgement.of.intent.to.draw.causal.inference",
                    "confounds" = "Anywhere.in.text..Is..confounding....confounders...or..third.variable..discussed.or.mentioned.in.relation.to.the.methods..results..and.or.interpretation.of.this.study.",
                    )

# Create factors from rater 1
rater1_df$abs.strength <- factor(rater1_df$abs.strength,
                                 levels = c("Strong", "Moderate", "Weak", "None"),
                                 ordered = TRUE)
rater1_df$abs.action <- factor(rater1_df$abs.action,
                                  levels = c("Yes", "No"))
rater1_df$abs.action.strength <- factor(rater1_df$abs.action.strength,
                                 levels = c("Strong", "Moderate", "Weak", "None"),
                                 ordered = TRUE)
rater1_df$causal.model.yn <- factor(rater1_df$causal.model.yn,
                               levels = c("Yes", "No"))
rater1_df$control <- factor(rater1_df$control,
                                    levels = c("Yes", "No"))
rater1_df$dis.strength <- factor(rater1_df$dis.strength,
                                        levels = c("Strong", "Moderate", "Weak", "None"),
                                        ordered = TRUE)
rater1_df$dis.action <- factor(rater1_df$dis.action,
                               levels = c("Yes", "No"))
rater1_df$dis.action.strength <- factor(rater1_df$dis.action.strength,
                                 levels = c("Strong", "Moderate", "Weak", "None"),
                                 ordered = TRUE)
rater1_df$disclaimer <- factor(rater1_df$disclaimer,
                               levels = c("Yes", "No"))
rater1_df$causal.intent <- factor(rater1_df$causal.intent,
                               levels = c("Yes", "No"))
rater1_df$confounds <- factor(rater1_df$confounds,
                                  levels = c("Yes", "No"))


# Convert a string to a strength rating
strength_function <- function(x){
  factor(
    ifelse(grepl("^Strong", x), "Strong",
           ifelse(grepl("^Weak", x), "Weak",
                  ifelse(grepl("^Moderate", x), "Moderate", 
                         ifelse(grepl("^None", x), "None",
                                NA)))),
    levels = c("Strong", "Moderate", "Weak", "None"), ordered = TRUE
  )
}

rater2_df$abs.strength <- strength_function(rater2_df$abs.strength)

rater2_df$abs.action <- factor(ifelse(nzchar(rater2_df$abs.action), "Yes", "No"),
                               levels = c("Yes", "No"))

rater2_df$abs.action.strength <- strength_function(rater2_df$abs.action.strength)

rater2_df$causal.model.yn <- factor(
  ifelse(grepl("^No", rater2_df$causal.model.yn), "No", "Yes"),
         levels = c("Yes", "No"))

rater2_df$control <- factor(rater2_df$control,
                            levels = c("Yes", "No"))

rater2_df$dis.strength <- strength_function(rater2_df$dis.strength)

rater2_df$dis.action <- factor(ifelse(nchar(rater2_df$dis.action)>1, "Yes", "No"), #Some single space entries that were really blank, hence >1
                               levels = c("Yes", "No"))

rater2_df$dis.action.strength <- strength_function(rater2_df$dis.action.strength)

rater2_df$disclaimer <- factor(ifelse(nzchar(rater2_df$disclaimer), "Yes", "No"),
                               levels = c("Yes", "No"))

rater2_df$causal.intent <- factor(ifelse(nzchar(rater2_df$causal.intent), "Yes", "No"),
                               levels = c("Yes", "No"))

rater2_df$confounds <- factor(
  ifelse(grepl("^No", rater2_df$confounds), "No", "Yes"),
  levels = c("Yes", "No"))

# RELIABILITY ESTIMATES --------
table_abs.strength <- table(rater1_df$abs.strength, rater2_df$abs.strength)
cohen.kappa(table_abs.strength) #Use weighted - ordinal
sum(rater1_df$abs.strength == rater2_df$abs.strength)/50 #percent agreement - could add for the others too

table_abs.action <- table(rater1_df$abs.action, rater2_df$abs.action)
cohen.kappa(table_abs.action)

table_abs.action.strength<- table(rater1_df$abs.action.strength, rater2_df$abs.action.strength)
table_abs.action.strength
cohen.kappa(table_abs.action.strength)

table_causal.model.yn<- table(rater1_df$causal.model.yn, rater2_df$causal.model.yn)
table_causal.model.yn
cohen.kappa(table_causal.model.yn)

table_control<- table(rater1_df$control, rater2_df$control)
table_control
cohen.kappa(table_control)

table_dis.strength <- table(rater1_df$dis.strength, rater2_df$dis.strength)
table_dis.strength
cohen.kappa(table_dis.strength) #Use weighted - ordinal

table_dis.action <- table(rater1_df$dis.action, rater2_df$dis.action)
cohen.kappa(table_dis.action)

table_dis.action.strength<- table(rater1_df$dis.action.strength, rater2_df$dis.action.strength)
table_dis.action.strength
cohen.kappa(table_dis.action.strength) 

table_disclaimer<- table(rater1_df$disclaimer, rater2_df$disclaimer)
table_disclaimer
cohen.kappa(table_disclaimer) 

table_causal.intent<- table(rater1_df$causal.intent, rater2_df$causal.intent)
table_causal.intent
cohen.kappa(table_causal.intent) #Very low kapp despite very high agreement - maybe due to high margins?

table_confounds<- table(rater1_df$confounds, rater2_df$confounds)
table_confounds
cohen.kappa(table_confounds) 


#Need to identify disagreements to discuss?
