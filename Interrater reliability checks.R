# LOAD DATA AND PACKAGES ----------
# The two lines below assume that the named files are present in the user's working directory (e.g., an RStudio project folder)
rater1_df <- read.csv("PsyCap Review_R data file.csv")
rater2_df <- read.csv("Second rater_PsyCap Review Tool (Responses).csv")
library(dplyr)
library(psych)
library(purrr)

# RATER 1: MAKE VARIABLES FACTOR ----
#This is useful for subsequent checks of reliability, especially for ordered factors.

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

# RENAMING VARIABLES --------

#Renames variables in rate 2 dataframe to be more concise and better match rater 1 names, 
#but first appending with ".raw" for preprocessed entries.

rater2_df <- rename(rater2_df,
                    "IV" =  "Independent.variable.s..of.interest",
                    "DV" = "Outcome.variable.s..of.interest",
                    "abs.link.sentence" = "Abstract..Primary.Linking.Sentence.s.",
                    "abs.link.phrase" = "Abstract..Primary.Linking.Word.Phrase",
                    "abs.mod.phrase" = "Abstract..Modifying.word.phrases",
                    "abs.strength.raw" = "Abstract..How.strongly.does.the.language.in.this.sentence.imply.that.the.authors.identified.a.causal.relationship.between.the.independent.variable.of.interest.and.the.outcome.of.interest.",
                    "abs.action.raw" = "Abstract..Action.Recommendation.s.",
                    "abs.action.strength.raw" = "Abstract..Action.recommendation.causal.implication",
                    "causal.model.yn.raw" = "Introduction.methods..Formal.causal.model",
                    "control.raw" = "Introduction.methods..Are.there.variables.controlled..adjusted..matched.or.stratified.on.",
                    "dis.link.sentence" = "Discussion.conclusions..Primary.Linking.Sentence.s.",
                    "dis.link.locate" = "Discussion.conclusions..Primary.Linking.Sentence.s..1",
                    "dis.link.phrase" = "Discussion.conclusions..Primary.Linking.Word.Phrase",
                    "dis.mod.phrase" = "Discussion.conclusions..Modifying.Word.Phrase",
                    "dis.strength.raw" = "Discussion.conclusions..How.strongly.does.the.language.in.this.sentence.imply.that.the.authors.identified.a.causal.relationship.between.the.primary.independent.variable.and.the.primary.outcome.",
                    "dis.action.raw" = "Discussion.conclusions..Action.Recommendation.s.",
                    "dis.action.strength.raw" = "Discussion.conclusion..Action.recommendation.causal.implication",
                    "disclaimer.raw" = "Anywhere.in.text..Causal.disclaimer.statements",
                    "causal.intent.raw" = "Anywhere.in.text...Acknowledgement.of.intent.to.draw.causal.inference",
                    "confounds.raw" = "Anywhere.in.text..Is..confounding....confounders...or..third.variable..discussed.or.mentioned.in.relation.to.the.methods..results..and.or.interpretation.of.this.study.",
                    )

# EXTRACT ROOT WORDS FROM LINKING PHRASES FOR RATER 2 ---------------
# Transform abstract linking phrases to root words

abs_root_map <- c(
  "higher" = "higher",
  "influence" = "influence",
  "influences" = "influence",
  "influenced" = "influence",
  "influence (prediction??)" = "influence",
  "correlation" = "correlate",
  "correlated" = "correlate",
  "alleviate" = "alleviate",
  "depends? effect?" = "depend",
  "relationship" = "relate",
  "relationships" = "relate",
  "impact" = "impact",
  "associated" = "associate",
  "association" = "associate",
  "link" = "link",
  "affects" = "affect",
  "affected" = "affect",
  "effect" = "affect",
  "effects" = "affect",
  "predict" = "predict",
  "predictor" = "predict",
  "predictive? role?" = "predict",
  "drive" = "drive",
  "increased/decreased" = "increase",
  "reduced" = "increase",
  "role" = "role",
  "enhance" = "enhance",
  "enhancing" = "enhance",
  "determinant" = "determine",
  "related" = "relate",
  "promote" = "promote"
)

rater2_df$abs.root <- map_chr(rater2_df$abs.link.phrase, ~ abs_root_map[.x])

dis_root_map <- c(
  "enhanced" = "enhance",
  "enhancing" = "enhance",
  "effect" = "affect",
  "effects" = "affect",
  "effect? enhancing?" = "affect",  # could also be "enhance", but defaulting to "affect"
  "affect" = "affect",
  "associated" = "associate",
  "correlation" = "correlate",
  "related" = "relate",
  "relation" = "relate",
  "relationship" = "relate",
  "relationships" = "relate",
  "impact" = "impact",
  "impacts" = "impact",
  "influence" = "influence",
  "influences" = "influence",
  "influenced" = "influence",
  "influenced (prediction??)" = "influence",
  "predict" = "predict",
  "predicted" = "predict",
  "predictor" = "predict",
  "higher" = "higher",
  "increased" = "increase",
  "alleviate" = "alleviate",
  "antecedent" = "antecedent",
  "constitute" = "constitute",
  "drivers" = "drive"
)

# Map each term to its root word
rater2_df$dis.root <- map_chr(rater2_df$dis.link.phrase, ~ dis_root_map[.x])


# CREATE PROCESSED VERSIONS OF RATER 2 ENTRIES TO MATCH RATER 1 PROCESSED DATA FORMAT ------------

# A function to convert a string to a strength rating (since this will be useful for several variables)
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

# Create processed variables
rater2_df$abs.strength <- strength_function(rater2_df$abs.strength.raw)

rater2_df$abs.action <- factor(ifelse(nzchar(rater2_df$abs.action.raw), "Yes", "No"),
                               levels = c("Yes", "No"))

rater2_df$abs.action.strength <- strength_function(rater2_df$abs.action.strength.raw)

rater2_df$causal.model.yn <- factor(
  ifelse(grepl("^No", rater2_df$causal.model.yn.raw), "No", "Yes"),
         levels = c("Yes", "No"))

rater2_df$control <- factor(rater2_df$control.raw,
                            levels = c("Yes", "No"))

rater2_df$dis.strength <- strength_function(rater2_df$dis.strength.raw)

rater2_df$dis.action <- factor(ifelse(nchar(rater2_df$dis.action.raw)>1, #Some single space entries that were really blank, hence >1
                                      "Yes", "No"), levels = c("Yes", "No"))

rater2_df$dis.action.strength <- strength_function(rater2_df$dis.action.strength.raw)

rater2_df$disclaimer <- factor(ifelse(nzchar(rater2_df$disclaimer.raw), "Yes", "No"),
                               levels = c("Yes", "No"))

rater2_df$causal.intent <- factor(ifelse(nzchar(rater2_df$causal.intent.raw), "Yes", "No"),
                               levels = c("Yes", "No"))

rater2_df$confounds <- factor(
  ifelse(grepl("^No", rater2_df$confounds.raw), "No", "Yes"),
  levels = c("Yes", "No"))

# re-order variables for easier checking
rater2_df = rater2_df[, c("Timestamp", "Article.Title", "identifier", "IV","DV",
                           "abs.link.sentence", "abs.link.phrase", "abs.root", "abs.mod.phrase", 
                          "abs.strength.raw", "abs.strength", "abs.action.raw", "abs.action",        
                          "abs.action.strength.raw", "abs.action.strength", "causal.model.yn.raw", "causal.model.yn",
                          "control.raw", "control", "dis.link.sentence", "dis.link.locate",
                          "dis.link.phrase", "dis.root", "dis.mod.phrase", "dis.strength.raw", "dis.strength", "dis.action.raw", 
                          "dis.action", "dis.action.strength.raw", "dis.action.strength", 
                           "disclaimer.raw", "disclaimer", "causal.intent.raw", "causal.intent", "confounds.raw",  "confounds",
                          "Additional.comments")]


# RELIABILITY ESTIMATES --------

#A function to extract percent agreement for descriptive purposes
get_percent <- function(x, y){
  matches <- na.omit(x == y)
  percent = sum(matches)/length(matches)
  percent
}

#A function to get a nice row of Cohen's kappa output
extract_row <- function(table, weighted, x, y){
  kappa_result <- cohen.kappa(table)
  c(if (weighted == TRUE) kappa_result$confi[2,] else kappa_result$confi[1,], percent = get_percent(x, y), weighted = weighted)
}

# Calculate reliability estimates for different variables, using weighted kappa for ordinal variables
table_abs.strength <- table(rater1_df$abs.strength, rater2_df$abs.strength)
abs.strength.out <- extract_row(table_abs.strength, weighted = TRUE, rater1_df$abs.strength, rater2_df$abs.strength)

table_abs.action <- table(rater1_df$abs.action, rater2_df$abs.action)
abs.action.out <- extract_row(table_abs.action, weighted = FALSE, rater1_df$abs.action, rater2_df$abs.action)

table_abs.action.strength<- table(rater1_df$abs.action.strength, rater2_df$abs.action.strength)
table_abs.action.strength
cohen.kappa(table_abs.action.strength)
abs.action.strength.out <- extract_row(table_abs.action.strength, weighted = TRUE, rater1_df$abs.action.strength, rater2_df$abs.action.strength)

table_causal.model.yn<- table(rater1_df$causal.model.yn, rater2_df$causal.model.yn)
table_causal.model.yn
cohen.kappa(table_causal.model.yn)
causal.model.out <- extract_row(table_causal.model.yn, weighted = FALSE, rater1_df$causal.model.yn, rater2_df$causal.model.yn)

table_control<- table(rater1_df$control, rater2_df$control)
table_control
cohen.kappa(table_control)
control.out <- extract_row(table_control, weighted = FALSE, rater1_df$control, rater2_df$control)

table_dis.strength <- table(rater1_df$dis.strength, rater2_df$dis.strength)
table_dis.strength
cohen.kappa(table_dis.strength) #Use weighted - ordinal
dis.strength.out <- extract_row(table_dis.strength, weighted = TRUE, rater1_df$dis.strength, rater2_df$dis.strength)

table_dis.action <- table(rater1_df$dis.action, rater2_df$dis.action)
cohen.kappa(table_dis.action)
dis.action.out <- extract_row(table_dis.action, weighted = FALSE, rater1_df$dis.action, rater2_df$dis.action)

table_dis.action.strength<- table(rater1_df$dis.action.strength, rater2_df$dis.action.strength)
table_dis.action.strength
cohen.kappa(table_dis.action.strength) 
dis.action.strength.out <- extract_row(table_dis.action.strength, weighted = TRUE, rater1_df$dis.action.strength, rater2_df$dis.action.strength)

table_disclaimer<- table(rater1_df$disclaimer, rater2_df$disclaimer)
table_disclaimer
cohen.kappa(table_disclaimer) 
disclaimer.out <- extract_row(table_disclaimer, weighted = FALSE, rater1_df$disclaimer, rater2_df$disclaimer)

table_causal.intent <- table(rater1_df$causal.intent, rater2_df$causal.intent)
table_causal.intent
cohen.kappa(table_causal.intent) #Very low kapp despite very high agreement - maybe due to high margins?
causal.intent.out <- extract_row(table_causal.intent, weighted = FALSE, rater1_df$causal.intent, rater2_df$causal.intent)

table_confounds<- table(rater1_df$confounds, rater2_df$confounds)
table_confounds
cohen.kappa(table_confounds) 
confounds.out <- extract_row(table_confounds, weighted = FALSE, rater1_df$confounds, rater2_df$confounds)

# Create nice table of output
table_out <- rbind(
  abs.strength.out,
  abs.action.out,
  abs.action.strength.out,
  causal.model.out,
  control.out,
  dis.strength.out,
  dis.action.out,
  dis.action.strength.out,
  disclaimer.out,
  causal.intent.out,
  confounds.out
)

table_out
