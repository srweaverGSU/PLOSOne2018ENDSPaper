# PROLOG-----------------------------------------------------------------------

# PROJECT: Are electronic nicotine delivery systems helping cigarette smokers
#          quit? Evidence from a prospective cohort study of U.S. adult smokers,
#          2015-2016
# URL:     http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0198047
# PURPOSE: Code for all analyses in article
# DATA:    analysis_data.sav
# AUTHOR:  John Wesley Heath
# CREATED: August 9, 2018
# LATEST:  August 9, 2018

# PROLOG------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------

library(dplyr)
library(haven)
library(survey)

# SET WORKING DIRECTORY TO PROJECT DIRECTORY
setwd()

# read in data and subset to follow-up respondents
data_file <- 'data/analysis_data.sav'
master <- read_sav(data_file)
df <- filter(master, QFLAG_2 == 1)




#-------------------------------------------------------------------------------
# Variable Creation
#-------------------------------------------------------------------------------

# dplyr::mutate() and dplyr:case_when() are used for recoding.
# see ?dplyr::case_when for info about syntax

# haven::as_factor() is used to leverage information about value labels
# that are included in the SPSS data set


df <- df %>% mutate(

  #--------------------------
  # primary outcome variables
  #--------------------------

  # smoking abstinence for at least 30 days at follow-up - BINARY
  # 0: currently smoking OR used in past 30 days
  # 1: not currently smoking AND haven't used in past 30 days
  quit_smoking = case_when(
    CGNOW_2 %in% c(1, 2) | CGUSE30_2 == 1      ~ 0,
    CGNOW_2 == 0 & CGUSE30_2 == 0              ~ 1
  ),

  # quit or past year quit attempt - BINARY
  # 0: currently smoking and no quit attempts in past year
  # 1: not currently smoking OR >= 1 quit attempt in past year
  quit_or_attempt = case_when(
    CGNOW_2 %in% c(1, 2) & CGQTATPY_2 == 0     ~ 0,
    CGNOW_2 == 0 | CGQTATPY_2 > 0              ~ 1
  ),

  # average number of cigarettes per day at follow-up - CONTINUOUS
  avg_num_cigs = case_when(
    CGNOW_2 == 2             ~ as.numeric((CGDYQTY_2 * 30) / 30),
    TRUE                     ~ as.numeric((CGDYQTY_2 * CGFRQ30_2) / 30)
  ),



  #-----------------------------------------------
  # intermediate variables, mostly for convenience
  #-----------------------------------------------

  baseline_ends = case_when(
    ECNOW %in% c(1, 2, 3)                         ~ 1,
    ECNOW == 0                                    ~ 0
  ),


  baseline_smoking = 1,  # everybody smoking at baseline

  baseline_dual_use = case_when(
    baseline_smoking == 0 | baseline_ends == 0    ~ 0,
    baseline_smoking == 1 & baseline_ends == 1    ~ 1
  ),

  between_ends = case_when(
    ECUSE30_2 == 1 | ECUSEYR_2 == 1               ~ 1,
    ECUSEYR_2 == 0                                ~ 0
  ),

  followup_ends = case_when(
    ECNOW_2 %in% c(1, 2, 3)                       ~ 1,
    ECNOW_2 == 0                                  ~ 0
  ),

  # recodes to ENDS device type variables to account for skip patterns.
  # those that did not use a rechargeable device (ECTYPRCH) did not use
  # cartridges (ECTYPCRT), and those that either did not use a rechargeable
  # device or did use cartridges did not use a tank system (ECTYPTNK).
  # variables are converted to numeric because importing with haven means that
  # they are of class "labelled"
  ECTYPCRT = case_when(
    ECTYPRCH == 0                        ~ 0,
    TRUE                                 ~ as.numeric(ECTYPCRT)
  ),
  ECTYPTNK = case_when(
    ECTYPRCH == 0 | ECTYPCRT == 1        ~ 0,
    TRUE                                 ~ as.numeric(ECTYPTNK)

  ),
  ECTYPCRT_2 = case_when(
    ECTYPRCH_2 == 0                      ~ 0,
    TRUE                                 ~ as.numeric(ECTYPCRT_2)
  ),
  ECTYPTNK_2 = case_when(
    ECTYPRCH_2 == 0 | ECTYPCRT_2 == 1    ~ 0,
    TRUE                                 ~ as.numeric(ECTYPTNK_2)
  ),



  #--------------------------------
  # primary ENDS exposure variables
  #--------------------------------

  # ENDS use groups

  # No ENDS Use:                                no use of ends at any point
  # ENDS use at baseline & follow-up:           use at baseline, follow-up, and
  #                                             in between
  # ENDS use initiated after baseline:          no use at baseline, but use at
  #                                             follow-up and in between
  # ENDS use but discontinued before follow-up: no use at follow-up AND (use at
  #                                             baseline and/or use in-between)

    ends_groups = factor(case_when(

    baseline_ends == 0 & between_ends == 0 & followup_ends == 0 ~ 'No ENDS Use',
    baseline_ends == 1 & between_ends == 1 & followup_ends == 1 ~ 'ENDS use at baseline & follow-up',
    baseline_ends == 0 & between_ends == 1 & followup_ends == 1 ~ 'ENDS use initiated after baseline',
    baseline_ends == 0 & between_ends == 1 & followup_ends == 0 ~ 'ENDS use but discontinued before follow-up',
    baseline_ends == 1 & between_ends == 0 & followup_ends == 0 ~ 'ENDS use but discontinued before follow-up',
    baseline_ends == 1 & between_ends == 1 & followup_ends == 0 ~ 'ENDS use but discontinued before follow-up'

  ), levels = c('No ENDS Use',
                'ENDS use at baseline & follow-up',
                'ENDS use initiated after baseline',
                'ENDS use but discontinued before follow-up')),


  # Any ENDS use

  # just a collapsed version of the above variable

  any_ends_use = factor(case_when(
    ends_groups == 'No ENDS Use'                            ~ 'No ENDS Use',
    ends_groups %in% c('ENDS use at baseline & follow-up',
                       'ENDS use initiated after baseline',
                       'ENDS use but discontinued before follow-up') ~ 'Any ENDS Use'
  ), levels = c('No ENDS Use',
                'Any ENDS Use')),


  # ENDS Use Frequency

  # Non-users:   did not use ENDS at any point during the study period
  # Never daily:
  # Ever daily:

    ends_use_frequency = factor(case_when(

    ends_groups == 'No ENDS Use'                                ~ 'Non-user',
    ECNOW == 3 | ECFRQ30 >= 25 | ECNOW_2 == 3 | ECFRQ30_2 >= 25 ~ 'Daily ENDS Use',
    baseline_ends == 1 | between_ends == 1 | followup_ends == 1 ~ 'Non-daily ENDS Use'

  ), levels = c('Non-user',
                'Non-daily ENDS Use',
                'Daily ENDS Use')),


  # Importance of ENDS Use for Quitting Smoking

  # Non-user:                    did not use ENDS at any point during the study
  #                              period
  # None or low importance:      less than 3 on importance question at baseline
  #                              and follow-up
  # Moderate to high importance: at least 3 on importance question at baseline
  #                              OR followup

    ends_used_to_quit_smoking = factor(case_when(

    ends_groups == 'No ENDS Use'                ~ 'Non-user',
    ECRESN4 >= 3 | ECRESN4_2 >= 3               ~ 'Moderate to high importance',
    ECRESN4 < 3 | ECRESN4_2 < 3                 ~ 'None or low importance'

  ), levels = c('Non-user',
                'None or low importance',
                'Moderate to high importance')),


  # ENDS Flavors

  # Non-user:                               did not use ENDS at any point during
  #                                         the study period
  # All other flavors (e.g., fruit, candy): selected any flavor other than
  #                                         tobacco or menthol/wintergreen/mint
  #                                         at baseline or follow-up
  # Methol/Wintergreen/Mint:                selected this flavor at baseline or
  #                                         follow-up, but no other flavor other
  #                                         than tobacco flavor or unflavored
  # Tobacco/unflavored:                     selected only tobacco flavor or
  #                                         unflavored at baseline and follow-up

  ends_flavor = factor(case_when(

    ends_groups == 'No ENDS Use'      ~ 'Non-user',
    ECFL30_2 == 1 | ECFL30_3 == 1 | ECFL30_4 == 1 |
      ECFL30_5 == 1 | ECFL30_6 == 1 | ECFL30_9 == 1 |
      ECFLV_2_2 == 1| ECFLV_2_3 == 1 | ECFLV_2_4 == 1 |
      ECFLV_2_5 == 1 | ECFLV_2_6 == 1 | ECFLV_2_7 == 1 |
      ECFLV_2_10 == 1                 ~ 'All other flavors (e.g., fruit, candy)',
    ECFL30_1 == 1 | ECFLV_2_1 == 1    ~ 'Menthol/Wintergreen/Mint',
    ECFFLV30 == 0 | ECFL30_7 == 1 |
      ECFLV_2_8 == 1 | ECFLV_2_9 == 1 ~ 'Tobacco/unflavored'

  ), levels = c('Non-user',
                'Tobacco/unflavored',
                'Menthol/Wintergreen/Mint',
                'All other flavors (e.g., fruit, candy)')),


  # ENDS Device Type

  # Non-user:         did not use ENDS at any point during the study period
  # Tank ENDS:        used tanks at baseline or follow-up
  # Cartridge ENDS:   used cartridges, but not tanks, at baseline or follow-up
  # Disposable/Other: else

  ends_device_type = factor(case_when(

    ends_groups == 'No ENDS Use'                    ~ 'Non-user',
    ECTYPTNK == 1 | ECTYPTNK_2 == 1                 ~ 'Tank ENDS',
    ECTYPCRT == 1 | ECTYPCRT_2 == 1                 ~ 'Cartridge ENDS',
    TRUE                                            ~ 'Disposable/Other'

  ), levels = c('Non-user', 'Tank ENDS', 'Cartridge ENDS', 'Disposable/Other')),


  # Baseline ENDS use

  # No ENDS Use at baseline: reported using ENDS "not at all" at baseline
  # ENDS Use at baseline:    reported using ENDS "rarely", "some days", or
  #                          "every day" at baseline

  baseline_ends_use = factor(case_when(
    baseline_ends == 0                              ~ 'No ENDS Use at Baseline',
    baseline_ends == 1                              ~ 'ENDS Use at Baseline',
    TRUE                                            ~ NA_character_
  ), levels = c('No ENDS Use at Baseline', 'ENDS Use at Baseline')),


  # Dual User follow-up status

  # Quit Smoking and Quit ENDS:     smoking abstinent for 30 days and
  #                                 not currently using ENDS at follow-up
  # Quit Smoking, Still Using ENDS: smoking abstinent for 30 days and
  #                                 currently using ENDS at follow-up
  # Still Smoking, Quit Using ENDS: smoking and not currently using ENDS
  #                                 at follow-up
  # Still Dual User:                smoking and currently using ENDS
  #                                 at follow-up

  dual_user_followup_status = factor(case_when(
    is.na(baseline_dual_use) | baseline_dual_use == 0 ~ NA_character_,
    quit_smoking == 1 & followup_ends == 0            ~ 'Quit Smoking and Quit ENDS',
    quit_smoking == 1 & followup_ends == 1            ~ 'Quit Smoking, Still Using ENDS',
    quit_smoking == 0 & followup_ends == 0            ~ 'Still Smoking, Quit Using ENDS',
    quit_smoking == 0 & followup_ends == 1            ~ 'Still Dual User'
  ), levels = c('Quit Smoking and Quit ENDS',
                'Quit Smoking, Still Using ENDS',
                'Still Smoking, Quit Using ENDS',
                'Still Dual User')),



  #-------------
  # quit methods
  #-------------

  # Currently smoking respondents received a different question about methods
  # used to attempt quitting (CGQSPY) than did currently non-smoking
  # respondents (CGQS).  These two versions were mutually exclusive and are
  # combined into a single variable for each quit method.

  quit_method_cold_turkey = case_when(
    !is.na(CGQS1_2)     ~ CGQS1_2,
    !is.na(CGQSPY1_2)   ~ CGQSPY1_2,
    TRUE                ~ NA_real_
  ),

  quit_method_gradually = case_when(
    !is.na(CGQS2_2)     ~ CGQS2_2,
    !is.na(CGQSPY2_2)   ~ CGQSPY2_2,
    TRUE                ~ NA_real_
  ),

  quit_method_completely_to_ends = case_when(
    !is.na(CGQS3_2)     ~ CGQS3_2,
    !is.na(CGQSPY3_2)   ~ CGQSPY3_2,
    TRUE                ~ NA_real_
  ),

  quit_method_partially_to_ends = case_when(
    !is.na(CGQS4_2)     ~ CGQS4_2,
    !is.na(CGQSPY4_2)   ~ CGQSPY4_2,
    TRUE                ~ NA_real_
  ),

  quit_method_nrt = case_when(
    !is.na(CGQS6a_2)    ~ CGQS6a_2,
    !is.na(CGQSPY6a_2)  ~ CGQSPY6a_2,
    TRUE                ~ NA_real_
  ),

  quit_method_pharma = case_when(
    !is.na(CGQS6b_2)    ~ CGQS6b_2,
    !is.na(CGQSPY6b_2)  ~ CGQSPY6b_2,
    TRUE                ~ NA_real_
  ),

  quit_method_counseling = case_when(
    !is.na(CGQS7_2)     ~ CGQS7_2,
    !is.na(CGQSPY7_2)   ~ CGQSPY7_2,
    TRUE                ~ NA_real_
  ),

  quit_method_lccs = case_when(
    !is.na(CGQS8_2)     ~ CGQS8_2,
    !is.na(CGQSPY8_2)   ~ CGQSPY8_2,
    TRUE                ~ NA_real_
  ),

  quit_method_other_tobacco = case_when(
    !is.na(CGQS9_2)     ~ CGQS9_2,
    !is.na(CGQSPY9_2)   ~ CGQSPY9_2,
    TRUE                ~ NA_real_
  ),

  quit_method_support = case_when(
    !is.na(CGQS10_2)    ~ CGQS10_2,
    !is.na(CGQSPY10_2)  ~ CGQSPY10_2,
    TRUE                ~ NA_real_
  ),



  #-----------
  # covariates
  #-----------

  # only a few covariates need to be created - most are used as they are
  # in the data set

  # continuous

  avg_cigs_per_day = case_when(
    CGNOW == 2                                    ~ (30 * CGDYQTY) / 30,
    TRUE                                          ~ (CGFRQ30 * CGDYQTY) / 30
  ),

  years_smoking = PPAGE - CGSTAGE,

  # categorical

  poly_use = factor(case_when(
    LCNOW == 0 & TCNOW == 0 & HKNOW == 0                                  ~ 'No',
    LCNOW %in% c(1, 2, 3) | TCNOW %in% c(1, 2, 3) | HKNOW %in% c(1, 2, 3) ~ 'Yes'
  )),

  children = factor(case_when(
    PPT01 == 0 & PPT25 == 0 & PPT612 == 0 & PPT1317 == 0                  ~ 'No',
    PPT01 > 0 | PPT25 > 0 | PPT612 > 0 | PPT1317 > 0                      ~ 'Yes'
  )),

  sexual_orientation = factor(case_when(
    is.na(ppp20063)   ~ 'MissingCategory',
    TRUE              ~ as.character(as_factor(ppp20063))
  ), levels = c('Heterosexual or straight', 'Gay', 'Lesbian', 'Bisexual',
                'Other, please specify', 'MissingCategory')),

  # categorical covariates are converted to factors

  CGADSF = droplevels(as_factor(CGADSF)),
  CGCRAVE = droplevels(as_factor(CGCRAVE)),
  CGQSE6 = droplevels(as_factor(CGQSE6)),
  PPMARIT = droplevels(as_factor(PPMARIT)),
  PPMSACAT = droplevels(as_factor(PPMSACAT)),
  PPREG4 = droplevels(as_factor(PPREG4)),
  PPWORK = droplevels(as_factor(PPWORK)),
  PPETHM = droplevels(as_factor(PPETHM)),
  PPGENDER = droplevels(as_factor(PPGENDER)),
  smkstudies = droplevels(as_factor(smkstudies)),
  pph10304 = droplevels(as_factor(pph10304)),
  pph10218 = droplevels(as_factor(pph10218)),
  pph1brea = droplevels(as_factor(pph1brea))

)




#-------------------------------------------------------------------------------
# Covariates Specification
#-------------------------------------------------------------------------------

# Character vectors of covariate names are created so they can be easily passed
# into analysis functions.  They are separated into continuous and categorical
# because they are treated differently in the demographics table (Table 2).
# The recodes above have ensured that all categorical covariates are factor
# type, so they will be treated appropriately in analysis functions.

categorical_covariates <- c(
  'CGADSF',             # perceived addiction to smoking
  'CGCRAVE',            # strength of cravings to smoke cigarettes
  'CGQSE6',             # prior use of FDA-approved cessation treatments
  'poly_use',           # dual/poly combustible tobacco use
  'PPMARIT',            # marital status
  'PPMSACAT',           # MSA status
  'PPREG4',             # US region
  'children',           # presence of children under 18 in household
  'PPWORK',             # employment status
  'PPETHM',             # race/ethnicity
  'PPGENDER',           # gender
  'sexual_orientation', # sexual orientation
  'smkstudies',         # number of past-year smoking-related studies
  'pph10304',           # past month consumption of alcohol
  'pph10218',           # prior mental health treatment
  'pph1brea'            # ever diagnosed with asthma, chronic bronchitis or COPD
)

continuous_covariates <- c(
  'avg_cigs_per_day',   # intensity of smoking
  'years_smoking',      # number of years smoking
  'CGQTPLN',            # reported intentions to quit smoking
  'CGQTATPY',           # number of past-year quit attempts
  'CGREG',              # regret having started smoking
  'PPAGE',              # age
  'PPEDUC',             # education
  'PPINCIMP',           # income
  'pph10001'            # self-reported physical health status
)




#-------------------------------------------------------------------------------
# Utility Functions
#-------------------------------------------------------------------------------

svy_cat_props <- function(cat_var, svy_des) {
  # Takes a variable name and a survey design object, and leverages
  # survey::svyciprop() to create a data frame that contains the
  # unweighted number of observations, weighted percentage, and 95% confidence
  # interval for the weighted percentage for every level of the variable.
  #
  # Args:
  #   cat_var: (character) name of variable to analyze
  #   svy_des: (survey design object) survey design that contains cat_var
  #
  # Returns:
  #   A data frame containing computed statistics.

  require(dplyr)
  require(survey)

  # get unweighted counts for all non-empty levels
  counts <- table(svy_des$variables[[cat_var]])
  counts <- counts[counts != 0]

  # the names of this table are the levels of the categorical variable
  levs <- names(counts)

  # build the table one level/row at a time
  rows <- lapply(levs, function(this_level) {

    # get unweighted count for current level of variable
    unweighted_n <- as.character(counts[this_level])

    # perform svyciprop for current level of variable
    f <- as.formula(paste0('~I(', cat_var, ' == "', this_level, '")'))
    result <- svyciprop(f, design = svy_des, method = 'logit', vartype = 'ci')

    # extract and format stats
    prop <- sprintf("%.2f", result[1] * 100)
    ci <- attr(result, 'ci', exact = TRUE)
    ci <- sprintf("(%.2f, %.2f)", ci[1] * 100, ci[2] * 100)

    # create single row data frame with needed stats
    row <- data.frame(level = this_level, n = unweighted_n,
                      prop = prop, ci = ci,
                      stringsAsFactors = FALSE)
    return(row)
  })

  # bind all the rows into a single data frame
  cat_prop_frame <- bind_rows(rows)
  names(cat_prop_frame) <- c(cat_var, 'Unweighted n',
                             'Weighted Percentage', 'Percentage CI')

  return(cat_prop_frame)
}


svy_cont_mean <- function(cont_var, svy_des) {
  # Takes a variable name and a survey design object, and leverages
  # survey::svymean() to create a data frame that contains the
  # number of non-missing values, weighted mean, and standard error and 95%
  # confidence interval for the weighted mean.
  #
  # Args:
  #   cont_var: (character) name of variable to analyze
  #   svy_des:  (survey design object) survey design that contains cont_var
  #
  # Returns:
  #   A data frame containing computed statistics.


  require(survey)

  non_missing <- sum(!is.na(svy_des$variables[[cont_var]]))

  # perform svymean for this variable
  f <- as.formula(paste0('~', cont_var))
  result <- svymean(f, design = svy_des, na.rm = TRUE)

  # extract and format stats
  mean_est <- round(result[1], 2)
  se <- round(sqrt(attr(result, 'var')), 2)
  ci <- round(confint(result), 2)
  ci <- paste0(ci[1], ', ', ci[2])

  # create single row data frame with stats
  mean_frame = data.frame(n = non_missing, mean = mean_est, se = se, ci = ci,
                          stringsAsFactors = FALSE)
  names(mean_frame) <- c(cont_var, 'Mean', 'SE', 'CI')

  return(mean_frame)
}


adjusted_logistic_table <- function(outcome, predictor,
                                    cat_covars, cont_covars,
                                    svy_des) {
  # Takes an outcome variable name, predictor of interest variable name, list of
  # categorical covariates, list of continuous covariate objects, and survey
  # design object; calculates the number of observations where outcome==1 and
  # leverages survey::svyby() and survey::svyciprop() to get the corresponding
  # weighted percentage and 95% CI for each level of the predictor of interest;
  # leverages survey::svyglm() to build a weighted logistic regression model
  # where the outcome variable is on the left hand side of the model formula
  # and the predictor variable of interest and all covariates are on the right
  # hand side in order to obtain weighted odds ratios and 95% confidence
  # intervals for the predictor of interest; and returns a data frame that
  # combines all the results.  Note that only the predictor of variable of
  # interest is included in the output results, though the covariates are also
  # included in the model.  Separate models are bulit for treating each level of
  # the predictor variable as the reference, so that odds ratios can be obtained
  # for all contrasts.
  #
  # Args:
  #   outcome:     (character) LHS outcome variable
  #   predictor:   (character) RHS variable of interest
  #   cat_covars:  (character vector) RHS categorical covariates
  #   cont_covars: (character vectors) RHS continuous covariates
  #   svy_des:     (survey design object) survey design
  #
  # Returns:
  #   A data frame containing computed statistics.

  require(dplyr)
  require(survey)

  # subset design object to observations with complete data
  complete_model_cases <- complete.cases(svy_des$variables[c(outcome,
                                                             predictor,
                                                             cat_covars,
                                                             cont_covars)])
  mdl_des <- subset(svy_des, complete_model_cases)

  # get unweighted n for each level of predictor
  denominator <- as.numeric(table(mdl_des$variables[[predictor]]))

  # get unweighted n for outcome at every level of predictor
  # ASSERT OUTCOME IS 0/1 BINARY
  outcome_count <- table(mdl_des$variables[[outcome]],
                         mdl_des$variables[[predictor]])
  outcome_count <- outcome_count['1', ]

  # get weighted percentages and CIs of outcome==1 for every level of predictor
  props <- svyby(as.formula(paste0('~', outcome)),
                 by = as.formula(paste0('~', predictor)),
                 design = mdl_des, svyciprop, vartype = 'ci', method = 'logit')
  props$percent <- sprintf("%.1f", props[[outcome]] * 100)
  props$percent_ci <- sprintf("(%.2f, %.2f)",
                              props$ci_l * 100,
                              props$ci_u * 100)
  props <- props[c('percent', 'percent_ci')]

  # get all odds ratios
  levs <- levels(mdl_des$variables[[predictor]])
  or_frames <- lapply(levs, function(this_level) {

    # make this level the reference
    mdl_des$variables[[predictor]] <- relevel(mdl_des$variables[[predictor]],
                                              ref = this_level)

    # build formula
    f <- as.formula(paste(outcome, '~',
                          paste(cat_covars, collapse = ' + '),
                          '+',
                          paste(cont_covars, collapse = ' + '),
                          '+',
                          predictor))

    # weighted and adjusted logistic model
    model <- svyglm(f, design = mdl_des, family = 'quasibinomial')

    # extract and format ORs and OR CIs from model object
    ors <- data.frame(cbind(exp(coef(model)), exp(confint(model))))
    ors$or <- sprintf("%.2f", ors[, 1])
    ors$or_ci <- sprintf("(%.2f, %.2f)", ors[, 2], ors[, 3])

    # return formatted ORs and CIs for predictor variable only
    or_frame <- ors[paste0(predictor, levs), c('or', 'or_ci')]
    return(or_frame)
  })

  # organize and format results
  all_ors <- bind_cols(or_frames)
  all_ors[is.na(all_ors)] <- 'REF'
  row.names(all_ors) <- paste0(predictor, levs)
  names(all_ors) <- rep(c('OR', 'OR_CI'), nrow(all_ors))

  adjusted_logistic_stats <- cbind(denominator, outcome_count, props, all_ors)

  return(adjusted_logistic_stats)

}


adjusted_linear_table <- function(outcome, predictor,
                                  cat_covars, cont_covars,
                                  svy_des) {
  # Takes an outcome variable name, predictor of interest variable name, list of
  # categorical covariates, list of continuous covariate objects, and survey
  # design object; calculates the number of non-missing outcome values for each
  # level of the predictor of interest; leverages survey::svyby() and
  # survey::svymean() to get the mean of the outcome variable and 95% confidence
  # intervals for each level of the predictor of interest; leverages
  # survey::svyglm() to build a weighted linear regression model where the
  # outcome variable is on the left hand side of the model formula and the
  # predictor variable of interest and all covariates are on the right hand
  # side in order to obtain weighted regression coefficients (adjusted mean
  # differences) 95% confidence intervals for the predictor of interest; and
  # returns a data frame that combines all the results.  Note that only the
  # predictor of variable of interest is included in the output results, though
  # the covariates are also included in the model.  Separate models are bulit
  # for treating each level of the predictor variable as the reference, so that
  # regression coefficients can be obtained for all contrasts.
  #
  # Args:
  #   outcome:     (character) LHS outcome variable
  #   predictor:   (character) RHS variable of interest
  #   cat_covars:  (character vector) RHS categorical covariates
  #   cont_covars: (character vectors) RHS continuous covariates
  #   svy_des:     (survey design object) survey design
  #
  # Returns:
  #   A data frame containing computed statistics.

  require(dplyr)
  require(survey)

  # subset design object to observations with complete data
  complete_model_cases <- complete.cases(svy_des$variables[c(outcome,
                                                             predictor,
                                                             cat_covars,
                                                             cont_covars)])
  mdl_des <- subset(svy_des, complete_model_cases)

  # get unumber of non-missing outcome values for each level of predictor
  non_missing <- as.numeric(table(mdl_des$variables[[predictor]]))

  # get means and CIs for outcome at each level of predictor
  means <- svyby(as.formula(paste0('~', outcome)),
                 by = as.formula(paste0('~', predictor)),
                 design = mdl_des, svymean, na.rm =TRUE, vartype = 'ci')
  means$mean <- sprintf("%.2f", means[[outcome]])
  means$mean_ci <- sprintf("(%.2f, %.2f)", means$ci_l, means$ci_u)
  means <- means[c('mean', 'mean_ci')]

  # get linear regression coefficients
  levs <- levels(mdl_des$variables[[predictor]])
  coef_list <- lapply(levs, function(this_level) {

    # make this level the reference
    mdl_des$variables[[predictor]] <- relevel(mdl_des$variables[[predictor]],
                                              ref = this_level)

    # build formula
    f <- as.formula(paste(outcome, '~',
                          paste(cat_covars, collapse = ' + '),
                          '+',
                          paste(cont_covars, collapse = ' + '),
                          '+',
                          predictor))

    # weighted and adjusted linear model
    model <- svyglm(f, design = mdl_des)

    # extract and format statistics from model object
    coefs <- as.data.frame(cbind(coef(model), confint(model)))
    coefs$coef <- sprintf("%.2f", coefs[, 1])
    coefs$coef_ci <- sprintf("(%.2f, %.2f)", coefs[, 2], coefs[, 3])

    # return formatted coefficients and CIs for predictor variable only
    coef_frame <- coefs[paste0(predictor, levs), c('coef', 'coef_ci')]
    return(coef_frame)
  })

  # organize and format results
  coefs <- bind_cols(coef_list)
  coefs[is.na(coefs)] <- 'REF'
  row.names(coefs) <- paste0(predictor, levs)
  names(coefs) <- rep(c('coef', 'coef_ci'), nrow(coefs))

  adjusted_linear_stats <- cbind(non_missing, means, coefs)

  return(adjusted_linear_stats)
}


get_demographics_table <- function(cat_vars, cont_vars, svy_des) {
  # Takes a list of categorical variable names, a list of continuous variable
  # names, and a survey design object, and leverages svy_cat_props() and
  # svy_cont_mean(), both defined above, to construct and return a data frame
  # of statistics for all variables.
  #
  # Args:
  #   cat_vars:     (character vector)
  #   cont_vars:   (character vector)
  #   svy_des:     (survey design object) survey design
  #
  # Returns:
  #   A data frame containing computed statistics for all specified variables.

  cat_var_stats <- lapply(cat_vars, svy_cat_props, svy_des)
  cont_var_stats <- lapply(cont_vars, svy_cont_mean, svy_des)
  cat_vars_frame <- do.call(rbind, lapply(cat_var_stats, function(frame) {
    rbind(names(frame), as.matrix(frame), c('', '', '', ''))
  }))
  cont_vars_frame <- do.call(rbind, lapply(cont_var_stats, function(frame) {
    rbind(names(frame), as.matrix(frame), c('', '', '', ''))
  }))
  demographics <- data.frame(rbind(cat_vars_frame, cont_vars_frame))
  row.names(demographics) <- NULL
  return(demographics)
}




#-------------------------------------------------------------------------------
# Survey Design Objects
#-------------------------------------------------------------------------------

# Survey design objects are required for all weighted analyses.  Domain analyses
# should be performed on survey design objects that are subset from the design
# object containing all respondents.  All design objects are created here for
# use in the analyses below.

# all respondents
overall_des <- svydesign(ids = ~1, weights = ~weight, data = df)

# respondents that were dual users at baseline
baseline_dual_user_des <- subset(overall_des, baseline_dual_use == 1)

# respondents that did and did not use ENDS at any point during the study period
any_ends_use_des <- subset(overall_des, any_ends_use == 'Any ENDS Use')
no_ends_use_des <- subset(overall_des, any_ends_use == 'No ENDS Use')

# respondents that reported smoking "everyday" at baseline
daily_smokers_des <- subset(overall_des, as_factor(CGNOW) == 'Everyday')

# Analysis of quit methods in table 7 are subset to respondents with complete
# information and where the outcome variable ">= 1 quit attempt during study"
# is 1.  This way, the numbers in table 7 correspond to the numbers in
# tables 3 and 4.  This subset is then further subdivided into groups based
# on follow-up smoking status and ENDS use, in order to perform analyses within
# each subgroup.
complete <- complete.cases(overall_des$variables[c('any_ends_use',
                                                   'quit_smoking',
                                                   categorical_covariates,
                                                   continuous_covariates)])

quit_method_des <- subset(overall_des, complete & quit_or_attempt == 1)

no_ends_smoking_des <- subset(quit_method_des,
                              any_ends_use == 'No ENDS Use' & quit_smoking == 0)
no_ends_quit_des <- subset(quit_method_des,
                           any_ends_use == 'No ENDS Use' & quit_smoking == 1)
ends_smoking_des <- subset(quit_method_des,
                           any_ends_use == 'Any ENDS Use' & quit_smoking == 0)
ends_quit_des <- subset(quit_method_des,
                        any_ends_use == 'Any ENDS Use' & quit_smoking == 1)




#-------------------------------------------------------------------------------
# Table 1. Smoking and ENDS use at one year follow-up for baseline dual users
#-------------------------------------------------------------------------------

svy_cat_props('dual_user_followup_status', baseline_dual_user_des)



#-------------------------------------------------------------------------------
# Table 2. Proportions/Means of covariates measures at baseline by ENDS use
#-------------------------------------------------------------------------------

get_demographics_table(categorical_covariates, continuous_covariates,
                       any_ends_use_des)

get_demographics_table(categorical_covariates, continuous_covariates,
                       no_ends_use_des)

# p values for comparisons
for (cat_var in categorical_covariates) {
  f <- as.formula(paste0('~', cat_var, ' + any_ends_use'))
  print(summary(svytable(f, overall_des)))
}

for (cont_var in continuous_covariates) {
  f <- as.formula(paste0(cont_var, '~ any_ends_use'))
  print(svyttest(f, overall_des))
}

rm(f, cat_var, cont_var)



#-------------------------------------------------------------------------------
# Table 3. Making a quit attempt and quitting smoking for >= 30 days by ENDS use
# among all baseline smokers (N = 822) and baseline daily smokers (N = 613)
#-------------------------------------------------------------------------------

# Model 1a: Baseline Ends Use
adjusted_logistic_table('quit_or_attempt', 'baseline_ends_use',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_smoking', 'baseline_ends_use',
                        categorical_covariates, continuous_covariates,
                        overall_des)

# Model 1b: Baseline Ends Use (Daily Smokers)
adjusted_logistic_table('quit_or_attempt', 'baseline_ends_use',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_smoking', 'baseline_ends_use',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

# Model 2a: Any Ends Use
adjusted_logistic_table('quit_or_attempt', 'any_ends_use',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_or_attempt', 'ends_groups',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_smoking', 'any_ends_use',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_smoking', 'ends_groups',
                        categorical_covariates, continuous_covariates,
                        overall_des)

# Model 2b: Any Ends Use (Daily Smokers)
adjusted_logistic_table('quit_or_attempt', 'any_ends_use',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_or_attempt', 'ends_groups',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_smoking', 'any_ends_use',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_smoking', 'ends_groups',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)



#-------------------------------------------------------------------------------
# Table 4. Making a quit attempt and quitting smoking for >= 30 days by ENDS use
# among all baseline smokers (N = 822) and baseline daily smokers (N = 613)
#-------------------------------------------------------------------------------

# Model 3a: ENDS Use Frequency
adjusted_logistic_table('quit_or_attempt', 'ends_use_frequency',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_smoking', 'ends_use_frequency',
                        categorical_covariates, continuous_covariates,
                        overall_des)

# Model 3b: ENDS Use Frequency (Daily Smokers)
adjusted_logistic_table('quit_or_attempt', 'ends_use_frequency',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_smoking', 'ends_use_frequency',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

# Model 4a: Importance of ENDS Use for Quitting Smoking
adjusted_logistic_table('quit_or_attempt', 'ends_used_to_quit_smoking',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_smoking', 'ends_used_to_quit_smoking',
                        categorical_covariates, continuous_covariates,
                        overall_des)

# Model 4b: Importance of ENDS Use for Quitting Smoking (Daily Smokers)
adjusted_logistic_table('quit_or_attempt', 'ends_used_to_quit_smoking',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_smoking', 'ends_used_to_quit_smoking',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

# Model 5a: ENDS Flavors
adjusted_logistic_table('quit_or_attempt', 'ends_flavor',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_smoking', 'ends_flavor',
                        categorical_covariates, continuous_covariates,
                        overall_des)

# Model 5b: ENDS Flavors (Daily Smokers)
adjusted_logistic_table('quit_or_attempt', 'ends_flavor',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_smoking', 'ends_flavor',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

# Model 6a: ENDS Device Type
adjusted_logistic_table('quit_or_attempt', 'ends_device_type',
                        categorical_covariates, continuous_covariates,
                        overall_des)

adjusted_logistic_table('quit_smoking', 'ends_device_type',
                        categorical_covariates, continuous_covariates,
                        overall_des)

# Model 6b: ENDS Device Type (Daily Smokers)
adjusted_logistic_table('quit_or_attempt', 'ends_device_type',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)

adjusted_logistic_table('quit_smoking', 'ends_device_type',
                        categorical_covariates, continuous_covariates,
                        daily_smokers_des)



#-------------------------------------------------------------------------------
# Table 5. Average daily cigarette consumption at one-year follow-up by ENDS use
# among non-quitters for all baseline smokers (N = 680) and baseline daily
# smokers (N = 543)
#-------------------------------------------------------------------------------

# Model 7a: Baseline ENDS Use
adjusted_linear_table('avg_num_cigs', 'baseline_ends_use',
                      categorical_covariates, continuous_covariates,
                      overall_des)

# Model 7b: Baseline ENDS Use (Daily Smokers)
adjusted_linear_table('avg_num_cigs', 'baseline_ends_use',
                      categorical_covariates, continuous_covariates,
                      daily_smokers_des)

# Model 8a: Any ENDS Use
adjusted_linear_table('avg_num_cigs', 'any_ends_use',
                      categorical_covariates, continuous_covariates,
                      overall_des)

adjusted_linear_table('avg_num_cigs', 'ends_groups',
                      categorical_covariates, continuous_covariates,
                      overall_des)

# Model 8b: Any ENDS Use (Daily Smokers)
adjusted_linear_table('avg_num_cigs', 'any_ends_use',
                      categorical_covariates, continuous_covariates,
                      daily_smokers_des)

adjusted_linear_table('avg_num_cigs', 'ends_groups',
                      categorical_covariates, continuous_covariates,
                      daily_smokers_des)



#-------------------------------------------------------------------------------
# Table 6. Average daily cigarette consumption at one-year follow-up by ENDS use
# and ends use characteristics among non-quitters for all baseline
# smokers (N = 680) and baseline daily smokers (N = 543)
#-------------------------------------------------------------------------------

# Model 9a: ENDS Use Frequency
adjusted_linear_table('avg_num_cigs', 'ends_use_frequency',
                      categorical_covariates, continuous_covariates,
                      overall_des)

# Model 9b: ENDS Use Frequency (Daily Smokers)
adjusted_linear_table('avg_num_cigs', 'ends_use_frequency',
                      categorical_covariates, continuous_covariates,
                      daily_smokers_des)

# Model 10a: Importance of ENDS Use for Quitting Smoking
adjusted_linear_table('avg_num_cigs', 'ends_used_to_quit_smoking',
                      categorical_covariates, continuous_covariates,
                      overall_des)

# Model 10b: Importance of ENDS Use for Quitting Smoking (Daily Smokers)
adjusted_linear_table('avg_num_cigs', 'ends_used_to_quit_smoking',
                      categorical_covariates, continuous_covariates,
                      daily_smokers_des)

# Model 11a: ENDS Flavors
adjusted_linear_table('avg_num_cigs', 'ends_flavor',
                      categorical_covariates, continuous_covariates,
                      overall_des)

# Model 11b: ENDS Flavors (Daily Smokers)
adjusted_linear_table('avg_num_cigs', 'ends_flavor',
                      categorical_covariates, continuous_covariates,
                      daily_smokers_des)

# Model 12a: ENDS Device Type
adjusted_linear_table('avg_num_cigs', 'ends_device_type',
                      categorical_covariates, continuous_covariates,
                      overall_des)

# Model 12b: ENDS Device Type (Daily Smokers)
adjusted_linear_table('avg_num_cigs', 'ends_device_type',
                      categorical_covariates, continuous_covariates,
                      daily_smokers_des)



#-------------------------------------------------------------------------------
# Table 7. Methods used to quit smoking by ENDS use and smoking status at
# follow-up (N = 374).
#-------------------------------------------------------------------------------

quit_method_design_objects <- list(no_ends_smoking_des,
                                   no_ends_quit_des,
                                   ends_smoking_des,
                                   ends_quit_des)

lapply(quit_method_design_objects, function(des) {
  print(des)
  print(svy_cat_props('quit_method_cold_turkey', des))
  print(svy_cat_props('quit_method_gradually', des))
  print(svy_cat_props('quit_method_completely_to_ends', des))
  print(svy_cat_props('quit_method_partially_to_ends', des))
  print(svy_cat_props('quit_method_nrt', des))
  print(svy_cat_props('quit_method_pharma', des))
  print(svy_cat_props('quit_method_counseling', des))
  print(svy_cat_props('quit_method_lccs', des))
  print(svy_cat_props('quit_method_other_tobacco', des))
  print(svy_cat_props('quit_method_support', des))
})

rm(quit_method_design_objects)




#-------------------------------------------------------------------------------
# Other Results Reported in Text Only
#-------------------------------------------------------------------------------

# Results - Descriptive data

# Among smokers who completed the follow-up survey, 27.1% (95% CI: 22.6%, 32.0%)
# reported using ENDS at baseline.
svy_cat_props('baseline_ends_use', overall_des)

# Among smokers who did not use ENDS at baseline, 73.5% (95% CI: 66.8, 81.2%)
# smoked daily compared 70.5% (95% CI: 60.6, 78.7%) among those who were using
# ENDS at baseline (p = .56).
svy_cat_props('CGNOW', subset(overall_des,
                              baseline_ends_use == 'No ENDS Use at Baseline'))

svy_cat_props('CGNOW', subset(overall_des,
                              baseline_ends_use == 'ENDS Use at Baseline'))

# Similarly, there was no statistically significant difference in the proportion
# of daily smokers among those who used ENDS at any point during the study and
# those that did not: 74.7% (95% CI: 66.8, 81.2%) and 71.4% (64.8, 77.2%),
# respectively (p = .50).
svy_cat_props('CGNOW', any_ends_use_des)
svy_cat_props('CGNOW', no_ends_use_des)
summary(svytable(~any_ends_use + CGNOW, overall_des))


# Discussion

# In this study, 16% of smokers in 2015 had stopped smoking a year later.
svy_cat_props('quit_smoking', overall_des)




#-------------------------------------------------------------------------------
# Session and Software Version Info
#-------------------------------------------------------------------------------

# > devtools::session_info()
# Session info -----------------------------------------------------------------
# setting  value
# version  R version 3.5.1 (2018-07-02)
# system   x86_64, mingw32
# ui       RStudio (1.1.447)
# language (EN)
# collate  English_United States.1252
# tz       America/New_York
# date     2018-08-09
#
# Packages ---------------------------------------------------------------------
# package    * version date       source
# assertthat   0.2.0   2017-04-11 CRAN (R 3.5.0)
# base       * 3.5.1   2018-07-02 local
# bindr        0.1.1   2018-03-13 CRAN (R 3.5.0)
# bindrcpp   * 0.2.2   2018-03-29 CRAN (R 3.5.0)
# compiler     3.5.1   2018-07-02 local
# crayon       1.3.4   2017-09-16 CRAN (R 3.5.0)
# datasets   * 3.5.1   2018-07-02 local
# devtools     1.13.6  2018-06-27 CRAN (R 3.5.1)
# digest       0.6.15  2018-01-28 CRAN (R 3.5.0)
# dplyr      * 0.7.6   2018-06-29 CRAN (R 3.5.1)
# forcats      0.3.0   2018-02-19 CRAN (R 3.5.0)
# glue         1.3.0   2018-07-17 CRAN (R 3.5.1)
# graphics   * 3.5.1   2018-07-02 local
# grDevices  * 3.5.1   2018-07-02 local
# grid       * 3.5.1   2018-07-02 local
# haven      * 1.1.2   2018-06-27 CRAN (R 3.5.1)
# hms          0.4.2   2018-03-10 CRAN (R 3.5.0)
# lattice      0.20-35 2017-03-25 CRAN (R 3.5.1)
# magrittr     1.5     2014-11-22 CRAN (R 3.5.0)
# Matrix     * 1.2-14  2018-04-13 CRAN (R 3.5.1)
# memoise      1.1.0   2017-04-21 CRAN (R 3.5.0)
# methods    * 3.5.1   2018-07-02 local
# pillar       1.3.0   2018-07-14 CRAN (R 3.5.1)
# pkgconfig    2.0.1   2017-03-21 CRAN (R 3.5.0)
# purrr        0.2.5   2018-05-29 CRAN (R 3.5.1)
# R6           2.2.2   2017-06-17 CRAN (R 3.5.0)
# Rcpp         0.12.18 2018-07-23 CRAN (R 3.5.1)
# readr        1.1.1   2017-05-16 CRAN (R 3.5.0)
# rlang        0.2.1   2018-05-30 CRAN (R 3.5.1)
# rstudioapi   0.7     2017-09-07 CRAN (R 3.5.0)
# splines      3.5.1   2018-07-02 local
# stats      * 3.5.1   2018-07-02 local
# survey     * 3.33-2  2018-03-13 CRAN (R 3.5.0)
# survival   * 2.42-3  2018-04-16 CRAN (R 3.5.1)
# tibble       1.4.2   2018-01-22 CRAN (R 3.5.0)
# tidyselect   0.2.4   2018-02-26 CRAN (R 3.5.0)
# tools        3.5.1   2018-07-02 local
# utils      * 3.5.1   2018-07-02 local
# withr        2.1.2   2018-03-15 CRAN (R 3.5.0)
# yaml         2.2.0   2018-07-25 CRAN (R 3.5.1)