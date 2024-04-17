# ClinicalCharacteristics 0.2.3

* bug fix specialty visit
* bug fix dat table interface (drop table and suffix for dat table name)
* bug fix cost labels
* bug fix score and categorize issues in snowflake

# ClinicalCharacteristics 0.2.2

* bug fix yearChar to add labels going back to 1987
* bug fix conceptType in CountChar

# ClinicalCharacteristics 0.2.1

* bug fix to aggregate yearChar
* bug fix label table for year coerce to char

# ClinicalCharacteristics 0.2.0

* Remove arrow dependency ==> aggregate on dbms
* add labels method for char classes
* add categorize sql 
* add charlson sql 
* condense runClinicalCharacteristics to tabulate and save results

# ClinicalCharacteristics 0.1.0

* overhaul software to remove FeatureExtraction dependency
* make clinChar object => specify data extraction
* add extract objects => specify types of extraction by demographics or domains
* summarize the clinChar object
* export summary to csv
* Build out the circe sql to pull concept sets
* as_sql method to coerce class into sql
* Available characterizations:
    - Presence: Drug, Condition, Procedure, Visit, Measurement, Observation
    - Count: Drug, Condition, Procedure, Visit, Measurement, Observation
    - Cost: Drug, Procedure, Visit
    - TimeTo: Drug, Condition, Procedure, Visit, Measurement, Observation
    - TimeIn: Cohort, Inpatient
    - Demographics: age, race, gender, ethnicity, location, index year
    - Visit Detail: care site and provider specialty
* Functions to categorize continuous vars (i.e. age to age10yrGroups)
* Functions to score categorical vars (i.e. conditions to Charlson Index)

# ClinicalCharacteristics 0.0.2

* add tempEmulationSchema for snowflake
* update settings function ui

# ClinicalCharacteristics 0.0.1

* Fix dealing with temporal covariates
* Add domain count 
* Add Visit characteristics
* Add measurement characteristics
* Start putting together table shells

# ClinicalCharacteristics 0.0.0.9000

* Initial version of package
* Build characteristics and output to folder
