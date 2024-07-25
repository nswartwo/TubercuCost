#'Function that will return default values of the health services
#'costs associated with LTBI and TB in the United States. These values
#'are all represented in 2020 USD.
#'
#'@name HealthServiceUnitCosts
#'@param uncertainty
#'@return vector of the default values
#'@export

HealthServiceUnitCosts <- function(uncertainty = TRUE,
                                   seed = 10){
  set.seed(seed)
  ## Define the factor for uncertainty calculations
  uncertaintyFactor <- 0.25
  ## Create an empty vector to hold the values
  DefaultCostsVector <- rep(NA,10)
  names(DefaultCostsVector) <- c('LTBIIdCost','TSTCost','IGRACost','NoTBCost',
                                 '3HPCost','4RCost','3HRCost','TBIdCost', 'TBtest',
                                 'TBtx')
  ##LTBI Care Cascade Associated Costs
  DefaultCostsVector['LTBIIdCost'] <- 0
  ### CMS 2021, 2022 Physician Fee CPT 86580
  DefaultCostsVector['TSTCost'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = 10.12, sd = 10.12*uncertaintyFactor), 10.12)
  ### CMS 2021, 2022 Clinical Lab Fee CPT 86480
  DefaultCostsVector['IGRACost'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = 61.98, sd = 61.98*uncertaintyFactor), 61.98)
  ### CMS 2021, Physician’s Fee CPT 71046
  DefaultCostsVector['NoTBCost'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = 34.61, sd = 34.61*uncertaintyFactor), 34.61)


  ## LTBI Treatment Regimens Costs
  ### for ease of update, we will define the daily cost of each drug
  ### then compute the combo below for the regimen
  ### we also include clinic cost, supplies, and adverse events clinic cost
  rifampinCost    <- 2.45 # rifampin 600 mg, $2.45/dose;
  isoniazidCost   <- 0.19 # isoniazid 300 mg, $0.19/dose;
  rifapentineCost <- 14.44 * (900/1200) #rifapentine 1,200 mg, $14.44/dose;

  ### 3HP = 3 months of weekly doeses of 900 isoniazid and 900 mg of rifapentine
  mean3HP <- 557.26; #12 * (rifapentineCost + 3*isoniazidCost) + 176 + 68 + 34
  DefaultCostsVector['3HPCost'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = mean3HP, sd = mean3HP*uncertaintyFactor), mean3HP) ### EID Publication 2023
  ### 4R = 4 months of daily doses of 600 rifampin
  mean4R <- 1275.91; #120 * rifampinCost + 203 + 72 + 11
  DefaultCostsVector['4RCost'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = mean4R, sd = mean4R*uncertaintyFactor), mean4R) ### EID Publication 2023
  ### 3HR = 3 months of daily doses of 300 isoniazid and 600 mg of rifampin
  mean3HR <- 1054.27; #90 * (rifampinCost + isoniazidCost) + 176 + 68 + 34
  DefaultCostsVector['3HRCost'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = mean3HR, sd = mean3HR*uncertaintyFactor), mean3HR) ### EID Publication 2023

  ## TB Care Cascade Costs
  DefaultCostsVector['TBIdCost'] <- 0
  DefaultCostsVector['TBtest'] <- 0
  # DefaultCostsVector['TBtx'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = 23060, sd = 23060*uncertaintyFactor), 23060) ### EID Publication 2023
  DefaultCostsVector['TBtx'] <- ifelse(uncertainty == TRUE, rnorm(n = 1, mean = 25285, sd = 25285*uncertaintyFactor), 25285) ### EID Publication 2023; includes MDR


  return(DefaultCostsVector)
}
