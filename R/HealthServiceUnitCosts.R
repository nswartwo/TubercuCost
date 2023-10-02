#'Function that will return default values of the health services
#'costs associated with LTBI and TB in the United States. These values
#'are all represented in 2020 USD.
#'
#'@name HealthServiceUnitCosts
#'@return vector of the default values
#'@export

HealthServiceUnitCosts <- function(){
  ## Create an empty vector to hold the values
  DefaultCostsVector <- rep(NA,10)
  names(DefaultCostsVector) <- c('LTBIIdCost','TSTCost','IGRACost','NoTBCost',
                                 '3HPCost','4RCost','3HRCost','TBIdCost', 'TBtest',
                                 'TBtx')
  ##LTBI Care Cascade Associated Costs
  DefaultCostsVector['LTBIIdCost'] <- 0
  DefaultCostsVector['TSTCost'] <- 10.12 ### CMS 2021, Physician Fee CPT 86580
  DefaultCostsVector['IGRACost'] <- 61.98 ### CMS 2021, Clinical Lab Fee CPT 86480
  DefaultCostsVector['NoTBCost'] <- 34.20 ### CMS 2021, Physician’s Fee CPT 71046


  ## LTBI Treatment Regimens Costs
  ### for ease of update, we will define the daily cost of each drug
  ### then compute the combo below for the regimen
  ### we also include clinic cost, supplies, and adverse events clinic cost
  rifampinCost    <- 2.45 # rifampin 600 mg, $2.45/dose;
  isoniazidCost   <- 0.19 # isoniazid 300 mg, $0.19/dose;
  rifapentineCost <- 14.44 * (900/1200) #rifapentine 1,200 mg, $14.44/dose;

  ### 3HP = 3 months of weekly doeses of 900 isoniazid and 900 mg of rifapentine
  DefaultCostsVector['3HPCost'] <- 12 * (rifapentineCost + 3*isoniazidCost) + 176 + 68 + 34
  ### 4R = 4 months of daily doses of 600 rifampin
  DefaultCostsVector['4RCost'] <- 120 * rifampinCost + 203 + 72 + 11
  ### 3HR = 3 months of daily doses of 300 isoniazid and 600 mg of rifampin
  DefaultCostsVector['3HRCost'] <- 90 * (rifampinCost + isoniazidCost) + 176 + 68 + 34
  DefaultCostsVector['3HRCost'] <- 353.61 ### Still 2020

  ## TB Care Cascade Costs
  DefaultCostsVector['TBIdCost'] <- 0
  DefaultCostsVector['TBtest'] <- 0
  DefaultCostsVector['TBtx'] <- 23060 ### EID Publication in press 2023

  return(DefaultCostsVector)
}
