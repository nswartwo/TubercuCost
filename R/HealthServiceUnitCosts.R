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
  DefaultCostsVector['TSTCost'] <- 9.38
  DefaultCostsVector['IGRACost'] <- 61.98
  DefaultCostsVector['NoTBCost'] <- 33.20

  ## LTBI Treatment Regimens Costs
  DefaultCostsVector['3HPCost'] <- 411.87
  DefaultCostsVector['4RCost'] <- 354.87
  DefaultCostsVector['3HRCost'] <- 353.61

  ## TB Care Cascade Costs
  DefaultCostsVector['TBIdCost'] <- 0
  DefaultCostsVector['TBtest'] <- 0
  DefaultCostsVector['TBtx'] <- 20211

  return(DefaultCostsVector)
}
