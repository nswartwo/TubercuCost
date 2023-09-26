#'Function that will return default values of the program change values.
#'
#'@name calculateHealthCost
#'@param TbCases
#'@param LtbiTxInits
#'@return
#'@export
calculateHealthCost <- function(UnitCosts = HealthServiceUnitCosts(),
                                TbCases,
                                TbDeaths,
                                LtbiTests,
                                IGRA_frc = 0.5,
                                discount = 0.03,
                                tx_dist = c(1/3, 1/3, 1/3),
                                LtbiTxInits,
                                Ages = "SingleYearAge"){

  ## Check for discount and add in appropriate data
  if (discount == 0){
      discountVec <- rep(1, nrow(TbCases))
      annual_healthExpend <- readRDS(system.file(paste0("HealthExpenditure", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ] # reported in 2020 dollars
      lifetime_healthExpend <- readRDS(system.file(paste0("LifetimeHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ] # reported in 2020 dollars
  } else if (discount == 0.03){
      discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[1:nrow(TbCases),2]
      annual_healthExpend <- readRDS(system.file(paste0("DiscountedHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ] # reported in 2020 dollars
      lifetime_healthExpend <- readRDS(system.file(paste0("DiscountedLifetimeHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ] # reported in 2020 dollars
  } else {
      print("Current discount level not supported.")
      break()
  }
# HEALTH SERVICES COSTS DUE TO TLTBI
# number of tests times the cost of those tests * the cost of the regimen.
# Each of these costs will be calculated as a weighted average.
LtbiTestCost <- LtbiTests * ((IGRA_frc*UnitCosts[['IGRACost']]) +
                            ((1-IGRA_frc)*UnitCosts[['TSTCost']]) +
                            UnitCosts['NoTBCost'])

TltbiCost    <- LtbiTxInits * ((tx_dist[1]*UnitCosts[['3HPCost']]) +
                               (tx_dist[2]*UnitCosts[['4RCost']]) +
                               (tx_dist[3]*UnitCosts[['3HRCost']]))

TltbiHealthCost <- 0 #((LtbiTestCost + TltbiCost)*discount)/1e3

# HEALTH SERVICES COSTS DUE TO TB DISEASE
# Number of tests * (cost of those tests + the cost of the regimen)
TbHealthCost <- (rowSums(TbCases * (UnitCosts[['TBtest']] + UnitCosts[['TBtx']])) * discountVec)

# nonTB healthcare expenditures
NonTBHealthExpenditure <- TbDeaths * (lifetime_healthExpend - (UnitCosts[['TBtest']] + UnitCosts[['TBtx']]))

HealthServiceCosts <- list()
HealthServiceCosts[["Ltbi"]] <- sum(TltbiHealthCost)
HealthServiceCosts[["TB"]] <- sum(TbHealthCost)
HealthServiceCosts[["nonTB"]] <- sum(NonTBHealthExpenditure)
HealthServiceCosts[["TotalHealthServiceCosts"]] <- TltbiHealthCost + TbHealthCost - NonTBHealthExpenditure

return(HealthServiceCosts)

}
