#'Function that will return default values of the program change values.
#'
#'@name calculateHealthCost
#'@param TbCases
#'@param LtbiTxInits
#'@return
#'@export
calculateHealthCost <- function(TbCases,
                                TbDeaths,
                                LtbiTests,
                                IGRA_frc = 0.5,
                                discount = 0.03,
                                tx_dist = c(1/3, 1/3, 1/3),
                                LtbiTxInits,
                                Ages = "SingleYearAge",
                                StartYear = 2023,
                                USDYear = 2022,
                                uncertainty = FALSE,
                                seed = 10,
                                population = NULL,
                                UnitCosts = HealthServiceUnitCosts(uncertainty)){

  ## Define the factor for uncertainty calculations
  uncertaintyFactor <- 0.25
  set.seed(seed)

  # UnitCosts = HealthServiceUnitCosts(uncertainty = uncertainty)
  ## Check for discount and add in appropriate data
  if (discount == 0){
      discountVec <- rep(1, nrow(TbCases))
      if (is.null(population)){
      annual_healthExpend <- readRDS(system.file(paste0(USDYear, "/HealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
      yearIndex <- which(rownames(annual_healthExpend) == StartYear) + (0:(nrow(TbDeaths)-1))
      annual_healthExpend <- annual_healthExpend[yearIndex,]
      lifetime_healthExpend <- readRDS(system.file(paste0(USDYear, "/LifetimeHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ]
      } else {
        annual_healthExpend <- readRDS(system.file(paste0(USDYear, "/HealthExpenditure", Ages, population, ".rds"), package = "TubercuCost"))
        yearIndex <- which(rownames(annual_healthExpend) == StartYear) + (0:(nrow(TbDeaths)-1))
        annual_healthExpend <- annual_healthExpend[yearIndex,]
        lifetime_healthExpend <- readRDS(system.file(paste0(USDYear, "/LifetimeHealthExpenditure", Ages, population, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ]
      }
  } else if (discount == 0.03){
    discountVec <- as.vector(unlist(readRDS(system.file("DiscountingThreePercentScalars.rds", package = "TubercuCost"))[1:nrow(TbCases),2]))
    if (is.null(population)){
        annual_healthExpend <- readRDS(system.file(paste0(USDYear, "/DiscountedHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
        yearIndex <- which(rownames(annual_healthExpend) == StartYear) + (0:(nrow(TbDeaths)-1))
        annual_healthExpend <- annual_healthExpend[yearIndex,]
        lifetime_healthExpend <- readRDS(system.file(paste0(USDYear, "/DiscountedLifetimeHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ]
      } else {
        annual_healthExpend <- readRDS(system.file(paste0(USDYear, "/DiscountedHealthExpenditure", Ages, population, ".rds"), package = "TubercuCost"))
        yearIndex <- which(rownames(annual_healthExpend) == StartYear) + (0:(nrow(TbDeaths)-1))
        annual_healthExpend <- annual_healthExpend[yearIndex,]
        lifetime_healthExpend <- readRDS(system.file(paste0(USDYear, "/DiscountedLifetimeHealthExpenditure", Ages, population, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ]
      }
  } else {
      print("Current discount level not supported.")
      break()
  }
  ### Check for uncertainty and adjust
  ### We want to move these all together
  ### Calculate the percentage to move by sampling
  # if(uncertainty == TRUE){
  #   uncertaintyPerc <- rnorm(n = 1, mean = 1, sd = uncertaintyFactor)
  #   annual_healthExpend <- annual_healthExpend*uncertaintyPerc
  #   lifetime_healthExpend <- lifetime_healthExpend*uncertaintyPerc
  # }

# HEALTH SERVICES COSTS DUE TO TLTBI
# number of tests times the cost of those tests * the cost of the regimen.
# Each of these costs will be calculated as a weighted average.
LtbiTestCost <- LtbiTests * ((IGRA_frc*UnitCosts[['IGRACost']]) +
                            ((1-IGRA_frc)*UnitCosts[['TSTCost']]) +
                            UnitCosts['NoTBCost']) * discountVec

TltbiCost    <- LtbiTxInits * ((tx_dist[1]*UnitCosts[['3HPCost']]) +
                               (tx_dist[2]*UnitCosts[['4RCost']]) +
                               (tx_dist[3]*UnitCosts[['3HRCost']])) * discountVec

TltbiHealthCost <- (LtbiTestCost + TltbiCost)

# HEALTH SERVICES COSTS DUE TO TB DISEASE
# Number of tests * (cost of those tests + the cost of the regimen)
# TbHealthCost <- (rowSums(TbCases * (UnitCosts[['TBtest']] + UnitCosts[['TBtx']])) * discountVec)
TbHealthCost <- TbCases * (UnitCosts[['TBtest']] + UnitCosts[['TBtx']]) * discountVec

# nonTB healthcare expenditures
NonTBHealthExpenditure <- TbDeaths * (lifetime_healthExpend - ((UnitCosts[['TBtest']] + UnitCosts[['TBtx']])* discountVec))

HealthServiceCosts <- list()
HealthServiceCosts[["LtbiTest"]] <- LtbiTestCost
HealthServiceCosts[["LtbiTx"]] <- TltbiCost
HealthServiceCosts[["TB"]] <- TbHealthCost
HealthServiceCosts[["nonTB"]] <- NonTBHealthExpenditure
HealthServiceCosts[["TotalHealthServiceCosts"]] <- TltbiHealthCost +
                                                   TbHealthCost -
                                                   NonTBHealthExpenditure

return(HealthServiceCosts)

}
