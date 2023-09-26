#'Function that will return default values of the program change values.
#'
#'@name calculateQALYs
#'@param TbCases
#'@param TbDeaths
#'@param LtbiTxInits
#'@return
#'@export
calculateQALYs <- function(TbCases,
                           TbDeaths,
                           LtbiTxInits,
                           discount = 0.03,
                           Ages = "SingleYearAge"){

  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## Define some constants used in QALY calculations below
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

  ## Utility weight with LTBI treatment without toxicity
  TLTBI_UW<-1
  ## Disutility weight with LTBI treatment with toxicity
  TLTBI_UW_tox<-1-.75
  ## Disutility weight with TB disease and treatment
  TB_UW<-1-.76
  ## Probability of toxicity during LTBI treatment
  P_TLTBI_tox<-0.003
  ## Duration of reduced quality of life with toxicity (two weeks)
  DUR_TLTBI_tox<-2/52
  DUR_TB_TX<-.75

  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## Load in life expectancy values and calculate the life years lost to tb death
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

  if (discount == 0){
      discountVec <- rep(1, nrow(TbCases))
      life_exp <- readRDS(system.file(paste0("LifeExpectancy", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ]
      # Calculate death QALYs
      # sum of deaths * age specific life expectancy
      DeathQaly <- TbDeaths*life_exp
  } else if (discount == 0.03){
      discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[1:nrow(TbDeaths),2]
      disc_life_exp <- readRDS(system.file(paste0("DiscountedLifeExpectancy", Ages, ".rds"), package = "TubercuCost"))[1:nrow(TbDeaths), ]
      # Calculate death QALYs
      # sum of deaths * age specific life expectancy
      DeathQaly <- TbDeaths*disc_life_exp
  } else {
      print("Current discount level not supported.")
      break()
  }

  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## Calculate QALYs due to LTBI treatment and TB cases
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

  TltbiQaly <- CaseQaly <- matrix(0, nrow = nrow(TbCases), ncol = ncol(TbCases))
  colnames(DeathQaly) <- colnames(TltbiQaly) <- colnames(CaseQaly) <- colnames(TbCases)


  for(i in 1:nrow(TbCases)){
    #dis-utility weight*probability*duration*number [...of events]
    TltbiQaly[i,] <- TLTBI_UW_tox*P_TLTBI_tox*DUR_TLTBI_tox*LtbiTxInits[i,]*discountVec[i]
    #dis-utility weight*duration*number [...of event]
    CaseQaly[i,]  <- TB_UW*DUR_TB_TX*TbCases[i,]*discountVec[i]
  }

  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## Create a list to hold the individual and total QALYs dataframes
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

  QALYs <- list()
  QALYs[["TltbiQaly"]] <- TltbiQaly
  QALYs[["CaseQaly"]]  <- CaseQaly
  QALYs[["DeathQaly"]] <- DeathQaly
  QALYs[["TotalQaly"]] <- CaseQaly + TltbiQaly + DeathQaly

  return(QALYs)

  }

