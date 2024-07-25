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
                           Ages = "SingleYearAge",
                           uncertainty = TRUE,
                           seed = 10,
                           StartYear = 2023,
                           popLifeExpRed = 0){

  ### Check input types
  TbCases <- as.matrix(TbCases)
  TbDeaths <- as.matrix(TbDeaths)
  LtbiTxInits <- as.matrix(LtbiTxInits)

  ## Define the factor for uncertainty calculations
  uncertaintyFactor <- 0.25
  set.seed(seed)

  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## Define some constants used in QALY calculations below
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

  ## Utility weight with LTBI treatment without toxicity
  TLTBI_UW <- 1
  ## Disutility weight with LTBI treatment with toxicity
  TLTBI_UW_tox <- 1-.75
  ## utility decrement with TB disease and treatment
  ## Guo, et. al 2008
  # TB_UW <- 1- (ifelse(uncertainty == TRUE,
  #                 rnorm(n=1, mean = .24, sd = 0.06),
  #                 0.24))
  ## Bauer, et. al, 2015
  TB_UW_prTX <- 1- (ifelse(uncertainty == TRUE,
                  rnorm(n=1, mean = .25, sd = 0.08),
                   0.25))
  #### This is for the 2-6 months of treatment
  #### Use preTX for first month
  TB_UW_TX <- 1- (ifelse(uncertainty == TRUE,
                           rnorm(n=1, mean = .11, sd = 0.09),
                           0.11))

  ## Probability of toxicity during LTBI treatment
  P_TLTBI_tox <- 0.003
  # average duration of TB prior to treatment
  DUR_TB_prTX <- ifelse(uncertainty == TRUE,
                        rnorm(n=1, mean = 4/12, sd = (4*uncertaintyFactor)/12),
                        4/12) ### 3 months preTX plus first month of TX
  ## Duration of reduced quality of life with toxicity (two weeks)
  DUR_TLTBI_tox <- 2/52
  DUR_TB_TX <- ifelse(uncertainty == TRUE,
                      rnorm(n=1, mean = 8/12, sd = (8*uncertaintyFactor)/12),
                      8/12) ### latter 8 Months

  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## Load in life expectancy values and calculate the life years lost to tb death
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

  if (discount == 0){
      discountVec <- rep(1, nrow(TbCases))
      ### We will hold life expectancy fixed (no uncertainty)
      life_exp <- readRDS(system.file(paste0("LifeExpectancy", Ages, ".rds"), package = "TubercuCost"))
      yearIndex <- which(life_exp$Year == StartYear) + (0:(nrow(TbDeaths)-1))
      life_exp <- life_exp[yearIndex,]; life_exp[,-1] <- life_exp[,-1] * (1-popLifeExpRed)

      # Calculate death QALYs
      # sum of deaths * age specific life expectancy
      DeathQaly <- TbDeaths*life_exp[,-1]
  } else if (discount == 0.03){
    discountVec <- as.vector(unlist(readRDS(system.file("DiscountingThreePercentScalars.rds", package = "TubercuCost"))[1:nrow(TbCases),2]))
    ### We will hold life expectancy fixed (no uncertainty)
      disc_life_exp <- readRDS(system.file(paste0("DiscountedLifeExpectancy", Ages, ".rds"), package = "TubercuCost"))
      # yearIndex <- which(disc_life_exp$X == 2024) + (0:(nrow(deaths)-1))
      yearIndex <- which(disc_life_exp$Year == StartYear) + (0:(nrow(TbDeaths)-1))
      disc_life_exp <- disc_life_exp[yearIndex,]; disc_life_exp[,-1] <- disc_life_exp[,-1] * (1-popLifeExpRed)

      # Calculate death QALYs
      # sum of deaths * age specific life expectancy
      DeathQaly <- TbDeaths*disc_life_exp[, -1]
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
    CaseQaly[i,]  <- ((TB_UW_TX * DUR_TB_TX) + (TB_UW_prTX * DUR_TB_prTX)) * TbCases[i,] * discountVec[i]
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

