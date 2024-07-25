#'Function that will return default values of the program change values.
#'
#'@name calculateProdCost
#'@param TbCases a matrix of tuberculosis cases by year and age group
#'@param TbDeaths a matrix of tuberculosis deaths by year and age group
#'@param LtbiTxInits a matrix of ltbi treatment initations cases by year and age group
#'@return
#'@export
calculateProdCost <- function(TbCases,
                              TbDeaths,
                              LtbiTxInits,
                              discount = 0.03,
                              tx_dist = c(1/3, 1/3, 1/3),
                              Ages="SingleYearAge",
                              StartYear = 2023,
                              USDYear = 2022,
                              uncertainty = TRUE,
                              seed = 10){
  ## Define the factor for uncertainty calculations
  uncertaintyFactor <- 0.25
  set.seed(seed)

  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  ## PRODUCTIVITY COST CALCULATION VALUES (in 2022 USD)
  ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
  TLTBI_clinic <- ifelse(uncertainty == TRUE,
                         rnorm(n=1, mean = (53.73+31.50), sd = (53.73+31.50)*uncertaintyFactor),
                         (53.73 + 31.50)) #initial and follow-up clinic visits
  TLTBI_ae <- ifelse(uncertainty == TRUE,
                     rnorm(n=1, mean = 6.48, sd = 6.48 * uncertaintyFactor),
                     6.48)#adverse event costs with 3HP or 3HR
  P_TB_hosp <- ifelse(uncertainty == TRUE,
                      rnorm(n=1, mean = 0.49, sd = 0.49 * uncertaintyFactor),
                      0.49) #probability of hospitalization with TB
  # average duration of TB prior to treatment
  DUR_TB_prTX <- ifelse(uncertainty == TRUE,
                        rnorm(n=1, mean = 3/12, sd = 3*uncertaintyFactor/12),
                        3/12) ### 3 months
  DUR_TB_TX_hosp <- ifelse(uncertainty == TRUE,
                           rnorm(n=1, mean = 24/365, sd = 24*uncertaintyFactor/365),
                           24/365) #average duration of hospitalization with TB (24 days)
  DUR_TB_TX_outpatient <- ifelse(uncertainty == TRUE,
                                 rnorm(n=1, mean = 6.8/365, sd = 6.8 * uncertaintyFactor/365),
                                 6.8/365) #duration of time loss from outpatient services (6.8 days)
  P_TLTBI_tox <- ifelse(uncertainty == TRUE,
                        rnorm(n=1, mean = (.003*(tx_dist[1]+tx_dist[2])), sd = (.003*(tx_dist[1]+tx_dist[2])*uncertaintyFactor)),
                              (.003*(tx_dist[1]+tx_dist[2]))) #CDC based

  ##### Bureau of Labor 2016 to 2020 conversion value
  # convFact <- 1.14
  ##### Bureau of Labor 2016 to 2021 conversion value
  # convFact <- 1.245
  ##### Bureau of Labor 2016 to 2022 conversion value
  # convFact <- 1.28

  ## Check for discount and add in appropriate data
  if (discount == 0){
    discountVec <- rep(1, nrow(TbCases))
    annual_prod <- readRDS(system.file(paste0(USDYear,"/AnnualProductivity", Ages, ".rds"), package = "TubercuCost"))

    yearIndex <- which(annual_prod$Year == StartYear) + (0:(nrow(TbDeaths)-1))
    annual_prod <- annual_prod[yearIndex,]; annual_prod <- annual_prod

    annual_expend <- readRDS(system.file(paste0(USDYear,"/NonHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
    yearIndex <- which(rownames(annual_expend) == StartYear) + (0:(nrow(TbDeaths)-1))
    annual_expend <- annual_expend[yearIndex,]

    lifetime_prod <- readRDS(system.file(paste0(USDYear,"/LifetimeProductivityCosts", Ages, ".rds"), package = "TubercuCost"))
    yearIndex <- which(rownames(lifetime_prod) == StartYear) + (0:(nrow(TbDeaths)-1))
    lifetime_prod <- lifetime_prod[yearIndex,]; lifetime_prod <- lifetime_prod

    lifetime_expend <- readRDS(system.file(paste0(USDYear,"/LifetimeNonHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
    yearIndex <- which(rownames(lifetime_expend) == StartYear) + (0:(nrow(TbDeaths)-1))
    lifetime_expend <- lifetime_expend[yearIndex,]

  } else if (discount == 0.03){

    discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[1:nrow(TbCases),2]
    ## annual productivity
    annual_prod <- readRDS(system.file(paste0(USDYear,"/DiscountedAnnualProductivityCosts", Ages, ".rds"), package = "TubercuCost"))
    yearIndex <- which(annual_prod$Year == StartYear) + (0:(nrow(TbDeaths)-1))
    annual_prod <- annual_prod[yearIndex,]; annual_prod[,2:ncol(annual_prod)] <- annual_prod[,2:ncol(annual_prod)]
    ## annual non health care expenditures
    annual_expend <- readRDS(system.file(paste0(USDYear,"/DiscountedNonHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
    yearIndex <- which(rownames(annual_expend) == StartYear) + (0:(nrow(TbDeaths)-1))
    annual_expend <- annual_expend[yearIndex,]
    ## lifetime productivity
    lifetime_prod <- readRDS(system.file(paste0(USDYear,"/DiscountedLifetimeProductivityCosts", Ages, ".rds"), package = "TubercuCost"))
    yearIndex <- which(rownames(lifetime_prod) == StartYear) + (0:(nrow(TbDeaths)-1))
    lifetime_prod <- lifetime_prod[yearIndex,]; lifetime_prod[,2:ncol(lifetime_prod)] <- lifetime_prod[,2:ncol(lifetime_prod)]
    ## lifetime non health care expenditures
    lifetime_expend <- readRDS(system.file(paste0(USDYear,"/DiscountedLifetimeNonHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
    yearIndex <- which(rownames(lifetime_expend) == StartYear) + (0:(nrow(TbDeaths)-1))
    lifetime_expend <- lifetime_expend[yearIndex,]
  } else {
    print("Current discount level not supported.")
    break()
  }
  ### Check for uncertainty and adjust
  ### We want to move these all together
  ### Calculate the percentage to move by sampling
  if(uncertainty == TRUE){
    uncertaintyPerc <- rnorm(n = 1, mean = 1, sd = uncertaintyFactor)
    annual_prod <- annual_prod*uncertaintyPerc
    annual_expend <- annual_expend*uncertaintyPerc
    lifetime_prod <- lifetime_prod*uncertaintyPerc
    lifetime_expend <- lifetime_expend*uncertaintyPerc
  }

  ##### CREATE A DATAFRAME OF COSTS, NOT THE TOTAL SUM
  ##### WE CAN CREATE SUMS ON OTHER SIDE
  TbMortCost <- TltbiProdCost <- TbProdCost <-matrix(0, nrow = nrow(TbCases),
                                                     ncol = ncol(TbCases))
  for (i in 1:nrow(TbCases)){
    for (j in 1:ncol(TbCases)){
      ##### PRODUCTIVITY COSTS DUE TO TLTBI
      ##### ##### we divide by 1e3 to get the costs in millions (counts are in 000s when read in)
      # number of initiations * clinic visits cost * adverse event costs
      TltbiProdCost[i,j] <- (LtbiTxInits[i,j] *(TLTBI_clinic+TLTBI_ae*P_TLTBI_tox)*discountVec[i])

      #PRODUCTIVITY COSTS DUE TO TB DISEASE
      #age specific TB treatment initiations*((probability of TB hospitalization*duration of TB hospitalization)+
      #duration of outpatient losses)*annual productivity estimates +
      #age specific TB deaths*lifetime productivity estimates
      TbProdCost[i,j] <- (TbCases[i,j] * (annual_prod[i,j] - annual_expend[i,j]))*((DUR_TB_prTX + P_TB_hosp*DUR_TB_TX_hosp)+DUR_TB_TX_outpatient) * discountVec[i]
      TbMortCost[i,j] <- (TbDeaths[i,j] * (lifetime_prod[i,j] - lifetime_expend[i,j]))
    }
  }
  colnames(TltbiProdCost) <- colnames(TbProdCost) <- colnames(TbCases)
  ProdCosts <- list()

  ProdCosts[["TltbiProdCost"]] <- TltbiProdCost
  ProdCosts[["TbProdCost"]]    <- TbProdCost
  ProdCosts[["TbMortCost"]]    <- TbMortCost

  ProdCosts[["TotalProdCost"]] <- TbProdCost + TbMortCost + TltbiProdCost

  return(ProdCosts)

}
