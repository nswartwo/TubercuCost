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
                              Ages="SingleYearAge"){

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
## PRODUCTIVITY COST CALCULATION VALUES
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
TLTBI_clinic<-48.01+28.16 #initial and follow-up clinic visits
TLTBI_ae<-5.27 #adverse event costs with 3HP or 3HR
P_TB_hosp<-0.49 #probability of hospitalization with TB
DUR_TB_prTX <- 3/12 # average duration of TB prior to treatment (3 months)
DUR_TB_TX_hosp<-24/365 #average duration of hospitalization with TB (24 days)
DUR_TB_TX_outpatient <- 6.8/365 #duration of time loss from outpatient services (6.8 days)
P_TLTBI_tox <- .003*(tx_dist[1]+tx_dist[2]) #CDC based

##### Bureau of Labor 2016 to 2020 conversion value
convFact <- 1.14

## Check for discount and add in appropriate data
if (discount == 0){
    discountVec <- rep(1, nrow(TbCases))
    annual_prod <- readRDS(system.file(paste0("AnnualProductivity", Ages, ".rds"), package = "TubercuCost"))*convFact
    annual_expend <- readRDS(system.file(paste0("NonHealthExpenditure", Ages, ".rds"), package = "TubercuCost")) # need to convert to 2020 dollars in csv and formatting
    lifetime_prod <- readRDS(system.file(paste0("LifetimeProductivityCosts", Ages, ".rds"), package = "TubercuCost"))*convFact
    lifetime_expend <- readRDS(system.file(paste0("LifetimeNonHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
} else if (discount == 0.03){
    discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[1:nrow(TbCases),2]
    annual_prod <- readRDS(system.file(paste0("DiscountedAnnualProductivityCosts", Ages, ".rds"), package = "TubercuCost"))*convFact
    annual_expend <- readRDS(system.file(paste0("DiscountedNonHealthExpenditure", Ages, ".rds"), package = "TubercuCost")) # need to convert to 2020 dollars in csv and formatting
    lifetime_prod <- readRDS(system.file(paste0("DiscountedLifetimeProductivityCosts", Ages, ".rds"), package = "TubercuCost"))*convFact
    lifetime_expend <- readRDS(system.file(paste0("DiscountedLifetimeNonHealthExpenditure", Ages, ".rds"), package = "TubercuCost"))
} else {
    print("Current discount level not supported.")
    break()
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
    # TltbiProdCost[i,j] <- (LtbiTxInits[i,j] *(TLTBI_clinic+TLTBI_ae*P_TLTBI_tox)*discountVec[i])

    #PRODUCTIVITY COSTS DUE TO TB DISEASE
    #age specific TB treatment initiations*((probability of TB hospitalization*duration of TB hospitalization)+
    #duration of outpatient losses)*annual productivity estimates +
    #age specific TB deaths*lifetime productivity estimates
    TbProdCost[i,j] <- (TbCases[i,j]*(annual_prod[i,j] - annual_expend[i,j]))*((DUR_TB_prTX + P_TB_hosp*DUR_TB_TX_hosp)+DUR_TB_TX_outpatient) * discountVec[i]
    TbMortCost[i,j] <- (TbDeaths[i,j]*(lifetime_prod[i,j]-lifetime_expend[i,j]))
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
