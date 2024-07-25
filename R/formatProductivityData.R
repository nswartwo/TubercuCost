format_exp_data <- function(prodConvFact = 1.28,
                            USDyear = 2022,
                            StartYear = 2024){

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### read in the discount file
  discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[,2]

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### read in life expectancy data
  lifeExpectancySingleAge <- readRDS("~/TubercuCost/inst/LifeExpectancySingleYearAge.rds")
  discLifeExpectancySingleAge <- readRDS("~/TubercuCost/inst/DiscountedLifeExpectancySingleYearAge.rds")

  lifeExpectancyAgeGroups <- readRDS("~/TubercuCost/inst/LifeExpectancyAgeGroups.rds")
  discLifeExpectancyAgeGroups <- readRDS("~/TubercuCost/inst/DiscountedLifeExpectancyAgeGroups.rds")

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### ANNUAL PRODUCTIVITY
  ##### Annual productivity data are in 2016 USD from
  ##### Grosse et al, Estimated annual and lifetime labor productivity
  ##### in the United States, 2016: implications for economic evaluations
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ##### read in the annual productivity data in 2016 USD
  annualProductivity0 <- read.csv("~/TubercuCost/inst/extdata/AnnualProductivityDataAgeGrps2016.csv",
                                      check.names = FALSE)[,1:2]
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  #### Annual productivity
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ##### Age groups #####
  annualProductivityAgeGroups <- lifeExpectancyAgeGroups[c(-1,-2,-3,-4),]
  for (i in 1:nrow(annualProductivityAgeGroups)){
    if (i == 1){
    annualProductivityAgeGroups[i,-1] <- annualProductivity0[,2] * prodConvFact
    } else {
      annualProductivityAgeGroups[i,-1] <- annualProductivityAgeGroups[i-1,-1] * 1.005
  }}
  saveRDS(annualProductivityAgeGroups,
          paste0("~/TubercuCost/inst/", USDyear, "/AnnualProductivityAgeGroups.rds"), version=2)

  discountedAnnualProductivityAgeGroups <- annualProductivityAgeGroups
  for (i in 2:ncol(annualProductivityAgeGroups)){
    discountedAnnualProductivityAgeGroups[,i] <- annualProductivityAgeGroups[,i] * discountVec[1:nrow(annualProductivityAgeGroups)]
  }

  saveRDS(discountedAnnualProductivityAgeGroups,
          paste0("~/TubercuCost/inst/", USDyear, "/DiscountedAnnualProductivityCostsAgeGroups.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### Single year of age #####
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ### Undiscounted annual productivity for single year of age
  pc_y <- annualProductivity0[-c(1,2),2]
  pc_x  <- c(20,30,40,50,60,70,80,90,100)
  est <- spline(pc_x,log(pc_y),xmin=16,xmax=100,n=(101-16))

  AnnualProductivitySingleAge <- matrix(c(rep(0,length(0:15)), exp(est$y)), ncol = length(c(rep(0,length(0:15)), est$y)), nrow = length(StartYear:2100), byrow = TRUE)
  colnames(AnnualProductivitySingleAge) <-  c(0:15, est$x)
  rownames(AnnualProductivitySingleAge) <- StartYear:2100
  saveRDS(AnnualProductivitySingleAge,
          paste0("~/TubercuCost/inst/", USDyear, "/AnnualProductivitySingleAgeSingleYearAge.rds"), version=2)

  ### Discounted annual productivity for single year of age
  DiscountAnnualProductivitySingleAge <- matrix(0, ncol = ncol(AnnualProductivitySingleAge), nrow = length(StartYear:2100))
  colnames(DiscountAnnualProductivitySingleAge) <- colnames(AnnualProductivitySingleAge)
  rownames(DiscountAnnualProductivitySingleAge) <- StartYear:2100
  for (i in 1:nrow(DiscountAnnualProductivitySingleAge)){
    for (j in 1:ncol(DiscountAnnualProductivitySingleAge)){
      DiscountAnnualProductivitySingleAge[i,j] <- AnnualProductivitySingleAge[i,j] * discountVec[i]
    } }

  saveRDS(DiscountAnnualProductivitySingleAge,
          paste0("~/TubercuCost/inst/", USDyear, "/DiscountedAnnualProductivitySingleAgeCostsSingleYearAge.rds"), version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ##### LIFETIME PRODUCTIVITY
  ##### Lifetime productivity data are in 2016 USD from
  ##### Grosse et al, Estimated annual and lifetime labor productivity
  ##### in the United States, 2016: implications for economic evaluations
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ##### Single year of age
  lifetimeProductivity0 <- read.csv("~/TubercuCost/inst/extdata/lifetimeProductivityEstimatesGross0perc.csv",
                                    check.names = FALSE)[,1:2]
  discountlifetimeProductivity0 <- read.csv("~/TubercuCost/inst/extdata/lifetimeProductivityEstimatesGross3perc.csv",
                                    check.names = FALSE)[,1:2]

  ### Undiscounted lifetime productivity for single year of age
  LifetimeProductivitySingleAge <- AnnualProductivitySingleAge
  for(i in 1:nrow(LifetimeProductivitySingleAge)){
    if (i == 1){
    LifetimeProductivitySingleAge[i,] <- lifetimeProductivity0[,2]  * prodConvFact
    } else {
    LifetimeProductivitySingleAge[i,] <- LifetimeProductivitySingleAge[i-1,] * 1.005
  }
}
  saveRDS(LifetimeProductivitySingleAge,
          paste0("~/TubercuCost/inst/", USDyear, "/LifetimeProductivityCostsSingleYearAge.rds"), version=2)

  ### Discounted lifetime productivity for single year of age
  DiscountLifetimeProductivitySingleAge <- DiscountAnnualProductivitySingleAge
  for(i in 1:nrow(DiscountLifetimeProductivitySingleAge)){
    if (i == 1){
      DiscountLifetimeProductivitySingleAge[i,] <- discountlifetimeProductivity0[,2] * prodConvFact
    } else {
      DiscountLifetimeProductivitySingleAge[i,] <- DiscountLifetimeProductivitySingleAge[i-1,] * 1.005
    }
  }
  saveRDS(DiscountLifetimeProductivitySingleAge,
          paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeProductivityCostsSingleYearAge.rds"), version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ##### AGE GROUPS
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### Create undiscounted lifetime productivity estimates for age group estimates
  LifetimeProductivityAgeGroup <- annualProductivityAgeGroups[,-1]
  rownames(LifetimeProductivityAgeGroup) <- StartYear:2100
  for(i in 1:nrow(LifetimeProductivityAgeGroup)){
    LifetimeProductivityAgeGroup[i,1] <- mean(LifetimeProductivitySingleAge[i,1:5])
    LifetimeProductivityAgeGroup[i,2] <- mean(LifetimeProductivitySingleAge[i,6:15])
    LifetimeProductivityAgeGroup[i,3] <- mean(LifetimeProductivitySingleAge[i,16:25])
    LifetimeProductivityAgeGroup[i,4] <- mean(LifetimeProductivitySingleAge[i,26:35])
    LifetimeProductivityAgeGroup[i,5] <- mean(LifetimeProductivitySingleAge[i,36:45])
    LifetimeProductivityAgeGroup[i,6] <- mean(LifetimeProductivitySingleAge[i,46:55])
    LifetimeProductivityAgeGroup[i,7] <- mean(LifetimeProductivitySingleAge[i,56:65])
    LifetimeProductivityAgeGroup[i,8] <- mean(LifetimeProductivitySingleAge[i,66:75])
    LifetimeProductivityAgeGroup[i,9] <- mean(LifetimeProductivitySingleAge[i,76:85])
    LifetimeProductivityAgeGroup[i,10] <- mean(LifetimeProductivitySingleAge[i,86:95])
    LifetimeProductivityAgeGroup[i,11] <- mean(LifetimeProductivitySingleAge[i,96:101])
  }

  saveRDS(LifetimeProductivityAgeGroup,
          paste0("~/TubercuCost/inst/", USDyear, "/LifetimeProductivityCostsAgeGroups.rds"), version=2)

  ### Create discounted lifetime productivity estimates for age group estimates
  discountLifetimeProductivityAgeGroup <- annualProductivityAgeGroups[,-1]
  rownames(discountLifetimeProductivityAgeGroup) <- StartYear:2100
  for(i in 1:nrow(discountLifetimeProductivityAgeGroup)){
    discountLifetimeProductivityAgeGroup[i,1] <- mean(DiscountLifetimeProductivitySingleAge[i,1:5])
    discountLifetimeProductivityAgeGroup[i,2] <- mean(DiscountLifetimeProductivitySingleAge[i,6:15])
    discountLifetimeProductivityAgeGroup[i,3] <- mean(DiscountLifetimeProductivitySingleAge[i,16:25])
    discountLifetimeProductivityAgeGroup[i,4] <- mean(DiscountLifetimeProductivitySingleAge[i,26:35])
    discountLifetimeProductivityAgeGroup[i,5] <- mean(DiscountLifetimeProductivitySingleAge[i,36:45])
    discountLifetimeProductivityAgeGroup[i,6] <- mean(DiscountLifetimeProductivitySingleAge[i,46:55])
    discountLifetimeProductivityAgeGroup[i,7] <- mean(DiscountLifetimeProductivitySingleAge[i,56:65])
    discountLifetimeProductivityAgeGroup[i,8] <- mean(DiscountLifetimeProductivitySingleAge[i,66:75])
    discountLifetimeProductivityAgeGroup[i,9] <- mean(DiscountLifetimeProductivitySingleAge[i,76:85])
    discountLifetimeProductivityAgeGroup[i,10] <- mean(DiscountLifetimeProductivitySingleAge[i,86:95])
    discountLifetimeProductivityAgeGroup[i,11] <- mean(DiscountLifetimeProductivitySingleAge[i,96:101])
  }
  saveRDS(discountLifetimeProductivityAgeGroup,
          paste0("~/TubercuCost/inst/", USDyear, "/DiscountedLifetimeProductivityCostsAgeGroups.rds"), version=2)

  }
