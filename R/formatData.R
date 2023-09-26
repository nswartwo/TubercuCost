format_data<-function(){

  ### read in the discount file
  discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[,2]

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###
  ### Create the undiscounted annual productivity estimates
  pc_y <- c(22989, 73742, 99206, 95024, 77509, 43895, 18259)
  ### create single year of age estimates using a spline
  pc_x  <- c(20,30,40,50,60,70,80)
  est <- spline(pc_x,log(pc_y),xmin=16,xmax=100,n=(101-16))

  AnnualProductivity <- matrix(c(rep(0,length(0:15)), exp(est$y)), ncol = length(c(rep(0,length(0:15)), est$y)), nrow = length(discountVec), byrow = TRUE)
  colnames(AnnualProductivity) <-  c(0:15, est$x)
  rownames(AnnualProductivity) <- 2020:2100
  saveRDS(AnnualProductivity, "~/TubercuCost/inst/AnnualProductivitySingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted annual productivity estimates for single year of age estimates
  DiscountAnnualProductivity <- matrix(0, ncol = ncol(AnnualProductivity), nrow = length(discountVec))
  colnames(DiscountAnnualProductivity) <- colnames(AnnualProductivity)
  rownames(DiscountAnnualProductivity) <- 2020:2100
  for (i in 1:nrow(DiscountAnnualProductivity)){
    for (j in 1:ncol(DiscountAnnualProductivity)){
      DiscountAnnualProductivity[i,j] <- AnnualProductivity[i,j] * discountVec[i]
    } }

  saveRDS(DiscountAnnualProductivity, "~/TubercuCost/inst/DiscountedAnnualProductivityCostsSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create annual productivity estimates for age group estimates
  LifetimeProductivity <- read.csv("~/TubercuCost/inst/extdata/DiscountedLifetimeProductivityCosts.csv")
  saveRDS(LifetimeProductivity, "~/TubercuCost/inst/DiscountedLifetimeProductivityCostsAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted annual productivity estimates for age group estimates
  DiscountAnnualProductivityGrp <- read.csv("~/TubercuCost/inst/extdata/DiscountedAnnualProductivityCosts.csv")
  saveRDS(DiscountAnnualProductivityGrp, "~/TubercuCost/inst/DiscountedAnnualProductivityCostsAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ###
  ### Create lifetime productivity estimates for single year of age estimates
  LifetimeProductivity0p <- read.csv("~/TubercuCost/inst/extdata/lifetimeProductivityEstimates0perc.csv")
  LifetimeProductivity <- matrix(LifetimeProductivity0p[,2], ncol = length(LifetimeProductivity0p[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(LifetimeProductivity) <- LifetimeProductivity0p[,1]
  rownames(LifetimeProductivity) <- 2020:2100

  saveRDS(LifetimeProductivity, "~/TubercuCost/inst/LifetimeProductivityCostsSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted lifetime productivity estimates for single year of age estimates
  LifetimeProductivity3p <- read.csv("~/TubercuCost/inst/extdata/lifetimeProductivityEstimates3perc.csv")

  DiscountLifetimeProductivity <- matrix(0, ncol = nrow(LifetimeProductivity3p), nrow = length(discountVec))
  colnames(DiscountLifetimeProductivity) <- LifetimeProductivity3p[,1]
  rownames(DiscountLifetimeProductivity) <- 2020:2100
  for (i in 1:nrow(DiscountLifetimeProductivity)){
    for (j in 1:ncol(DiscountLifetimeProductivity)){
      DiscountLifetimeProductivity[i,j] <- LifetimeProductivity3p[j,2] * discountVec[i]
    } }

  saveRDS(DiscountLifetimeProductivity, "~/TubercuCost/inst/DiscountedLifetimeProductivityCostsSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ###
  ### Create discounted lifetime productivity estimates for age group estimates
  DiscountLifeExpectancy <- read.csv("~/TubercuCost/inst/extdata/DiscountedLifeExpectancies.csv")
  saveRDS(DiscountLifeExpectancy, "~/TubercuCost/inst/DiscountedLifeExpectancyAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create life expectancy estimates for single year of age estimates
  LifeExpectancy<-c(78.7,72.0,62.2,52.7,43.4,34.2,25.7,18.0,11.3,6.19,3.18)
  names(LifeExpectancy) <- colnames(DiscountLifeExpectancy)[-1]
  saveRDS(LifeExpectancy, "~/TubercuCost/inst/LifeExpectancyAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create life expectancy estimates for single year of age estimates

  fullLifeExpectancy0 <- read.csv("~/TubercuCost/inst/extdata/lifeExpectancies.csv")[1:101,]

  fullLifeExpectancy <- matrix(fullLifeExpectancy0[,2], ncol = length(fullLifeExpectancy0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullLifeExpectancy) <- fullLifeExpectancy0[,1]
  rownames(fullLifeExpectancy) <- 2020:2100

  saveRDS(fullLifeExpectancy, "~/TubercuCost/inst/LifeExpectancySingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted life expectancy estimates for single year of age estimates

  DiscountLifeExpectancy <- matrix(0, nrow = nrow(fullLifeExpectancy), ncol = ncol(fullLifeExpectancy))
  colnames(DiscountLifeExpectancy) <- fullLifeExpectancy0[,1]
  rownames(DiscountLifeExpectancy) <- 2020:2100
  for (i in 1:nrow(DiscountLifeExpectancy)){
    for (j in 1:ncol(DiscountLifeExpectancy)){
      DiscountLifeExpectancy[i,j] <- fullLifeExpectancy[i,j] * discountVec[i]
    } }

  saveRDS(DiscountLifeExpectancy, "~/TubercuCost/inst/DiscountedLifeExpectancySingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### format expenditures; we will need to create two data files:
  ### non-healthcare expenditures for the societal persepective
  ### healthcare expenditures for both the societal and healthcare perspectives

  ### read in the csv
  expenditure0 <- read.csv("~/TubercuCost/inst/extdata/ExpenditureDataRaw.csv")

  ### Create undiscounted health expenditure estimates for single year of age estimates
  fullHealthExpenditure <- matrix(expenditure0[,3], ncol = length(expenditure0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullHealthExpenditure) <- expenditure0[,1]
  rownames(fullHealthExpenditure) <- 2020:2100

  saveRDS(fullHealthExpenditure, "~/TubercuCost/inst/HealthExpenditureSingleYearAge.rds", version=2)


  ### ### ### ### ### ### ### ###
  ### Create discounted health expenditure estimates for single year of age estimates

  DiscountHealthExpenditure <- matrix(0, nrow = nrow(fullHealthExpenditure), ncol = ncol(fullHealthExpenditure))
  colnames(DiscountHealthExpenditure) <- expenditure0[,1]
  rownames(DiscountHealthExpenditure) <- 2020:2100
  for (i in 1:nrow(DiscountHealthExpenditure)){
    for (j in 1:ncol(DiscountHealthExpenditure)){
      DiscountHealthExpenditure[i,j] <- fullHealthExpenditure[i,j] * discountVec[i]
    } }

  saveRDS(DiscountHealthExpenditure, "~/TubercuCost/inst/DiscountedHealthExpenditureSingleYearAge.rds", version=2)


  ### Create undiscounted health expenditure estimates for single year of age estimates - TB mortality
  fullHealthExpenditureLifetime <- matrix(expenditure0[,3], ncol = length(expenditure0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullHealthExpenditureLifetime) <- expenditure0[,1]
  rownames(fullHealthExpenditureLifetime) <- 2020:2100

  for (i in 1:nrow(fullHealthExpenditureLifetime)){
    for(j in 1:ncol(fullHealthExpenditureLifetime)){
      # years of lost life expectancy
      yearsLost <- fullLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(fullHealthExpenditureLifetime))
      lastCol <- min(j + yearsLost, ncol(fullHealthExpenditureLifetime))
      fullHealthExpenditureLifetime[i,j] <- sum(diag(fullHealthExpenditureLifetime[i:lastRow,j:lastCol]))
    }
  }

  saveRDS(fullHealthExpenditureLifetime, "~/TubercuCost/inst/LifetimeHealthExpenditureSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted Health expenditure estimates for single year of age estimates - TB mortality

  DiscountHealthExpenditureLifetime <- matrix(0, nrow = nrow(fullHealthExpenditure), ncol = ncol(fullHealthExpenditure))
  colnames(DiscountHealthExpenditureLifetime) <- expenditure0[,1]
  rownames(DiscountHealthExpenditureLifetime) <- 2020:2100
  for (i in 1:nrow(DiscountHealthExpenditureLifetime)){
    for (j in 1:ncol(DiscountHealthExpenditureLifetime)){
      # years of lost life expectancy
      yearsLost <- DiscountLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(DiscountHealthExpenditureLifetime))
      lastCol <- min(j + yearsLost, ncol(DiscountHealthExpenditureLifetime))

      DiscountHealthExpenditureLifetime[i,j] <- sum(diag(DiscountHealthExpenditureLifetime[i:lastRow,j:lastCol]))
    } }

  saveRDS(DiscountHealthExpenditureLifetime, "~/TubercuCost/inst/DiscountedLifetimeHealthExpenditureSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### CREATE NON HEALTH CARE EXPENDITURES
  ### 1. ANNUAL NON HEALTH CARE EXPENDITURE UNDISCOUNTED
  ### 2. ANNUAL NON HEALTH CARE EXPENDITURE DISCOUNTED 3% ANNUALLY
  ### 3. LIFETIME NON HEALTH CARE EXPENDITURE UNDISCOUNTED
  ### 4. LIFETIME NON HEALTH CARE EXPENDITURE DISCOUNTED 3% ANNUALLY

  ### Create undiscounted non-health expenditure estimates for single year of age estimates
  fullNonHealthExpenditure <- matrix(expenditure0[,4], ncol = length(expenditure0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullNonHealthExpenditure) <- expenditure0[,1]
  rownames(fullNonHealthExpenditure) <- 2020:2100

  saveRDS(fullNonHealthExpenditure, "~/TubercuCost/inst/NonHealthExpenditureSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted non-Health expenditure estimates for single year of age estimates

  DiscountNonHealthExpenditure <- matrix(0, nrow = nrow(fullNonHealthExpenditure), ncol = ncol(fullNonHealthExpenditure))
  colnames(DiscountNonHealthExpenditure) <- expenditure0[,1]
  rownames(DiscountNonHealthExpenditure) <- 2020:2100
  for (i in 1:nrow(DiscountNonHealthExpenditure)){
    for (j in 1:ncol(DiscountNonHealthExpenditure)){
      DiscountNonHealthExpenditure[i,j] <- fullNonHealthExpenditure[i,j] * discountVec[i]
    } }

  saveRDS(DiscountNonHealthExpenditure, "~/TubercuCost/inst/DiscountedNonHealthExpenditureSingleYearAge.rds", version=2)


  ### Create undiscounted non health expenditure estimates for single year of age estimates - TB mortality
  fullNonHealthExpenditureLifetime <- matrix(0, ncol = length(expenditure0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullNonHealthExpenditureLifetime) <- expenditure0[,1] # ages
  rownames(fullNonHealthExpenditureLifetime) <- 2020:2100 # years

  for (i in 1:nrow(fullNonHealthExpenditureLifetime)){
    for(j in 1:ncol(fullNonHealthExpenditureLifetime)){
      # years of lost life expectancy
      yearsLost <- fullLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(fullNonHealthExpenditureLifetime))
      lastCol <- min(j + yearsLost, ncol(fullNonHealthExpenditureLifetime))
      fullNonHealthExpenditureLifetime[i,j] <- sum(diag(fullNonHealthExpenditure[i:lastRow,j:lastCol]))
    }
  }

  saveRDS(fullNonHealthExpenditureLifetime, "~/TubercuCost/inst/LifetimeNonHealthExpenditureSingleYearAge.rds", version=2)


  ### ### ### ### ### ### ### ###
  ### Create discounted non-Health expenditure estimates for single year of age estimates - TB mortality

  DiscountNonHealthExpenditureLifetime <- matrix(0, nrow = nrow(fullNonHealthExpenditureLifetime), ncol = ncol(fullNonHealthExpenditureLifetime))
  colnames(DiscountNonHealthExpenditureLifetime) <- expenditure0[,1]
  rownames(DiscountNonHealthExpenditureLifetime) <- 2020:2100
  for (i in 1:nrow(DiscountNonHealthExpenditureLifetime)){
    for (j in 1:ncol(DiscountNonHealthExpenditureLifetime)){
      # years of lost life expectancy
      yearsLost <- DiscountLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(fullNonHealthExpenditureLifetime))
      lastCol <- min(j + yearsLost, ncol(fullNonHealthExpenditureLifetime))

      DiscountNonHealthExpenditureLifetime[i,j] <- sum(diag(DiscountNonHealthExpenditure[i:lastRow,j:lastCol]))
    } }

  saveRDS(DiscountNonHealthExpenditureLifetime, "~/TubercuCost/inst/DiscountedLifetimeNonHealthExpenditureSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
}


