format_data<-function(){

  ### read in the discount file
  discountVec <- read.csv("~/TubercuCost/inst/extdata/DiscountingThreePercentScalars.csv")[,2]

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###          LIFE EXPECTANCIES          ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ###
  ### Create discounted lifetime productivity estimates for age group estimates
  DiscountLifeExpectancyAgeGroups <- read.csv("~/TubercuCost/inst/extdata/DiscountedLifeExpectancies.csv")
  ageGroupsVector <- gsub("\\.", "-", gsub("X", "", colnames(DiscountLifeExpectancyAgeGroups)[-1]))
  colnames(DiscountLifeExpectancyAgeGroups)[2:12] <- ageGroupsVector
  # saveRDS(DiscountLifeExpectancyAgeGroups, "~/TubercuCost/inst/DiscountedLifeExpectancyAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create life ExpectancyAgeGroups estimates for age group estimates
  LifeExpectancyAgeGroups<-DiscountLifeExpectancyAgeGroups
  LifeExpectancyAgeGroups[, 2:12] <- matrix(c(78.7,72.0,62.2,52.7,43.4,34.2,25.7,18.0,11.3,6.19,3.18),
                                   ncol = 11, nrow = nrow(DiscountLifeExpectancyAgeGroups), byrow = TRUE)
  # saveRDS(LifeExpectancyAgeGroups, "~/TubercuCost/inst/LifeExpectancyAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create life expectancy estimates for single year of age estimates
  fullLifeExpectancy0 <- read.csv("~/TubercuCost/inst/extdata/lifeExpectancies.csv")[1:101,]

  fullLifeExpectancy <- matrix(fullLifeExpectancy0[,2], ncol = length(fullLifeExpectancy0[,2]), nrow = length(2024:2150), byrow = TRUE)
  colnames(fullLifeExpectancy) <- fullLifeExpectancy0[,1]
  rownames(fullLifeExpectancy) <- 2024:2150

  # saveRDS(fullLifeExpectancy, "~/TubercuCost/inst/LifeExpectancySingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted life expectancy estimates for single year of age estimates

  DiscountLifeExpectancy <- matrix(0, nrow = nrow(fullLifeExpectancy), ncol = ncol(fullLifeExpectancy))
  colnames(DiscountLifeExpectancy) <- fullLifeExpectancy0[,1]
  rownames(DiscountLifeExpectancy) <- 2024:2150
  for (i in 1:nrow(DiscountLifeExpectancy)){
    for (j in 1:ncol(DiscountLifeExpectancy)){
      DiscountLifeExpectancy[i,j] <- fullLifeExpectancy[i,j] * discountVec[i]
    } }

  # saveRDS(DiscountLifeExpectancy, "~/TubercuCost/inst/DiscountedLifeExpectancySingleYearAge.rds", version=2)


  ## ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###             PRODUCTIVITY            ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###


  ### ### ### ### ### ### ### ###          ANNUAL PRODUCTIVITY        ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ###
  ### Create discounted annual productivity estimates for age group estimates
  DiscountAnnualProductivityAgeGroups <- read.csv("~/TubercuCost/inst/extdata/DiscountedAnnualProductivityCosts.csv")
  # saveRDS(DiscountAnnualProductivityAgeGroups, "~/TubercuCost/inst/DiscountedAnnualProductivityCostsAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create undiscounted annual productivity estimates for age group estimates
  annualProductivityAgeGroups0 <- read.csv("~/TubercuCost/inst/extdata/AnnualProductivityCosts.csv")## From Grosse et al 2016
  ### fit the last two age groups using a spline
  pc_y <- annualProductivityAgeGroups0[1,-c(1,2,3)]
  pc_x  <- c(20,30,40,50,60,70,80)
  est <- spline(pc_x,log(pc_y),xmin=20,xmax=100,n=9)

  annualProductivityAgeGroups <- matrix(c(annualProductivityAgeGroups0[1:2], exp(est$y)),
                                        ncol = length(ageGroupsVector),
                                        nrow = length(discountVec),
                                        byrow = TRUE)

  colnames(annualProductivityAgeGroups) <-  ageGroupsVector
  rownames(annualProductivityAgeGroups) <- 2020:2100

  # saveRDS(annualProductivityAgeGroups, "~/TubercuCost/inst/AnnualProductivityAgeGroups.rds", version=2)

  ## ### ### ### ### ### ### ###
  ### Create undiscounted annual productivity estimates for single year of age estimates
  ### create single year of age estimates using a spline
  est <- spline(pc_x,log(pc_y),xmin=16,xmax=100,n=(101-16))

  AnnualProductivitySingleAge <- matrix(c(rep(0,length(0:15)), exp(est$y)), ncol = length(c(rep(0,length(0:15)), est$y)), nrow = length(discountVec), byrow = TRUE)
  colnames(AnnualProductivitySingleAge) <-  c(0:15, est$x)
  rownames(AnnualProductivitySingleAge) <- 2020:2100
  # saveRDS(AnnualProductivitySingleAge, "~/TubercuCost/inst/AnnualProductivitySingleAgeSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted annual productivity estimates for single year of age estimates
  DiscountAnnualProductivitySingleAge <- matrix(0, ncol = ncol(AnnualProductivitySingleAge), nrow = length(discountVec))
  colnames(DiscountAnnualProductivitySingleAge) <- colnames(AnnualProductivitySingleAge)
  rownames(DiscountAnnualProductivitySingleAge) <- 2020:2100
  for (i in 1:nrow(DiscountAnnualProductivitySingleAge)){
    for (j in 1:ncol(DiscountAnnualProductivitySingleAge)){
      DiscountAnnualProductivitySingleAge[i,j] <- AnnualProductivitySingleAge[i,j] * discountVec[i]
    } }

  # saveRDS(DiscountAnnualProductivitySingleAge, "~/TubercuCost/inst/DiscountedAnnualProductivitySingleAgeCostsSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###         LIFETIME PRODUCTIVITY       ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ###
  ### Create undiscounted annual productivity estimates for age group estimates
  ### Leverage shape from discounted dataframe
  LifetimeProductivityAgeGroups <- annualProductivityAgeGroups
  LifetimeProductivityAgeGroups[,1:11] <- matrix((c(5023618.16, 4862594.24, 4580218.94,
                                           3965997.71, 2971750.03, 1919650.66,
                                           1037270.45, 448019.48, 159472.84,
                                           30907.27, 6276.53) *.97^2),
                                           ncol = 11, nrow = nrow(LifetimeProductivityAgeGroups),
                                           byrow = TRUE)

  # saveRDS(LifetimeProductivityAgeGroups, "~/TubercuCost/inst/LifetimeProductivityCostsAgeGroups.rds", version=2)


  ## ### ### ### ### ### ### ###
  ### Create discounted lifetime productivity estimates for age group estimates
  # DiscountLifetimeProductivityAgeGroups <- read.csv("~/TubercuCost/inst/extdata/DiscountedLifetimeProductivityCosts.csv")
  DiscountLifetimeProductivityAgeGroups <- LifetimeProductivityAgeGroups
  for (i in 1:nrow(DiscountLifetimeProductivityAgeGroups)){
    for (j in 1:ncol(DiscountLifetimeProductivityAgeGroups)){
      DiscountLifetimeProductivityAgeGroups[i,j] <- LifetimeProductivityAgeGroups[i,j] * discountVec[i]
    } }
  # saveRDS(DiscountLifetimeProductivityAgeGroups, "~/TubercuCost/inst/DiscountedLifetimeProductivityCostsAgeGroups.rds", version=2)


  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### ### ### ### ### ### ### ###
  ### Create lifetime productivity estimates for single year of age estimates
  LifetimeProductivity0p <- read.csv("~/TubercuCost/inst/extdata/lifetimeProductivityEstimates0perc.csv")
  LifetimeProductivity <- matrix(LifetimeProductivity0p[,2], ncol = length(LifetimeProductivity0p[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(LifetimeProductivity) <- LifetimeProductivity0p[,1]
  rownames(LifetimeProductivity) <- 2020:2100

  # saveRDS(LifetimeProductivity, "~/TubercuCost/inst/LifetimeProductivityCostsSingleYearAge.rds", version=2)

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

  # saveRDS(DiscountLifetimeProductivity, "~/TubercuCost/inst/DiscountedLifetimeProductivityCostsSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###             EXPENDITURES            ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###

  ### format expenditures; we will need to create two data files:
  ### non-healthcare expenditures for the societal persepective
  ### healthcare expenditures for both the societal and healthcare perspectives

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ##  SINGLE YEAR OF AGE  ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### read in the csv
  expenditureSingleAge0 <- read.csv("~/TubercuCost/inst/extdata/ExpenditureDataRaw.csv")

  ### Create undiscounted health expenditure estimates for single year of age estimates
  fullHealthExpenditureSingleAge <- matrix(expenditureSingleAge0[,3], ncol = length(expenditureSingleAge0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullHealthExpenditureSingleAge) <- expenditureSingleAge0[,1]
  rownames(fullHealthExpenditureSingleAge) <- 2020:2100

  # saveRDS(fullHealthExpenditureSingleAge, "~/TubercuCost/inst/HealthExpenditureSingleYearAge.rds", version=2)


  ### Create discounted health expenditure estimates for single year of age estimates
  DiscountHealthExpenditureSingleAge <- matrix(0, nrow = nrow(fullHealthExpenditureSingleAge), ncol = ncol(fullHealthExpenditureSingleAge))
  colnames(DiscountHealthExpenditureSingleAge) <- expenditureSingleAge0[,1]
  rownames(DiscountHealthExpenditureSingleAge) <- 2020:2100
  for (i in 1:nrow(DiscountHealthExpenditureSingleAge)){
    for (j in 1:ncol(DiscountHealthExpenditureSingleAge)){
      DiscountHealthExpenditureSingleAge[i,j] <- fullHealthExpenditureSingleAge[i,j] * discountVec[i]
    } }

  # saveRDS(DiscountHealthExpenditureSingleAge, "~/TubercuCost/inst/DiscountedHealthExpenditureSingleYearAge.rds", version=2)


  ### Create undiscounted health expenditure estimates for single year of age estimates - TB mortality
  HealthExpenditureLifetimeSingleAge <- matrix(expenditureSingleAge0[,3], ncol = length(expenditureSingleAge0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(HealthExpenditureLifetimeSingleAge) <- expenditureSingleAge0[,1]
  rownames(HealthExpenditureLifetimeSingleAge) <- 2020:2100

  for (i in 1:nrow(HealthExpenditureLifetimeSingleAge)){
    for(j in 1:ncol(HealthExpenditureLifetimeSingleAge)){
      # years of lost life expectancy
      yearsLost <- fullLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(HealthExpenditureLifetimeSingleAge))
      lastCol <- min(j + yearsLost, ncol(HealthExpenditureLifetimeSingleAge))
      HealthExpenditureLifetimeSingleAge[i,j] <- sum(diag(fullHealthExpenditureSingleAge[i:lastRow,j:lastCol]))
    }
  }

  # saveRDS(HealthExpenditureLifetimeSingleAge, "~/TubercuCost/inst/LifetimeHealthExpenditureSingleYearAge.rds", version=2)

  ## ### ### ### ### ### ### ###
  ### Create discounted Health expenditure estimates for single year of age estimates - TB mortality

  DiscountHealthExpenditureLifetimeSingleAge <- matrix(0, nrow = nrow(fullHealthExpenditureSingleAge), ncol = ncol(fullHealthExpenditureSingleAge))
  colnames(DiscountHealthExpenditureLifetimeSingleAge) <- expenditureSingleAge0[,1]
  rownames(DiscountHealthExpenditureLifetimeSingleAge) <- 2020:2100
  for (i in 1:nrow(DiscountHealthExpenditureLifetimeSingleAge)){
    for (j in 1:ncol(DiscountHealthExpenditureLifetimeSingleAge)){
      # years of lost life expectancy
      yearsLost <- DiscountLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(DiscountHealthExpenditureLifetimeSingleAge))
      lastCol <- min(j + yearsLost, ncol(DiscountHealthExpenditureLifetimeSingleAge))

      DiscountHealthExpenditureLifetimeSingleAge[i,j] <- sum(diag(DiscountHealthExpenditureSingleAge[i:lastRow,j:lastCol]))
    } }

  # saveRDS(DiscountHealthExpenditureLifetimeSingleAge, "~/TubercuCost/inst/DiscountedLifetimeHealthExpenditureSingleYearAge.rds", version=2)

  ## ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### CREATE NON HEALTH CARE EXPENDITURES
  ### 1. ANNUAL NON HEALTH CARE EXPENDITURE UNDISCOUNTED
  ### 2. ANNUAL NON HEALTH CARE EXPENDITURE DISCOUNTED 3% ANNUALLY
  ### 3. LIFETIME NON HEALTH CARE EXPENDITURE UNDISCOUNTED
  ### 4. LIFETIME NON HEALTH CARE EXPENDITURE DISCOUNTED 3% ANNUALLY

  ### Create undiscounted non-health expenditure estimates for single year of age estimates
  fullnonHealthExpenditureSingleAge <- matrix(expenditureSingleAge0[,4], ncol = length(expenditureSingleAge0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullnonHealthExpenditureSingleAge) <- expenditureSingleAge0[,1]
  rownames(fullnonHealthExpenditureSingleAge) <- 2020:2100

  # saveRDS(fullnonHealthExpenditureSingleAge, "~/TubercuCost/inst/nonHealthExpenditureSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted non-Health expenditure estimates for single year of age estimates

  DiscountnonHealthExpenditureSingleAge <- matrix(0, nrow = nrow(fullnonHealthExpenditureSingleAge), ncol = ncol(fullnonHealthExpenditureSingleAge))
  colnames(DiscountnonHealthExpenditureSingleAge) <- expenditureSingleAge0[,1]
  rownames(DiscountnonHealthExpenditureSingleAge) <- 2020:2100
  for (i in 1:nrow(DiscountnonHealthExpenditureSingleAge)){
    for (j in 1:ncol(DiscountnonHealthExpenditureSingleAge)){
      DiscountnonHealthExpenditureSingleAge[i,j] <- fullnonHealthExpenditureSingleAge[i,j] * discountVec[i]
    } }

  # saveRDS(DiscountnonHealthExpenditureSingleAge, "~/TubercuCost/inst/DiscountednonHealthExpenditureeSingleYearAge.rds", version=2)


  ### Create undiscounted non health expenditure estimates for single year of age estimates - TB mortality
  fullnonHealthExpenditureLifetimeSingleAge <- matrix(0, ncol = length(expenditureSingleAge0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullnonHealthExpenditureLifetimeSingleAge) <- expenditureSingleAge0[,1] # ages
  rownames(fullnonHealthExpenditureLifetimeSingleAge) <- 2020:2100 # years

  for (i in 1:nrow(fullnonHealthExpenditureLifetimeSingleAge)){
    for(j in 1:ncol(fullnonHealthExpenditureLifetimeSingleAge)){
      # years of lost life expectancy
      yearsLost <- fullLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(fullnonHealthExpenditureLifetimeSingleAge))
      lastCol <- min(j + yearsLost, ncol(fullnonHealthExpenditureLifetimeSingleAge))
      fullnonHealthExpenditureLifetimeSingleAge[i,j] <- sum(diag(fullnonHealthExpenditureSingleAge[i:lastRow,j:lastCol]))
    }
  }

  # saveRDS(fullnonHealthExpenditureLifetimeSingleAge, "~/TubercuCost/inst/LifetimeNonHealthExpenditureSingleYearAge.rds", version=2)


  ### ### ### ### ### ### ### ###
  ### Create discounted non-Health expenditure estimates for single year of age estimates - TB mortality

  DiscountnonHealthExpenditureLifetimeSingleAge <- matrix(0, nrow = nrow(fullnonHealthExpenditureLifetimeSingleAge), ncol = ncol(fullnonHealthExpenditureLifetimeSingleAge))
  colnames(DiscountnonHealthExpenditureLifetimeSingleAge) <- expenditureSingleAge0[,1]
  rownames(DiscountnonHealthExpenditureLifetimeSingleAge) <- 2020:2100
  for (i in 1:nrow(DiscountnonHealthExpenditureLifetimeSingleAge)){
    for (j in 1:ncol(DiscountnonHealthExpenditureLifetimeSingleAge)){
      # years of lost life expectancy
      yearsLost <- DiscountLifeExpectancy[i,j]
      # use years lost to find the last col and row of our matrix of expenditures
      lastRow <- min(i + yearsLost, nrow(fullnonHealthExpenditureLifetimeSingleAge))
      lastCol <- min(j + yearsLost, ncol(fullnonHealthExpenditureLifetimeSingleAge))

      DiscountnonHealthExpenditureLifetimeSingleAge[i,j] <- sum(diag(DiscountnonHealthExpenditureSingleAge[i:lastRow,j:lastCol]))
    } }

  # saveRDS(DiscountnonHealthExpenditureLifetimeSingleAge, "~/TubercuCost/inst/DiscountedLifetimeNonHealthExpenditureSingleYearAge.rds", version=2)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ##  AGE GROUPS  ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  ### read in the csv
  expenditureAgeGroups0 <- read.csv("~/TubercuCost/inst/extdata/ExpenditureDataAgeGrps2022.csv")

  ### Create undiscounted health expenditure estimates for age group estimates
  fullHealthExpenditureAgeGroups <- matrix(expenditureAgeGroups0[,3], ncol = length(expenditureAgeGroups0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullHealthExpenditureAgeGroups) <- expenditureAgeGroups0[,1]
  rownames(fullHealthExpenditureAgeGroups) <- 2020:2100

  # saveRDS(fullHealthExpenditureAgeGroups, "~/TubercuCost/inst/HealthExpenditureAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted health expenditure estimates for age group estimates

  DiscountHealthExpenditureAgeGroups <- matrix(0, nrow = nrow(fullHealthExpenditureAgeGroups), ncol = ncol(fullHealthExpenditureAgeGroups))
  colnames(DiscountHealthExpenditureAgeGroups) <- expenditureAgeGroups0[,1]
  rownames(DiscountHealthExpenditureAgeGroups) <- 2020:2100
  for (i in 1:nrow(DiscountHealthExpenditureAgeGroups)){
    for (j in 1:ncol(DiscountHealthExpenditureAgeGroups)){
      DiscountHealthExpenditureAgeGroups[i,j] <- fullHealthExpenditureAgeGroups[i,j] * discountVec[i]
    } }

  # saveRDS(DiscountHealthExpenditureAgeGroups, "~/TubercuCost/inst/DiscountedHealthExpenditureAgeGroups.rds", version=2)


  ### Create undiscounted health expenditure estimates for age group estimates - TB mortality
  fullHealthExpenditureLifetimeAgeGroup <- matrix(expenditureAgeGroups0[,3], ncol = length(expenditureAgeGroups0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullHealthExpenditureLifetimeAgeGroup) <- expenditureAgeGroups0[,1]
  rownames(fullHealthExpenditureLifetimeAgeGroup) <- 2020:2100

  fullHealthExpenditureLifetimeAgeGroup[,1] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,1:5])
  fullHealthExpenditureLifetimeAgeGroup[,2] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,6:15])
  fullHealthExpenditureLifetimeAgeGroup[,3] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,16:25])
  fullHealthExpenditureLifetimeAgeGroup[,4] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,26:35])
  fullHealthExpenditureLifetimeAgeGroup[,5] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,36:45])
  fullHealthExpenditureLifetimeAgeGroup[,6] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,46:55])
  fullHealthExpenditureLifetimeAgeGroup[,7] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,56:65])
  fullHealthExpenditureLifetimeAgeGroup[,8] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,66:75])
  fullHealthExpenditureLifetimeAgeGroup[,9] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,76:85])
  fullHealthExpenditureLifetimeAgeGroup[,10] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,86:95])
  fullHealthExpenditureLifetimeAgeGroup[,11] <- mean(DiscountHealthExpenditureLifetimeSingleAge[1,96:101])

  # saveRDS(fullHealthExpenditureLifetimeAgeGroup, "~/TubercuCost/inst/LifetimeHealthExpenditureAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted Health expenditure estimates for age group estimates - TB mortality

  DiscountHealthExpenditureLifetimeAgeGroups <- matrix(0, nrow = nrow(fullHealthExpenditureAgeGroups), ncol = ncol(fullHealthExpenditureAgeGroups))
  colnames(DiscountHealthExpenditureLifetimeAgeGroups) <- expenditureAgeGroups0[,1]
  rownames(DiscountHealthExpenditureLifetimeAgeGroups) <- 2020:2100
  for (i in 1:nrow(DiscountHealthExpenditureLifetimeAgeGroups)){
    DiscountHealthExpenditureLifetimeAgeGroups[i,1] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,1:5])
    DiscountHealthExpenditureLifetimeAgeGroups[i,2] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,6:15])
    DiscountHealthExpenditureLifetimeAgeGroups[i,3] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,16:25])
    DiscountHealthExpenditureLifetimeAgeGroups[i,4] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,26:35])
    DiscountHealthExpenditureLifetimeAgeGroups[i,5] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,36:45])
    DiscountHealthExpenditureLifetimeAgeGroups[i,6] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,46:55])
    DiscountHealthExpenditureLifetimeAgeGroups[i,7] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,56:65])
    DiscountHealthExpenditureLifetimeAgeGroups[i,8] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,66:75])
    DiscountHealthExpenditureLifetimeAgeGroups[i,9] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,76:85])
    DiscountHealthExpenditureLifetimeAgeGroups[i,10] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,86:95])
    DiscountHealthExpenditureLifetimeAgeGroups[i,11] <- mean(DiscountHealthExpenditureLifetimeSingleAge[i,96:101])
}
  # saveRDS(DiscountHealthExpenditureLifetimeAgeGroups, "~/TubercuCost/inst/DiscountedLifetimeHealthExpenditureAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###   ### ### ### ### ### ### ### ###
  ### CREATE NON HEALTH CARE EXPENDITURES
  ### 1. ANNUAL NON HEALTH CARE EXPENDITURE UNDISCOUNTED
  ### 2. ANNUAL NON HEALTH CARE EXPENDITURE DISCOUNTED 3% ANNUALLY
  ### 3. LIFETIME NON HEALTH CARE EXPENDITURE UNDISCOUNTED
  ### 4. LIFETIME NON HEALTH CARE EXPENDITURE DISCOUNTED 3% ANNUALLY

  ### Create undiscounted non-health expenditure estimates for age group estimates
  fullNonHealthExpenditureAgeGroups <- matrix(expenditureAgeGroups0[,4], ncol = length(expenditureAgeGroups0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullNonHealthExpenditureAgeGroups) <- expenditureAgeGroups0[,1]
  rownames(fullNonHealthExpenditureAgeGroups) <- 2020:2100

  # saveRDS(fullNonHealthExpenditureAgeGroups, "~/TubercuCost/inst/NonHealthExpenditureAgeGroups.rds", version=2)

  ### ### ### ### ### ### ### ###
  ### Create discounted non-Health expenditure estimates for age group estimates

  DiscountNonHealthExpenditureAgeGroups <- matrix(0, nrow = nrow(fullNonHealthExpenditureAgeGroups), ncol = ncol(fullNonHealthExpenditureAgeGroups))
  colnames(DiscountNonHealthExpenditureAgeGroups) <- expenditureAgeGroups0[,1]
  rownames(DiscountNonHealthExpenditureAgeGroups) <- 2020:2100
  for (i in 1:nrow(DiscountNonHealthExpenditureAgeGroups)){
    for (j in 1:ncol(DiscountNonHealthExpenditureAgeGroups)){
      DiscountNonHealthExpenditureAgeGroups[i,j] <- fullNonHealthExpenditureAgeGroups[i,j] * discountVec[i]
    } }

  # saveRDS(DiscountNonHealthExpenditureAgeGroups, "~/TubercuCost/inst/DiscountedNonHealthExpenditureAgeGroups.rds", version=2)

  ### LIFE TIME ESTIMATES ###
  ### Create undiscounted non health expenditure estimates for age group estimates - TB mortality
  fullNonHealthExpenditureLifetimeAgeGroups <- matrix(0, ncol = length(expenditureAgeGroups0[,2]), nrow = length(discountVec), byrow = TRUE)
  colnames(fullNonHealthExpenditureLifetimeAgeGroups) <- expenditureAgeGroups0[,1] # ages
  rownames(fullNonHealthExpenditureLifetimeAgeGroups) <- 2020:2100 # years

  fullNonHealthExpenditureLifetimeAgeGroups[,1] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,1:5])
  fullNonHealthExpenditureLifetimeAgeGroups[,2] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,6:15])
  fullNonHealthExpenditureLifetimeAgeGroups[,3] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,16:25])
  fullNonHealthExpenditureLifetimeAgeGroups[,4] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,26:35])
  fullNonHealthExpenditureLifetimeAgeGroups[,5] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,36:45])
  fullNonHealthExpenditureLifetimeAgeGroups[,6] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,46:55])
  fullNonHealthExpenditureLifetimeAgeGroups[,7] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,56:65])
  fullNonHealthExpenditureLifetimeAgeGroups[,8] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,66:75])
  fullNonHealthExpenditureLifetimeAgeGroups[,9] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,76:85])
  fullNonHealthExpenditureLifetimeAgeGroups[,10] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,86:95])
  fullNonHealthExpenditureLifetimeAgeGroups[,11] <- mean(fullnonHealthExpenditureLifetimeSingleAge[1,96:101])

  # saveRDS(fullNonHealthExpenditureLifetimeAgeGroups, "~/TubercuCost/inst/LifetimeNonHealthExpenditureAgeGroups.rds", version=2)


  ### ### ### ### ### ### ### ###
  ### Create discounted non-Health expenditure estimates for single year of age estimates - TB mortality

  DiscountNonHealthExpenditureLifetimeAgeGroups <- matrix(0, nrow = nrow(fullNonHealthExpenditureLifetimeAgeGroups), ncol = ncol(fullNonHealthExpenditureLifetimeAgeGroups))
  colnames(DiscountNonHealthExpenditureLifetimeAgeGroups) <- expenditureAgeGroups0[,1]
  rownames(DiscountNonHealthExpenditureLifetimeAgeGroups) <- 2020:2100
  for (i in 1:nrow(DiscountNonHealthExpenditureLifetimeAgeGroups)){
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,1] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,1:5])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,2] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,6:15])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,3] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,16:25])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,4] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,26:35])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,5] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,36:45])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,6] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,46:55])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,7] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,56:65])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,8] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,66:75])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,9] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,76:85])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,10] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,86:95])
  DiscountNonHealthExpenditureLifetimeAgeGroups[i,11] <- mean(DiscountnonHealthExpenditureLifetimeSingleAge[i,96:101])
}
  saveRDS(DiscountNonHealthExpenditureLifetimeAgeGroups, "~/TubercuCost/inst/DiscountedLifetimeNonHealthExpenditureAgeGroups.rds", version=2)
}


