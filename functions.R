# description
    ## this function runs a single one-way ANOVA with the selected parameters.
# inputs
    ## means, vector with each element corresponding to the mean of each group.
    ## sds, vector with each element corresponding to the sd of each group.
    ## grpsize, vector with each element corresponding to the size of each group.
    ## num.grp, numeric value denoting the number of groups.
      ### should correspond to the length of means, sds and grpsize vector.
    ## alphalvl, the alpha level for NHST, defaults to 0.05.
# output
    ## the output of this function is either 0 or 1, 
    ## 0 = main effect was not statistically significant,
    ## 1 = main effect was statistically significant.
anova1way <- function(means, sds, grpsize, num.grp, alphalvl = 0.05){
  
  # this loop creates the arguments for simulateData()
  # each iteration is simulates the data for one group
  for (r in 1:num.grp) {
    # create the model formula to be used in simulateData(model = ...)
    # assign the model formula to an object called grp*.model,
    # where * is 1 for group 1 and so on...
    assign(paste0("grp", r, ".model"),
           paste0("y ~ ", means[r], "*1", 
                  "\n y ~~ ", sds[r] ^ 2, "*y"))
    
    # run simulateData() and save dataset into an object called 
    # data.g*, where * is 1 for group 1 and so on...
    assign(paste0("data.g", r),
           simulateData(eval(parse(
             text = paste0("grp", r, ".model")
           )),
           sample.nobs = grpsize[r]
           ))
  }
  
  # retrieve names of all objects in the environment starting with data.g
  alldata.obs <- ls(pattern = "^data.g")
  # create function call to create the overall dataframe, 
  # consisting of data from each group
  # alldat.formula would take the form of "data.g1[,1], data.g2[,2]"
  alldat.formula <- paste0(alldata.obs, "[,1]", collapse = ",")
  # wrap alldat.formula in c( and ) to complete the command
  alldat.formula <- paste0("c(", alldat.formula, ")")
  
  # create a vector X to be used as a dummy variable to indicate the group membership
  # of each row of the results in alldat (below)
  X <- c()
  for (r in 1:num.grp) {
    X <- append(X, rep(paste(r), grpsize[r]))
  }
  
  # alldat is the overall dataframe that contains the data of all the groups
  # in a long format
  alldat <- data.frame(Y = eval(parse(text = alldat.formula)),
                       X1 = X)
  # save the p-value of the main effect from the one-way ANOVA
  anova.results <- Anova(lm(Y ~ X1,
                            # use type II for now
                            data = alldat), type = 2)[1,4]
  
  # if the p value is statistically significant, let sig = 1,
  # else sig = 0
  if (anova.results < alphalvl) {
    sig <- 1
  } else {
    sig <- 0
  }
  
  return(sig)
}

# description
    ## this function is the same as anova1way above but it does not analysis 
    ## the generated dataset.
# inputs
    ## means, vector with each element corresponding to the mean of each group.
    ## sds, vector with each element corresponding to the sd of each group.
    ## grpsize, vector with each element corresponding to the size of each group.
    ## num.grp, numeric value denoting the number of groups.
      ### should correspond to the length of means, sds and grpsize vector.
    ## alphalvl, the alpha level for NHST, defaults to 0.05.
# output
    ## the output of this function is a dataframe of the generated data. 
anova1way.sim <- function(means, sds, grpsize, num.grp){
  
  for (r in 1:num.grp) {
    # create the model formula to be used in simulateData(model = ...)
    # assign the model formula to an object called grp*.model,
    # where * is 1 for group 1 and so on...
    assign(paste0("grp", r, ".model"),
           paste0("y ~ ", means[r], "*1", 
                  "\n y ~~ ", sds[r] ^ 2, "*y"))
    
    # run simulateData() and save dataset into an object called 
    # data.g*, where * is 1 for group 1 and so on...
    assign(paste0("data.g", r),
           simulateData(eval(parse(
             text = paste0("grp", r, ".model")
           )),
           sample.nobs = grpsize[r]
           ))
  }
  
  # retrieve names of all objects in the environment starting with data.g
  alldata.obs <- ls(pattern = "^data.g")
  # create function call to create the overall dataframe, 
  # consisting of data from each group
  # alldat.formula would take the form of "data.g1[,1], data.g2[,2]"
  alldat.formula <- paste0(alldata.obs, "[,1]", collapse = ",")
  # wrap alldat.formula in c( and ) to complete the command
  alldat.formula <- paste0("c(", alldat.formula, ")")
  
  # create a vector X to be used as a dummy variable to indicate the group membership
  # of each row of the results in alldat (below)
  X <- c()
  for (r in 1:num.grp) {
    X <- append(X, rep(paste(r), grpsize[r]))
  }
  
  # alldat is the overall dataframe that contains the data of all the groups
  # in a long format
  alldat <- data.frame(Y = eval(parse(text = alldat.formula)),
                       X1 = X)
}