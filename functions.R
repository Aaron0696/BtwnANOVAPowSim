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
    ## the output is a vector with two elements
    ## the first element indicates if the effect is statistically significant,
    ## 1 = sig, 0 = ns.
    ## the second element is an NA, which I may have some use for later.
anova1way <- function(means, sds, grpsize, num.grp, alphalvl = 0.05){
  # this loop creates the arguments for simulateData()
  # each iteration is simulates the data for one group
  for(r in 1:num.grp){
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
  for(r in 1:num.grp){
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
  if(anova.results < alphalvl){
    sig <- 1
  } else {
    sig <- 0
  }
  return(c(sig,NA))
}

# description
    ## this function runs a single two-way ANOVA with the selected parameters.
# inputs
    ## iv1.lvl, the number of levels that iv1 possess
    ## iv2.lvl the number of levels that iv2 possess
      ### multiplying iv1.lvl and iv2.lvl together gives us the number of groups
      ### for means, sds and grp size, they are arranged such that each value of iv1 is permutated
      ### across first, then iv2.
      ### for example, suppose we are running a 2 X 3 ANOVA
      ### iv1.lvl is 2, iv2.lvl is 3, total of 6 groups
      ### the means vector would be arranged like so:
      ### c(grp11,12,21,22,31,33)
      ### where grp12 refers to the group assigned to level 1 of iv1 and level 2 of iv2
    ## means, vector with each element corresponding to the mean of each group.
    ## sds, vector with each element corresponding to the sd of each group.
    ## grpsize, vector with each element corresponding to the size of each group.
    ## alphalvl, the alpha level for NHST, defaults to 0.05.
# output
    ## the output of this function is a dataframe with 2 colums, Effects and Sig
    ## Effects indicate whether the sig refers to the main effects or interaction effects
    ## Sig indicates whether the effect was statistically significant given the alpha level,
    ## 0 = n.s, 1 = sig
anova2way <- function(means, sds, grpsize, iv1.lvl, iv2.lvl, alphalvl = 0.05){
  
  # get number of groups
  num.grp <- length(iv1.lvl) * length(iv2.lvl)
  # index for looping through means, sds and grpsize
  j <- 1
  
  # this loop creates the arguments for simulateData()
  # each iteration simulates the data for one group
  for (r in iv2.lvl) {
    # create the model formula to be used in simulateData(model = ...)
    # assign the model formula to an object called grp**.model,
    # where ** is 11 for group11, with condition 1 for iv1 and 
    # condition 1 for iv2 and so on...
    for (i in iv1.lvl) {
      assign(paste0("grp", i, r, ".model"),
             paste0("y ~ ", means[j], "*1", 
                    "\n y ~~ ", sds[j] ^ 2, "*y"),
             envir = .GlobalEnv)
      
      # run simulateData() and save dataset into an object called 
      # data.g**, where data.g11 is the dataset for the condition in level1 of iv1 and level1 of iv2
      # data.g23 for the condition in level2 of iv1 and level3 of iv2
      assign(paste0("data.g", i, r),
             simulateData(eval(parse(
               text = paste0("grp", i, r, ".model")
             )),
             sample.nobs = grpsize[j]
             ))
      j <- j + 1
    }
  return(sig)
}