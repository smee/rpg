require("FrF2")

rgpGmogpScreeningDesign <- pb(
  nruns = 48,
  n12.taguchi = FALSE,
  nfactors = 12, #48 -1,
  ncenter = 24, 
  replications = 1, 
  repeat.only = TRUE,
  randomize = TRUE ,
  seed = 1 ,
  factor.names = list( 
    buildingBlockSetNumber = c(1, 4),
    constantMutationProbability =c (0, 1),
    crossoverProbability = c(0, 1),
    enableAgeCriterion = c(0, 1),
    errorMeasureNumber = c(1, 4),
    functionMutationProbability = c(0, 1),
    lambdaRel = c(0, 1),
    mu = c(8, 256),
    nuRel = c(0, 1),
    parentSelectionProbability = c(0, 1),
    selectionFunctionNumber = c(1, 2),
    subtreeMutationProbability = c(0, 1)))
## creator element of design.info will be different, when using the command line command!

#fix(rgpGmogpScreeningDesign) # invoke editor on object, then save edited version to rgpGmogpScreeningDesign
FILENAME <- "./rgpGmogpScreeningDesign.RData"
save("rgpGmogpScreeningDesign", file = FILENAME)
message("Saved Plackett-Burman screening design to '", FILENAME, "'.")

