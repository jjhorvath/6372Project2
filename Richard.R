# Richard's R File
require("mlbench")
require("corrplot")
data(PimaIndiansDiabetes2,package = "mlbench") #see note about correction to dataset at https://www.rdocumentation.org/packages/mlbench/versions/2.1-1/topics/PimaIndiansDiabetes
Pima <- PimaIndiansDiabetes2;
rm(PimaIndiansDiabetes2)

%>% mutate(Attrition=ifelse(Attrition=="Yes", 1, 0))
tempdf <- tempdf %>% mutate(diabetes=ifelse(diabetes=="pos", 1, 0))
