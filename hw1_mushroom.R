library(tidyverse)


# set location to csv file folder
setwd("D:/cstu/cstuปี4เทอม2/forecast")

#get mushroom data
mushroomData <- read.csv("mushrooms.csv")
mushroomData1 <- read.csv("mushrooms.csv")
# Rename the variables
colnames(mushroomData) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")

# Defining the levels for the categorical variables 
## We make each variable as a factor
mushroomData <- mushroomData %>% map_df(function(.x) as.factor(.x))

## We redefine each of the category for each of the variables
levels(mushroomData$edibility) <- c("edible", "poisonous")
levels(mushroomData$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroomData$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushroomData$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushroomData$bruises) <- c("no", "yes")
levels(mushroomData$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroomData$gill_attachement) <- c("attached", "free")
levels(mushroomData$gill_spacing) <- c("close", "crowded")
levels(mushroomData$gill_size) <- c("broad", "narrow")
levels(mushroomData$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushroomData$stalk_shape) <- c("enlarging", "tapering")
levels(mushroomData$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushroomData$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroomData$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroomData$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroomData$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroomData$veil_type) <- "partial"
levels(mushroomData$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushroomData$ring_number) <- c("none", "one", "two")
levels(mushroomData$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroomData$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushroomData$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroomData$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

#explore mushroomData
summary(mushroomData)
map_dbl(mushroomData, function(.x) {sum(is.na(.x))})




#set seed for reproduce result
set.seed(142)

#partiiton data to testset 30 %(2438),trainset 70%(5686) 
sample <- sample(1:8124,5686)
trainset <- mushroomData[sample,]
testset <- mushroomData[-sample,]



#classify by rpart decision-tree library
library(rpart)
library(rpart.plot)

set.seed(143)
model_tree <- rpart(edibility ~ ., data = trainset, method = "class")

#test tree
test_tree <- predict(model_tree,testset, type = "class")
caret::confusionMatrix(data=test_tree,reference = testset$edibility, positive="edible")

#plot result tree 
#rpart.plot(model_tree, extra = 104, box.palette = "GnBu",branch.lty = 3, shadow.col = "gray", nn = TRUE)

#classify by lab decision-tree library
library(data.tree)

IsPure <- function(data){
  length(unique(data[,1])) == 1
}
Entropy <- function(vls) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

InformationGain <- function(tble){
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum(s/sum(s) * apply(tble,MARGIN = 1,FUN = Entropy))
  informationGain <- entropyBefore - entropyAfter
  return(informationGain)
}
TrainID3 <- function(node, data) {
  node$obsCount <- nrow(data)
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,1]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    feature <- names(ig)[ig == max(ig)][1]
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], 
                      data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
  }
}

TreePredict <- function(tree,features){
  if(tree$children[[1]]$isLeaf)
    return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return (TreePredict(child,features))
} 

tree_lab <- Node$new("mushroomtree")

#mushsample <- caret::createDataPartition(y = mushroomData$edibility, times = 1, p = 0.8, list = FALSE)

labtrainset <- as.data.frame(trainset)

TrainID3(tree_lab,labtrainset)

print(tree_lab,"feature","obsCount")

TreePredict(tree_lab,testset[1,-1])


#information gain measure attribute

InformationGain(table(labtrainset[,c('stalk_root','gill_color')]))
InformationGain(table(labtrainset[,c('gill_color','odor')]))
InformationGain(table(labtrainset[,c('habitat','stalk_root')]))

InformationGain(table(mushroom[,c('size','points')]))
InformationGain(table(mushroom[,c('size','color')]))
InformationGain(table(mushroom[,c('points','color')]))


#visualize data
library(ggplot2)
ggplot(labtrainset, aes(x = stalk_root, y = habitat, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))
ggplot(labtrainset, aes(x = stalk_root, y = gill_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))
ggplot(labtrainset, aes(x = odor, y = gill_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))
