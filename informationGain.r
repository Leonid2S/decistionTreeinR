informationGain = function(tble){
  tble = as.data.frame.matrix(tble)
  entropyBefore = Entropy(colSums(tble))
  s = rowSums(tble)
  entropyAfter = sum(s/sum(s)*apply(tble,MARGIN = 1,FUN = Entropy))
  infg = entropyBefore - entropyAfter
  infg
}