Entropy <- function(vls){
  res = vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] = 0
  -sum(res)
}