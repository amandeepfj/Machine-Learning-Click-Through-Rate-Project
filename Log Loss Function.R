# @author: Alex Su, Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

Logloss <- function(pred, validy){
  if(is.factor(validy)){
    validy <- as.numeric(levels(validy))[validy]
  }
  pred <- (as.vector(pred))
  eps <- 1e-15
  pred <- pmax(pmin(pred, 1 - eps), eps)
  return( - mean(validy*log(pred)+(1-validy)*log(1-pred), na.rm = TRUE))
}
