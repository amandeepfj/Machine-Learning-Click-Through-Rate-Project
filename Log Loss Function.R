# @author: Alex Su, Amandeep
# "We are drowning in information, while starving for wisdom - E. O. Wilson"

Logloss <- function(pred, validy){
  if(is.factor(validy)){
    validy <- as.numeric(levels(validy))[validy]
  }
  pred <- (as.vector(pred))
  eps <- 1e-15
  pred <- pmax(pmin(pred, 1 - eps), eps)
  return(LogLoss(pred, validy))
}
