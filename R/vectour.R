convert.vecteur <- function(vector){
  if(is.numeric(vector)){
    char<- paste0("  ", deparse(substitute(vector)), " ~ ", paste(vector,collapse = ","))
    }
  else {
    char <- paste0("  ", deparse(substitute(vector)), " ~ ", paste(vector,collapse = ","))
    }
  return(char)
}

