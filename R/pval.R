#' # Input a non-truncated p-value, the function returns a rounded p-value to p<1/10^digits or 
#' @param pval A p-value to be truncated
#' @param digits To how many decimal points the p-value should be truncated, e.g. 3 digits returns p < 0.001 as lowest threshold
#' @param flag If TRUE, a flag is returned instead of p-values. p-values 0.01-0.05, "*"; 0.001-0.01, "**"; 0.0001-0.001, "***"; < 0.0001, "****".
#' @param plot If TRUE, p-value is written to be inserted in e.g. ggplot2 figures in the form bquote(italic(p)<.(lowp)), elese returned as character for printing in text.
#' @param NAs What to do if NA
#' @param sign What sign to print if flag = TRUE
#' @return Truncetad p-values or flag to be used in reporting e.g. rmarkdown documents or figures.
#' @export
pval<-function(pval, digits = 3, flag = FALSE, plot = FALSE, include.p = TRUE, NAs = "-", sign = "\U002A"){
        
        if(is.na(pval)) return(NAs) else {
                
        lowp<-1/(10^digits)
          if(!(flag)){
                if(plot){
                        if(pval<lowp){
                                return(paste0("italic(P)<", lowp, collapse=""))
                        } else {
                                return(paste0("italic(P)==", sprintf(paste0("%.", digits,"f"), pval), collapse=""))
                        }     
                }
                if(!(plot)){
                        
                        if(include.p == TRUE) {
                                if(pval<lowp){
                                
                                return(as.character(paste0("P<",lowp)))
                                
                        } else {
                                return(as.character(paste0("P=",sprintf(paste0("%.", digits,"f"), pval))))
                        }   
                        } else {
                                if(pval<lowp){
                                        
                                        return(as.character(paste0("<",lowp)))
                                        
                                } else {
                                        return(as.character(paste0(sprintf(paste0("%.", digits,"f"), pval))))
                                }      
                                }
                        
                            
                }
                  
  } 
        if(flag){
                
              significance.flag  <- ifelse(findInterval(pval, c(0.01, 0.05)) == 1, paste0(rep(sign, 1), collapse = ""), 
                       ifelse(findInterval(pval, c(0.001, 0.01)) == 1, paste0(rep(sign, 2), collapse = ""),
                              ifelse(findInterval(pval, c(0.0001, 0.001)) == 1,paste0(rep(sign, 3), collapse = ""),
                                     ifelse(findInterval(pval, c(0, 0.0001)) == 1, paste0(rep(sign, 4), collapse = ""), ""))))
                
               as.character(significance.flag)
                
        }


        
        
        }       
  }              

        
        