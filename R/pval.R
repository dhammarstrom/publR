#' # Input a non-truncated p-value, the function returns a rounded p-value to p<1/10^digits or 
#' @param pval A p-value to be truncated
#' @param digits To how many decimal points the p-value should be truncated, e.g. 3 digits returns p < 0.001 as lowest threshold
#' @param flag If TRUE, a flag is returned instead of p-values. p-values 0.01-0.05, "*"; 0.001-0.01, "**"; 0.0001-0.001, "***"; < 0.0001, "****".
#' @param plot If TRUE, p-value is written to be inserted in e.g. ggplot2 figures in the form bquote(italic(p)<.(lowp)), elese returned as character for printing in text.
#' @return Truncetad p-values or flag to be used in reporting e.g. rmarkdown documents or figures.
#' @export
pval<-function(pval, digits = 3, flag = FALSE, plot = FALSE){
                
        lowp<-1/(10^digits)
  
        
        if(!(flag)){
        
                if(plot){
                        if(pval<lowp){
                                
                                return(paste0("italic(p)<", lowp, collapse=""))
                                
                        } else {
                                return(paste0("italic(p)==", format(pval, digits=digits), collapse=""))
                        }     
                }
                if(!(plot)){
                        if(pval<lowp){
                                
                                return(as.character(paste0("*p*<",lowp)))
                                
                        } else {
                                return(as.character(paste0("*p*=",format(pval, digits=digits))))
                                
                        }       
                        
                }
                
                
                  
  } 
        if(flag){
                
              significance.flag  <- ifelse(findInterval(pval, c(0.01, 0.05)) == 1, "*", 
                       ifelse(findInterval(pval, c(0.001, 0.01)) == 1, "**",
                              ifelse(findInterval(pval, c(0.0001, 0.001)) == 1,"***",
                                     ifelse(findInterval(pval, c(0, 0.0001)) == 1, "****", ""))))
                
               as.character(significance.flag)
                
        }


        
        
          
  }              

        
        