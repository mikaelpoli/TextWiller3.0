#' normalizzacaratteri
#'
#' \code{normalizzacaratteri} replaces escape and punctation codes in \code{testo} with a blank.
#'  
#' @param testo a set of texts to be stripped of escape and punctation codes
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @return a set of processed texts
#' @author Livio Finos
#' @examples 
#'
#'  testo<-c("\t","\r") 
#'  normalizzacaratteri(testo)
#'  
#'  

normalizzacaratteri <- function(testo,fixed=TRUE){
  
 	testo <- gsub("\001" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\002" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\003" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\004" ," ", testo, fixed=fixed, useBytes=FALSE)			
	testo <- gsub("\005" ," ", testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\006" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\007" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\008" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\009" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\010" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\011" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\012" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\013" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\014" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\015" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\016" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\017" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\018" ," ", testo, fixed=fixed, useBytes=FALSE)
#	testo <- gsub("\019" ," ", testo, fixed=fixed, useBytes=FALSE)																
	testo <- gsub("\020" ," ", testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\021" ,"!", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\022" ,'"', testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\023" ,' ', testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\027" ,"'", testo, fixed=fixed, useBytes=FALSE)		
	testo <- gsub("\030" ,"'", testo, fixed=fixed, useBytes=FALSE)	
	testo <- gsub("\031" ," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\032" ,"'", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\033" ," ", testo, fixed=fixed, useBytes=FALSE)		
	testo <- gsub("\034" ,"'", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\035" ,"'", testo, fixed=fixed, useBytes=FALSE)
  testo <- gsub("“" ,"'", testo, fixed=fixed, useBytes=FALSE)
  testo <- gsub("”" ,"'", testo, fixed=fixed, useBytes=FALSE)

 	
 
   
	testo <- gsub("\n"," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\r"," ", testo, fixed=fixed, useBytes=FALSE)
	testo <- gsub("\t"," ", testo, fixed=fixed, useBytes=FALSE)
   
 	
  testo
}
