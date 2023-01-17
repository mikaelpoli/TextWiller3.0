#' normalizzapunteggiatura
#'
#' \code{normalizzapunteggiatura} Rimuove caratteri escape e punteggiatura da un set di testi.
#' 
#' @param testo Un set di testi da cui rimuovere caratteri escape e punteggiatura.
#' @param removeUnderscore Logical. Se \code{TRUE}, rimuove gli undescore "_" dai testi.
#' @param fixed Logical. Se \code{TRUE}, il pattern e' una stringa da essere matchata com'e'. 
#' Esclude tutti gli argomenti in conflitto.
#' @param perl Logical. Se \code{TRUE}, utilizza regex compatibili con Perl.
#' @return Un set di testi processati.
#' @author Livio Finos
#' @examples 
#'
#'  testo<-c(testo<-c("@ ","@retweet","# ","#ciao")) 
#'  normalizzapunteggiatura(testo)
#'  
#'

normalizzapunteggiatura <-
function(testo,removeUnderscore=TRUE, perl=TRUE,fixed=TRUE){
	
	testo <- paste(" ",testo," ", sep="")
	testo <- gsub("#\\s+", "#", testo, perl=perl)
	testo <- gsub("#", " #", testo, perl=perl)
	testo <- gsub("@\\s+", "@", testo, perl=perl)	
	testo <- gsub("@", " @", testo, perl=perl)
	
	testo <- gsub("\n",' ',testo,fixed=fixed)
	testo <- gsub("\t",' ',testo,fixed=fixed)
	testo <- gsub("\r",' ',testo,fixed=fixed)
	testo <- gsub("\\s+", " ", testo, perl=perl)	
  
#	testo <- gsub('^( ["][@]| [@]| \034[@])',' RT @', testo, perl=perl)	
#	testo <- gsub("^( ['][@])",' RT @', testo, perl=perl)
#	testo <- gsub("^( RT: @)",' RT @', testo, perl=perl)
#	testo <- gsub("^( RT '@)",' RT @', testo, perl=perl)			
	
  
  testo <- gsub('[//?]+',' ', testo, perl=perl)
	testo <- gsub('[//!]+',' ', testo, perl=perl)
	
  
  testo <- gsub('[///]+',' ', testo, perl=perl)
	testo <- gsub('[//?]+',' ', testo, perl=perl)
	testo <- gsub('[//!]+',' ', testo, perl=perl)
	testo <- gsub('[//|]+',' ', testo, perl=perl)
	testo <- gsub("'"," ",testo,fixed=fixed)
	testo <- gsub("’"," ",testo,fixed=fixed)
	testo <- gsub("‘"," ",testo,fixed=fixed)
	testo <- gsub('"',' ',testo,fixed=fixed)
	testo <- gsub('\uc294|\uc293|\ucc8f|\ucc8e|\ucc8b|\ucbb6|\ucbb5|\ucbae|\ucb9d|\ucaba',' ',testo)
	testo <- gsub('\uc2bb|\uc2ab',' ',testo,fixed=fixed)
	testo <- gsub('\uc291|`|\uc292',' ',testo)	
	testo <- gsub('\uc285',' ',testo,fixed=fixed)	
	testo <- gsub("\\\\"," ",testo)
	testo <- gsub("\\/"," ",testo)
	testo <- gsub("[=]"," ",testo,perl=perl)
	testo <- gsub("[+]"," ",testo,perl=perl)
	testo <- gsub("[-]"," ",testo,perl=perl)
	testo <- gsub('\\*{1}',' ', testo, perl=perl)
	testo <- gsub('«',' ', testo, perl=perl)
	testo <- gsub('»',' ', testo, perl=perl)
	testo <- gsub("[<]"," ",testo,perl=perl)
	testo <- gsub("[>]"," ",testo,perl=perl)
	testo <- gsub("[~]"," ",testo,perl=perl)
	testo <- gsub("[\\^]"," ",testo,perl=perl)
	testo <- gsub("[,]"," ",testo,perl=perl)
	testo <- gsub("[;]"," ",testo,perl=perl)
	testo <- gsub("[:]"," ",testo,perl=perl)
	testo <- gsub('[\\.]',' ', testo, perl=perl)
	testo <- gsub('…',' ', testo, perl=perl)
	testo <- gsub("[&]"," & ",testo,perl=perl)
	# parentesi
	testo <- gsub("[(]"," ",testo,perl=perl)
	testo <- gsub("[)]"," ",testo,perl=perl)
	testo <- gsub("[{]"," ",testo,perl=perl)
	testo <- gsub("[}]"," ",testo,perl=perl)
	testo <- gsub("[[]"," ",testo,perl=perl)
	testo <- gsub("[]]"," ",testo,perl=perl)

  if(removeUnderscore) 
    testo=gsub("_", " ",testo, perl=perl)
    
	testo <- gsub("\\s+", " ", testo, perl=perl)
testo
	}
