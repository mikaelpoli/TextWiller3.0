#' normalizzahtml
#'
#' \code{normalizzahtml} Sostituisce link e URL in \code{testo} con la parola chiave WWWURLWWW.
#' 
#' @param testo Un set di testi da preprocessare.
#' @return Un set di testi in cui gli URL sono stati sostituiti con la parola chiave WWWURLWWW.
#' @param fixed Logical. Se \code{TRUE}, il pattern e' una strings da matchare com'e'. 
#' Esclude tutti gli argomenti in conflitto.
#' @param perl Logical. Se \code{TRUE}, utilizza regex compatibili con Perl-compatible.
#' @author Livio Finos
#' @examples 
#' 
#'  testo<-c("http:textwiller.com","www.textwiller.com") 
#'  normalizzahtml(testo)
#'  
#'  

normalizzahtml <-
function(testo,perl=TRUE,fixed=TRUE){
	
	# fonte: http://htmlhelp.com/reference/html40/entities/special.html

  testo <- gsub("https.*"," WWWURLWWW ",testo,perl=perl)
  testo <- gsub("http.*"," WWWURLWWW ",testo,perl=perl)
  testo <- gsub("www.*"," WWWURLWWW ",testo,perl=perl)
  
  
  testo <- gsub("&quot;",'"',testo)
  testo <- gsub("&amp;","&",testo)
  testo <- gsub("&lt;","<",testo)
  testo <- gsub("&gt;",">",testo)
  testo <- gsub("&circ;","^",testo)
  testo <- gsub("&tilde;","~",testo)	
  testo <- gsub("&lsquo;","'",testo)
  testo <- gsub("&rsquo;","'",testo)
  testo <- gsub("&ldquo;",'"',testo)	




#testo <- gsub("\n"," ",testo,fixed=TRUE)
#testo <- gsub("\t"," ",testo,fixed=TRUE)
#testo <- gsub('\\\"','"',testo,perl=TRUE)	


testo <- gsub("[[:blank:]]+"," ",testo, perl=perl)
testo
	}
