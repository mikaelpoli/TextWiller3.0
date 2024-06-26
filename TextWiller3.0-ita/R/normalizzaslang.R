#' normalizzaslang
#' 
#' Sostituisce esperessioni gergali italiane in \code{testo}
#' con una parola chiave corrispondente.
#' 
#' La parola chiave pu� essere la parola stessa normalizzata (es., nn = non)
#' oppure un'emoticon (es., zzz = EMOTEZZZ). 
#' 
#' @name normalizzaslang
#' @param testo Oggetto \code{character vector} di testi da normalizzare
#' @param perl Logico. Se \code{TRUE}, vengono utilizzate regex compatibili con Perl.
#' @return Un oggetto \code{character vector} di testi in cui le espressioni 
#' gergali italiane sono state sostituite da una parola chiave.
#' @author Livio Finos
#' @examples 
#' testo <- c("grandissima", "aaa", "nun")
#' normalizzaslang(testo)
#' 
#' @export normalizzaslang

normalizzaslang <-function(testo,perl=TRUE){
	testo <- gsub(" (#?zz+|#?u+ff[aif]+?|#?r+o+n+f+|#uff|ronf) "," EMOTEZZZ ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?sii+|#si+|#?yes+|#?s\uc38c\uc38c+) "," EMOTESIII ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?noo+|#no+|#?nuu+) "," EMOTENOOO ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?ahh+) "," EMOTEAHHH ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?ehh+) "," EMOTEEHHH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?o?h?o+h[oh]+) "," EMOTEOHOH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?i?h?i+h[ih]+) "," EMOTEIHIH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?i?h?i+h[ih]+) "," EMOTEUHUH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?a?h?ah[^h][ah]+) "," EMOTEAHAH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?e?h?eh[^h][eh]+) "," EMOTEEHEH ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?azz+) "," EMOTEAZZ ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?[dv]aii+|#[dv]ai|forzaa+) "," EMOTEDAIII ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?cazz[oi]+) "," cazzo ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?cazzat[a]+) "," cazzata ",testo, perl=perl, ignore.case=TRUE)  
	testo <- gsub(" (#?merd[a]+) "," merda ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub("ca\\*\\*o|c\\*\\*\\*+o","cazzo",testo, perl=perl)
	testo <- gsub(" (#?aaa+) "," EMOTEAAA ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#?ooo+) "," EMOTEOOO ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" (#?eee+) "," EMOTEEEE ",testo, perl=perl, ignore.case=TRUE)		
	testo <- gsub(" (#?l+o+l+|#?r+o+f+t+l+) "," EMOTELOL ",testo, perl=perl, ignore.case=TRUE)		
	testo <- gsub(" (#?aiutoo+|#?sos|#?help+) "," EMOTESOS ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" (#ba+sta+|ba+staa+) "," EMOTEBASTA ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?([uw]+[ao]+[uw]+) "," EMOTEWOW ",testo, perl=perl, ignore.case=TRUE)		
	
	testo <- gsub(" (#?grande[e]+|#?grandi[i]+) "," grandeee ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?(grand(issim)[aeoi]+) "," grandissimo ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" #?(brav[aeoi]+) "," bravo ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?(brav(issim)[aeoi]+) "," bravissimo ",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub(" #?(bell[aeoi]+) "," bello ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" #?(mit[t]ic([aio]|he)+) "," mitico ",testo, perl=perl, ignore.case=TRUE)			
	testo <- gsub(" #?(graziee+) "," grazieee ",testo, perl=perl, ignore.case=TRUE)	
	testo <- gsub(" #?(stronz[oaie]+) "," stronzooo ",testo, perl=perl, ignore.case=TRUE)	
		
	# ALTRO		
	testo <- gsub("perch[\uc3a9e\uc3a8]","perch\uc3a9",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("x(ch|k)[\uc3a9e\uc3a8]","perch\uc3a9",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("nn","non",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("nun","non",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("o+","o",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("i+","i",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("e+","e",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("u+","u",testo, perl=perl, ignore.case=TRUE)
	testo <- gsub("a+","a",testo, perl=perl, ignore.case=TRUE)
		testo
		}
