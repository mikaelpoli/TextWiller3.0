#' Sentiment analysis
#' 
#' Assegna un punteggio sentiment (sentiment score) per ogni testo in \code{text}.
#' 
#' @aliases sentiment sentimentVocabularies vocabolariMadda vocabolarioMattivio
#' @param text Vettore di testi.
#' @param algorithm \code{"Mattivio"} (default), \code{"Maddalena"}, o una funzione
#' che restituisce il punteggio.
#' @param vocabularies \code{vocabolarioMattivio} di default se \code{algorithm == "Mattivio"}; 
#' \code{vocabolariMadda} di default se \code{algorithm == "Maddalena"}, oppure un 
#' oggetto usato dall'algoritmo.
#' @param normalizzaTesti \code{TRUE} di default.
#' @param get_labels \code{TRUE} di default. Se \code{FALSE}, assegna un punteggio
#' quantitativo; Se \code{TRUE}, assegna le etichette -1, 0, +1 (i.e., \code{sign(score)})
#' @return Un array contenente un singolo valore numerico per ogni elemento del vettore \code{text}.
#' @note %% ~~further notes~~
#' @author Maddalena Branca, Mattia Da Pont, Livio Finos
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' sentiment(c("ciao bella", "mi piaci", "wow!!","good","casa", "farabutto!","ti odio"))
#' 
#' @export sentiment
#' 
sentiment <- function(text, algorithm="Mattivio", 
                      vocabularies=NULL,
                      normalizzaTesti=TRUE, get_labels=TRUE,...){
  if(!is.null(text)){ #se c'e' almeno un testo
    if(is.null(algorithm)) algorithm="Mattivio"
    if(is.null(vocabularies)) {
      if(algorithm=="Mattivio") {
        data(vocabolarioMattivio) 
        vocabularies=vocabolarioMattivio
        } else if(algorithm=="Maddalena") {
          data(vocabolariMadda) 
          vocabularies=vocabolariMadda
        }
    }
    
    if(normalizzaTesti==TRUE)
      text<-normalizzaTesti(text,suppressInvalidTexts=FALSE,contaStringhe=NULL,...)
    
    #choose and perform algorithm
    if(is.function(algorithm)) {
      sent=algorithm(text=text, vocabularies=vocabularies,...)
      return(sent)} else 
        if(algorithm=="Maddalena") {
      sent=.sentiment.maddalena(text=text, vocabularies=vocabularies,...)
      
    } else if(algorithm=="Mattivio") {
      sent=.sentiment.mattivio(text=text, vocabularies=vocabularies,...)
    } else sent=NULL
    
    if(get_labels)
      return(sign(sent)) else
        return(sent)
    
  } #end if(!is.null(text))
}

