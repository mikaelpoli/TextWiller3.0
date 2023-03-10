#' Classificazione per vocabolario di utenti
#' 
#' Associa i nomi in \code{names} ai valori indicati da un vocabolario. Ad esempio,
#' \code{data(dizionario_nomi_propri)} assegna il genere e \code{data(dizionario_luoghi)}
#' l'area geografica (vedi esempio).
#' 
#' 
#' Per il \code{data(dizionario_luoghi)} sono stati esclusi i paesi Re (800
#' abitanti, Nord-ovest) e Lu (1200 abitanti, Nord-ovest) perche' in conflitto
#' con le sigle delle province.
#' 
#' (update 05-09-2016) Ponendo particolare attenzione alla detection dei nomi propri,
#' si e' pensato di introdurre nella funzione tre diversi "tentativi" di classificazione.
#' Mentre nei primi due, con diverse accortezze, si cercano parole suddivise da spazi, nel
#' terzo ed ultimo step si prova a ricercare le stringhe di caratteri anche internamente alle parole
#' grazie alla funzione \code{grepl}. Alcuni parametri, descritti successivamente, regolano
#' questa parte della classificazione.
#' 
#' @aliases classificaUtenti dizionario_nomi_propri dizionario_luoghi
#' @param names Vettore di nomi.
#' @param vocabolario \code{data.frame} di una colonna con la classificazione
#' da associare. I \code{rownames(vocabolario)} devono essere unici (sono i
#' nomi unici su cui viene fatto il controllo). Il vocabolario fornito da noi
#' e' \code{data(dizionario_nomi_propri)}. ATTENZIONE: Nel \code{vocabolario}
#' usare solo lower-case e non usare mai "NA" ("na" e' valido).
#' @param scan_interno Se \code{TRUE}, la funzione effettua anche la parte di
#' scanning interno (\code{grepl} sui vocaboli passati.
#' @param vocab_interno Il vocabolario su cui viene effettuato (eventualmente)
#' lo scanning interno (di default, se previsto, e' pari a tutti i nomi di vocabolario
#' di lunghezza >= 5 caratteri).
#' @param how_class Gestisce la classificazione in casi "ambigui", ovvero in cui per
#' un nome vengano ritrovati piu' match; sono stati pensati tre casi:
#' \code{modeFirst} (default), in cui viene presa la categoria modale tra le riconosciute e,
#' nel caso di multimodalita', il primo in ordine tra i match; \code{first}, che classifica
#' con il primo match nella stringa, e \code{last} che classifica con l'ultimo.
#' @param cat_interna \code{NULL} (default) permette di identificare una o piu' categorie della classificazione
#' per cui vengono tenuti nello scanning interno tutti i termini di vocabolario e non solo
#' quelli con 5 o piu' caratteri.
#' @return Restituisce un named vector con elementi dalla colonna
#' \code{categoria} del data.frame \code{vocabolario}. Per
#' \code{vocabolario=dizionario_nomi_propri} le modalita' sono
#' \code{c('masc','femm','ente')}.
#' @author Mattia Uttini, Livio Finos, Andrea Mamprin, Dario Solari
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' \dontrun{data(dizionario_nomi_propri)}
#' \dontrun{str(dizionario_nomi_propri)}
#' classificaUtenti(c('livio','alessandra','alessandraRossi', 'mariobianchi'), scan_interno=TRUE)
#' \dontrun{data(dizionario_luoghi)}
#' classificaUtenti(c('Bosa','Pordenone, Italy'), dizionario_luoghi)
#' 
#' @export classificaUtenti
classificaUtenti <- function(names, vocabolario=NULL, scan_interno=FALSE, vocab_interno=NULL, how_class="modeFirst", cat_interna=NULL){
  
  if (is.factor(names)){
    names <- as.character(names)
  }
  
  
  if (is.null(vocabolario)) {
    data(dizionario_nomi_propri)
    vocabolario = dizionario_nomi_propri
  }
  
  if(scan_interno&is.null(vocab_interno)){
    vocab_interno <- vocabolario[which(nchar((rownames(vocabolario))>=5)|(substr(rownames(vocabolario),1,1)==".")|(vocabolario[,1]%in%cat_interna)),,drop=FALSE]
  }
  
  Encoding(names) <- "UTF-8"
  names <- iconv(names, "UTF-8", "UTF-8", sub='')
  nomi_originali <- .togliSpaziEsterni(names)
  
  names <- tolower(names)
  
  idspazi = grep("\\W", rownames(vocabolario))
  conspazi = rownames(vocabolario)[idspazi]
  conspazi = conspazi[order(sapply(conspazi, nchar), decreasing = TRUE)]
  for (i in conspazi) {
    names = gsub(i, gsub("\\W", "_", i), names)
  }
  rownames(vocabolario)[idspazi] = gsub("\\W", "_", rownames(vocabolario)[idspazi])
  

  classifica <- function(class,how_class){
    
    if(how_class=="modeFirst"){
      ct <- table(class)
      return(ifelse(sum(ct==max(ct))==1, names(ct)[which.max(ct)], class[1]))
    }
    
    if(how_class=="first") return(class[1])
    
    if(how_class=="last") return(class[length(class)])
    
  }
  
  
  ## prima classificazione
  txt1 <- names
  classificazione <- sapply(txt1, function(txt){
    cl <- as.character(vocabolario[match(strsplit(txt, split="[[:punct:][:space:]]")[[1]], row.names(vocabolario)), "categoria"])
    cl <- cl[!is.na(cl)]
    return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
  }
  )
  
  ## seconda classificazione su NA della prima
  na1 <- is.na(classificazione)
  txt2 <- nomi_originali[na1]
  txt2 <- gsub('([[:upper:]])',  ' \\1', txt2)
  txt2 <- tolower(txt2)
  txt2 <- gsub("\\d", "", txt2)
  classificazione[which(na1)] <- sapply(txt2, function(txt){
    cl <- as.character(vocabolario[match(strsplit(txt, "\\W")[[1]], row.names(vocabolario)), "categoria"])
    cl <- cl[!is.na(cl)]
    return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
  }
  )
  
  ## terza classificazione, se scan_interno==T
  if(scan_interno){
    na2 <- is.na(classificazione)
    txt3 <- nomi_originali[na2]
    txt3 <- tolower(txt3)
    classificazione[which(na2)] <- sapply(txt3, function(txt){
      cl <- as.character(vocab_interno$categoria[sapply(ifelse(substr(rownames(vocab_interno),1,1)==".",paste0("\\",rownames(vocab_interno)),rownames(vocab_interno)), function(x) grepl(x, txt))])
      return(ifelse(length(cl)==0,NA,classifica(cl, how_class)))
    }
    )
  }
  
  return(unlist(classificazione))
  
}
