% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classificaUtenti.R
\name{classificaUtenti}
\alias{classificaUtenti}
\alias{dizionario_nomi_propri}
\alias{dizionario_luoghi}
\title{Classificazione per vocabolario di utenti}
\usage{
classificaUtenti(
  names,
  vocabolario = NULL,
  scan_interno = FALSE,
  vocab_interno = NULL,
  how_class = "modeFirst",
  cat_interna = NULL
)
}
\arguments{
\item{names}{Vettore di nomi.}

\item{vocabolario}{\code{data.frame} di una colonna con la classificazione
da associare. I \code{rownames(vocabolario)} devono essere unici (sono i
nomi unici su cui viene fatto il controllo). Il vocabolario fornito da noi
e' \code{data(dizionario_nomi_propri)}. ATTENZIONE: Nel \code{vocabolario}
usare solo lower-case e non usare mai "NA" ("na" e' valido).}

\item{scan_interno}{Se \code{TRUE}, la funzione effettua anche la parte di
scanning interno (\code{grepl} sui vocaboli passati.}

\item{vocab_interno}{Il vocabolario su cui viene effettuato (eventualmente)
lo scanning interno (di default, se previsto, e' pari a tutti i nomi di vocabolario
di lunghezza >= 5 caratteri).}

\item{how_class}{Gestisce la classificazione in casi "ambigui", ovvero in cui per
un nome vengano ritrovati piu' match; sono stati pensati tre casi:
\code{modeFirst} (default), in cui viene presa la categoria modale tra le riconosciute e,
nel caso di multimodalita', il primo in ordine tra i match; \code{first}, che classifica
con il primo match nella stringa, e \code{last} che classifica con l'ultimo.}

\item{cat_interna}{\code{NULL} (default) permette di identificare una o piu' categorie della classificazione
per cui vengono tenuti nello scanning interno tutti i termini di vocabolario e non solo
quelli con 5 o piu' caratteri.}
}
\value{
Restituisce un named vector con elementi dalla colonna
\code{categoria} del data.frame \code{vocabolario}. Per
\code{vocabolario=dizionario_nomi_propri} le modalita' sono
\code{c('masc','femm','ente')}.
}
\description{
Associa i nomi in \code{names} ai valori indicati da un vocabolario. Ad esempio,
\code{data(dizionario_nomi_propri)} assegna il genere e \code{data(dizionario_luoghi)}
l'area geografica (vedi esempio).
}
\details{
Per il \code{data(dizionario_luoghi)} sono stati esclusi i paesi Re (800
abitanti, Nord-ovest) e Lu (1200 abitanti, Nord-ovest) perche' in conflitto
con le sigle delle province.

(update 05-09-2016) Ponendo particolare attenzione alla detection dei nomi propri,
si e' pensato di introdurre nella funzione tre diversi "tentativi" di classificazione.
Mentre nei primi due, con diverse accortezze, si cercano parole suddivise da spazi, nel
terzo ed ultimo step si prova a ricercare le stringhe di caratteri anche internamente alle parole
grazie alla funzione \code{grepl}. Alcuni parametri, descritti successivamente, regolano
questa parte della classificazione.
}
\examples{

\dontrun{data(dizionario_nomi_propri)}
\dontrun{str(dizionario_nomi_propri)}
classificaUtenti(c('livio','alessandra','alessandraRossi', 'mariobianchi'), scan_interno=TRUE)
\dontrun{data(dizionario_luoghi)}
classificaUtenti(c('Bosa','Pordenone, Italy'), dizionario_luoghi)

}
\author{
Mattia Uttini, Livio Finos, Andrea Mamprin, Dario Solari
}
\keyword{~kwd1}
\keyword{~kwd2}
