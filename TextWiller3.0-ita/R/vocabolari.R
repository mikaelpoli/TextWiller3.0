#' Vocabolari
#' 
#' TextWiller contiene diversi vocabolari in lingua italiana utili per 
#' l'analisi del testo. Questo file decrive i vocabolari contenuti nel pacchetto 
#' e i loro usi, divisi in base alla loro funzione primaria: sentiment analysis, 
#' normalizzazione del testo, o categorizzazione. 
#' 
#' @name vocabolari
#' @docType data
#' @references
#' Gupta, S., Singh, A., & Ranjan, J. (2021). Emoji Score and Polarity Evaluation 
#' Using CLDR Short Name and Expression Sentiment. Advances in Intelligent Systems 
#' and Computing, 1009-1016. \url{https://doi.org/10.1007/978-3-030-73689-7_95}\cr
#' 
#' Kralj Novak, P., Smailovic, J., Sluban, B., & Mozetic, I. (2015). Sentiment 
#' of Emojis. PLOS ONE, 10(12), e0144296. \url{https://doi.org/10.1371/journal.pone.0144296}\cr
#' 
#' Katherine Roehrick (2020). vader: Valence Aware Dictionary and sEntiment Reasoner 
#' (VADER). R package version 0.2.1. \url{https://CRAN.R-project.org/package=vader}\cr
#' 
#' Tim Loughran & Bill McDonald. (n.d.). Loughran-McDonald Master Dictionary w/ 
#' Sentiment Word Lists [Dataset]. \url{https://sraf.nd.edu/loughranmcdonald-master-dictionary/}\cr
#' 
#' Rinker, T. W. (2018). lexicon: Lexicon Data version 1.2.1. 
#' \url{http://github.com/trinker/lexicon}\cr
#' 
#' Rinker, T. W. (2021). sentimentr: Calculate Text Polarity Sentiment 
#' version 7.1.2. \url{https://github.com/trinker/sentimentr}
#' 
#' @keywords vocabolari, dizionari
#' 
#' @section Sentiment analysis:
#' 
#' \enumerate{
#' \item \code{dizionario_sentiment_ita}:
#' Questo vocabolario contiene 3179 parole italiane e 1853 emoji. \cr
#' Tipo: data.table.
#' \itemize{
#' \item \code{keyword}: Parola italiana o identificativo emoji,
#' quest'ultimo in inglese nella forma "emoji_cldr_short_name". \cr
#' Tipo: character.
#' \item \code{code}: Stringa byte UTF-8 nella forma <xx><xx><xx>, che rappresenta 
#' ciascuna emoji. Per le parole, il suo valore � NA. \cr
#' Tipo: character.
#' \item \code{score}: Polarit� degli item o "punteggio sentiment". Range = -1, 
#' +1 dove -1 = polarit� negativa e +1 = polarit� positiva. 
#' Le parole italiane sono classificate come negative (-1) o positive (+1). 
#' Le emoji sono classificate utilizzando l'intero range di valori 
#' (con 0 = neutrale). Il punteggio sentiment delle emoji � stato calcolato 
#' utilizzando la procedura di Gupta et al. (2021): 1) Il punteggi di 676 emojis 
#' sono stati recuperati da \pkg{lexicon::emojis_sentiment} (Rinker, 2019), 
#' una versione leggermente modificata del dataset di sentiment delle emoji di 
#' Novak et al. (2015); 2) Utilizzando \pkg{VADER} (Roehrick, 2020) � stato calcolato 
#' un ulteriore punteggio sentiment per tutte le 1853 emoji tramite una sentiment 
#' analysis del loro identificativo ("CLDR short name"); 3) Il punteggio 
#' sentiment finale � dato dunque da: il punteggio VADER per le emoji 
#' che non compaiono in \pkg{lexicon::emojis_sentiment} (n = 1177); la media aritmetica 
#' dei punteggi \pkg{VADER} e \pkg{lexicon::emojis_sentiment} per le emoji restanti (n = 676). 
#' La lista di emoji � stata recuperata da: \url{http://www.unicode.org/emoji/charts/full-emoji-list.html}.\cr 
#' Tipo: numeric.
#' \item \code{ngram}: Ordine dell'n-gramma che compare nella variabile "keyword", 
#' calcolato come il numero di parole da cui � composta. \cr
#' Tipo: integer.}
#' 
#' \item \code{dizionario_sentiment_ita_r}: Versione di \code{dizionario_sentiment_ita}
#' compatibile con \pkg{sentimentr::sentiment}.\cr
#' Tipo: data.table.
#' \itemize{
#' \item \code{x}: Parola.
#' \item \code{y}: Sentiment score.}
#' 
#' \item \code{dizionario_loughran_ita}: 
#' Traduzione italiana del vocabolario Loughran-McDonald per la sentiment analysis 
#' di documenti finanziari.\cr
#' Tipo: data.table.
#' \itemize{
#' \item \code{x}: Parola.\cr
#' Tipo: character.
#' \item \code{y}: Polarit� degli item o "punteggio sentiment" (valori: -1 = negativo, 
#' 0 = incerto, +1 = positivo).\cr
#' Tipo: numeric.}}
#' 
#' @section Normalizzazione del testo:
#' 
#' \enumerate{
#' \item \code{dizionario_emoji_id}:
#' Questo dataset contiene 1853 emoji e la loro relativa rappresentazione UTF-8 
#' in byte. E' compatibile con \pkg{textclean::replace_emoji()} e pu� essere utilizzato 
#' come valore dell'argomento \code{emoji_dt} per la normalizzazione del testo, 
#' restituendo il nome inglese delle emoji contenute nel documento nella forma 
#' "emoji_cldr_short_name". Rappresenta un'espansione del dataset \code{lexicon::hash_emoji}.\cr
#' Tipo: data.table. 
#' \itemize{
#' \item \code{x}: Stringa byte UTF-8 nella forma <xx><xx><xx>.\cr
#' Tipo: character.
#' \item \code{y}: Nome emoji inglese nella forma "emoji_cldr_short_name".\cr
#' Tipo: character.}
#' 
#' \item \code{stopwords_ita}: Lista di stopword italiane da utlizzare per la 
#' normalizzazione del testo.\cr
#' Tipo: character.}
#' 
#' @section Categorizzazione:
#' 
#' \enumerate{
#' \item \code{dizionario_luoghi}:
#' Come valore dell'argomento \code{vocabolario} nella funzione 
#' \pkg{TextWiller::classificaUtenti()}, categorizza elementi di vettori di tipo 
#' character contententi nomi di citt� in base alla loro posizione geografica 
#' ("Estero" per quelle non italiane, "Nord-ovest", "Nord-est", "Centro", "Sud", 
#' "Isole" per quelle italiane).
#' 
#' \item \code{dizionario_nomi_propri}:
#' Come valore dell'argomento \code{vocabolario} nella funzione 
#' \pkg{TextWiller::classificaUtenti()}, categorizza elementi di vettori di 
#' tipo character contenenti nomi propri italiani come "Maschio" o "Femmina".}
#' 
NULL