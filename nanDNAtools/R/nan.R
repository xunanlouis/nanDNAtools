#' DNA sequences were coded as protein sequences
#'
#' @param DNASEQ DNA sequences
#' @param splits Whether to divide
#'
#' @return
#' @export
#'
#' @examples
DNA_to_prt<-function(DNASEQ=NULL,splits=F){
  DNASEQ<-toupper(DNASEQ)
  seqn<-nchar(DNASEQ)
  seq3split<-substring(DNASEQ,
                       first = seq(from=1,to=seqn,by=3),
                       last = seq(from=1,to=seqn,by=3)+2)
  if (nchar(tail(seq3split,1))<3) return(warning("Incomplete sequence"))
  seqprt_fac<-factor(seq3split,
                     levels = codon_all_data$codon,
                     labels = codon_all_data$protein)
  seqprt_str<-as.character(seqprt_fac)
  if (splits) {
    return(seqprt_str)
  }else{
    return(paste(seqprt_str,collapse = ""))
  }
}



#' Title Codon splitting of DNA sequences
#'
#' @param DNASEQ DNA sequences
#'
#' @return
#' @export
#'
#' @examples
DNA_to_split<-function(DNASEQ=NULL){
  DNASEQ<-toupper(DNASEQ)
  seqn<-nchar(DNASEQ)
  seq3split<-substring(DNASEQ,
                       first = seq(from=1,to=seqn,by=3),
                       last = seq(from=1,to=seqn,by=3)+2)
  if (nchar(tail(seq3split,1))<3) return(warning("Incomplete sequence"))
  return(seq3split)
}

