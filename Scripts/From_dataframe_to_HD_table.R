library(HistDAWass)
library(tidyverse)
# from_json_to_HD<-function(a,file="tmp.json"){
#   return 0
# }
# from_HD_to_json<-function(file="tmp.json",MH){
#   return 0
# }

from_DFg_to_MH<-function(DFg,labs_rows,typeH="base",br=20){
  require(tidyverse)
  rows<-length(DFg)
  name_rows<-character()
  name_cols<-DFg[[1]]%>% select(where(is.numeric)) %>% colnames()
  M<-HistDAWass::MatH(nrows = rows,
                      ncols = length(name_cols),
                      varnames = name_cols)
  for (i in 1:rows){
    name_rows<-c(name_rows,
                 as.character((DFg[[i]] %>% dplyr::select(labs_rows) %>% 
                                 unique() %>% pull()))[1])
    for (j in 1:length(name_cols)){
      h<-hist(DFg[[i]] %>% dplyr::select(name_cols[j]) %>% pull())
      x<-h$breaks
      cdf<-cumsum(c(0,h$counts))/sum(h$counts)
      tmp<-HistDAWass::distributionH(x=x,p=cdf)
      M@M[i,j][[1]]<-tmp
    }
  }
  row.names(M@M)<-name_rows
  return(M)
}

 tmp2<-diamonds %>% group_by(color) %>% group_split()
 tmp3<-from_DFg_to_MH(tmp2,br=8,labs_rows = "color")
show(plot(tmp3,type="DENS"))