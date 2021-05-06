library(HistDAWass)
library(tidyverse)


from_DFg_to_MH<-function(DFg, # a grouped data.frame or tibble
                         labs_rows, #the name of the variables that defines groups
                         typeH="base",# this will be used when several histogram estimation methods will be available
                         br=20){
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
      h<-hist(DFg[[i]] %>% dplyr::select(name_cols[j]) %>% pull(),plot=FALSE)
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
p1<-plot(tmp3,type="DENS")
show(p1)

## Mix cut and color

tmp5<-diamonds %>% mutate(GR=paste0(cut,color,sep="_")) %>% 
  group_by(GR) %>% group_split()
tmp6<-from_DFg_to_MH(tmp5,br=8,labs_rows = "GR")
p2<-plot(tmp6,type="DENS")
show(p2)
