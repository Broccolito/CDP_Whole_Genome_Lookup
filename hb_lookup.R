library(readxl)

get_directory = function(){
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="
  rstudio <- "RStudio"
  
  match <- grep(rstudio, args)
  if(length(match) > 0){
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  }else{
    match <- grep(file, args)
    if (length(match) > 0) {
      return(dirname(normalizePath(sub(file, "", args[match]))))
    }else{
      return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
    }
  }
}

wd = get_directory()
setwd(wd)

p = read_excel("WG_030720_CDP_MASTER.xlsx")
g = read_excel("SNPs.xlsx")

p_list = vector()
for(i in g$id){
  p_list = rbind(p_list, c(i, mean(p[p$id == i,]$hb, na.rm = TRUE), 
                           p[p$id == i,]$Sex[1],
                           mean(p[p$id == i,]$age, na.rm = TRUE),
                           mean(p[p$id == i,]$BMI, na.rm = TRUE)))
}

pmat = data.frame(id = as.numeric(p_list[,1]), 
           hb = as.numeric(p_list[,2]),
           sex = as.factor(p_list[,3]),
           age = as.numeric(p_list[,4]),
           BMI = as.numeric(p_list[,5]))

pgmat = cbind.data.frame(pmat, g[,3:7])

write.csv(pgmat, file = "physio_gene_mat.csv", quote = FALSE, row.names = FALSE)
