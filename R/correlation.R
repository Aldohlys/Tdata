# library(corrplot)
# library(purrr)
# portf=c("CCJ","FNV","MS")
# sym_OHLC=getSym(portf)
# x=sym_OHLC[1]
# x1=x[[1]]
# x1[,6]
#
#
# sym_all=reduce(sym_OHLC,\(acc,x) cbind(acc,x) )
# names(sym_all)
# adj_sym=sym_all[,grepl("Adjusted",names(sym_all))]
# colnames(adj_sym)=sub(".Adjusted","",colnames(adj_sym))
