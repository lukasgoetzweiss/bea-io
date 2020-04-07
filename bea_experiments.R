# functions to access BEA data

# this gives some metadata on table names
# https://apps.bea.gov/api/data/?&UserID={user_id}&method=GetParameterValues&DataSetName=InputOutput&ParameterName=TableID&ResultFormat=xml

library(bea.R)
library(data.table)
library(stringr)
library(glue)
library(ggplot2)

#### data functions ####

bea_url = function(){
  str_c(
    "https://apps.bea.gov/api/data/?&",
    "UserID={user_id}&method=GetData&DataSetName=InputOutput&Year=All&",
    "tableID={table_id}&ResultFormat=xml"
  )
}

load_bea = function(table_id, user_id){
  bea.R::bea2Tab(httr::GET(glue(bea_url())))
}

get_bea_years = function(bea_data){
  as.numeric(str_sub(str_subset(names(bea_data), "DataValue"), -4, -1))
}

bea_make_sym = function(bea_data){
  incl_vals = bea_data[, intersect(RowDescr, ColDescr)]
  return(bea_data[RowDescr %in% incl_vals & ColDescr %in% incl_vals])
}

#### summary methods ####

get_trend = function(bea_data, by_col){
  
  res = NULL
  for(y in get_bea_years(bea_data)){
    y.data = bea_data[
      , .(value = sum(get(glue("DataValue_{y}")))),
      .(gp = get(by_col))
    ]
    
    y.data[, year := y]
    
    res = rbind(res, y.data)
  }
  res
}

#### eigen methods ####

io_eigen = function(bea_data, year){
  
  io.tbl = data.table(dcast(
    bea_data, 
    RowDescr ~ ColDescr, 
    value.var = str_c("DataValue_", year),
    fill = 0
  ))
  
  io.tbl = as.matrix(io.tbl[, !c(
    "RowDescr"
  )])

  # io.tbl_n = t(diag(1 / rowSums(io.tbl)) %*% io.tbl)
  
  # io.tbl_n = t(io.tbl %*% diag(1 / colSums(io.tbl)))
  
  io.tbl_n = io.tbl %*% diag(1 / colSums(io.tbl))
  
  io.eig = eigen(io.tbl_n)
  
  io.evec = io.eig$vectors[, 1]
  
  if(max(abs(Im(io.evec))) > 0){
    warning(str_c("Ignoring has imaginary component (<",
                  max(abs(Im(io.evec))), ")"))
  }
  
  return(data.table(
    industry = colnames(io.tbl),
    eig_val = Re(abs(io.evec))
  ))
  
}

io_eigen_trend = function(bea_data){
  bea.eigen_trend = NULL
  for(y in get_bea_years(bea_data)){
    y.eigen = io_eigen(bea_data, y)
    y.eigen[, year := y]
    bea.eigen_trend = rbind(bea.eigen_trend, y.eigen)
  }
  return(bea.eigen_trend)
}

io_eigen_yy = function(io_eigen_trend){
  yy_change = NULL
  y_ax = sort(get_bea_years(bea_data))
  for(y in y_ax[2:length(y_ax)]){
    yy = merge(
      io_eigen_trend[year == y - 1],
      io_eigen_trend[year == y],
      by = "industry",
      suffixes = c(".ly", "")
    )
    
    yy.disim = yy[, 1 - sum(abs(Re(eig_val.ly)) * abs(Re(eig_val)))]
    
    yy_change = rbind(
      yy_change,
      data.table(year = y, eigen_disimilarity = yy.disim)
    )
  }
  return(yy_change)
}

#### dot product methods ####

cos_disim_yy = function(bea_data, year, year_ = year - 1){
  
  yy = merge(
    bea_data[, .(RowCode, ColCode, y = get(glue("DataValue_{year}")))],
    bea_data[, .(RowCode, ColCode, y_ = get(glue("DataValue_{year_}")))]
  )
  
  # THERE SHOULD PROBABLY BE SOME NORMALIZATION HERE
  1 - yy[, sum(y * y_)] / ( sqrt(yy[, sum(y^2)]) * sqrt(yy[, sum(y_^2)]) )
  
}

cos_disim_trend = function(bea_data){
  disim_trend = NULL
  y_ax = sort(get_bea_years(bea_data))
  for(y in y_ax[2:length(y_ax)]){
    disim_trend = rbind(
      disim_trend,
      data.table(
        year = y,
        disim = cos_disim_yy(bea_data, y, y - 1)
      )
    )
  }
  disim_trend
}


#### example ####

if(F){
  
  # pull data
  bea_data = load_bea(258, readLines("~/Documents/Research/econ/bea/bea.key"))
  
  bea_data = bea_data[complete.cases(bea_data)]
  
  # plot I/O table
  ggplot(bea_data, aes(ColDescr, RowDescr, fill = DataValue_2018)) + 
    geom_raster() + 
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_fill_distiller(palette = "Spectral", trans = "log10")
  
  # Plot row and column totals over time
  ggplot(mapping = aes(year, value, color = gp)) + 
    geom_line(data = get_trend(bea_data, "RowDescr"), 
              aes(linetype = "Spent On")) + 
    geom_line(data = get_trend(bea_data, "ColDescr"), 
              aes(linetype = "Spent By")) + 
    facet_wrap(~gp, scale = "free") + 
    guides(color = F)
  
  # I don't understand why these don't add up ...
  merge(bea_data[, .(rowSum = sum(DataValue_2018)), .(ix = RowDescr)],
        bea_data[, .(colSum = sum(DataValue_2018)), .(ix = ColDescr)],
        all = T)[, sum(rowSum, na.rm = T) - sum(colSum, na.rm = T)]
  
  # eigen experiments
  
  # plot first eigenvector over time
  ggplot(io_eigen_trend(bea_make_sym(bea_data)), 
         aes(year, eig_val, color = industry)) +
    geom_line() +
    facet_wrap(~industry, scale = "free") +
    guides(color = F) +
    xlab("") +
    ylab("Relative Centrality") +
    theme(strip.background = element_blank())
  
  gg.eigen = io_eigen_trend(bea_make_sym(bea_data))
  
  ggplot(merge(gg.eigen[year == 2007, .(industry, val.pre = eig_val)], 
               gg.eigen[year == 2010, .(industry, val.post = eig_val)]),
    aes(val.pre, val.post)) +
    geom_point() + 
    geom_text(aes(label = industry), check_overlap = T)
  
  gg.delta = merge(
    gg.eigen[year == 2007, .(industry, val.pre = eig_val)], 
    gg.eigen[year == 2017, .(industry, val.post = eig_val)]
  )
  
  ggplot(gg.delta[, .(val.pre, val.post, 
                      industry = reorder(industry, val.post - val.pre))],
         aes(industry, (val.post - val.pre) )) +
    geom_col() + 
    coord_flip()
  
  View(
    cbind(gg.eigen[year == 2007][order(-eig_val), !"year"],
          gg.eigen[year == 2010][order(-eig_val), !"year"])
  )
  
  gg.eigen_disim = io_eigen_yy(io_eigen_trend(bea_data))
  gg.cosin_disim = cos_disim_trend(bea_data)
  
  gg.eigen_disim[, eigen_disimilarity := eigen_disimilarity / max(eigen_disimilarity)]
  
  ggplot(mapping = aes(year, d)) + 
    geom_line(data = gg.eigen_disim[, .(year, d = eigen_disimilarity)],
              aes(color = "Eigen")) + 
    geom_line(data = gg.cosin_disim[, .(year, d = disim / max(disim))], 
              aes(color = "Cosine"))
  
}
