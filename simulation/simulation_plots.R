library(data.table)
library(ggplot2)
library(gridExtra)

setwd('~/github/mini-project-1/simulation/')
list.files('results')

which_sim = 'samples_1_5000_0.25_2019-05-22 22:50:06'

load(paste0('results/', which_sim, '/results_summary.rda'))

#get list of methods
methods = gsub('_brainvol','',names(accuracy)[grepl('brainvol', names(accuracy))])
methods = methods[-which(methods %in% c('pop', 'samp'))]

accuracy[, raw_error := samp_brainvol - pop_brainvol]
accuracy[, paste0(methods, '_error') := lapply(.SD, function(col) col - pop_brainvol), .SDcols = paste0(methods, '_brainvol')]


plotError = function(data, methods, plot_style = 'overlap'){
  plot = ggplot(data, aes(x = raw_error))
  
  if(plot_style == 'tiled'){
    
    ymin = min(data[, lapply(.SD, min), .SDcols = paste0(methods, '_error')])
    ymax = max(data[, lapply(.SD, max), .SDcols = paste0(methods, '_error')])
    
    subplots = lapply(methods, function(m){
      plot + geom_point(aes(y = get(paste0(m, '_error')), color = m)) + 
        ylab('Weighted error') + 
        xlab('Raw error') + 
        ggtitle(m) + 
        ylim(ymin, ymax)
    })
    
    ncol = ceiling(sqrt(length(methods)))
    nrow = ceiling(length(methods)/ncol)
    plot = marrangeGrob(subplots, ncol = ncol, nrow = nrow)
    
  }else{
    
    plot = plot + unlist(lapply(methods, function(m){
      geom_point(aes(y = get(paste0(m, '_error')), color = m))
    }))
    
    plot = plot + geom_hline(aes(yintercept = 0), lty = 2, color = 'grey')
    
    plot = plot + ylab('Weighted error') + xlab('Raw error') + ggtitle('Weighted error by raw sample error')
  }

  
  return(plot)
}

print(plotError(accuracy[var == 'has_t1_MRI',], methods = methods))
print(plotError(accuracy[var == 'has_t1_MRI',], methods = methods, plot_style = 'tiled'))


mse = merge(mse, accuracy[, .(pop_prop = min(pop_prop)
                              , samp_prop = min(samp_count/max(accuracy$samp_count))
                              ), by = .(var, level)], by = c('var','level'))

plotMSE = function(data, methods, x_axis = 'pop_prop'){
  plot = ggplot(data, aes(x = get(x_axis)))
  
  plot = plot + unlist(lapply(methods, function(m){
    geom_line(aes(y = get(paste0(m, '_brainvol_mse')), color = m))
  }))
  
  plot = plot + ylab('Weighted error') + ggtitle('Weighted error')
  plot = plot + xlab(if(x_axis == 'samp_prop') "Samp %" else if(x_axis == 'pop_prop') "Pop %" else x_axis )
  
  return(plot)
}


print(plotMSE(mse, methods = methods, x_axis = 'samp_prop'))

