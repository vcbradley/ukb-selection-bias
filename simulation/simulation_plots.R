library(data.table)
library(ggplot2)
library(gridExtra)

setwd('~/github/mini-project-1/simulation/')
list.files('results')

which_sim = 'sim_1_5000_1'

load(paste0('results/', which_sim, '/results_summary.rda'))

#set directory for plots
plot_dir = paste0('results/', which_sim, '/plots')

#get list of methods
methods = gsub('_brainvol','',names(accuracy)[grepl('brainvol', names(accuracy))])
methods = methods[-which(methods %in% c('pop', 'samp'))]

all_props = unique(accuracy[, prop_sampled])

accuracy[, .N, prop_sampled]

##############################
# PLOT Weighted v. raw error #
##############################

plotError = function(data, methods, plot_style = 'overlap', x_var = 'samp_error', extra_title = NULL){
  plot = ggplot(data, aes(x = get(x_var)))
  
  if(plot_style == 'tiled'){
    
    ymin = min(data[, lapply(.SD, min), .SDcols = paste0(methods, '_error')])
    ymax = max(data[, lapply(.SD, max), .SDcols = paste0(methods, '_error')])
    
    subplots = lapply(methods, function(m){
      plot + geom_point(aes(y = get(paste0(m, '_error')))) + 
        ylab('Weighted error') + 
        xlab('Raw error') + 
        ggtitle(m) + 
        ylim(ymin, ymax)
    })
    
    plot = plot + theme(legend.position = "none")
    
    ncol = ceiling(sqrt(length(methods)))
    nrow = ceiling(length(methods)/ncol)
    plot = marrangeGrob(subplots, ncol = ncol, nrow = nrow, top=paste0('Weighted error by raw sample error', ifelse(!is.null(extra_title), extra_title, '')))
    
  }else{
    
    plot = plot + unlist(lapply(methods, function(m){
      geom_point(aes(y = get(paste0(m, '_error')), color = m))
    }))
    
    plot = plot + geom_hline(aes(yintercept = 0), lty = 2, color = 'grey')
    
    plot = plot + ylab('Weighted error') + xlab('Raw error')
  }

  return(plot)
}


#print(plotError(accuracy[var == 'has_t1_MRI' & prop_sampled == '0.5',], methods = methods))


### Create plots
for(p in all_props){
  plot = plotError(accuracy[var == 'has_t1_MRI'& prop_sampled == p,], methods = methods, plot_style = 'tiled', extra_title = paste0('\nprop sampled = ', p))
  ggsave(paste0(plot_dir, '/plot_error_', p, '.pdf'), plot = plot, height = 6, width = 10, units = 'in', device = 'pdf')
  
}

error_melted = melt(accuracy, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'), measure.vars = names(accuracy)[grepl('error', names(accuracy))])
outcome_melted = melt(accuracy, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'), measure.vars = names(accuracy)[grepl('brainvol', names(accuracy))])


plot_error_by_sampsize = ggplot(error_melted[var == 'has_t1_MRI' & variable != 'samp_error']
                                , aes(x = factor(round(prop_sampled * max(accuracy$pop_count))), y = value, color = factor(round(prop_sampled * max(accuracy$pop_count))))) + 
  geom_boxplot() +
  facet_grid(. ~ gsub("_error","",variable)) +
  xlab('Sample size') + ylab('Weighted error') + ggtitle('Error by sample size') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(color = "N")
ggsave(filename = paste0(plot_dir, '/error_by_sample_size_method.pdf'), plot = plot_error_by_sampsize, device = 'pdf', width = 10, height = 5)


summary(lm(rake_error ~ samp_error, data = accuracy))



plotMSE = function(data, methods, x_axis = 'pop_prop'){
  plot = ggplot(data, aes(x = get(x_axis)))
  
  plot = plot + unlist(lapply(methods, function(m){
    geom_line(aes(y = get(paste0(m, '_brainvol_mse')), color = m))
  }))
  
  plot = plot + ylab('Weighted error') + ggtitle('Weighted error')
  plot = plot + xlab(if(x_axis == 'samp_prop') "Samp %" else if(x_axis == 'pop_prop') "Pop %" else x_axis )
  
  return(plot)
}


print(plotMSE(mse[prop_sampled == 0.02], methods = methods, x_axis = 'samp_prop'))



total_error = merge(accuracy[var == 'has_t1_MRI'], variance, by = c('sim_num', 'prop_sampled'))


plot_total_error = lapply(methods, function(m){
  ymin = min(total_error[, lapply(.SD, min), .SDcols = paste0('var_', methods)])
  #ymax = max(total_error[, lapply(.SD, max), .SDcols = paste0('var_', methods)])
  ymax = 0.5
  
  plot = ggplot(total_error, aes(x = get(paste0(m, '_error')), y = get(paste0('var_', m)))) + 
    stat_density_2d(aes(fill = ..level..), geom = "polygon")
  
  plot = plot + ggtitle(m) + xlab(paste0(m, '_error')) + ylab(paste0('var_', m)) + theme(legend.title = element_blank())
  
  plot = plot + ylim(ymin, ymax)
  
  return(plot)
})
marrangeGrob(plot_total_error, nrow = 2, ncol = 3)



# var distributions

plot = ggplot(variance[prop_sampled == 0.5])
plot = plot + unlist(lapply(methods[-which(methods == 'logit')], function(m){
  geom_density(aes(get(paste0('var_', m)), color = m))
}))

print(plot)

