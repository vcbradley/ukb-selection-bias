library(data.table)
library(ggplot2)
library(gridExtra)

setwd('~/github/mini-project-1/simulation/')
list.files('results')

which_sim = 'sim_5000_1'

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
  plot = ggplot(data, aes(x = get(x_var))) + theme_light()
  
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
    plot = marrangeGrob(subplots, ncol = ncol, nrow = nrow, top=paste0('Weighted by raw brain volume error', ifelse(!is.null(extra_title), extra_title, '')))
    
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


#############################
# PLOT Error by sample size #
#############################

error_melted = melt(accuracy, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'), measure.vars = names(accuracy)[grepl('error', names(accuracy))])
outcome_melted = melt(accuracy, id.vars = c('prop_sampled', 'var', 'level', 'sim_num'), measure.vars = names(accuracy)[grepl('brainvol', names(accuracy))])


plot_error_by_sampsize = ggplot(error_melted[var == 'has_t1_MRI' & variable != 'samp_error']
                                , aes(x = factor(round(prop_sampled * max(accuracy$pop_count)))
                                      , y = value, color = factor(round(prop_sampled * max(accuracy$pop_count))))) + 
  geom_boxplot() +
  facet_grid(. ~ gsub("_error","",variable)) +
  xlab('Sample size') + ylab('Weighted error') + ggtitle('Error by sample size') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_light() +
  labs(color = "N") 
ggsave(filename = paste0(plot_dir, '/error_by_sample_size_method.pdf'), plot = plot_error_by_sampsize, device = 'pdf', width = 10, height = 5)


summary(lm(rake_error ~ samp_error, data = accuracy))



plotMSE = function(data, methods, x_axis = 'pop_prop'){
  plot = ggplot(data, aes(x = get(x_axis)))
  
  plot = plot + unlist(lapply(methods, function(m){
    geom_point(aes(y = get(paste0(m, '_brainvol_mse')), color = m, group = prop_sampled))
  }))
  
  plot = plot + ylab('Weighted error') + ggtitle('Weighted error')
  plot = plot + xlab(if(x_axis == 'samp_prop') "Samp %" else if(x_axis == 'pop_prop') "Pop %" else x_axis )
  
  return(plot)
}


print(plotMSE(mse, methods = methods, x_axis = 'samp_prop'))



accuracy[var == 'has_t1_MRI', ]



#######################
# PLOT Subgroup error #
#######################

plotSubgroupError = function(data, methods, p, extra_title){
  subplots = lapply(methods, function(m){
    plot = ggplot(data, aes(x = samp_brainvol_error, y = get(paste0(m, '_brainvol_mse')), size = pop_prop)) + 
      stat_density_2d(aes(fill = ..level..), geom = "polygon") +
      xlab('Sample error') + ylab('Weighted log(MSE)') +
      ggtitle(m)
  })
  
  ncol = ceiling(sqrt(length(methods)))
  nrow = ceiling(length(methods)/ncol)
  plot = marrangeGrob(subplots, ncol = ncol, nrow = nrow, top=paste0('Subgroup brain volume error', ifelse(!is.null(extra_title), extra_title, '')))
  
  return(plot)
}



for(p in all_props){
  plot_subgroup_err = plotSubgroupError(data = mse[prop_sampled == p & pop_prop < 0.25,], methods = methods, extra_title = paste0('\nprop sampled = ', p))
  ggsave(filename = paste0(plot_dir, '/plot_subgroup_err_', p, '.pdf'), plot = plot_subgroup_err, device = 'pdf', width = 10, height = 6, units = 'in')
}


ggplot(accuracy[prop_sampled == 0.02, .(pop_prop = min(pop_prop)
                                        , rake_error = log(sum(rake_error ^ 2))
                      , strat_error = log(sum(strat_error ^ 2))
                      , calib_error = log(sum(calib_error ^ 2))
                      , lasso_error = log(sum(lasso_error ^ 2))
                      , logit_error = log(sum(logit_error ^ 2))
                      , bart_error = log(sum(bart_error ^ 2))
                      )
                , by = .(var, level, prop_sampled)]) + 
  geom_point(aes(x = pop_prop, y = rake_error)) +
  geom_point(aes(x = pop_prop, y = strat_error)) +
  geom_point(aes(x = pop_prop, y = calib_error)) +
  geom_point(aes(x = pop_prop, y = lasso_error)) +
  geom_point(aes(x = pop_prop, y = logit_error)) +
  geom_point(aes(x = pop_prop, y = bart_error))








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




mse[var == 'has_t1_MRI', .(prop_sampled
                           , rake_brainvol_mse
                           , strat_brainvol_mse
                           , calib_brainvol_mse
                           , lasso_brainvol_mse
                           , logit_brainvol_mse
                           , bart_brainvol_mse
)]

ggplot(mse[var == 'has_t1_MRI']) + 
  geom_line(aes(x = prop_sampled, y = rake_brainvol_mse, color = 'rake')) +
  geom_line(aes(x = prop_sampled, y = strat_brainvol_mse, color = 'strat')) +
  geom_line(aes(x = prop_sampled, y = calib_brainvol_mse, color = 'calib')) +
  geom_line(aes(x = prop_sampled, y = lasso_brainvol_mse, color = 'lassp')) +
  geom_line(aes(x = prop_sampled, y = logit_brainvol_mse, color = 'logit')) +
  geom_line(aes(x = prop_sampled, y = bart_brainvol_mse, color = 'bart'))



###########################
# PLOT var x prop sampled #
###########################

plot_var_x_prop_sampled = ggplot(variance[, lapply(.SD, mean), .SDcols = grepl('var', names(variance)), by = prop_sampled]) + 
  xlab('Proportion sampled') +
  ylab('Weight variance') +
  ggtitle("Variance by proportion sampled")+
  theme_classic() +
  geom_line(aes(x = prop_sampled, y = var_rake, color = 'rake')) +
  geom_line(aes(x = prop_sampled, y = var_strat, color = 'strat')) +
  geom_line(aes(x = prop_sampled, y = var_calib, color = 'calib')) +
  geom_line(aes(x = prop_sampled, y = var_lasso, color = 'lasso')) +
  geom_line(aes(x = prop_sampled, y = var_logit, color = 'logit')) +
  geom_line(aes(x = prop_sampled, y = var_bart, color = 'bart')) 

ggsave(filename = paste0(plot_dir, '/plot_var_x_prop_sampled.pdf'), plot = plot_var_x_prop_sampled, device = 'pdf', width = 10, height = 6, units = 'in')



###################
# PLOT var by MSE #
###################

variance_melted = melt(variance, id.vars = c('prop_sampled', 'sim_num'))
variance_melted = variance_melted[value > 0, .(Var = mean(value)), by = .(prop_sampled, variable)]
variance_melted[, variable := gsub('var_', '', variable)]

mse_melted = melt(mse[var == 'has_t1_MRI', grepl('prop_sampled|mse', names(mse)), with = F], id.vars = c('prop_sampled'), value.name = 'MSE')
mse_melted[, variable := gsub('_brainvol_mse', '', variable)]

var_and_mse = merge(mse_melted, variance_melted, by = c('prop_sampled', 'variable'))


plot_var_by_MSE = ggplot(var_and_mse, aes(x = MSE, y = Var, color = variable)) + 
  geom_line() + theme_classic() + 
  xlab('Log(MSE)') + ylab('Weight variance') +
  ggtitle('Variance by MSE')

ggsave(filename = paste0(plot_dir, '/plot_var_by_MSE.pdf'), plot = plot_var_by_MSE, device = 'pdf', width = 10, height = 6, units = 'in')


var_and_mse[order(variable)]

