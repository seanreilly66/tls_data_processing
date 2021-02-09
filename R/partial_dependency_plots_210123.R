library(randomForest)  # for fitting random forests
library(pdp)           # for partial dependence plots
library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr)

r_data_tls <- load('D:/Analyses/rf_train_rdata/ladder_fuels_rf_train_TLS.RData')


tls_1to2 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to2", plot = TRUE,
              plot.engine = "ggplot2")
tls_1to2

tls_1to3 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to3", plot = TRUE,
                   plot.engine = "ggplot2")
tls_1to3

tls_7to8 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_7to8", plot = TRUE,
                   plot.engine = "ggplot2")
tls_7to8

tls_plots <- ggarrange(tls_1to2,tls_1to3, tls_7to8, ncol=3, nrow=1)
tls_plots

ggsave('D:/Analyses/output/partial_plots_tls.png', 
       plot = tls_plots, 
       width = 13,
       height = 4,
       units = 'in',
       dpi=300)

tls_var <- varImp(rfTrain)

r_data_als <- load('D:/Analyses/rf_train_rdata/ladder_fuels_rf_train_ALS.RData')


als_1to2 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to2", plot = TRUE,
                         plot.engine = "ggplot2")
als_1to2

als_1to3 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to3", plot = TRUE,
                         plot.engine = "ggplot2")
als_1to3

als_7to8 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_7to8", plot = TRUE,
                         plot.engine = "ggplot2")
als_7to8

als_plots <- ggarrange(als_1to2,als_1to3, als_7to8, ncol=3, nrow=1)
als_plots

als_var <- varImp(rfTrain)


r_data_uas <- load('D:/Analyses/rf_train_rdata/ladder_fuels_rf_train_UAS.RData')


uas_1to2 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to2", plot = TRUE,
                         plot.engine = "ggplot2")
uas_1to2

uas_1to3 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to3", plot = TRUE,
                         plot.engine = "ggplot2")
uas_1to3

uas_7to8 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_7to8", plot = TRUE,
                         plot.engine = "ggplot2")
uas_7to8

uas_plots <- ggarrange(uas_1to2,uas_1to3, uas_7to8, ncol=3, nrow=1)
uas_plots

uas_var <- varImp(rfTrain)

r_data_zeb <- load('D:/Analyses/rf_train_rdata/ladder_fuels_rf_train_ZEB.RData')


zeb_1to2 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to2", plot = TRUE,
                         plot.engine = "ggplot2")
zeb_1to2

zeb_1to3 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to3", plot = TRUE,
                         plot.engine = "ggplot2")
zeb_1to3

zeb_7to8 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_7to8", plot = TRUE,
                         plot.engine = "ggplot2")
zeb_7to8

zeb_plots <- ggarrange(zeb_1to2,zeb_1to3, zeb_7to8, ncol=3, nrow=1)
zeb_plots

zeb_var <- varImp(rfTrain)

r_data_banner <- load('D:/Analyses/rf_train_rdata/ladder_fuels_rf_train_Banner.RData')


banner_1to2 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to2", plot = TRUE,
                         plot.engine = "ggplot2")
banner_1to2

banner_1to3 <- pdp::partial(rfTrain, pred.var = "ladder_fuel_1to3", plot = TRUE,
                         plot.engine = "ggplot2")
banner_1to3

banner_plots <- ggarrange(banner_1to2,banner_1to3,  ncol=3, nrow=1)
banner_plots

banner_var <- varImp(rfTrain)

all_plots <- ggarrange(tls_plots, als_plots, uas_plots, zeb_plots, banner_plots,  ncol=1, nrow=5)
all_plots

ggsave('D:/Analyses/output/partial_plots_all_methods.png', 
       plot = all_plots, 
       width = 13,
       height = 20,
       units = 'in',
       dpi=300)

tls_var_plot <-  plot(tls_var, top = 3)
als_var_plot <-  plot(als_var, top = 3)
uas_var_plot <-  plot(uas_var, top = 3)
zeb_var_plot <-  plot(zeb_var, top = 3)
banner_var_plot <-  plot(banner_var, top = 3)

all_var_plots <- ggarrange(tls_var_plot, als_var_plot, uas_var_plot, zeb_var_plot, banner_var_plot,  ncol=1, nrow=5)
all_var_plots

ggsave('D:/Analyses/output/variable_importance_all_methods.png', 
       plot = all_var_plots, 
       width = 4,
       height = 20,
       units = 'in',
       dpi=300)

