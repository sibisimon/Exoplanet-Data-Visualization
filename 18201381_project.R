library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(viridis)
library(gganimate)
library(gapminder)
library(rstan)

set.seed(18201381)

# Q1
data = read_csv("exo_data.csv")
data <- data %>% mutate(year = as.integer(year), meth = as.factor(meth), flag = as.factor(flag))

# Q2
data <- data %>% filter(!is.na(meth))

# Q3
ggplot(data, aes(x=log(dist), fill=meth)) +
  geom_histogram(alpha = 0.5, position="dodge", bins=15)

# Q4
data$onclick =  sprintf("window.open(\"%s%s\")",
                        "http://www.openexoplanetcatalogue.com/planet/",
                         as.character(data$id))
gg_planet = ggplot(data,
                  aes(x = log(dist),
                      y = log(mass),
                      color = meth)) +
  geom_point_interactive(aes(data_id = id,
                             tooltip = id,
                             onclick = onclick)) +
  facet_grid(. ~ meth, scales = 'free_x') +
  theme(legend.position = 'None') +
  theme_bw() 


ggiraph(code = print(gg_planet))

# Q5

data <- rename(data, jupiter_radius = 'radius')
data$earth_radius <- data$jupiter_radius * 11.2


#Q6
exo_planet_data_cluster <- data[complete.cases(data[, c("period", "earth_radius")]),]
exo_planet_data_with_na <- data[!complete.cases(data[, c("period", "earth_radius")]),]
km_cl <- kmeans(log(exo_planet_data_cluster %>% select(period, earth_radius)), 
                4, nstart = 10)

#Q7

exo_planet_data_cluster$type <- km_cl$cluster
exo_planet_data_cluster$type <- as.factor(exo_planet_data_cluster$type)
exo_planet_data_with_na$type <- 'unknown'
levels(exo_planet_data_cluster$type) <- c('hot_jupiters', 'rocky', 'others', 'cold_gas_giants')
ggplot(data=exo_planet_data_cluster, aes(x=log(period), y=log(earth_radius), color=type)) + 
  geom_point() + 
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = type)) +
  theme_bw()

#Q8

exoplanet_cmplt_data <- rbind(exo_planet_data_cluster, exo_planet_data_with_na)

ggplot(exoplanet_cmplt_data, aes(x=log(mass), fill=type)) +
  geom_histogram(bins=15, colour='black') + 
  labs(title="Histogram of mass by type", x = "Mass") +
  theme_bw()

ggplot(exoplanet_cmplt_data, aes(x=type, y=log(mass), fill=type)) + 
  geom_violin(trim=FALSE,  color="black")+
  labs(title="Plot of mass by type",x="Type", y = "Mass")+
  theme_bw()

#Q9

find_second_val <- function(time){
  as.numeric(time[1]) * 3600 + as.numeric(time[2]) * 60 + as.numeric(time[3])
}

r_asc = lapply(strsplit(exoplanet_cmplt_data$r_asc, " "), find_second_val)
decl = lapply(strsplit(exoplanet_cmplt_data$decl, " "), find_second_val)
celestial_data = tibble(r_asc=unlist(r_asc), decl=unlist(decl))

ggplot(
  celestial_data,
  aes(r_asc, decl)) +
  geom_point() + 
  labs(title="Celestial map",x="Right ascension", y = "Declination") +
  theme_bw()


#Q10

time_series_data = exoplanet_cmplt_data %>% group_by(meth, year) %>% summarise(Freq=n()) %>% na.omit()
time_series_data <- time_series_data %>% group_by(meth) %>% mutate(cum_freq = cumsum(Freq))
ggplot(
  time_series_data,
  aes(year, cum_freq, color=meth)) +
  geom_line(size=1.5) +
  labs(title = 'Times series plot for exoplanet discoverd', x = 'Year', y = 'Frequency of exoplanet discovered till year') +
  transition_reveal(year) +
  theme_bw()

#Q12

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


model_data <- exoplanet_cmplt_data  %>%  select(period, host_mass, host_temp, axis) %>% na.omit()
model_data <- log(model_data)

x <-model_data %>% select(host_mass, host_temp, axis) %>% as.matrix()

exoplanet_data_lr = list(N = nrow(model_data),
                     K = 3,
                     x = x,
                     y = model_data$period)

# Maximum likelihood version
stan_model_lr = stan_model('lin_reg.stan')
stan_run_lr_ml = optimizing(stan_model_lr, data = exoplanet_data_lr)
print(stan_run_lr_ml)


#Q13

stan_model_bin_reg_1 = stan_model('samp.stan')
stan_run_bin_reg_1 = sampling(stan_model_bin_reg_1, data = exoplanet_data_lr)
print(stan_run_bin_reg_1)

#Q14
plot(stan_run_bin_reg_1, par = c("alpha"))
plot(stan_run_bin_reg_1, par = c("beta[1]"))


  


