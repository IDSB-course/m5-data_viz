library(tidyverse)
library(janitor)

iris %>% 
  clean_names() %>% 
  filter(species == 'versicolor') %>% 
  ggplot(aes(x = sepal_length, y = sepal_width))+
  geom_point()+
  geom_smooth(method = 'lm',se = F, col = 'red')+
  theme_classic()



iris %>% 
  clean_names() %>% 
  filter(species == 'versicolor') %>% 
  summarise(sepal_length_mean = mean(sepal_length),
            sepal_length_sd = sd(sepal_length),
            sepal_width_mean = mean(sepal_width),
            sepal_width_sd = sd(sepal_width),
            cor = cor(sepal_length,sepal_width)) %>% t()



cor(iris %>% 
      filter(Species == 'versicolor') %>% 
      pull(Sepal.Length),
    iris %>% 
      filter(Species == 'versicolor') %>% 
      pull(Sepal.Width))



data.frame(A= rnorm(6),
           B = rnorm(6),
           Z = rnorm(6))



install.packages('gapminder')
library(gapminder)

gapminder


glimpse(gapminder)



ggplot(gapminder, aes(x = lifeExp))+
  geom_histogram()

ggplot(gapminder, aes(x = pop ))+
  geom_histogram()

ggplot(gapminder, aes(x = gdpPercap ))+
  geom_histogram()


ggplot(gapminder, aes(x = gdpPercap ))+
  geom_density()

ggplot(gapminder, aes(x = gdpPercap ))+
  geom_boxplot()

ggplot(gapminder, aes(x  ='',y = gdpPercap ))+
  geom_violin()


ggplot(gapminder, aes(x = continent))+
  geom_bar()

ggplot(gapminder, aes(x = year))+
  geom_bar()


ggplot(gapminder, aes(x = as.factor(year)))+
  geom_bar()

ggplot(gapminder, aes(x = lifeExp, y = gdpPercap))+
  geom_point(alpha = .1)


options(scipen = 20)
ggplot(gapminder, aes(x = lifeExp, y = gdpPercap))+
  geom_point(alpha = .1, col = 'steelblue')+
  geom_smooth(method = 'lm', se = FALSE, col = 'red')+
  scale_y_log10()+
  xlab('Expectativa de Vida')+
  ylab('PBI per cápita')+
  ggtitle('PBI per cap. y Exp de Vida')+
  theme_bw()


ggplot(gapminder, aes(x = lifeExp, y = gdpPercap))+
  geom_density_2d()

ggplot(gapminder, aes(x = lifeExp, y = gdpPercap))+
  geom_bin2d()+
  scale_fill_viridis_b()


gapminder %>% 
  filter(country == 'Argentina') %>% 
  ggplot(aes(x = year, y = gdpPercap))+
  geom_line()

gapminder %>% 
  group_by(year, continent) %>% 
  summarise(gdpPercap = median(gdpPercap)) %>% 
  ggplot(aes(x = year, y = gdpPercap))+
  geom_line()+
  facet_wrap(~continent,scales = 'free_y')


gapminder %>% 
  filter(country == 'Argentina') %>% 
  ggplot(aes(x = as.factor(year), y = gdpPercap, group = 1))+
  geom_line(col = 'steelblue', size = 1)+
  geom_point()+
  xlab('Año')+
  ylab('Pbi per Cápita')+
  ggtitle('Evolución Pbi per cápita en Argentina')+
  theme_bw()



gapminder %>% 
  filter(country == 'Argentina') %>% 
  ggplot(aes(x = year, y = gdpPercap))+
  geom_line(aes(size = pop))
  