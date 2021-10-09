library(tidyverse)
library(janitor)
library(lubridate)
library(plotly)

#0 import data 

kiva_df <- read_csv('data/kiva/kiva_clean.csv')
kiva_df %>% sample_n(500) %>% View()

glimpse(kiva_df)


#2 univariate plots
options(scipen = 20)


ggplot(kiva_df)+
  geom_histogram(aes(x = funded_amount), fill = 'steelblue', col = 'grey')+
  theme_bw()+
  ggtitle('Funded amount')+
  scale_x_log10()


ggplot(kiva_df)+
  geom_histogram(aes(x = loan_amount), fill = 'steelblue', col = 'grey')+
  theme_bw()+
  ggtitle('Loan amount')+
  scale_x_log10()

kiva_df$activity %>% unique() %>% length()
kiva_df %>% 
  group_by(activity) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  slice(1:5) %>% 
  ggplot()+
  geom_col(aes(x = activity, y = total))


kiva_df$sector %>% unique() %>% length()
ggplot(kiva_clean)+
  geom_bar(aes(x= sector), fill = 'steelblue', col = 'grey')+
  theme_bw()+
  ggtitle('Sectores')


sector_lvls <- kiva_df %>% 
  group_by(sector) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  pull(sector)



ggplot(kiva_clean)+
  geom_bar(aes(x= factor(sector,levels = sector_lvls )), fill = 'steelblue', col = 'grey')+
  theme_bw()+
  ggtitle('Sectores')

# 3
kiva_clean %>% 
  mutate(tot_recaudado = funded_amount == loan_amount) %>% 
  ggplot()+
  geom_boxplot(aes(x = tot_recaudado, y = loan_amount))+
  scale_y_log10()


#4
kiva_clean %>% 
  group_by(sector, year = year(disbursed_time), month = month(disbursed_time)) %>% 
  summarise(total = sum(funded_amount)) %>% 
  filter(!is.na(year)) %>% 
  mutate(periodo = as.Date(paste(year,month,'01',sep = '-'))) %>% 
  ggplot()+
  geom_line(aes(x = periodo, y =total, col = sector))+
  facet_wrap(~sector,scales = 'free_y')+
  theme_bw()
  
#5




plot_bubbles <- function(x){
  ggplotly(
    kiva_clean %>%
      filter(sector == x) %>%
      group_by(country, repayment_interval, activity) %>%
      summarise(
        tot_loan = sum(loan_amount, na.rm = T),
        borrowers_f = sum(borrowers_f, na.rm = T),
        borrowers_m = sum(borrowers_m, na.rm = T)
      ) %>%
      ggplot(aes(text = country)) +
      geom_point(
        aes(
          x = borrowers_f,
          y = borrowers_m,
          size = tot_loan,
          col = repayment_interval
        ),
        alpha = .6
      ) +
      scale_y_log10() +
      scale_x_log10()+
      theme_bw()
  )
}


plot_bubbles('Agriculture')



# kiva_df <- read_csv('data/kiva/kiva_loans.csv')
# kiva_clean <- kiva_df %>%
#   mutate(borrowers_f = str_count(borrower_genders, 'female'),
#          borrowers_m = str_count(borrower_genders, regex('(?<!e)male'))) %>%
#   select(- tags,- use, -term_in_months, -date, -id, -borrower_genders, -partner_id)
# 
# 
# write_csv(kiva_clean, 'data/kiva/kiva_clean.csv')
