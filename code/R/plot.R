#############
### Setup ###
#############

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(stringr)
})

title = 'Median Base Salary (2017)'
cap = 'Source: https://php.app.com/agent/rutgersemployees/search\n By @TheRealEveret, inspired by @johnjhorton & @avyfain'
leg_breaks = c("Professor ACD YR",
               "Assoc Professor ACD YR",
               "Asst Professor ACD YR")

plot_salaries = function(data, subtitle) {

  data %>%
    ggplot(mapping = aes(x = med_base_salary,
                         y = reorder(dept,
                                     med_base_salary,
                                     FUN = max))) +
    geom_point(aes(color = title,
                   shape = title),
               size = 2) +
    scale_x_continuous(labels = scales::dollar,
                       limits = c(xmin, xmax)) +
    scale_color_discrete(breaks = leg_breaks) +
    scale_shape_discrete(breaks = leg_breaks) +
    labs(title = title,
         subtitle = subtitle,
         caption = cap,
         x = '', y = '',
         color = 'Rank', shape = 'Rank') +
    theme_bw()

}

collagg = function(data) {

  data %>%
    group_by(dept, title) %>%
    summarize(n = n(),
              med_base_salary = median(base_salary)) %>%
    ungroup() %>%
    arrange(dept, desc(title))

}


######################
### Filter & Clean ###
######################

rutgers = readRDS('./data/base/rutgers.Rds') %>%
  select(-first, -last)

my_rutgers = rutgers %>%
  mutate_at(.vars = vars(base_salary, gross_salary),
            .funs = readr::parse_number) %>%
  # ACD YR professors are most common, & comparable to each other. Use them.
  filter(title %in% c('PROFESSOR I ACD YR', 'ASSOC PROFESSOR ACD YR',
                      'ASST PROFESSOR ACD YR')) %>%
  mutate(
    title = stringr::str_replace_all(string = title,
                                     c('PROFESSOR I' = 'Professor',
                                       'ASSOC PROFESSOR' = 'Assoc Professor',
                                       'ASST PROFESSOR' = 'Asst Professor')),
    dept = stringr::str_to_title(dept)
  )


################
### Colleges ###
################

sas_micro = my_rutgers %>%
  filter(grepl('(?i)sas', dept)) %>%
  mutate(
    dept = str_replace_all(string = dept,
                           pattern = '(?i)sas|dls|\\s*-\\s*',
                           replacement = '')
  ) %>%
  # Drop departments which prob aren't comparable to the others.
  filter(!grepl('(?i)undergrad|dean|ctr|lab|edison|writing|pals|esl',
                dept))

fasn_micro = my_rutgers %>%
  filter(grepl('(?i)fasn', dept)) %>%
  mutate(dept = str_replace_all(dept, '(?i)fasn|\\s*-\\s*', '')) %>%
  filter(!grepl('(?i)writing|honors', x = dept))

fasc_micro = my_rutgers %>%
  filter(grepl('(?i)fasc', dept)) %>%
  mutate(dept = str_replace_all(dept, '(?i)fasc|\\s*-\\s*', ''))


newb_micro = my_rutgers %>%
  filter(grepl('(?i)sas|engn|school of social work|sebs|mgsa|Sc\\&I', dept)) %>%
  mutate(dept = str_replace_all(dept,
                                '(?i)sas|dls|engn|sebs|dept|mgsa|sc\\&i|\\s*-\\s*',
                                '')) %>%
  filter(!grepl('(?i)undergrad|dean|ctr|lab|edison|writing|pals|esl',
                dept))

newa_micro = my_rutgers %>%
  filter(grepl('(?i)fasn|College Of Nursing|School Of Criminal Justice|School Public Affairs \\& Admin',
               dept)) %>%
  mutate(dept = str_replace_all(dept, '(?i)fasn|\\s*-\\s*', '')) %>%
  filter(!grepl('(?i)writing|honors', x = dept))

camd_micro = my_rutgers %>%
  filter(grepl('(?i)fasc|Camden School Of Nursing|Childhood Studies|School Of Business - Cmd',
               dept)) %>%
  mutate(dept = str_replace_all(dept, '(?i)fasc|cmd|camden|\\s*-\\s*', ''))


sas = sas_micro %>%
  collagg()

fasn = fasn_micro %>%
  collagg()

fasc = fasc_micro %>%
  collagg()

newb = newb_micro %>%
  collagg()

newa = newa_micro %>%
  collagg()

camd = camd_micro %>%
  collagg()


############
### Plot ###
############

xmin = bind_rows(sas, fasn, fasc, newb, newa, camd) %>%
  filter(n > 1) %>%
  pull(med_base_salary) %>%
  min() %>%
  `*`(0.99) %>%
  round(digits = -3)

xmax = bind_rows(sas, fasn, fasc, newb, newa, camd) %>%
  filter(n > 1) %>%
  pull(med_base_salary) %>%
  max() %>%
  round(digits = -4)

(
  p_sas = sas %>%
    filter(n > 1) %>%
    plot_salaries(subtitle = 'Rutgers University-New Brunswick, School of Arts & Sciences')
)

(
  p_fasn = fasn %>%
    filter(n > 1) %>%
    plot_salaries(subtitle = 'Rutgers University-Newark, Faculty of Arts & Sciences Newark')
)

(
  p_fasc = fasc %>%
    filter(n > 1) %>%
    plot_salaries(subtitle = 'Rutgers University-Camden, Faculty of Arts & Sciences Camden')
)

(
  p_newb = newb %>%
    filter(n > 1) %>%
    plot_salaries(subtitle = 'Rutgers University-New Brunswick')
)

(
  p_newa = newa %>%
    filter(n > 1) %>%
    plot_salaries(subtitle = 'Rutgers University-Newark')
)

(
  p_camd = camd %>%
    filter(n > 1) %>%
    plot_salaries(subtitle = 'Rutgers University-Camden')
)


##############
### Export ###
##############

write_csv(x = my_rutgers,
          path = './data/processed/rutgers_micro.csv')


write_csv(sas_micro,
          './data/processed/sas_micro.csv')

write_csv(fasn_micro,
          './data/processed/fasn_micro.csv')

write_csv(fasc_micro,
          './data/processed/fasc_micro.csv')

write_csv(newb_micro,
          './data/processed/newb_micro.csv')

write_csv(newb_micro,
          './data/processed/newa_micro.csv')

write_csv(newb_micro,
          './data/processed/camd_micro.csv')


write_csv(sas,
          './data/processed/sas.csv')

write_csv(fasn,
          './data/processed/fasn.csv')

write_csv(fasc,
          './data/processed/fasc.csv')

write_csv(newb,
          './data/processed/newb.csv')

write_csv(newb,
          './data/processed/newa.csv')

write_csv(newb,
          './data/processed/camd.csv')


ggsave(plot = p_sas,
       file = './graphics/sas.png',
       height = 10, width = 14)

ggsave(plot = p_fasn,
       file = './graphics/fasn.png',
       height = 10, width = 14)

ggsave(plot = p_fasc,
       file = 'graphics/fasc.png',
       height = 10, width = 14)

ggsave(plot = p_newb,
       file = 'graphics/newb.png',
       height = 10, width = 14)

ggsave(plot = p_newa,
       file = 'graphics/newa.png',
       height = 10, width = 14)

ggsave(plot = p_camd,
       file = 'graphics/camd.png',
       height = 10, width = 14)
