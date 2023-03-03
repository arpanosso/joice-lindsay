
library(tidymodels)

# primeiro arquivo casa ---------------------------------------------------

sheets <- readxl::excel_sheets("data-raw/Estat_stica CASA.xlsx")
casa <- readxl::read_excel("data-raw/Estat_stica CASA.xlsx",
                           sheet = sheets,
                           na = ".") %>%
  janitor::clean_names()
glimpse(casa)
readr::write_rds(casa,"data/casa.rds")

# citomeria ---------------------------------------------------------------
sheets <- readxl::excel_sheets("data-raw/Estat_stica citometria de fluxo.xlsx")
plan1 <- readxl::read_excel("data-raw/Estat_stica citometria de fluxo.xlsx",
                           sheet = sheets[1],
                           na = ".") %>%
  janitor::clean_names()
glimpse(plan1)

plan2 <- readxl::read_excel("data-raw/Estat_stica citometria de fluxo.xlsx",
                            sheet = sheets[2],
                            na = ".") %>%
  janitor::clean_names() %>%
  mutate(mpal_percent = as.numeric(mpal_percent))
glimpse(plan2)

p2_1 <- plan2 %>% filter(hora==0)
p2_2 <- plan2 %>% filter(hora==4)

p2_1$grupo == p2_2$grupo
p2_1$touro == p2_2$touro
p2_1$coleta == p2_2$coleta
p2_2 <- p2_2[9:12]
names(p2_2) <- c("mplai_percent_4", "mpal_percent_4",  "mpai_percent_4",
                 "mpial_percent_4" )
plan2 <- cbind(p2_1,p2_2) %>% select(-hora) %>% as_tibble() %>%
  rename("mplai_percent_0"= mplai_percent,
         "mpal_percent_0"=mpal_percent,
         "mpai_percent_0"=mpai_percent,
         "mpial_percent_0"=mpial_percent) %>%
  mutate(
    mplai_percent_m = (mplai_percent_0+mplai_percent_4)/2,
    mpal_percent_m = (mpal_percent_0+mpal_percent_4)/2,
    mpai_percent_m = (mpai_percent_0+mpai_percent_4) /2,
    mpial_percent_m= (mpial_percent_0+mpial_percent_4)/2
  )

plan3 <- readxl::read_excel("data-raw/Estat_stica citometria de fluxo.xlsx",
                            sheet = sheets[3],
                            na = ".") %>%
  janitor::clean_names()
glimpse(plan3)

p3_1 <- plan3 %>% filter(hora==0)
p3_2 <- plan3 %>% filter(hora==4)

p3_1$grupo == p3_2$grupo
p3_1$touro == p3_2$touro
p3_1$coleta == p3_2$coleta
p3_2 <- p3_2[9]
names(p3_2) <- c("pi_a_median_4")
plan3 <- cbind(p3_1,p3_2) %>% select(-hora) %>% as_tibble() %>%
  rename("pi_a_median_0"= pi_a_median) %>%
  mutate(pi_a_median_m = (pi_a_median_0+pi_a_median_4)/2)


plan4 <- readxl::read_excel("data-raw/Estat_stica citometria de fluxo.xlsx",
                            sheet = sheets[4],
                            na = ".") %>%
  janitor::clean_names()
glimpse(plan4)

p4_1 <- plan4 %>% filter(hora==0)
p4_2 <- plan4 %>% filter(hora==4)

p4_1$grupo == p4_2$grupo
p4_1$touro == p4_2$touro
p4_1$coleta == p4_2$coleta
p4_2 <- p4_2[9:13]
names(p4_2) <- c("hpm_percent_4",  "stable_apc_a_4", "hpm_apc_a_4",
                 "o2_percent_4", "o2_pi_a_4")
plan4 <- cbind(p4_1,p4_2) %>% select(-hora) %>% as_tibble() %>%
  rename(o2_percent_0= o2_percent,
         hpm_percent_0 = hpm_percent,
         stable_apc_a_0 = stable_apc_a,
         hpm_apc_a_0 = hpm_apc_a,
         o2_pi_a_0= o2_pi_a) %>%
  mutate(o2_percent_m = (o2_percent_0+o2_percent_4)/2,
         hpm_percent_m = (hpm_percent_0+hpm_percent_4)/2,
         stable_apc_a_m = (stable_apc_a_0+stable_apc_a_4)/2,
         hpm_apc_a_m = (hpm_apc_a_0+hpm_apc_a_4)/2,
         o2_pi_a_m = (o2_pi_a_0+o2_pi_a_4)/2)

glimpse(plan1)
glimpse(plan2)
citomeria <- left_join(plan1,plan2 %>%
                         select(-cod_touro,
                                -nascim,
                                -data_coleta),by=c("grupo","touro","coleta","cod_tubo"))
glimpse(citomeria)

glimpse(plan3)
citomeria <- left_join(citomeria,plan3 %>%
                         select(-cod_touro,
                                -nascim,
                                -data_coleta),by=c("grupo","touro","coleta","cod_tubo"))
glimpse(citomeria)

glimpse(plan4)
citomeria <- left_join(citomeria,plan4 %>%
                         select(-cod_touro,
                                -nascim,
                                -data_coleta),by=c("grupo","touro","coleta","cod_tubo"))
glimpse(citomeria)
readr::write_rds(citomeria,"data/citomeria.rds")

# morfologia, motilidade e vigor ------------------------------------------
sheets <- readxl::excel_sheets("data-raw/Estat_stica Inicial - Morfologia, motilidade e vigor.xlsx")
morf_mot_vig <- readxl::read_excel("data-raw/Estat_stica Inicial - Morfologia, motilidade e vigor.xlsx",
                           sheet = sheets,
                           na = ".") %>%
  janitor::clean_names()
glimpse(morf_mot_vig)
readr::write_rds(morf_mot_vig,"data/morf_mot_vig.rds")


# morfologia, motilidade e vigor 4 de cada ------------------------------------------
sheets <- readxl::excel_sheets("data-raw/Estat_stica Inicial - Morfologia, motilidade e vigor -  animais selecionados  (4 de cada grupo).xlsx")
morf_mot_vig_4 <- readxl::read_excel("data-raw/Estat_stica Inicial - Morfologia, motilidade e vigor -  animais selecionados  (4 de cada grupo).xlsx",
                                   sheet = sheets,
                                   na = ".") %>%
  janitor::clean_names()
glimpse(morf_mot_vig_4)
readr::write_rds(morf_mot_vig_4,"data/morf_mot_vig_4.rds")
