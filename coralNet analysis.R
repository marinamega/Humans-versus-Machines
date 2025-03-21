#################################################################
#################################################################

library(tidyverse)
source("functions/kappa_function.R")

### LENDO ARQUIVOS FONTE ###

machine <- read.csv("data/EVSET_COM.csv", header=T, sep = ";", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_com = Label,
         mfg_com = Functional.Group) %>% 
  mutate(label_com = recode(label_com, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))


autora <- read.csv("data/EVSET_AUT.csv", header=T, sep = ",", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_aut = Label,
         mfg_aut = Functional.Group) %>% 
  mutate(label_aut = recode(label_aut, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))


expert <- read.csv("data/EVSET_EXP.csv", header=T, sep = ";", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_exp = Label,
         mfg_exp = Functional.Group) %>% 
  mutate(label_exp = recode(label_exp, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))


## dados de qualidade de imagem

#files <- list.files(path = "data/", pattern = ".xlsx") # read all spreadsheets from working folder

#dados <- plyr::adply(paste0("data/", files), 1, readxl::read_excel) %>% select(locality:filed_loss) # stack all spreadsheets


files <- list.files(path = "C:/Users/marin/Documents/R Arquivos", pattern = ".xlsx") # read all spreadsheets from working folder

dados <- plyr::adply(paste0(files), 1, readxl::read_excel) %>% select(locality:filed_loss) # stack all spreadsheets



#################################################################
#################################################################

### CALCULO Kappa ###
library(psych)

## total (JUNTAR DATA FRAMES) ##
tudo <- bind_cols(machine, 
                  autora %>% select(label_aut, mfg_aut),
                  expert %>% select(label_exp, mfg_exp))


## CALCULAR KAPPA E DESVIOS ENTRE DATA FRAMES -> TOTAIS

# EXPERT X AUTORA
kappa_psy_exp.aut <- tudo %>% 
  select(label_exp, label_aut) %>% 
  # irr::kappa2() # %>% # indica Kappa, Z e p-value
  psych::cohen.kappa(alpha = 0.05) # indica Kappa e confidence interval

## EXPERT X COMPUTADOR
kappa_psy_exp.com <- tudo %>% 
  select(label_exp, label_com) %>% 
  # irr::kappa2() # %>% # indica Kappa, Z e p-value
  psych::cohen.kappa(alpha = 0.05) # indica Kappa e confidence interval

## AUTORA X COMPUTADOR
kappa_psy_aut.com <- tudo %>% 
  select(label_aut, label_com) %>% 
  # irr::kappa2() # %>% # indica Kappa, Z e p-value
  psych::cohen.kappa(alpha = 0.05) # indica Kappa e confidence interval

# JUNTANDO VALORES DE KAPPA E DESVIOS (LOWER E UPPER) E PLOTANDO
bind_rows(kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
          kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
          kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()) %>% 
  mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
         comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(-rowname) %>% 
  ggplot(aes(x = comparison, y = estimate)) +
    geom_bar(stat = 'identity', fill = "lightgray") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    theme_classic() +
    labs(x = "")

bind_rows(kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
          kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
          kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()) %>% 
  mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
         comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(-rowname) %>% 
  ggplot(aes(x = comparison, y = estimate)) +
  geom_point(size = 3) +  # Adiciona os pontos
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +  # Adiciona as barras de erro
  theme_classic() +
  labs(x = "") +
  scale_x_discrete(labels = c("exp.aut" = "Performance Humano", 
                              "exp.com" = "Performance IA", 
                              "aut.com" = "Concordância Humano-IA"))
#################################################################
#################################################################

### CALCULAR KAPPA POR CATEGORIA E DESVIOS ENTRE DATA FRAMES ###

### parcial
tudo %>% 
  filter(Name == "PC_2017_aberta_t1_q (8).JPG") %>% 
  select(label_exp, label_aut) %>% 
  # irr::kappa2() # indica Kappa, Z e p-value
  psych::cohen.kappa(alpha = 0.05) # indica Kappa e confidence interval

### Kappa por imagem

## EXP x AUT
exp.aut <- tudo %>% 
  select(Name, label_exp, label_aut) %>% 
  split(.$Name) %>% 
  map(~ (.x %>% select(-Name))) %>% 
  map(~ psych::cohen.kappa(., alpha = 0.05)) %>% 
  map(~ (.$confid %>% data.frame() %>% slice(2))) # %>% reduce(bind_rows)

exp.aut.k <- as.data.frame(do.call(rbind, lapply(exp.aut, as.data.frame))) %>% 
  rownames_to_column("Name")

## EXP x COM
exp.com <- tudo %>% 
  select(Name, label_exp, label_com) %>% 
  split(.$Name) %>% 
  map(~ (.x %>% select(-Name))) %>% 
  map(~ psych::cohen.kappa(., alpha = 0.05)) %>% 
  map(~ (.$confid %>% data.frame() %>% slice(2))) # %>% reduce(bind_rows)

exp.com.k <- as.data.frame(do.call(rbind, lapply(exp.com, as.data.frame))) %>% 
  rownames_to_column("Name")


## AUT x COM
aut.com <- tudo %>% 
  select(Name, label_aut, label_com) %>% 
  split(.$Name) %>% 
  map(~ (.x %>% select(-Name))) %>% 
  map(~ psych::cohen.kappa(., alpha = 0.05)) %>% 
  map(~ (.$confid %>% data.frame() %>% slice(2))) # %>% reduce(bind_rows)

aut.com.k <- as.data.frame(do.call(rbind, lapply(aut.com, as.data.frame))) %>% 
  rownames_to_column("Name")


### TODOS JUNTOS

kappa_figs <- bind_rows(exp.aut.k %>% mutate(source = "exp.aut"),
                        exp.com.k %>% mutate(source = "exp.com"),
                        aut.com.k %>% mutate(source = "aut.com")) %>% 
  rename(image = Name) %>% 
  left_join(dados)


kappa_figs %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  mutate(across(c(shadow:filed_loss), as.factor)) %>% 
  lm(estimate ~ ., .) %>% 
  anova()

#############
## PLOT (FAZER UM PRA CADA TIPO DE DEFEITO)

# suspension
suspension <- kappa_figs %>%
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(color = as.factor(suspension), y = estimate, x = source)) +
    geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
    geom_boxplot(alpha = 0.4) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "Kappa", title = "Suspension")

# water_glin
water_glin <- kappa_figs %>%
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(color = as.factor(water_glin), y = estimate, x = source)) + 
    geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
    geom_boxplot(alpha = 0.4) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "Kappa", title = "Water glin")

# overexposure
overexposure <- kappa_figs %>%
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(color = as.factor(overexposure), y = estimate, x = source)) +
    geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
    geom_boxplot(alpha = 0.4) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "Kappa", title = "Overexposure")

# unfocused
unfocused <- kappa_figs %>%
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(color = as.factor(unfocused), y = estimate, x = source)) +
    geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
    geom_boxplot(alpha = 0.4) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "Kappa", title = "Unfocused")

# parallax
parallax <- kappa_figs %>%
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(color = as.factor(parallax), y = estimate, x = source)) +
    geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
    geom_boxplot(alpha = 0.4) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "Kappa", title = "Parallax")

# filed_loss
filed_loss <- kappa_figs %>%
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(color = as.factor(filed_loss), y = estimate, x = source)) +
    geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
    geom_boxplot(alpha = 0.4) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = "Kappa", title = "Field loss")

## juntar tudo
library(patchwork)

(suspension + water_glin + overexposure) / (unfocused + parallax + filed_loss)

#################################################################
#################################################################

##### KAPPA POR ROTULO #####

### USO INDIVIDUAL

kappa_func(tudo, "ZOS")

# ALL OK -> "ARC"    "BRSP"   "CAUV"   "CNA_CC"  "TFL"   "WAND"   "ZOS" "OMFC"   "OMFL1"  "OMFL2"  "POA"    "SARG"   "SHAD"   "SID"    "TCC"
# autora all NO -> "Dict"
# computador all NO  -> "DIGS"   "INC2"   "INC3"   "OMCA"  "OMCR1"     "Unk" 
# NO sense -> "SHAD", "WAND"
# TOO small -> "OMFL1"

### TODOS OS GRUPOS

output <- data.frame()
for(i in c("ARC", "BRSP", "CAUV", "CNA_CC", "TFL", "ZOS", "OMFC", "OMFL2", "POA", "SARG", "SID", "TCC")) {
  result <- kappa_func(df = tudo, i) 
  output <- rbind(output, result) 
}

### Graficos comparacao performance ###
output %>% 
  bind_rows(
    bind_rows(kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
              kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
              kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()) %>% 
      mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
             comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
      select(-rowname) %>% 
      mutate(label_taxa = "Total", Accuracy = NA, AccuracyPValue = NA, McnemarPValue = NA)
  ) %>% 
  mutate(label_taxa = factor(label_taxa, levels = output %>% 
                               group_by(label_taxa) %>% 
                               summarise(mean_k = mean(estimate)) %>% 
                               arrange(-mean_k) %>% 
                               pull(label_taxa) %>% 
                               c("Total", .))) %>% 
  ggplot(aes(x = label_taxa, y = estimate, color = comparison)) +
    geom_point(position=position_dodge(width=0.7), size = 3) + #fill = "lightgray",
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width=0.7), width = 0) +
    theme_classic() +
    labs(x = "")

total_df <- bind_rows(
  kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
  kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
  kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()
) %>% 
  mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
         comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(comparison, estimate, lower, upper)  # Selecionando apenas as colunas necessárias


#################################################################
#################################################################

#### KAPPA POR MFG ####

# FILT, MCA, MCT, UNK
### USO INDIVIDUAL
kappa_mfg(tudo, "ZOANT")

### TODOS OS MFG
types_mfg <- unique(c(intersect(tudo$mfg_exp, tudo$mfg_aut),
                  intersect(tudo$mfg_exp, tudo$mfg_com),
                  intersect(tudo$mfg_com, tudo$mfg_aut))) %>% sort() # all types of cover


output_mfg <- data.frame()
for(i in c("CORAL", "MCO", "MCR", "MFC", "MFL", "SUB", "TURF", "ZOANT")) {
  result_mfg <- kappa_mfg(df = tudo, i) 
  output_mfg <- rbind(output_mfg, result_mfg) 
}

### Graficos comparacao performance ###
output_mfg %>% 
  bind_rows(
    bind_rows(kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
              kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
              kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()) %>% 
      mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
             comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
      select(-rowname) %>% 
      mutate(mfg = "Total", Accuracy = NA, AccuracyPValue = NA, McnemarPValue = NA)
  ) %>% 
  mutate(mfg = factor(mfg, levels = output_mfg %>% 
                        group_by(mfg) %>% 
                        summarise(mean_k = mean(estimate)) %>% 
                        arrange(-mean_k) %>% 
                        pull(mfg)%>% 
                        c("Total", .))) %>% 
  ggplot(aes(x = mfg, y = estimate, color = comparison)) +
  geom_point(position=position_dodge(width=0.7), size = 3) + #fill = "lightgray",
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width=0.7), width = 0) +
  theme_classic() +
  labs(x = "")

total_df_mfg <- bind_rows(
  kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
  kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
  kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()
) %>% 
  mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
         comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(comparison, estimate, lower, upper)  # Selecionando apenas as colunas necessárias



#################################################################
#################################################################

### COBERTURA RELATIVA ###

## JUNTAR DATA FRAMES DE COBERTURA
all_wide <- bind_rows(
  tudo %>% # COMPUTADOR
    group_by(Name, label_com) %>% 
    dplyr::summarise(cover_com = length(label_com)/50) %>% 
    pivot_wider(names_from = label_com, values_from = cover_com) %>% 
    mutate(source = "machine") %>% 
    select(source, Name, everything()),
  tudo %>% # AUTORA
    group_by(Name, label_aut) %>% 
    dplyr::summarise(cover_aut = length(label_aut)/50) %>% 
    pivot_wider(names_from = label_aut, values_from = cover_aut) %>% 
    mutate(source = "autora") %>% 
    select(source, Name, everything()),
  tudo %>% # EXPERT
    group_by(Name, label_exp) %>% 
    dplyr::summarise(cover_exp = length(label_exp)/50) %>% 
    pivot_wider(names_from = label_exp, values_from = cover_exp) %>% 
    mutate(source = "expert") %>% 
    select(source, Name, everything())
) %>% 
  mutate_all(~ replace(., is.na(.), 0))


### COBERTURA RELATIVA EXPERT
# ordem
ordem_exp_mfg <- tudo %>% # EXPERT
  select(label_exp, mfg_exp) %>%
  group_by(mfg_exp) %>% 
  summarise(conta = length(label_exp)) %>% 
  arrange(-conta) %>% 
  pull(mfg_exp)

ordem_exp_label <- tudo %>% # EXPERT
  select(label_exp, mfg_exp) %>%
  group_by(label_exp) %>% 
  summarise(conta = length(mfg_exp)) %>% 
  arrange(-conta) %>% 
  pull(label_exp)


## BOXPLOT
tudo %>% # EXPERT
  group_by(Name, label_exp) %>% 
  dplyr::summarise(cover_exp = length(label_exp)/50) %>% 
  pivot_wider(names_from = label_exp, values_from = cover_exp) %>% 
  select(Name, everything()) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  pivot_longer(cols = ARC:CAUC, names_to = "label_exp", values_to = "cover") %>% 
  left_join(tudo %>% select(label_exp, mfg_exp) %>% distinct()) %>% 
  mutate(mfg_exp = factor(mfg_exp, levels = ordem_exp),
         label_exp = factor(label_exp, levels = ordem_exp_label)) %>%
  ggplot(aes(x = label_exp, y = cover, fill = mfg_exp)) +
    geom_boxplot() +
    theme_classic()

## BARRAS EMPILHADAS
tudo %>% # EXPERT
  select(label_exp, mfg_exp) %>%
  mutate(mfg_exp = factor(mfg_exp, levels = ordem_exp),
         label_exp = factor(label_exp, levels = ordem_exp_label)) %>% 
  filter(label_exp %in% c("ARC", "BRSP", "CAUV", "CNA_CC", "TFL", "ZOS", 
                          "OMFC", "OMFL2", "POA", "SARG", "SID", "TCC")) %>% 
  ggplot(aes(fill = label_exp, mfg_exp)) +
  geom_bar(stat = "count", position = "stack") + # position = "fill"
  theme_classic()



##########################
#### CONFUSION MATRIX ####

## PLOT CoralNet model

# TODOS OS TIPOS DE COBERTURA
types <- unique(c(intersect(tudo$label_exp, tudo$label_aut),
                  intersect(tudo$label_exp, tudo$label_com),
                  intersect(tudo$label_com, tudo$label_aut))) %>% sort() # all types of cover

# APENAS OS MAIS ABUNDANTES E QUE SAO COBERTURA
types1 <- c("ARC", "BRSP", "CAUV", "CNA_CC", "TFL", "ZOS", "OMFC", "OMFL2", "POA", "SARG", "SID", "TCC")

###
# CRIAR MATRIZ COM COBERTURA E VALORES IDENTIFICADOS (EXP x AUT)
confCN <- data.frame()
for(i in types) {
  b <- tudo %>% 
    select(label_exp, label_aut) %>% 
    filter(label_exp == i) %>% 
    table() %>% 
    data.frame() %>% 
    mutate(freq_rel = round(Freq/sum(Freq), 2),
           points_n = sum(Freq))
  confCN <- rbind(confCN, b) 
}

# order axis
ordem_axis_exp.aut <- confCN %>% 
  select(label_exp, points_n) %>% 
  distinct() %>% 
  arrange(-points_n) %>% 
  pull(label_exp) %>% as.character()

# plot confusion matrix (labels = agreement / number of points)
confCN %>% 
  mutate(label_exp = factor(label_exp, levels = rev(ordem_axis)),
         label_aut = factor(label_aut, levels = ordem_axis),
         etiqueta = paste(freq_rel, points_n, sep = " / ")) %>%
  filter(!is.na(label_aut)) %>% 
  complete(., label_exp, label_aut) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  ggplot(aes(y = label_exp, x = label_aut, fill = freq_rel)) + 
    geom_tile(na.rm = TRUE) +
    geom_text(aes(label = etiqueta), size = 3) +
    theme_classic() +
    scale_fill_gradient2(high = "red", low = "blue")

###
# CRIAR MATRIZ COM COBERTURA E VALORES IDENTIFICADOS (EXP x COM)
confCN_ec <- data.frame()
for(i in types) {
  b <- tudo %>% 
    select(label_exp, label_com) %>% 
    filter(label_exp == i) %>% 
    table() %>% 
    data.frame() %>% 
    mutate(freq_rel = round(Freq/sum(Freq), 2),
           points_n = sum(Freq))
  confCN_ec <- rbind(confCN_ec, b) 
}

# order axis
ordem_axis_exp.com <- confCN_ec %>% 
  select(label_exp, points_n) %>% 
  distinct() %>% 
  arrange(-points_n) %>% 
  pull(label_exp) %>% as.character()

# plot confusion matrix (labels = agreement / number of points)
confCN_ec %>% 
  mutate(label_exp = factor(label_exp, levels = rev(ordem_axis)),
         label_com = factor(label_com, levels = ordem_axis),
         etiqueta = paste(freq_rel, points_n, sep = " / ")) %>%
  filter(!is.na(label_com)) %>% 
  complete(., label_exp, label_com) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  ggplot(aes(y = label_exp, x = label_com, fill = freq_rel)) + 
  geom_tile(na.rm = TRUE) +
  geom_text(aes(label = etiqueta), size = 3) +
  theme_classic() +
  scale_fill_gradient2(high = "red", low = "blue")


###
# CRIAR MATRIZ COM COBERTURA E VALORES IDENTIFICADOS (AUT x COM)
confCN_ac <- data.frame()
for(i in types) {
  b <- tudo %>% 
    select(label_aut, label_com) %>% 
    filter(label_aut == i) %>% 
    table() %>% 
    data.frame() %>% 
    mutate(freq_rel = round(Freq/sum(Freq), 2),
           points_n = sum(Freq))
  confCN_ac <- rbind(confCN_ac, b) 
}

# order axis
ordem_axis_aut.com <- confCN_ac %>% 
  select(label_aut, points_n) %>% 
  distinct() %>% 
  arrange(-points_n) %>% 
  pull(label_aut) %>% as.character()

# plot confusion matrix (labels = agreement / number of points)
confCN_ac %>% 
  mutate(label_aut = factor(label_aut, levels = rev(ordem_axis)),
         label_com = factor(label_com, levels = ordem_axis),
         etiqueta = paste(freq_rel, points_n, sep = " / ")) %>%
  filter(!is.na(label_com), !is.na(label_aut)) %>% 
  complete(., label_aut, label_com) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  ggplot(aes(y = label_aut, x = label_com, fill = freq_rel)) + 
  geom_tile(na.rm = TRUE) +
  geom_text(aes(label = etiqueta), size = 3) +
  theme_classic() +
  scale_fill_gradient2(high = "red", low = "blue")


#################################################################
#################################################################


###########################
# MULTIVAR
###########################

library(vegan)

# selecting most abundant types of cover
tipos <- all_wide %>% 
  data.frame() %>% 
  select(ARC:CAUC) %>% 
  colSums() %>% 
  data.frame() %>% 
  arrange(-.) %>% 
  rownames_to_column("tipo") %>% 
  filter(`.` > 5) %>% 
  pull(tipo)

#
wide_red <- all_wide %>% 
  rownames_to_column("id") %>% 
  select(id:Name, tipos, -WAND)

# wide_red %>% 
#   data.frame() %>% 
#   select(-id:-Name) %>% 
#   rowSums() %>% summary()

# arcsine of the column
asin(sqrt(data$col1))
asin(sqrt(data$col3))

euc_plus5 <- wide_red %>% 
  data.frame() %>% 
  select(-id:-Name) %>%
  vegdist(method = "euclidean", binary = FALSE) %>% 
  metaMDS(trymax = 100)

nmds <- euc_plus5$points %>%
  data.frame() %>%
  bind_cols(source = all_wide$source)

# polygon
hullSR <- nmds %>%
  group_by(source) %>% 
  dplyr::slice(chull(MDS1, MDS2))

# plot
ggplot() +
  geom_point(data = nmds, aes(MDS1, MDS2, color = source), size = 1) + #, shape = subregion
  scale_shape_manual(values = c(1, 21:25)) +
  geom_point(data = hullSR, aes(MDS1, MDS2), shape = 3, size = 2) +
  # geom_text(data = hullSR, aes(MDS1, MDS2, label = row.names(sp)), vjust = -1) +
  # geom_segment(data = yz %>% slice(2:4), 
  #              aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #              arrow = arrow(angle = 22.5, length = unit(0.35,"cm"), type = "closed"), 
  #              linetype = 1, size = 0.6, colour = "blue") +
  # geom_segment(data = yz %>% slice(1, 5:7), 
  #              aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
  #              arrow = arrow(angle = 22.5, length = unit(0.3,"cm"), type = "closed"), 
  #              linetype = 2, size = 0.6, colour = "black") +
  # geom_text_repel(data = yz, aes(RDA1, RDA2, label = row.names(yz)), label.padding = 0.25) +
  # labs(x = paste("RDA 1 (", format(100 *ii$concont[[1]][2,1], digits=4), "%)", sep=""),
  #      y = paste("RDA 2 (", format(100 *ii$concont[[1]][2,2], digits=4), "%)", sep=""))+
  # guides(shape=guide_legend(title=NULL,color="black"),
  #        fill=guide_legend(title=NULL))+
  theme_bw() +
  geom_polygon(data = hullSR, aes(x = MDS1, y = MDS2, fill = source, color = source), alpha = 0.3, linetype = 2) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  scale_fill_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))


#### PERMDISP ####
# FAZER PRIMEIRO PERMDISP PRA VER SE TEM DISPERSAO ENTRE GRUPOS, SE NAO TIVER, PODE USAR PERMANOVA
# https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/betadisper

## Euclidian distances between samples
dis <- vegdist(wide_red %>% data.frame() %>% select(TFL:DICT), method = "euclidian")
groups <- wide_red$source

## Calculate multivariate dispersions
mod <- betadisper(dis, groups)

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 999)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## with data ellipses instead of hulls
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

## can also specify which axes to plot, ordering respected
plot(mod, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## Using group centroids
mod3 <- betadisper(dis, groups, type = "centroid")
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))

# SEM DISPERSAO MULTIVARIADA, PASSEMOS A PERMANOVA

#### PERMANOVA ####

## Euclidian distances between samples
devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
perm_label <- adonis(wide_red %>% data.frame() %>% select(TFL:DICT) ~ source, wide_red, permutations = 999, method = "euclidian")
perm_label$f.perms %>% hist(las=1)

## post hoc

# install.packages('devtools')
# library(devtools)
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(pairwiseAdonis)
pairwise.adonis2(wide_red %>% data.frame() %>% select(TFL:DICT) ~ source, data = wide_red)
