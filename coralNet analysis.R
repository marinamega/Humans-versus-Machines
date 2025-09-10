#################################################################
#################################################################
setwd('C:/Users/marin/Documents/R Arquivos/Dados Monografia/')
library(tidyverse)
source("kappa_function.R")

### LENDO ARQUIVOS FONTE ###

machine <- read.csv("EVSET_COM.csv", header=T, sep = ",", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_com = Label,
         mfg_com = Functional.Group) %>% 
  mutate(label_com = recode(label_com, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))


autora <- read.csv("EVSET_AU.csv", header=T, sep = ",", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_aut = Label,
         mfg_aut = Functional.Group) %>% 
  mutate(label_aut = recode(label_aut, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))


expert <- read.csv("EVSET_EXP.csv", header=T, sep = ",", fileEncoding = 'WINDOWS-1252') %>% 
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

fill_labels <- c("0" = "with no imperfections", "1" = "with imperfections")

# suspension
suspension <- kappa_figs %>%
  filter(source != 'aut.com') %>% 
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(fill = as.factor(suspension), y = estimate, x = source)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width=0.7), color = "black", show.legend = FALSE) +
  geom_boxplot(alpha = 0.6, color = "black", size = 0.6, show.legend = FALSE) +
  scale_fill_grey(start = 0.2, end = 0.8) + # mais contraste
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Cohen's kappa", title = "a) suspension *")

sun_glint <- kappa_figs %>%
  filter(source != 'aut.com') %>% 
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(fill = as.factor(water_glin), y = estimate, x = source)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width=0.7), color = "black", show.legend = FALSE) +
  geom_boxplot(alpha = 0.6, color = "black", size = 0.6, show.legend = FALSE) +
  scale_fill_grey(start = 0.2, end = 0.8) + # mais contraste
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Cohen's kappa", title = "b) sun glint *")

overexposure <- kappa_figs %>%
  filter(source != 'aut.com') %>% 
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(fill = as.factor(overexposure), y = estimate, x = source)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width=0.7), color = "black", show.legend = FALSE) +
  geom_boxplot(alpha = 0.6, color = "black", size = 0.6, show.legend = FALSE) +
  scale_fill_grey(start = 0.2, end = 0.8) + # mais contraste
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Cohen's kappa", title = "c) overexposure *")

parallax <- kappa_figs %>%
  filter(source != 'aut.com') %>% 
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(fill = as.factor(parallax), y = estimate, x = source)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width=0.7), color = "black", show.legend = FALSE) +
  geom_boxplot(alpha = 0.6, color = "black", size = 0.6, show.legend = FALSE) +
  scale_fill_grey(start = 0.2, end = 0.8) + # mais contraste
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Cohen's kappa", title = "d) parallax error *")

unfocused <- kappa_figs %>%
  filter(source != 'aut.com') %>% 
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(fill = as.factor(unfocused), y = estimate, x = source)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width=0.7), color = "black", show.legend = FALSE) +
  geom_boxplot(alpha = 0.6, color = "black", size = 0.6, show.legend = FALSE) +
  scale_fill_grey(start = 0.2, end = 0.8) + # mais contraste
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Cohen's kappa", title = "e) unfocus")

shadow <- kappa_figs %>%
  filter(source != 'aut.com') %>% 
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(fill = as.factor(shadow), y = estimate, x = source)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width=0.7), color = "black", show.legend = FALSE) +
  geom_boxplot(alpha = 0.6, color = "black", size = 0.6, show.legend = FALSE) +
  scale_fill_grey(start = 0.2, end = 0.8) + # mais contraste
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Cohen's kappa", title = "f) shadow")

distortion <- kappa_figs %>%
  filter(source != 'aut.com') %>% 
  mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  na.omit() %>% 
  ggplot(aes(fill = as.factor(distortion), y = estimate, x = source)) +
  geom_jitter(alpha = 0.5, position = position_dodge(width=0.7), color = "black") +
  geom_boxplot(alpha = 0.6, color = "black", size = 0.6) +
  scale_fill_grey(start = 0.2, end = 0.8, labels = fill_labels) + # mais contraste
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(family = "Arial", size = 10)) +
  labs(x = "", y = "Cohen's kappa", title = "g) distortion")

# # water_glin
# water_glin <- kappa_figs %>%
#   mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
#   select(estimate, source, shadow:filed_loss) %>% 
#   na.omit() %>% 
#   ggplot(aes(color = as.factor(water_glin), y = estimate, x = source)) + 
#     geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
#     geom_boxplot(alpha = 0.4) +
#     theme_classic() +
#     theme(legend.title = element_blank()) +
#     labs(x = "", y = "Kappa", title = "Water glin")
# 
# # overexposure
# overexposure <- kappa_figs %>%
#   mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
#   select(estimate, source, shadow:filed_loss) %>% 
#   na.omit() %>% 
#   ggplot(aes(color = as.factor(overexposure), y = estimate, x = source)) +
#     geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
#     geom_boxplot(alpha = 0.4) +
#     theme_classic() +
#     theme(legend.title = element_blank()) +
#     labs(x = "", y = "Kappa", title = "Overexposure")
# 
# # unfocused
# unfocused <- kappa_figs %>%
#   mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
#   select(estimate, source, shadow:filed_loss) %>% 
#   na.omit() %>% 
#   ggplot(aes(color = as.factor(unfocused), y = estimate, x = source)) +
#     geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
#     geom_boxplot(alpha = 0.4) +
#     theme_classic() +
#     theme(legend.title = element_blank()) +
#     labs(x = "", y = "Kappa", title = "Unfocused")
# 
# # parallax
# parallax <- kappa_figs %>%
#   mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
#   select(estimate, source, shadow:filed_loss) %>% 
#   na.omit() %>% 
#   ggplot(aes(color = as.factor(parallax), y = estimate, x = source)) +
#     geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
#     geom_boxplot(alpha = 0.4) +
#     theme_classic() +
#     theme(legend.title = element_blank()) +
#     labs(x = "", y = "Kappa", title = "Parallax")
# 
# # filed_loss
# filed_loss <- kappa_figs %>%
#   mutate(source = factor(source, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
#   select(estimate, source, shadow:filed_loss) %>% 
#   na.omit() %>% 
#   ggplot(aes(color = as.factor(filed_loss), y = estimate, x = source)) +
#     geom_jitter(alpha = 0.5, position = position_dodge(width=0.7)) +
#     geom_boxplot(alpha = 0.4) +
#     theme_classic() +
#     theme(legend.title = element_blank()) +
#     labs(x = "", y = "Kappa", title = "Field loss")

## juntar tudo
library(patchwork)

final_fig <- (suspension + sun_glint) /
  (overexposure + parallax) /
  (unfocused + shadow) /
  (distortion + plot_spacer()) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


ggsave("Fig1.eps", final_fig, 
       width = 6.85,    # largura 2 colunas
       height = 9.21,   # altura máxima permitida
       units = "in", 
       device = cairo_ps)  # EPS vetorial

ggsave("Fig1_manuscript.png", final_fig,
       width = 6.85,
       height = 9.21,
       units = "in",
       dpi = 300)

ggsave("Fig1_manuscript.tiff", final_fig,
       width = 6.85,      # largura 2 colunas
       height = 9.21,     # altura máxima permitida
       units = "in",
       dpi = 300,         # resolução suficiente para revisão
       compression = "lzw")

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
for(i in c("ARC", "BRSP", "CAUV", "CNA_CC", "TFL", "ZOS", "POA", "SARG", "SID", "DICT")) {
  result <- kappa_func(df = tudo, i) 
  output <- rbind(output, result) 
}

### Graficos comparacao performance ###

shape_values <- c("exp.aut" = 24,   # triângulo
                  "exp.com" = 21,   # círculo
                  "aut.com" = 22)   # quadrado

fill_values <- c("exp.aut" = "#619CFF",
                 "exp.com" = "#00BA38",
                 "aut.com" = "#F8766D")

fill_labels <- c("aut.com" = "between annotators", "exp.aut" = "manual's performance", 'exp.com' = "automatic's performance")

library(showtext)

# font_add("Arial", regular = "C:/Windows/Fonts/arial.ttf")
showtext_auto() # Ativa o uso de showtext para renderizar o texto

fig2<- output %>%
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
  ggplot(aes(x = label_taxa, y = estimate, shape = comparison, fill = comparison)) +
  geom_point(position = position_dodge(width = 0.7), size = 3, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.7), width = 0, color = "black") +
  scale_shape_manual(
    values = shape_values,
    labels = c("exp.aut" = "manual's performance",
               "exp.com" = "automatic's performance",
               "aut.com" = "between annotators"),
    name = NULL  # remove o título da legenda
  ) +
  scale_fill_manual(
    values = fill_values,
    labels = c("exp.aut" = "manual's performance",
               "exp.com" = "automatic's performance",
               "aut.com" = "between annotators"),
    name = NULL  # remove o título da legenda
  ) +
  theme_classic(base_size = 12, , base_family = "Arial") + # Removido base_family aqui
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, family = "arial"), # Usa a fonte sans-serif
    text = element_text(family = "arial"),
    legend.position = "bottom",
    legend.spacing = unit(0, "pt"),                  # remove espaçamento entre legendas se houver múltiplas linhas
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0), # espaço acima da legenda
    plot.margin   = margin(t = 10, r = 10, b = 0, l = 10) # espaço mínimo inferior do plot
  ) +
  # theme(
  #   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Usa a fonte sans-serif
  #   legend.position = "bottom",
  #   axis.text = element_text(size = 28),
  #   legend.text = element_text(size = 28),
  #   axis.title = element_text(size = 36))+
  labs(x = '', y = "Cohen's kappa") +
  scale_x_discrete(labels = c(
    "CAUV" = expression(italic("Caulerpa verticillata")),
    "SID" = expression(italic("Siderastrea")~"spp."),
    "SARG" = expression(italic("Sargassum")~"spp."),
    "ARC" = expression("substrate"),
    "BRSP" = expression(italic("Bryopsis")~"spp."),
    "POA" = expression(italic("Porites astreoides")),
    "ZOS" = expression(italic("Zoanthus sociatus")),
    "TFL" = expression("filamentous Turf"),
    "DICT" = expression(italic("Dictyota")~"spp."),
    "CNA_CC" = expression("crustose calcareous algae"),
    "Total" = "Total"
  ))

ggsave("Fig2.eps", fig2, 
       width = 6.85,
       height = 7,  
       units = "in", 
       device = cairo_ps)  # EPS vetorial

ggsave("Fig2_manuscript_png.png", fig2,
       width = 6.85,
       height = 7,
       units = "in",
       dpi = 300,
       scale = 1
)

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
fig1<- output_mfg %>%
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
                               pull(mfg) %>%
                               c("Total", .))) %>%
  ggplot(aes(x = mfg, y = estimate, shape = comparison, fill = comparison)) +
  geom_point(position = position_dodge(width = 0.7), size = 3, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.7), width = 0, color = "black") +
  scale_shape_manual(
    values = shape_values,
    labels = c("exp.aut" = "manual's performance",
               "exp.com" = "automatic's performance",
               "aut.com" = "between annotators"),
    name = NULL  # remove o título da legenda
  ) +
  scale_fill_manual(
    values = fill_values,
    labels = c("exp.aut" = "manual's performance",
               "exp.com" = "automatic's performance",
               "aut.com" = "between annotators"),
    name = NULL  # remove o título da legenda
  ) +
  theme_classic(base_size = 12, , base_family = "Arial") + # Removido base_family aqui
  # theme(
  #   axis.text.x = element_text(angle = 90, hjust = 1, family = "arial"), # Usa a fonte sans-serif
  #   text = element_text(family = "arial"),
  #   legend.position = "bottom",
  #   legend.spacing = unit(0, "pt"),                  # remove espaçamento entre legendas se houver múltiplas linhas
  #   legend.margin = margin(t = 0, r = 0, b = 0, l = 0), # espaço acima da legenda
  #   plot.margin   = margin(t = 10, r = 10, b = 0, l = 10) # espaço mínimo inferior do plot
  # ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Usa a fonte sans-serif
    legend.position = "bottom",
    axis.text = element_text(size = 28),
    legend.text = element_text(size = 28),
    axis.title = element_text(size = 36))+
  labs(x = '', y = "Cohen's kappa") +
  scale_x_discrete(labels = c(
    "CORAL" = "coral",
    "MCO" = "leathery macroalgae",
    "SUB" = "substrate",
    "MFL" = "filamentous macroalgae",
    "TURF" = "TURF",
    "ZOANT" = "zoanthids",
    "MFC" = "foliaceous macroalgae",
    "MCR" = "crustose macroalgae",
    "Total" = "total"
  ))

ggsave("Fig1.eps", fig1, 
       width = 6.85,
       height = 7,  
       units = "in", 
       device = cairo_ps)  # EPS vetorial

ggsave("Fig1_manuscript_png.png", fig1,
       width = 6.85,
       height = 7,
       units = "in",
       dpi = 300,
       scale = 1
)


# output_mfg %>% 
#   bind_rows(
#     bind_rows(kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
#               kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
#               kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()) %>% 
#       mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
#              comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
#       select(-rowname) %>% 
#       mutate(mfg = "Total", Accuracy = NA, AccuracyPValue = NA, McnemarPValue = NA)
#   ) %>% 
#   mutate(mfg = factor(mfg, levels = output_mfg %>% 
#                         group_by(mfg) %>% 
#                         summarise(mean_k = mean(estimate)) %>% 
#                         arrange(-mean_k) %>% 
#                         pull(mfg)%>% 
#                         c("Total", .))) %>% 
#   ggplot(aes(x = mfg, y = estimate, color = comparison)) +
#   geom_point(position=position_dodge(width=0.7), size = 3) + #fill = "lightgray",
#   geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width=0.7), width = 0) +
#   theme_classic() +
#   labs(x = "")

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
