setwd('C:/Users/marin/Documents/R Arquivos/Dados Monografia/')
library(tidyverse)
library(openxlsx)
library(showtext)

# font_add("Arial", regular = "C:/Windows/Fonts/arial.ttf")
showtext_auto() # Enable showtext for text rendering

### READING SOURCE FILES ###
# Only annotations from the evaluation set 
machine <- read.csv("EVSET_COM.csv", header=T, sep = ",", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_com = Label,
         mfg_com = Functional.Group) %>% 
  mutate(label_com = recode(label_com, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))

author <- read.csv("EVSET_AU.csv", header=T, sep = ",", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_aut = Label,
         mfg_aut = Functional.Group) %>% 
  mutate(label_aut = recode(label_aut, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))

expert <- read.csv("EVSET_EXP.csv", header=T, sep = ",", fileEncoding = 'WINDOWS-1252') %>% 
  rename(label_exp = Label,
         mfg_exp = Functional.Group) %>% 
  mutate(label_exp = recode(label_exp, "CNA/CC" = "CNA_CC")) %>% 
  filter(!is.na(Year))

## total (MERGING DATA FRAMES) ##
# Expert annotations are treated as the reference standard in all comparisons.
all <- bind_cols(machine, 
                 author %>% select(label_aut, mfg_aut),
                 expert %>% select(label_exp, mfg_exp))


#########################
### RELATIVE COVERAGE ###
#########################
# Estimate benthic relative cover using expert-annotated images only.
# Each quadrat consists of 50 annotated points, allowing conversion of point counts
# into percentage cover per category.

# Here, we used all expert-annotated images to estimate benthic relative cover
all_expert <- read.xlsx("all_expert_annotations.xlsx")

label_to_mfg <- all_expert %>%
  select(label_exp, mfg_exp) %>%
  distinct() %>%
  arrange(label_exp)

label_to_mfg <- label_to_mfg %>%
  mutate(
    mfg_exp = if_else(label_exp == "TFL", "TURF", mfg_exp)
  )

all_expert <- all_expert %>%
filter(!label_exp %in% c("WAND", "SHAD")) %>% # Remove categories that do not represent benthic cover (e.g., shadows or water column artifacts).
  group_by(Name, Year, Site, Transect, Quadrat, label_exp) %>%
  summarise(
    count_group = n(),
    .groups = "drop"
  ) %>%
  mutate(
    count_group = as.numeric(count_group)
  )

coverage_per_quadrat <- all_expert %>%
  group_by(Name, Year, Site, Transect, Quadrat, label_exp) %>%
  mutate(
    coverage = (count_group / 50.0) * 100
  )

coverage_per_quadrat<- coverage_per_quadrat %>% 
  left_join(
  label_to_mfg %>% select(label_exp, mfg_exp) %>% distinct(),
  by = "label_exp"
)

mean_coverage <- coverage_per_quadrat %>%
  group_by(mfg_exp) %>%
  summarise(
    mean_cov = mean(coverage, na.rm = TRUE),
    sd_cov   = sd(coverage, na.rm = TRUE),
    n        = n(),
    se       = sd_cov / sqrt(n),               
    ci_lower = mean_cov - 1.96 * se,          
    ci_upper = mean_cov + 1.96 * se,
    .groups = "drop"
  )

## PLOTS
boxplot_cover<- coverage_per_quadrat %>% 
  filter(!mfg_exp %in% c("OTHERS")) %>% 
  ggplot(aes(x = mfg_exp, y = coverage)) +
  geom_boxplot(alpha = 0.6, color = "black", show.legend = FALSE,outlier.shape = NA) +
  #geom_jitter(width = 0.05, alpha = 0.5, color = "black") + 
  ylim(0,30) +
  theme_classic(base_size = 12, , base_family = "Arial") + 
  theme(
    axis.text.x = element_text(size = 40, angle = 45, hjust = 1, family = "arial"), 
    axis.text.y = element_text(size = 40),
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    text = element_text(family = "arial"),
    legend.position = "bottom",
    legend.spacing = unit(0, "pt"),                  
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0), 
    plot.margin   = margin(t = 10, r = 10, b = 0, l = 10) 
  ) +
  labs(
    x = "",
    y = "Cover (%)"
  ) +
  scale_x_discrete(labels = c(
    "CORAL" = "Coral",
    "FILT" = "Filter",
    "MCA" = "Articulated calcareous macroalgae",
    "MCO" = "Leathery macroalgae",
    "MCR" =  "Crustose macroalgae",
    "MCT" = "Corticated macroalgae",
    "MFC" = "Foliaceous macroalgae",
    "MFL" = "Filamentous macroalgae",
    "TURF"  = "Turf algae",
    "ZOANT" = "Zoanthid",
    "SUB" = "Substrate"
  )) 


ggsave("FigS1.png", boxplot_cover,
       width = 6.85,
       height = 9.21,
       units = "in",
       dpi = 300)

# order
ordem_exp_mfg <- coverage_per_quadrat %>% # EXPERT
  filter(!mfg_exp %in% c("OTHERS", "SUB", "MCA", "MCT")) %>% 
  select(label_exp, mfg_exp) %>%
  group_by(mfg_exp) %>% 
  summarise(conta = length(label_exp)) %>% 
  arrange(-conta) %>% 
  pull(mfg_exp)

ordem_exp_label <- coverage_per_quadrat %>% # EXPERT
  filter(!label_exp %in% c("Unk", "ARC", "OMCA", "GAL", "OMCT")) %>%
  select(label_exp, mfg_exp) %>%
  group_by(label_exp) %>% 
  summarise(conta = length(mfg_exp)) %>% 
  arrange(-conta) %>% 
  pull(label_exp)

colors <- c(
  "TFL"  = "#F5FFD3",
  "TCC"   = "#BEBAD8",
  "CNA_CC"   = "#FF7F00",
  "BRSP"   = "#FACEE4",
  "ZOS"   = "#377EB8",
  "SID"   = "#8DA0CB",
  "DICT"   = "#4CAF4A",
  "CAUV"   = "#FFED6F",
  "OMFL2"   = "#E6F5C9",
  "SARG"   = "#D95F02",
  "OMFL1"   = "#E4211C",
  "OMFC" = "#1B9E77",
  "POA" = "#FC8D62",
  "OMCR1"        = "#A65629",
  "OMFL3"  = "#E78AC3",
  "INC2"    = "#8DD3C7",
  "CLAD"    = "#7570B3",
  "CHAE"    = "#F2F2F2",
  "DICP"   = "#B3E2CD",
  "INC3"    = "#FDCDAC",
  "DICVERS"  = "pink",
  "CAUV"   = "#FFD92F",
  "FAV"  = "#CCEBC5",
  "MUH"  = "#FFFB32",
  "OMCO" = "#A6D854",
  "PAD"   = "#984EA3", 
  "PEN"  = "#F781BF"
)

stack_bars<- coverage_per_quadrat %>% 
  filter(!mfg_exp %in% c("OTHERS", "SUB", "MCA", "MCT"), #Here, we did not include these categories because of their low cover at the MFG level.
         !label_exp %in% c("Unk", "ARC", "OMCA", "GAL", "OMCT")) %>%
  mutate(
    mfg_exp   = factor(mfg_exp, levels = ordem_exp_mfg),
    label_exp = factor(label_exp, levels = ordem_exp_label)
  ) %>% 
  ggplot(aes(x = mfg_exp, fill = label_exp)) +
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(
    name   = "Benthic category",
    values = colors,
    labels = c(
      "TFL"   = expression("Filamentous TURF"),
      "ZOS" = expression(italic("Zoanthus sociatus")),
      "BRSP"  = expression(italic("Bryopsis")~"spp."),
      "CAUV"  = expression(italic("Caulerpa verticillata")),
      "CHAE"  = expression(italic("Chaetomorpha")~"spp."),
      "CLAD"  = expression(italic("Cladophora")~"spp."),
      "CNA_CC"= expression("Crustose calcareous algae"),
      "DICP"  = expression(italic("Dictyopteris")~"spp."),
      "DICT"  = expression(italic("Dictyota")~"spp."),
      "DICVERS" = expression(italic("Dictyosphaeria versluysii")),
      "FAV"   = expression(italic("Favia gravida")),
      "INC2"  = expression("Sponge 1"),
      "INC3"  = expression("Sponge 2"),
      "MUH"   = expression(italic("Mussismilia hispida")),
      "OMCO"  = expression("Other corticated macroalgae"),
      "OMCR1" = expression("Other crustose macroalgae"),
      "OMFC"  = expression("Other foliaceous macroalgae"),
      "OMFL1" = expression("Filamentous macroalgae 1"),
      "OMFL2" = expression("Filamentous macroalgae 2"),
      "OMFL3" = expression("Filamentous macroalgae 3"),
      "PAD"   = expression(italic("Padina")~"spp."),
      "PEN"   = expression(italic("Penicillus")~"spp."),
      "POA"   = expression(italic("Porites astreoides")),
      "SARG"  = expression(italic("Sargassum")~"spp."),
      "SID"   = expression(italic("Siderastrea")~"spp."),
      "TCC"   = expression("Calcareous turf")
    ),
    drop = FALSE
  ) +
  scale_x_discrete(labels = c(
    "CORAL" = "Coral",
    "FILT"  = "Filter feeders",
    "MCO"   = "Leathery macroalgae",
    "MCR"   = "Crustose macroalgae",
    "MFC"   = "Foliaceous macroalgae",
    "MFL"   = "Filamentous macroalgae",
    "TURF"  = "Turf algae",
    "ZOANT" = "Zoanthids"
  )) +
  theme_classic(base_size = 12, , base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 35, angle = 45, hjust = 1, family = "arial"),
    #legend.position = "bottom",
    #legend.title.position  = "top",
    axis.text.y = element_text(size = 35),
    axis.title.y = element_text(size = 40),
    text = element_text(family = "arial"),
    legend.text  = element_text(size = 30),
    legend.title = element_text(size = 35)
  ) +
  labs(
    x = "",
    y = "Proportion")


ggsave("FigS2.png", stack_bars,
       width = 10.62,
       height = 5.94,
       units = "in",
       dpi = 300)

ggplot(mean_coverage, aes(x = mfg_exp, y = mean_cov)) +
  geom_point(size = 3, color = "blue") +                      
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),       
                width = 0.2, color = "black") +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  ylim(0,45) +
  theme_classic(base_size = 14) +
  labs(
    x = "Label",
    y = "Mean cover (%)",
    title = "Mean cover per label_exp with 95% CI"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#####################################################################

####### CREATING FUNCTIONS TO CALCULATE KAPPA ######
##### KAPPA BY LABEL #####
# load necessary packages
library(caret)

kappa_func <- function(df, taxa) {
  
  a <- df %>% 
    mutate(expert = ifelse(label_exp == taxa, "YES", "NO") %>% as.factor(),
           autora = ifelse(label_aut == taxa, "YES", "NO") %>% as.factor(),
           computador = ifelse(label_com == taxa, "YES", "NO") %>% as.factor()) %>% 
    select(expert, autora, computador)
  
  kappa_psy_exp.aut <- a %>% 
    select(expert, autora) %>% 
    psych::cohen.kappa(alpha = 0.05)
  
  kappa_psy_exp.com <- a %>% 
    select(expert, computador) %>% 
    psych::cohen.kappa(alpha = 0.05) 
  
  kappa_psy_aut.com <- a %>% 
    select(autora, computador) %>% 
    psych::cohen.kappa(alpha = 0.05) 
  
  one <- bind_rows(kappa_psy_exp.aut$confid %>% data.frame() %>% slice(1) %>% rownames_to_column(),
                   kappa_psy_exp.com$confid %>% data.frame() %>% slice(1) %>% rownames_to_column(),
                   kappa_psy_aut.com$confid %>% data.frame() %>% slice(1) %>% rownames_to_column()) %>%
    mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
           comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com")),
           label_taxa = taxa) %>%
    select(-rowname)
  
  acc_exp.aut <- caret::confusionMatrix(a$expert, a$autora) # predicted, observed
  acc_exp.com <- caret::confusionMatrix(a$expert, a$computador) # predicted, observed
  acc_aut.com <- caret::confusionMatrix(a$autora, a$computador) # predicted, observed
  
  two <- data.frame(exp.aut = acc_exp.aut$overall %>% round(2), 
                    exp.com = acc_exp.com$overall %>% round(2), 
                    aut.com = acc_aut.com$overall %>% round(2)) %>% 
    rownames_to_column("variables") %>% 
    pivot_longer(cols = exp.aut:aut.com, names_to = "comparison") %>% 
    mutate(label_taxa = taxa) %>% 
    filter(variables %in% c("Accuracy", "AccuracyPValue", "McnemarPValue")) %>% 
    pivot_wider(names_from = variables, values_from = value)
  
  left_join(two, one)
  
}

#### KAPPA BY MORPHOLOGICAL FUNCTIONAL GROUP (MFG) ####

kappa_mfg <- function(df, label_mfg) {
  
  a <- df %>% 
    mutate(expert = ifelse(mfg_exp == label_mfg, "YES", "NO") %>% as.factor(),
           autora = ifelse(mfg_aut == label_mfg, "YES", "NO") %>% as.factor(),
           computador = ifelse(mfg_com == label_mfg, "YES", "NO") %>% as.factor()) %>% 
    select(expert, autora, computador)
  
  kappa_psy_exp.aut <- a %>% 
    select(expert, autora) %>% 
    psych::cohen.kappa(alpha = 0.05)
  
  kappa_psy_exp.com <- a %>% 
    select(expert, computador) %>% 
    psych::cohen.kappa(alpha = 0.05) 
  
  kappa_psy_aut.com <- a %>% 
    select(autora, computador) %>% 
    psych::cohen.kappa(alpha = 0.05) 
  
  one <- bind_rows(kappa_psy_exp.aut$confid %>% data.frame() %>% slice(1) %>% rownames_to_column(),
                   kappa_psy_exp.com$confid %>% data.frame() %>% slice(1) %>% rownames_to_column(),
                   kappa_psy_aut.com$confid %>% data.frame() %>% slice(1) %>% rownames_to_column()) %>%
    mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
           comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com")),
           mfg = label_mfg) %>%
    select(-rowname)
  
  acc_exp.aut <- caret::confusionMatrix(a$expert, a$autora) # predicted, observed
  acc_exp.com <- caret::confusionMatrix(a$expert, a$computador) # predicted, observed
  acc_aut.com <- caret::confusionMatrix(a$autora, a$computador) # predicted, observed
  
  two <- data.frame(exp.aut = acc_exp.aut$overall %>% round(2), 
                    exp.com = acc_exp.com$overall %>% round(2), 
                    aut.com = acc_aut.com$overall %>% round(2)) %>% 
    rownames_to_column("variables") %>% 
    pivot_longer(cols = exp.aut:aut.com, names_to = "comparison") %>% 
    mutate(mfg = label_mfg) %>% 
    filter(variables %in% c("Accuracy", "AccuracyPValue", "McnemarPValue")) %>% 
    pivot_wider(names_from = variables, values_from = value)
  
  left_join(two, one)
  
}


### KAPPA CALCULATION ###
library(psych)

## total (MERGING DATA FRAMES) ##
all <- bind_cols(machine, 
                  author %>% select(label_aut, mfg_aut),
                  expert %>% select(label_exp, mfg_exp))


## CALCULATING KAPPA AND DEVIATIONS BETWEEN DATA FRAMES -> TOTALS

# EXPERT X AUTHOR
kappa_psy_exp.aut <- all %>% 
  select(label_exp, label_aut) %>% 
  # irr::kappa2() # %>% # reports Kappa, Z and p-value
  psych::cohen.kappa(alpha = 0.05) # reports Kappa and confidence interval

## EXPERT X COMPUTER
kappa_psy_exp.com <- all %>% 
  select(label_exp, label_com) %>% 
  # irr::kappa2() # %>% # # reports Kappa, Z and p-value
  psych::cohen.kappa(alpha = 0.05) # reports Kappa and confidence interval

## AUTHOR X COMPUTER
kappa_psy_aut.com <- all %>% 
  select(label_aut, label_com) %>% 
  # irr::kappa2() # %>% # # reports Kappa, Z and p-value
  psych::cohen.kappa(alpha = 0.05) # reports Kappa and confidence interval

# MERGING KAPPA VALUES AND DEVIATIONS (LOWER AND UPPER) AND PLOTTING
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
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +  # Adiciona as barras de erro
  theme_classic() +
  labs(x = "") +
  scale_x_discrete(labels = c("exp.aut" = "Human Performance", 
                              "exp.com" = "AI Performance", 
                              "aut.com" = "Human–AI Agreement"))

#################################################################
#################################################################

#######################
#### IMAGE QUALITY ####
#######################
# Assess the effect of image-quality imperfections on annotation agreement.
# Cohen’s kappa is calculated per image and modeled as a function of
# binary image-quality attributes (e.g., suspension, shadow, parallax).

files <- list.files(path = "C:/Users/marin/Documents/R Arquivos", pattern = ".xlsx") # read all spreadsheets from working folder

data <- plyr::adply(paste0(files), 1, readxl::read_excel) %>% select(locality:filed_loss) # stack all spreadsheets

### CALCULATE KAPPA BY CATEGORY AND DEVIATIONS BETWEEN DATA FRAMES ###

### partial
all %>% 
  filter(Name == "PC_2017_aberta_t1_q (8).JPG") %>% 
  select(label_exp, label_aut) %>% 
  # irr::kappa2() # reports Kappa, Z and p-value
  psych::cohen.kappa(alpha = 0.05) # reports Kappa and confidence interval

### Kappa by image

## EXP x AUT
exp.aut <- all %>% 
  select(Name, label_exp, label_aut) %>% 
  split(.$Name) %>% 
  map(~ (.x %>% select(-Name))) %>% 
  map(~ psych::cohen.kappa(., alpha = 0.05)) %>% 
  map(~ (.$confid %>% data.frame() %>% slice(2))) # %>% reduce(bind_rows)

exp.aut.k <- as.data.frame(do.call(rbind, lapply(exp.aut, as.data.frame))) %>% 
  rownames_to_column("Name")

## EXP x COM
exp.com <- all %>% 
  select(Name, label_exp, label_com) %>% 
  split(.$Name) %>% 
  map(~ (.x %>% select(-Name))) %>% 
  map(~ psych::cohen.kappa(., alpha = 0.05)) %>% 
  map(~ (.$confid %>% data.frame() %>% slice(2))) # %>% reduce(bind_rows)

exp.com.k <- as.data.frame(do.call(rbind, lapply(exp.com, as.data.frame))) %>% 
  rownames_to_column("Name")


## AUT x COM
aut.com <- all %>% 
  select(Name, label_aut, label_com) %>% 
  split(.$Name) %>% 
  map(~ (.x %>% select(-Name))) %>% 
  map(~ psych::cohen.kappa(., alpha = 0.05)) %>% 
  map(~ (.$confid %>% data.frame() %>% slice(2))) # %>% reduce(bind_rows)

aut.com.k <- as.data.frame(do.call(rbind, lapply(aut.com, as.data.frame))) %>% 
  rownames_to_column("Name")


### ALL TOGETHER
kappa_figs <- bind_rows(exp.aut.k %>% mutate(source = "exp.aut"),
                        exp.com.k %>% mutate(source = "exp.com"),
                        aut.com.k %>% mutate(source = "aut.com")) %>% 
  rename(image = Name) %>% 
  left_join(data)


kappa_figs %>% 
  select(estimate, source, shadow:filed_loss) %>% 
  mutate(across(c(shadow:filed_loss), as.factor)) %>% 
  lm(estimate ~ ., .) %>% 
  anova()

#############
## PLOT (ONE FOR EACH TYPE OF IMPERFECTION)

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
  scale_fill_grey(start = 0.2, end = 0.8) + 
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
  scale_fill_grey(start = 0.2, end = 0.8) + 
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
  scale_fill_grey(start = 0.2, end = 0.8) + 
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
  scale_fill_grey(start = 0.2, end = 0.8) +
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
  scale_fill_grey(start = 0.2, end = 0.8) + 
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
  scale_fill_grey(start = 0.2, end = 0.8) +
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
  scale_fill_grey(start = 0.2, end = 0.8, labels = fill_labels) + 
  scale_x_discrete(labels = c("exp.aut" = "manual", "exp.com" = "automatic")) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(family = "Arial", size = 10)) +
  labs(x = "", y = "Cohen's kappa", title = "g) distortion")


## combine all
library(patchwork)

final_fig <- (suspension + sun_glint) /
  (overexposure + parallax) /
  (unfocused + shadow) /
  (distortion + plot_spacer()) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# saving figures
ggsave("Fig1.eps", final_fig, 
       width = 6.85,    
       height = 9.21,   
       units = "in", 
       device = cairo_ps)  

ggsave("Fig1_manuscript.png", final_fig,
       width = 6.85,
       height = 9.21,
       units = "in",
       dpi = 300)

ggsave("Fig1_manuscript.tiff", final_fig,
       width = 6.85,      
       height = 9.21,     
       units = "in",
       dpi = 300,         
       compression = "lzw")

#################################################################
#################################################################
######## PERFORMANCE AND AGREEMENT #########
############################################

##### KAPPA BY LABEL #####

### INDIVIDUAL USE

kappa_func(all, "ZOS")

### ALL REPRESENTATIVE GROUPS
output <- data.frame()
for(i in c("ARC", "BRSP", "CAUV", "CNA_CC", "TFL", "ZOS", "POA", "SARG", "SID", "DICT")) {
  result <- kappa_func(df = all, i) 
  output <- rbind(output, result) 
}

### PERFORMANCE COMPARISON PLOTS ###

shape_values <- c("exp.aut" = 24,   # triangle
                  "exp.com" = 21,   # circle
                  "aut.com" = 22)   # square

fill_values <- c("exp.aut" = "#619CFF",
                 "exp.com" = "#00BA38",
                 "aut.com" = "#F8766D")

fill_labels <- c("aut.com" = "between annotators", "exp.aut" = "manual's performance", 'exp.com' = "automatic's performance")

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
    name = NULL  
  ) +
  scale_fill_manual(
    values = fill_values,
    labels = c("exp.aut" = "manual's performance",
               "exp.com" = "automatic's performance",
               "aut.com" = "between annotators"),
    name = NULL  
  ) +
  theme_classic(base_size = 12, , base_family = "Arial") + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, family = "arial"), 
    text = element_text(family = "arial"),
    legend.position = "bottom",
    legend.spacing = unit(0, "pt"),                  
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0), 
    plot.margin   = margin(t = 10, r = 10, b = 0, l = 10) 
  ) +
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
       device = cairo_ps)  

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
  select(comparison, estimate, lower, upper)


#### KAPPA BY MFG ####
# FILT, MCA, MCT, UNK
### INDIVIDUAL USE
kappa_mfg(all, "ZOANT")

### ALL MFGs
types_mfg <- unique(c(intersect(all$mfg_exp, all$mfg_aut),
                  intersect(all$mfg_exp, all$mfg_com),
                  intersect(all$mfg_com, all$mfg_aut))) %>% sort() # all types of cover


output_mfg <- data.frame()
for(i in c("CORAL", "MCO", "MCR", "MFC", "MFL", "SUB", "TURF", "ZOANT")) {
  result_mfg <- kappa_mfg(df = all, i) 
  output_mfg <- rbind(output_mfg, result_mfg) 
}

### plot - MFG ###
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
    name = NULL  
  ) +
  scale_fill_manual(
    values = fill_values,
    labels = c("exp.aut" = "manual's performance",
               "exp.com" = "automatic's performance",
               "aut.com" = "between annotators"),
    name = NULL  
  ) +
  theme_classic(base_size = 12, , base_family = "Arial") + 
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
       device = cairo_ps)  

ggsave("Fig1_manuscript_png.png", fig1,
       width = 6.85,
       height = 7,
       units = "in",
       dpi = 300,
       scale = 1
)

total_df_mfg <- bind_rows(
  kappa_psy_exp.aut$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
  kappa_psy_exp.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column(),
  kappa_psy_aut.com$confid %>% data.frame() %>% slice(2) %>% rownames_to_column()
) %>% 
  mutate(comparison = c("exp.aut", "exp.com", "aut.com"),
         comparison = factor(comparison, levels = c("exp.aut", "exp.com", "aut.com"))) %>% 
  select(comparison, estimate, lower, upper)