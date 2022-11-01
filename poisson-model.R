library(data.table)
library(magrittr)
library(emmeans)
library(ggpubr)
library(readxl)
library(brms)
library(bayesplot)
library(bayestestR)
library(multcompView)
library(gmodels)

sessionInfo()
# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] gmodels_2.18.1.1   multcompView_0.1-8 bayestestR_0.13.0  bayesplot_1.9.0    brms_2.18.0        Rcpp_1.0.9         readxl_1.4.1      
#  [8] ggpubr_0.4.0       ggplot2_3.3.6      emmeans_1.8.1-1    magrittr_2.0.3     data.table_1.14.2 
# 
# loaded via a namespace (and not attached):
#   [1] minqa_1.2.4          TH.data_1.1-1        colorspace_2.0-3     ggsignif_0.6.4       ellipsis_0.3.2       ggridges_0.5.4       estimability_1.4.1  
#   [8] markdown_1.1         base64enc_0.1-3      logspline_2.1.17     rstudioapi_0.14      farver_2.1.1         rstan_2.26.11        DT_0.26             
#  [15] fansi_1.0.3          mvtnorm_1.1-3        bridgesampling_1.1-2 codetools_0.2-18     splines_4.2.1        shinythemes_1.2.0    projpred_2.2.1      
#  [22] jsonlite_1.8.2       nloptr_2.0.3         broom_1.0.1          shiny_1.7.2          compiler_4.2.1       backports_1.4.1      assertthat_0.2.1    
#  [29] Matrix_1.5-1         fastmap_1.1.0        cli_3.4.1            later_1.3.0          htmltools_0.5.3      prettyunits_1.1.1    tools_4.2.1         
#  [36] igraph_1.3.5         coda_0.19-4          gtable_0.3.1         glue_1.6.2           reshape2_1.4.4       dplyr_1.0.10         posterior_1.3.1     
#  [43] V8_4.2.1             carData_3.0-5        cellranger_1.1.0     vctrs_0.4.2          gdata_2.18.0.1       nlme_3.1-160         crosstalk_1.2.0     
#  [50] insight_0.18.5       tensorA_0.36.2       stringr_1.4.1        ps_1.7.1             lme4_1.1-30          mime_0.12            miniUI_0.1.1.1      
#  [57] lifecycle_1.0.3      gtools_3.9.3         rstatix_0.7.0        MASS_7.3-58.1        zoo_1.8-11           scales_1.2.1         colourpicker_1.1.1  
#  [64] promises_1.2.0.1     Brobdingnag_1.2-9    parallel_4.2.1       sandwich_3.0-2       inline_0.3.19        shinystan_2.6.0      gamm4_0.2-6         
#  [71] curl_4.3.3           gridExtra_2.3        loo_2.5.1            StanHeaders_2.26.11  stringi_1.7.8        dygraphs_1.1.1.6     checkmate_2.1.0     
#  [78] boot_1.3-28          pkgbuild_1.3.1       rlang_1.0.6          pkgconfig_2.0.3      matrixStats_0.62.0   distributional_0.3.1 lattice_0.20-45     
#  [85] purrr_0.3.5          labeling_0.4.2       rstantools_2.2.0     htmlwidgets_1.5.4    tidyselect_1.2.0     processx_3.7.0       plyr_1.8.7          
#  [92] R6_2.5.1             generics_0.1.3       multcomp_1.4-20      DBI_1.1.3            mgcv_1.8-40          pillar_1.8.1         withr_2.5.0         
#  [99] xts_0.12.1           datawizard_0.6.2     survival_3.4-0       abind_1.4-5          tibble_3.1.8         crayon_1.5.2         car_3.1-1           
# [106] utf8_1.2.2           grid_4.2.1           callr_3.7.2          threejs_0.3.3        digest_0.6.29        xtable_1.8-4         tidyr_1.2.1         
# [113] httpuv_1.6.6         RcppParallel_5.1.5   stats4_4.2.1         munsell_0.5.0        shinyjs_2.1.0       


## read data
d <- 
  read_xlsx("Copy of Fish.data.2020.xlsx", 1) %>%
  .[, c(1:2, 4:8)] %>%
  as.data.table %>%
  .[, site := factor(site, labels = c("slope", "flat"))] %>%
  .[, species := as.factor(species)] %>%
  .[, habit := "Carnivore"] %>%
  .[which(
    species %in% c(
      "Pomacentrus.grammorhynchus",
      "Hemiglyphidodon.plagiometopon",
      "Neoglyphidodon.nigroris",
      "Pomacentrus.adelus"
    )
  ), habit := "Herbivore"] %>%
  .[, habit := as.factor(habit)] %>%
  .[, Gal.bites.strikes := Gal.bites + Gal.strike]

## setting contrasts
contrasts(d$species) <- contr.bayes
contrasts(d$site) <- c(-0.5, 0.5)
contrasts(d$habit) <- c(-0.5, 0.5)

## descriptive statistics
d[, .(
  .N,
  duration = sum(duration),
  bites.strikes.rate = mean(Gal.bites.strikes / duration),
  chases.rate = mean(chases / duration),
  ins.rate = mean(inspections / duration)
), by = .(species, habit, site)]
#                           species     habit  site N duration bites.strikes.rate chases.rate    ins.rate
#  1:          Cheilinus.chlorourus Carnivore slope 5   143.77        0.005116398 0.000000000 0.005116398
#  2:           Epibulus.insidiator Carnivore slope 5   143.77        0.005116398 0.000000000 0.005116398
#  3:      Halichoeres.trimaculatus Carnivore slope 5   143.77        0.000000000 0.000000000 0.000000000
#  4: Hemiglyphidodon.plagiometopon Herbivore slope 5   143.77        5.152880986 0.074437412 0.000000000
#  5:         Meiacanthus.gramistes Carnivore slope 5   143.77        0.345268025 0.000000000 0.137620703
#  6:       Neoglyphidodon.nigroris Herbivore slope 5   143.77        0.504242424 0.000000000 0.000000000
#  7:     Parupeneus.multifasciatus Carnivore slope 5   143.77        0.020465592 0.000000000 0.010232796
#  8:            Pomacentrus.adelus Herbivore slope 5   143.77        1.874464002 0.005116398 0.000000000
#  9:    Pomacentrus.grammorhynchus Herbivore slope 5   143.77        3.025170354 0.047758650 0.000000000
# 10:      Stethojulis.strigiventer Carnivore slope 5   143.77        0.020465592 0.000000000 0.000000000
# 11:          Cheilinus.chlorourus Carnivore  flat 3    91.26        0.000000000 0.000000000 0.000000000
# 12:           Epibulus.insidiator Carnivore  flat 3    91.26        0.000000000 0.000000000 0.000000000
# 13:      Halichoeres.trimaculatus Carnivore  flat 3    91.26        0.248630580 0.000000000 0.248630580
# 14: Hemiglyphidodon.plagiometopon Herbivore  flat 3    91.26        0.232712155 0.000000000 0.000000000
# 15:         Meiacanthus.gramistes Carnivore  flat 3    91.26        0.076463497 0.000000000 0.090751253
# 16:       Neoglyphidodon.nigroris Herbivore  flat 3    91.26        0.221239345 0.000000000 0.000000000
# 17:     Parupeneus.multifasciatus Carnivore  flat 3    91.26        0.000000000 0.000000000 0.000000000
# 18:            Pomacentrus.adelus Herbivore  flat 3    91.26        0.144843569 0.000000000 0.000000000
# 19:    Pomacentrus.grammorhynchus Herbivore  flat 3    91.26        1.355676765 0.019312476 0.000000000
# 20:      Stethojulis.strigiventer Carnivore  flat 3    91.26        0.466991925 0.000000000 0.476968978


## Fitting multinomial Poisson model
m31 <-
  brm(
    mvbind(chases, Gal.bites.strikes, inspections) ~
      offset(log(duration)) + site * habit + (1 + site | species),
    data = d,
    family = poisson,
    chains = 10,
    cores = 10,
    iter = 20000,
    save_all_pars = T,
    sample_prior = "yes",
    set.seed(1234),
    prior =
      prior(student_t(3, 0, 2), class = b, resp = chases) +
      prior(student_t(3, 0, 2), class = b, resp = Galbitesstrikes) +
      prior(student_t(3, 0, 2), class = b, resp = inspections) +
      prior(student_t(3, 0, 5), class = sd, resp = chases) +
      prior(student_t(3, 0, 5), class = sd, resp = Galbitesstrikes) +
      prior(student_t(3, 0, 5), class = sd, resp = inspections),
    control = list(adapt_delta = 0.99, max_treedepth = 20)
  )
# save(m31, file = "multi-poisson-mixed-model-3dv-0.5contrast.RData")
# load("multi-poisson-mixed-model-3dv-0.5contrast.RData")

describe_posterior(m31, ci_method = "HDI", test = "pd")
# Parameter    | Response | Median |          95% CI |     pd |  Rhat |      ESS
# ------------------------------------------------------------------------------
# (Intercept)  |   chases |  -7.75 | [-11.25, -5.36] |   100% | 1.000 | 44043.00
# site1        |   chases |  -1.19 | [ -4.15,  1.52] | 82.88% | 1.000 | 75302.00
# habit1       |   chases |   3.19 | [ -1.14,  8.58] | 94.03% | 1.000 | 41112.00
# site1:habit1 |   chases |  -1.06 | [ -5.54,  2.86] | 72.09% | 1.000 | 93868.00
# 
# # Fixed effects Galbitesstrikes
# 
# Parameter    |        Response | Median |          95% CI |     pd |  Rhat |      ESS
# -------------------------------------------------------------------------------------
# (Intercept)  | Galbitesstrikes |  -2.21 | [ -3.65, -1.04] | 99.93% | 1.000 | 32245.00
# site1        | Galbitesstrikes |  -0.68 | [ -2.54,  1.14] | 79.35% | 1.000 | 43472.00
# habit1       | Galbitesstrikes |   3.38 | [  0.83,  5.73] | 99.19% | 1.000 | 31339.00
# site1:habit1 | Galbitesstrikes |  -1.36 | [ -4.30,  1.65] | 82.82% | 1.000 | 50760.00
# 
# # Fixed effects inspections
# 
# Parameter    |    Response | Median |          95% CI |     pd |  Rhat |      ESS
# ---------------------------------------------------------------------------------
# (Intercept)  | inspections |  -7.41 | [-10.43, -5.09] |   100% | 1.000 | 46905.00
# site1        | inspections |   0.24 | [ -2.98,  3.31] | 56.52% | 1.000 | 78797.00
# habit1       | inspections |  -4.11 | [-10.11,  0.49] | 97.69% | 1.000 | 43771.00
# site1:habit1 | inspections |  -0.16 | [ -4.74,  4.22] | 53.46% | 1.000 | 1.02e+05



## multiple comparisons
mbest <- m31
ROPE <- c(4/5, 5/4)
d.p <- 
  data.table(
    species = gl(10, 2, 20, labels = levels(d$species)),
    site = gl(2, 1, 20, labels = levels(d$site)),
    duration = 1,
    habit = c(
      "Carnivore",  
      "Carnivore",
      "Carnivore",
      "Herbivore",
      "Carnivore",
      "Herbivore",
      "Carnivore",
      "Herbivore",
      "Herbivore",
      "Carnivore"
    ) %>% rep(each = 2) %>% as.factor
  )
contrasts(d$species) <- contr.bayes
contrasts(d$site) <- c(-0.5, 0.5)
contrasts(d$habit) <- c(-0.5, 0.5)

mbest.post <-
  cbind(d.p, posterior_linpred(mbest, newdata = d.p, resp = "chases", re_formula = NULL) %>% t) %>%
  melt(id.vars = 1:4, value.name = "chases") %>%
  merge(
    cbind(
      d.p,
      posterior_linpred(mbest, newdata = d.p, resp = "Galbitesstrikes", re_formula = NULL) %>% t
    ) %>%
      melt(id.vars = 1:4, value.name = "Galbitesstrikes"),
    by = c("species", "site", "duration", "variable", "habit")
  ) %>%
  merge(
    cbind(d.p, posterior_linpred(mbest, newdata = d.p, resp = "inspections", re_formula = NULL) %>% t) %>%
      melt(id.vars = 1:4, value.name = "inspections"),
    by = c("species", "site", "duration", "variable", "habit")
  ) %>%
  .[, duration := NULL] %>%
  setkey(species, site, variable, habit) %>%
  setnames("variable", "mcmc") %>%
  melt(id.vars = c("species", "site", "mcmc", "habit")) %>%
  setkey(species, site, mcmc, variable, habit)

mbest.post.diff <-
  split(mbest.post, mbest.post$variable) %>%
  lapply(., function(x) {
    dcast(x, mcmc ~ species + site, value.var = "value") %>%
      .[, -1] %>%
      as.matrix %>% {
        mat <- .
        apply(combn(1:ncol(mat), 2), 2, function(x) {
          mat[, x[2]] - mat[, x[1]]
        }) %>%
          set_colnames(combn(colnames(mat), 2, function(x) {
            paste(x[2], "-", x[1], sep = "")
          }))
      }
  })
mbest.post.diff.sig <-
  lapply(mbest.post.diff, function(x) {
    describe_posterior(
      x %>% as.mcmc,
      ci = 0.95,
      ci_method = "hdi",
      test = "rope",
      rope_ci = 0.95,
      rope_range = ROPE %>% log
    ) %>%
      as.data.table %>%
      .[, is.sig := (CI_low < ROPE_low & CI_high < ROPE_low) | (CI_low > ROPE_high & CI_high > ROPE_high)]
  })
mbest.post.diff.sig.res <-
  lapply(mbest.post.diff.sig, function(x) {
    x$is.sig %>%
      set_names(x$Parameter) %>%
      multcompLetters %>%
      .$Letters %>% {
        data.table(parameter = names(.),
                   rank = .)
      } %>%
      .[, c("species", "site") := tstrsplit(parameter, "_")]
  }) %T>% 
  print

## figures
merge(mbest.post[, .(M = median(value)), by = .(species, site, variable)],
      mbest.post[, hdi(value, ci = 0.95), by = .(species, site, variable)]) %>%
  merge(d[, .(species, site, habit)] %>% unique, by = c("species", "site")) %>%
  merge(
    rbindlist(mbest.post.diff.sig.res, use.names = T, idcol = T) %>% setnames(".id", "variable"),
    by = c("species", "site", "variable")
  ) %>%
  ggplot(aes(species), data = .) +
  geom_point(aes(y = sample.mean), color = 8, size = 2, shape = 4,
             data = d %>% copy %>% setnames(c("Gal.bites.strikes"), c("Galbitesstrikes")) %>%
               .[, Gal.bites:=NULL] %>% .[, Gal.strike:=NULL] %>%
               melt(id.vars = c("species", "site", "duration", "habit")) %>%
               .[, value := value / duration] %>%
               .[, .(sample.mean = mean(value)), by = .(species, habit, site, variable)]) +
  geom_errorbar(
    aes(
      ymin = CI_low %>% exp,
      ymax = CI_high %>% exp,
      color = habit,
      size = habit,
    ),
    width = 0.6
  ) +
  geom_point(aes(y = M %>% exp, color = habit)) +
  geom_text(
    aes(y = CI_high %>% exp, label = rank),
    size = 1 / 0.352777778,
    position = position_dodge2(width = 0.8),
    hjust = 0,
  ) +
  facet_wrap(variable ~ site, scale = "free", nrow = 4) +
  theme_pubr(8, border = T) +
  theme(strip.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  scale_y_continuous("Frequency (median ± 95% HDI)", expand = expansion(mult = c(0.04, 0.12))) +
  scale_x_discrete(
    "Species",
    limits = c(
      "Cheilinus.chlorourus",
      "Epibulus.insidiator",
      "Halichoeres.trimaculatus",
      "Meiacanthus.gramistes",
      "Parupeneus.multifasciatus",
      "Stethojulis.strigiventer",
      "Hemiglyphidodon.plagiometopon",
      "Neoglyphidodon.nigroris",
      "Pomacentrus.adelus",
      "Pomacentrus.grammorhynchus"
    ),
    labels = c(
      "Cheilinus.chlorourus",
      "Epibulus.insidiator",
      "Halichoeres.trimaculatus",
      "Meiacanthus.gramistes",
      "Parupeneus.multifasciatus",
      "Stethojulis.strigiventer",
      "Hemiglyphidodon.plagiometopon",
      "Neoglyphidodon.nigroris",
      "Pomacentrus.adelus",
      "Pomacentrus.grammorhynchus"
    ) %>% gsub("\\.", " ", .)
  ) +
  coord_flip() +
  scale_color_manual(values = c("#f1a340", "#998ec3") %>% rev) +
  scale_fill_manual(values = c("#ffffff", "#998ec3")) +
  scale_size_manual(values = c(0.3, 1) %>% rev) +
  scale_shape_manual(values = c(21, 21)) + 
  windows(19/2.54, 18/2.54)
ggsave("bite-mixed-model.pdf")


## Bayes Factor (Savage-Dickey density ratio)
posterior_samples(mbest, "^b")[, -1:-3] %>%
  bayesfactor_pointnull(
    prior = mbest %>% prior_samples %>% .[, rep(1:3, each = 3)]
  )
# Bayes Factor (Savage-Dickey density ratio)
# Parameter                      |    BF
# --------------------------------------
# b_chases_site1                 | 0.930
# b_chases_habit1                |  3.08
# b_chases_site1:habit1          | 0.973
# b_Galbitesstrikes_site1        | 0.062
# b_Galbitesstrikes_habit1       |  1.76
# b_Galbitesstrikes_site1:habit1 | 0.123
# b_inspections_site1            | 0.679
# b_inspections_habit1           |  6.16
# b_inspections_site1:habit1     | 0.846




## table
posterior_samples(mbest, "^b")[, -1:-3] %>%
  describe_posterior(
    ci = 0.95,
    test = "rope", 
    rope_ci = 0.95,
    rope_range = log(ROPE)
  ) %>%
  as.data.table
#                         Parameter     Median   CI      CI_low     CI_high ROPE_CI   ROPE_low ROPE_high ROPE_Percentage
# 1:                 b_chases_site1 -1.1862448 0.95  -4.3243921  1.38713438    0.95 -0.2231436 0.2231436     0.092484211
# 2:                b_chases_habit1  3.1885104 0.95  -0.7937064  9.10349374    0.95 -0.2231436 0.2231436     0.027526316
# 3:          b_chases_site1:habit1 -1.0555491 0.95  -5.7648471  2.67198845    0.95 -0.2231436 0.2231436     0.089452632
# 4:        b_Galbitesstrikes_site1 -0.6784106 0.95  -2.5701748  1.10751077    0.95 -0.2231436 0.2231436     0.156421053
# 5:       b_Galbitesstrikes_habit1  3.3761905 0.95   0.7646008  5.68374949    0.95 -0.2231436 0.2231436     0.000000000
# 6: b_Galbitesstrikes_site1:habit1 -1.3616135 0.95  -4.3285224  1.62972955    0.95 -0.2231436 0.2231436     0.079600000
# 7:            b_inspections_site1  0.2376686 0.95  -2.9703004  3.32045542    0.95 -0.2231436 0.2231436     0.125915789
# 8:           b_inspections_habit1 -4.1050634 0.95 -11.0556875 -0.06692739    0.95 -0.2231436 0.2231436     0.005989474
# 9:     b_inspections_site1:habit1 -0.1596785 0.95  -4.7080868  4.26822796    0.95 -0.2231436 0.2231436     0.101189474

posterior_samples(mbest, "^b") %>%
  describe_posterior(ci = 0.95, ci_method = "hdi",
                     test = "p_direction") %>%
  as.data.table %>%
  .[, `Median (95% HDI)` := paste0(round(Median, 2),
                                   " (",
                                   round(CI_low, 2),
                                   ", ",
                                   round(CI_high, 2),
                                   ")")] %>%
  .[, `exp Median (95% HDI)` := paste0(round(Median %>% exp, 4),
                                       " (",
                                       round(CI_low %>% exp, 4),
                                       ", ",
                                       round(CI_high %>% exp, 4),
                                       ")")] %>%
  .[, pd := round(pd, 3)] %>%
  .[c(2, 7:9, 1, 4:6, 3, 10:12), c(1, 7, 8, 6)]
#                          Parameter      Median (95% HDI)  exp Median (95% HDI)    pd
#  1:    b_Galbitesstrikes_Intercept  -2.21 (-3.65, -1.04)     0.11 (0.03, 0.35) 0.999
#  2:        b_Galbitesstrikes_site1   -0.68 (-2.54, 1.14)     0.51 (0.08, 3.11) 0.793
#  3:       b_Galbitesstrikes_habit1     3.38 (0.83, 5.73)  29.26 (2.29, 307.96) 0.992
#  4: b_Galbitesstrikes_site1:habit1    -1.36 (-4.3, 1.65)     0.26 (0.01, 5.23) 0.828
#  5:             b_chases_Intercept -7.75 (-11.25, -5.36)              0 (0, 0) 1.000
#  6:                 b_chases_site1   -1.19 (-4.15, 1.52)     0.31 (0.02, 4.55) 0.829
#  7:                b_chases_habit1    3.19 (-1.14, 8.58) 24.25 (0.32, 5303.17) 0.940
#  8:          b_chases_site1:habit1   -1.06 (-5.54, 2.86)       0.35 (0, 17.41) 0.721
#  9:        b_inspections_Intercept -7.41 (-10.43, -5.09)           0 (0, 0.01) 1.000
# 10:            b_inspections_site1    0.24 (-2.98, 3.31)    1.27 (0.05, 27.29) 0.565
# 11:           b_inspections_habit1  -4.11 (-10.11, 0.49)        0.02 (0, 1.63) 0.977
# 12:     b_inspections_site1:habit1   -0.16 (-4.74, 4.22)    0.85 (0.01, 68.27) 0.535









## how to intepret A+B+A:B 
model.matrix(~site * habit, d) %>% cbind(d) %>% .[, -7:-11] %>% unique %T>% print %>%
  .[, 1:4] %>% unique %>% as.matrix %>% t %>% solve
#      (Intercept) site1 habit1 site1:habit1
# [1,]        0.25  -0.5   -0.5            1
# [2,]        0.25  -0.5    0.5           -1
# [3,]        0.25   0.5   -0.5           -1
# [4,]        0.25   0.5    0.5            1

