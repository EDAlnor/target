
# **************************************************************************** #
### 0. Packages                                                             ####
# **************************************************************************** #

setwd("C:/Users/au544242/OneDrive - Aarhus universitet/Ph.d/Projekter/Target/Code/parts") # Change to your prefered working directory

library(dplyr)
library(stringr)
library(tidyr)
library(arrow)
library(statmod)
library(broom)
library(quantreg)
library(ggplot2)
library(mediation)
library(patchwork)
library(future.apply)
library(data.table)
library(stargazer)
library(sandwich)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

library(igraph)
library(readxl)
library(XML)
library(proxyC)
library(Matrix)

# **************************************************************************** #
#******************************************************************************#
### 1. Data, processing                                                     ####
#******************************************************************************#
# **************************************************************************** #

# This work is done after creating list of PA/RFA+R01 CPNs in Python. Here we
# remove 1) rows which are not in said CPNs, and 2) columns which we don't need.
# This work must be done i R because currently the project file contains nested
# dataframes.

# Remove rows ******************************************************************
ids <- read_feather('cpn_clean.feather') |> select(cpn)

proj_filtered <- readRDS('projects.rds') |> 
  semi_join(ids, by = c('core_project_num' = 'cpn')) |>  
  filter(
    mechanism_code_dc == 'RP' & #thats true for 99.9% 
    !is_active #Cannot analyse publications from active projects  
  ) 

saveRDS(proj_filtered, 'projects_filtered.rds')

# Remove columns ***************************************************************
projects_clean <- readRDS('projects_filtered.rds') |> 
  mutate(
    organization.us = ifelse(organization.org_country == 'UNITED STATES', T, F),
    
    project_start_year  = substr(project_start_date, 1, 4) %>% as.integer(),
    project_start_month = substr(project_start_date, 6, 7) %>% as.integer(),
    project_end_year    = substr(project_end_date, 1, 4) %>% as.integer(),
    project_end_month   = substr(project_end_date, 6, 7) %>% as.integer(),
    
    n_pi = sapply(principal_investigators, nrow),
    n_ic = sapply(agency_ic_fundings, nrow)
  ) %>% 
  select(
    appl_id, subproject_id, core_project_num, project_num, project_serial_num, opportunity_number, # IDs
    
    project_num_split.appl_type_code, project_num_split.activity_code, project_num_split.ic_code, project_num_split.support_year, project_num_split.full_support_year, project_num_split.suffix_code, project_detail_url, #Project info
    
    project_start_year, project_start_month, project_end_year, project_end_month, fiscal_year, project_start_date, project_end_date, #Dates
    
    n_ic, agency_code, agency_ic_admin.code, award_amount, direct_cost_amt, indirect_cost_amt, #Funding
    
    n_pi, contact_pi_name, principal_investigators, # PI
    
    organization.org_name, organization.external_org_id, organization.us #Organization
)

#Remove duplicates (there due to download proces)
projects_clean <- projects_clean[!duplicated(projects_clean$appl_id), ]

#Extract names from the nested dataframe and add them
pi_name <- projects_clean %>% 
  select(appl_id, principal_investigators) %>% 
  unnest(principal_investigators) %>% 
  filter(is_contact_pi) |>  
  select(appl_id, pi_id = profile_id, pi_lastName_firstNames = full_name, first_name)

projects_clean_2 <- inner_join(projects_clean, pi_name, by = 'appl_id') %>% 
  select(-principal_investigators) |> 
  relocate(pi_lastName_firstNames, pi_id, .before = organization.org_name)

# End
write_feather(projects_clean_2, 'projects_applid_clean.feather')
rm(directionality, ids, directionality_cols, pi_name, projects_clean, projects_clean_2)


# **************************************************************************** #
#******************************************************************************#
### 2. MeSH                                                                 ####
#******************************************************************************#
# **************************************************************************** #

# This part is done in R instead of Python because 1) I already had the script 
# to do many things https://github.com/EDAlnor/MeSH-relatedness (see here for 
# the making of 'tree.rda')  2) The graph-related things, e.g., shortest
# distances are much faster in R.

# **************************************************************************** #
#### 2.1 Tree                                                               ####
# **************************************************************************** #

# 1) Parse data *********************************

lines <- readLines('d2025.bin')

mh   <- lines[str_detect(lines, "MH = ")] |> str_remove("MH = ")
muid <- lines[str_detect(lines, "UI = ")] |> str_remove("UI = ")
txt  <- lines[str_detect(lines, 'MS = ')] |> str_remove('MS = ')

#Extract tree numbers
nrl <- which(lines == "*NEWRECORD")

tnl <- list()

for (i in 1:length(nrl)) {
  j <- nrl[i]
  
  if (j == max(nrl)) k <- nrl[length(nrl)] else k <- nrl[i+1]
  
  s <- lines[j:k]
  
  t <- list(s[str_detect(s, "MN = ")]) |> 
    lapply(function(x) gsub("MN = ", "", x))
  
  tnl[[i]] <- t
}

tn <- sapply(
  tnl,
  function(x) unlist(x) %>% paste(collapse = ";")
) 

tree <- data.frame(muid, mh, txt, tn)

# 2) Add descendants ****************************

# To get all descendants of a given MeSH, look for rows that have the same
# string (not matching full case), as there is in the tree number of the given
# MeSH. The descendants will have a longer string as their tree number, but
# their first x characters will be identical to the string that is their parents
# tree number. This logic is written to function and applied using futures,
# because its computationally heavy.
tree$pattern <- paste0(
  str_replace_all(tree$tn, ";", "\\\\.|"),
  "\\."
)

fnDesc <- function(x) {
  
  index <- which(str_detect(tree$tn, x))
  
  nodes <- paste(
    tree$muid[index],
    collapse = ";"
  ) %>% 
    ifelse(. == "", NA, .)
}

plan(multisession, workers = 6) 
tree$desc <- future_sapply(tree$pattern, fnDesc)
tree$pattern <- NULL

saveRDS(tree, 'tree_temp.rds')

# 3) Create edgelist ****************************

tree <- readRDS('tree_temp.rds')

# We start by creating a long dataframe, with one row for each tree-number of
# each MeSH.
long <- tree |>
  select(muid, tn) |> 
  separate_rows(tn, sep = ';') |> 
  filter(!muid %in% c('D005260', 'D008297', 'D000099094')) #Remove check-tags

# Removing from the tree number 'the last x digits untill a dot (including the
# dot)' will give the tree number of the parent.
child <- long |>  
  rename(tn_child = tn, child = muid) |> 
  mutate(tn_parent = str_remove(tn_child, "\\.?\\d+$")) 

#For edgelist we need edges between categories and highest level MeSH. This we
#don't need for tree.
edgelist_temp <- left_join(long, child, by = c('tn' = 'tn_parent')) |> 
  filter(!is.na(child)) |> 
  distinct(muid, child, .keep_all = T) #|> #Some Mesh are parents of others in multiple tree numbers

# Add edges between highest level MeSH and categories
cats <- edgelist_temp |> 
  filter(!str_detect(tn, '\\.')) |> #Tree numbers without '.' are highest lvl
  distinct(muid, tn) |> 
  mutate(muid_cat = str_extract(tn, '^.')) |> #Take the category letter
  rename(child = muid, muid = muid_cat) |> 
  select(-tn)

edgelist <- bind_rows(edgelist_temp, cats)

saveRDS(edgelist, 'edgelist2025.rds')

# 4) Add children (direct descendants) **********

#The tree data.frame should have 1 row pr. MeSH
children <- edgelist_temp |>
  group_by(muid) |> summarise(
    child = paste(child, collapse = ';'),
    .groups = 'drop'
  ) #Dataframe should have 1 row pr. MeSH

tree_final <- left_join(tree, children, by = 'muid') |> 
  mutate(miid = muid |> str_remove('^D') |> str_remove('^0+') |> as.integer()) |>
  relocate(miid, .after = muid)

write_feather(tree_final, "tree2025.feather")

rm(tnl, tn, tree, i, j, s, t, k, txt, lines, mh, muid, nrl, fnDesc, long, child, edgelist, tree_final, children, edgelist, cats)

# **************************************************************************** #
#### 2.2 IC                                                                 ####
# **************************************************************************** #

# Here we calculate the information content (IC) of each MeSH based on the
# formula from Ahlgreen et al.

# To see how 'tree.rda' is made visit https://github.com/EDAlnor/MeSH-relatedness

mesh <- bind_rows(
  read_feather('mesh_grants.feather') |> select(pmid, muid = descriptor_ui),
  read_feather('amesh.feather') |> select(pmid, muid = descriptor_ui)
  ) |> distinct()

tree <- read_feather('tree2025.feather') |> select(muid, desc)

# Count the frequency of each MeSH and add a column showing their descendants
mesh_freq <- mesh |> 
  dplyr::count(muid) |>  
  right_join(tree, by = 'muid')

# For each MeSH, count the frequency of their descendants.
desc_freq <- mesh_freq |> 
  filter(!is.na(desc)) |>
  select(-n) |> 
  separate_rows(desc, sep = ';') |>
  left_join(mesh_freq, by = c('desc' = 'muid')) |> #n now shows the frequency of the MeSH in 'desc'
  select(muid, n) |>
  group_by(muid) |> summarise(ndesc = sum(n, na.rm = T))

# Now calculate IC by implementing formula in Ahlgreen et al.
mesh_ic <- left_join(mesh_freq, desc_freq, by = 'muid') |> 
  select(-desc) |> 
  mutate(
    n     = ifelse(is.na(n), 0, n),
    ndesc = ifelse(is.na(ndesc), 0, ndesc),
    ntot  = n + ndesc,
    ic    = -log(ntot / sum(ntot)) %>% ifelse(is.infinite(.), NA, .)
)

write_feather(mesh_ic, 'mesh_ic.feather')

mesh_ic_for_xl <- mesh_ic |>
  select(muid, ic) |> 
  inner_join(read_feather('tree2025.feather') |> select(muid, miid, mh, txt), by ='muid') |> 
  mutate(ic = round(ic, 2))
openxlsx::write.xlsx(mesh_ic_for_xl, "mesh_ic_2025.xlsx")

rm(desc_freq, mesh_freq, tree, mesh, mesh_ic, mesh_ic_for_xl)

# **************************************************************************** #
#### 2.3 Similarity matrix                                                  ####
# **************************************************************************** #

mesh_ic <- read_feather('mesh_ic.feather') |> select(-n, -ndesc)
el <- readRDS('edgelist2025.rds') |> select(muid, child)
sum_ntot <- sum(mesh_ic$ntot)

# Weights for edges between categories and their children
cats_el <- el |> 
  filter(str_detect(muid, '^[A-Z]$') & muid != 'V') |> #V is publication type
  left_join(mesh_ic, by = c('child' = 'muid')) |> 
  rename(c_ic = ic, c_ntot = ntot) |> 
  group_by(muid) |> 
  mutate(
    ntot = sum(c_ntot),
    ic = -log(ntot / sum_ntot),
    delta_ic = abs(ic - c_ic)
  ) |>
  select(from = muid, to = child, weight = delta_ic)

# Weighted edgelist between MeSH
mesh_el <- el |> 
  filter(!str_detect(muid, '^[A-Z]$')) |> #Remove categories
  left_join(mesh_ic, by = c('child' = 'muid')) |> #Add IC of children
  rename(cic = ic) |> 
  left_join(mesh_ic, by = 'muid') |>  #Add IC of parents
  filter(!is.na(cic) & !is.na(muid)) |>  
  mutate(
    ic = ifelse(is.na(ic), cic, ic),
    delta_ic = abs(cic - ic)
  ) |>  
  select(from = muid, to = child, weight = delta_ic)

# Combine and turn into graph
wel <- bind_rows(cats_el, mesh_el) |> filter(!is.na(weight))
  
g <- graph_from_data_frame(wel, directed = F)
rm(mesh_ic, el, sum_ntot, cats_el, mesh_el, wel)

# Make distance matrix between terms. Then make a lighter version, by subsetting
# only MeSH that are present in the NIH corpus. Finally, convert to similarity
# matrix using formula in Alnor 2026, and save as dataframe-feather-file for
# import in Python.
dm <- distances(g, weight = E(g)$weight) 
rm(g)
present_mesh <- read_feather('mesh_ic.feather') |> pull(muid) |> unique()
print(all(rownames(dm) == colnames(dm)))
keep <- (colnames(dm) %in% present_mesh) 
dm <- dm[keep, keep]
print(all(rownames(dm) == colnames(dm)))

sm <- exp(-dm)
saveRDS(dm, 'dm.rds')
rm(dm)

saveRDS(sm, 'sm_mat.rds')
write_feather(data.frame(sm), 'sm.feather')
rm(sm, keep, present_mesh)


# **************************************************************************** #
#******************************************************************************#
### 3. Analyze                                                              ####
#******************************************************************************#
# **************************************************************************** #

# Data has been processed in Python. Some of the analysis is also done in
# Python, but R is much better for inference, so that part is done here.

# Load the data, define global settings, and make some functions to make the
# code more concise.

# Data
cpn <- read_feather('cpn_ts7.feather') |> 
  mutate(target = ifelse(target, 0, 1)) |> 
  select(cpn, pi_id, target, mean_ncs5, topic_switch, log_prev_mean_ncs5, log_seniority)

# Settings
taus <- c(0.1, 0.5, 0.9)

nb = 1000 # Number of bootstrap iterations in std.error. calculation

# Functions
fn_coefs_qr <- function(m, se) {
  mapply(
    function(fit, s, tau) {
      coefs <- s$coefficients
      tibble(
        term      = rownames(coefs),
        estimate  = coefs[, "Value"],
        conf.low  = coefs[, "Value"] - 1.96 * coefs[, "Std. Error"],
        conf.high = coefs[, "Value"] + 1.96 * coefs[, "Std. Error"],
        tau       = tau,
        label     = paste0(tau * 100, '%')
      )
    },
    m, se, taus,
    SIMPLIFY = FALSE
  ) |> bind_rows() |> filter(term != "(Intercept)")
}

fn_coefs_gr <- function(m, se) {
  tidy(m) |> 
    filter(term != "(Intercept)") |>
    mutate(
      std.error = se[term],
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      label = 'Mean', tau = 1
    ) |>
    select(-statistic, -p.value, -std.error) |>
    mutate(across(c(estimate, conf.low, conf.high), ~ exp(.) - 1))
}

fn_coef_plot <- function(df, title) {
  df |> 
    ggplot(aes(x = estimate, y = label)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_grid(term ~ ., scales = "free_y", switch = "y") +
    theme_minimal() +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(
        angle = 90, hjust = 0.5, margin = margin(r = 25)
      ),
      plot.title = element_text(size = 10)
    ) +
    labs(y = NULL, x = NULL) +
    ggtitle(title)
}

fn_se_mean <- function(m) {
  vcov_cl <- vcovCL(m, cluster = ~pi_id, data = cpn)
  se_cl <- sqrt(diag(vcov_cl))
}

fn_se_qr <- function(fit) {
  set.seed(123)
  summary(fit, se = "boot", bsmethod = "cluster", cluster = cpn$pi_id, R = nb)
}

fn_pval_gr <- function(m, se) {
  2 * pnorm(abs(coef(m) / se), lower.tail = FALSE)
}

# All analysis is wrapped in a function to allow easy robustness analysis with
# alternative PI publication cutoffs (5 and 10, instead of 3 as used in the main
# text) in the appendix. To replicate the main analysis, run the function body
# instead of defining the function.
fn_robust <- function(data) {

cpn <- data
  
# **************************************************************************** #
#### 3.1 Type --> Topic switch                                              ####
# **************************************************************************** #

# Mean ******************************************

m_lm <- lm(
  topic_switch ~ target + log_prev_mean_ncs5 + log_seniority,
  data = cpn
)
se_lm <- fn_se_mean(m_lm)
pval_lm  <- 2 * pt(abs(coef(m_lm) / se_lm), df = df.residual(m_lm), lower.tail = F)

coefs_lm <- tidy(m_lm) |>
  filter(term != "(Intercept)") |>
  mutate(
    std.error = se_lm[term],
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    label = 'Mean', tau = 1
  ) |>
  select(-statistic, -p.value, -std.error)

# Quantiles *************************************

m_qrs <- lapply(taus, function(tau) {
  rq(topic_switch ~ target + log_prev_mean_ncs5 + log_seniority,
     data = cpn, tau = tau)
})

se_qr <- lapply(m_qrs, fn_se_qr)
coefs_qr <- fn_coefs_qr(m_qrs, se_qr)

# Figure ****************************************

coef_df <- bind_rows(coefs_lm, coefs_qr)
fn_coef_plot(coef_df, '')

coef_df |> filter(term == 'target') |>
  fn_coef_plot('') + theme(strip.text.y.left = element_blank())
ggsave('../results/type_ts.png', height = 1.5, width = 6)

# Stargazer *************************************

se_list   <- lapply(se_qr, \(s) s$coefficients[, "Std. Error"])
pval_list <- lapply(se_qr, \(s) s$coefficients[, "Pr(>|t|)"])

stargazer(
  m_lm, m_qrs[[1]], m_qrs[[2]], m_qrs[[3]],
  se   = c(list(se_lm), se_list),
  p    = c(list(pval_lm), pval_list),
  covariate.labels = c('Grant type (targeted = 1)', 'ln(PI pre-MNCS)', 'ln(Seniority)'),
  out   = '../results/reg_type_ts.html',
  
  omit.stat = 'all',
  star.cutoffs = c(0.05, 0.01, 0.001),
  type  = "html",
  column.labels = c('Mean', paste0('τ = ', taus)),
  model.numbers = F,
  dep.var.caption = '',
  single.row = T
)

rm(m_lm,  m_qrs, se_qr, se_list, pval_list, coef_df, coefs_lm, coefs_qr, pval_lm, se_lm)

# **************************************************************************** #
#### 3.2 Topic switch --> Citation impact                                   ####
# **************************************************************************** #

# Mean center topic switch and pre-MNCS to use in the interaction analysis
cpn <- cpn |> mutate( 
  log_prev_mean_ncs5_mc = log_prev_mean_ncs5 - mean(log_prev_mean_ncs5),
  topic_switch_mc = topic_switch - mean(topic_switch),
  log_seniority_mc = log_seniority - mean(log_seniority)
)

##### 3.2.1 Naive ********************************************************* ####

# Mean ******************************************

m_gr <- glm(
  mean_ncs5 ~ topic_switch,
  family = Gamma(link = 'log'), data = cpn
) 

se_gr <- fn_se_mean(m_gr)
coefs_gr <- fn_coefs_gr(m_gr, se_gr)

# Quantiles *************************************

m_qrs <- lapply(taus, function(tau) {
  rq(mean_ncs5 ~ topic_switch,
     data = cpn, tau = tau)
})

se_qr <- lapply(m_qrs, fn_se_qr)
coefs_qr <- fn_coefs_qr(m_qrs, se_qr)

# Figure ****************************************

coef_df_naiv <- bind_rows(coefs_gr, coefs_qr) |> mutate(term = 'Topic switch')
p_ts_ci <- fn_coef_plot(coef_df_naiv, 'Naïve')
p_ts_ci
ggsave('../results/ts_ci.png', height = 1.5, width = 6)

# Stargazer ****************************************

stargazer(
  m_gr, m_qrs[[1]], m_qrs[[2]], m_qrs[[3]],
  se = c(
    list(se_gr), 
    lapply(se_qr, \(s) s$coefficients[, "Std. Error"])
  ),
  p = c(
    list(fn_pval_gr(m_gr, se_gr)),
    lapply(se_qr, \(s) s$coefficients[, "Pr(>|t|)"])
  ),
  out = '../results/reg_ts_ci.html',
  covariate.labels = 'Topic Switch',
  
  star.cutoffs = c(0.05, 0.01, 0.001),
  type = 'html',
  omit.stat = 'all',
  column.labels = c('Mean', paste0('τ = ', taus)),
  model.numbers = F,
  dep.var.caption = '',
  single.row = T
)

##### 3.2.2 With controls ************************************************* ####

# Mean ******************************************

m_gr_cont <- glm(
  mean_ncs5 ~ topic_switch + log_prev_mean_ncs5 + log_seniority,
  family = Gamma(link = 'log'), data = cpn
)

se_gr_cont <- fn_se_mean(m_gr_cont)
pval_gr_cont <- fn_pval_gr(m_gr_cont, se_gr_cont)
coef_gr_cont <- fn_coefs_gr(m_gr_cont, se_gr_cont)

# Quantiles *************************************

m_qrs_cont <- lapply(taus, function(tau) {
  rq(mean_ncs5 ~ topic_switch + log_prev_mean_ncs5 + log_seniority,
     data = cpn, tau = tau)
})

se_qr_cont <- lapply(m_qrs_cont, fn_se_qr)
coefs_qr_cont <- fn_coefs_qr(m_qrs_cont, se_qr_cont)

# Figure ****************************************

coef_df_cont <- bind_rows(coef_gr_cont, coefs_qr_cont) |> 
  mutate(term = case_when(
    term == 'topic_switch' ~ 'Topic switch',
    term == 'log_prev_mean_ncs5' ~ 'ln(PI pre-MNCS)',
    term == 'log_seniority' ~ 'ln(Seniority)'
  ))

p_ts_ci_cont <- fn_coef_plot(
  coef_df_cont, 
  'Controlling for pre-grant mean NCS'
)
p_ts_ci_cont
ggsave('../results/ts_ci_control.png', height = 3, width = 6)

# Stargazer *************************************

stargazer(
  m_gr_cont, m_qrs_cont[[1]], m_qrs_cont[[2]], m_qrs_cont[[3]],
  se = c(
    list(se_gr_cont), 
    lapply(se_qr_cont, \(s) s$coefficients[, "Std. Error"])
  ),
  p = c(
    list(fn_pval_gr(m_gr_cont, se_gr_cont)),
    lapply(se_qr_cont, \(s) s$coefficients[, "Pr(>|t|)"])
  ),
  out = '../results/reg_ts_ci_cont.html',
  covariate.labels = c('Topic Switch', 'ln(PI pre-MNCS)', 'ln(Seniority)'),
  
  star.cutoffs = c(0.05, 0.01, 0.001),
  type = 'html',
  omit.stat = 'all',
  column.labels = c('Mean', paste0('τ = ', taus)),
  model.numbers = F,
  dep.var.caption = '',
  single.row = T
)

##### 3.2.3 Interaction *************************************************** ####

# Mean

m_gr_int <- glm(
  mean_ncs5 ~ topic_switch_mc * log_prev_mean_ncs5_mc + log_seniority_mc,
  family = Gamma(link = 'log'), data = cpn
) 

se_gr_int <- fn_se_mean(m_gr_int)
pval_gr_int <- fn_pval_gr(m_gr_int, se_gr_int)
coef_gr_int <- fn_coefs_gr(m_gr_int, se_gr_int)

# Quantiles

m_qrs_int <- lapply(taus, function(tau) {
  rq(mean_ncs5 ~ topic_switch_mc * log_prev_mean_ncs5_mc + log_seniority_mc,
     data = cpn, tau = tau)
})

se_qr_int <- lapply(m_qrs_int, fn_se_qr)
coefs_qr_int <- fn_coefs_qr(m_qrs_int, se_qr_int)

# Figure

coef_df_int <- bind_rows(coefs_qr_int, coef_gr_int) |> 
  mutate(term = case_when(
    term == 'topic_switch_mc' ~ 'Topic switch',
    term == 'log_prev_mean_ncs5_mc' ~ 'ln(PI pre-MNCS)',
    term == 'log_seniority' ~ 'ln(Seniority)',
    term == 'topic_switch_mc:log_prev_mean_ncs5_mc' ~ 'Topic switch ×\n ln(PI pre-MNCS)'
  ))

p_ts_ci_int <- fn_coef_plot(
  coef_df_int, 
  'Interacting with pre-grant mean NCS'
)
p_ts_ci_int
ggsave('../results/ts_ci_interact.png', height = 4.5, width = 6)

# Stargazer

stargazer(
  m_gr_int, m_qrs_int[[1]], m_qrs_int[[2]], m_qrs_int[[3]],
  se = c(
    list(se_gr_int), 
    lapply(se_qr_int, \(s) s$coefficients[, "Std. Error"])
  ),
  p = c(
    list(fn_pval_gr(m_gr_int, se_gr_int)),
    lapply(se_qr_int, \(s) s$coefficients[, "Pr(>|t|)"])
  ),
  out = '../results/reg_ts_ci_int.html',
  order = c(1, 2, 4, 3),
  covariate.labels = c('Topic switch', 'ln(PI pre-MNCS)', 'Topic switch × ln(PI pre-MNCS)', 'ln(Seniority)'),
  
  star.cutoffs = c(0.05, 0.01, 0.001),
  type = 'html',
  omit.stat = 'all',
  column.labels = c('Mean', paste0('τ = ', taus)),
  model.numbers = F,
  dep.var.caption = '',
  single.row = T
)

# Plot all coefficients ********************************************************

p_ts_ci_all <- p_ts_ci / p_ts_ci_cont / p_ts_ci_int +
  plot_layout(height = 1:3)
p_ts_ci_all

# Plot coefficients of interest
p_ts_ci <- fn_coef_plot(
  coef_df_naiv |> filter(term == 'Topic switch'), 
  'Naïve'
)
p_ts_ci_control <- fn_coef_plot(
  coef_df_cont |> filter(term == 'Topic switch'),
  'With controls'
)
p_ts_ci_interact <- fn_coef_plot(
  coef_df_int |> filter(str_detect(term, 'Topic switch')),
  'With controls and ln(PI pre-MNCS) as interaction term'
)

p_ts_ci_all_clean <- p_ts_ci / p_ts_ci_control / p_ts_ci_interact +
  plot_layout(height = c(1, 1, 2))
p_ts_ci_all_clean
ggsave('../results/ts_cs_all.png', height = 6, width = 6)

# Plot marginal effects ********************************************************

pre <- cpn$log_prev_mean_ncs5

cpn <- cpn |> mutate(
  pre_10 = pre - quantile(pre, 0.1),
  pre_50 = pre - quantile(pre, 0.5),
  pre_90 = pre - quantile(pre, 0.9)
)

m10 <- rq(mean_ncs5 ~ topic_switch * pre_10 + log_seniority, data = cpn, tau = taus)
m50 <- rq(mean_ncs5 ~ topic_switch * pre_50 + log_seniority, data = cpn, tau = taus)
m90 <- rq(mean_ncs5 ~ topic_switch * pre_90 + log_seniority, data = cpn, tau = taus)

tidy_m10 <- tidy(m10, se.type = 'boot', R = nb) |> mutate(pre_grant_ncs_perc = 10)
tidy_m50 <- tidy(m50, se.type = 'boot', R = nb) |> mutate(pre_grant_ncs_perc = 50)
tidy_m90 <- tidy(m90, se.type = 'boot', R = nb) |> mutate(pre_grant_ncs_perc = 90)

coef_df <- bind_rows(tidy_m10, tidy_m50, tidy_m90) |>
  filter(term == "topic_switch") |> 
  mutate(
    grant_ncs_perc = tau*100, 
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error) |>
  select(-term, -tau)

ggplot(
  coef_df,
  aes(
    x = pre_grant_ncs_perc,
    y = estimate,
    color = factor(grant_ncs_perc, levels = rev(taus*100))
  )) +
  geom_line() +
  geom_point() +
  geom_ribbon(
    aes(
      ymin = conf.low,
      ymax = conf.high,
      fill = factor(grant_ncs_perc, levels = rev(taus*100))
    ),
    alpha = 0.2,
    color = NA) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(
    "ln(PI pre-MNCS) percentile",
    breaks = unique(coef_df$pre_grant_ncs_perc)
  ) +
  scale_color_viridis_d("Grant MNCS\npercentile") +
  scale_fill_viridis_d("Grant MNCS\npercentile") +
  theme_minimal() +
  labs(y = NULL)

ggsave('../results/tsXpre_ci.png', height = 3, width = 6)

rm(m_gr, coef_df_naiv, p_ts_ci, m_gr_cont, coef_df_cont, p_ts_ci_cont, m_gr_int, m10, m50, m90, p_ts_ci_all, p_ts_ci_all_clean, pre, coef_df_int, coef_gr_cont, coef_gr_int, coefs_gr, coefs_qr, coefs_qr_cont, coefs_qr_int, m_gr_tx, m_qrs, m_qrs_cont, m_qrs_int, p_ts_ci_control, p_ts_ci_int, p_ts_ci_interact, pval_gr_cont, pval_gr_int, se_gr, se_gr_cont, se_gr_int, se_qr, se_qr_cont, se_qr_int, coef_df)

# **************************************************************************** #
#### 3.3 Mediation: Type --> TS --> CI                                      ####
# **************************************************************************** #

##### 3.3.1 Y=T+X ********************************************************* ####

# Gamma regression ******************************

m_gr_tx <- glm(
  mean_ncs5 ~ target + log_prev_mean_ncs5 + log_seniority,
  family = Gamma(link = "log"), data = cpn
)

se_gr_tx <- fn_se_mean(m_gr_tx)
coefs_gr_tx <- fn_coefs_gr(m_gr_tx, se_gr_tx)

# Quantiles *************************************

m_qrs_tx <- lapply(taus, function(tau) {
  rq(
    mean_ncs5 ~ target + log_prev_mean_ncs5 + log_seniority,
    data = cpn, tau = tau
  )
})

se_qr_tx <- lapply(m_qrs_tx, fn_se_qr)
coefs_qr_tx <- fn_coefs_qr(m_qrs_tx, se_qr_tx)

# Plot ******************************************

coefs_tx <- bind_rows(coefs_gr_tx, coefs_qr_tx)
fn_coef_plot(coefs_tx, '')

coefs_tx <- coefs_tx |> 
  filter(term == 'target') |> 
  mutate(term = 'Grant type')
p1 <- fn_coef_plot(coefs_tx, 'Controlling for ln(PI pre-MNCS) and ln(Seniority)')

##### 3.3.2 Y=T+X+M ******************************************************* ####

# Gamma *****************************************

m_gr_tmx <- glm(
  mean_ncs5 ~ target + topic_switch + log_prev_mean_ncs5 + log_seniority, 
  family = Gamma(link = 'log'), data = cpn
)

se_gr_tmx <- fn_se_mean(m_gr_tmx)
coefs_gr_tmx <- fn_coefs_gr(m_gr_tmx, se_gr_tmx)

# Quantile **************************************

m_qrs_tmx <- lapply(taus, function(tau) {
  rq(
    mean_ncs5 ~ target + topic_switch + log_prev_mean_ncs5 + log_seniority,
    data = cpn, tau = tau
  )
})

se_qr_tmx <- lapply(m_qrs_tmx, fn_se_qr)
coefs_qr_tmx <- fn_coefs_qr(m_qrs_tmx, se_qr_tmx)

# Plot ******************************************

coefs_txm <- bind_rows(coefs_gr_tmx, coefs_qr_tmx)

fn_coef_plot(coefs_txm, '')
coefs_txm <- coefs_txm |> 
  filter(term %in% c('target', 'topic_switch')) |> 
  mutate(term = case_when(
    term == 'target' ~ 'Grant type',
    term == 'topic_switch' ~ 'Topic switch')
)
p2 <- fn_coef_plot(coefs_txm, 'Controlling for ln(PI pre-MNCS), ln(Seniority), and topic switch')

x_ax <- scale_x_continuous(limits = c(-0.4, 0.85))

p_m <- ((p1 + x_ax) / (p2 + x_ax)) +
  plot_layout(heights = c(1, 2))
p_m

ggsave('../results/TMX.png', height = 4, width = 6)

# Table ******************************************

m1 <- m_gr_tx # stargazer can't handle "long" model names in large tables
m2 <- m_qrs_tx[[1]]
m3 <- m_qrs_tx[[2]]
m4 <- m_qrs_tx[[3]]
m5 <- m_gr_tmx
m6 <- m_qrs_tmx[[1]]
m7 <- m_qrs_tmx[[2]]
m8 <- m_qrs_tmx[[3]]

stargazer(
  m1, m2, m3, m4, m5, m6, m7, m8,
  se = c(
    list(se_gr_tx),
    lapply(se_qr_tx, \(s) s$coefficients[, "Std. Error"]),
    list(se_gr_tmx),
    lapply(se_qr_tmx, \(s) s$coefficients[, "Std. Error"])
  ),
  p = c(
    list(fn_pval_gr(m_gr_tx, se_gr_tx)),
    lapply(se_qr_tx, \(s) s$coefficients[, "Pr(>|t|)"]),
    list(fn_pval_gr(m_gr_tmx, se_gr_tmx)),
    lapply(se_qr_tmx, \(s) s$coefficients[, "Pr(>|t|)"])
  ),
  covariate.labels = c('Grant type (targeted = 1)', 'Topic switch', 'ln(PI pre-MNCS)', 'ln(Seniority)'),
  out = '../results/reg_mediation.html',
  
  star.cutoffs = c(0.05, 0.01, 0.001),
  type = 'html',
  omit.stat = 'all',
  column.labels = rep(c('Mean', paste0('τ = ', taus)), 2),
  model.numbers = F,
  dep.var.caption = '',
  single.row = T
)

rm(coefs_gr_tmx, coefs_gr_tx, coefs_qr, coefs_qr_tmx, coefs_qr_tx, coefs_tx, coefs_txm, m_gr_tmx, m_gr_tx, m_qrs_tmx, m_qrs_tx, m_rqs_tx, m1, m2, m3, m4, m5, m6, m7, m8, p_m, p1, p2, se_gr_tmx, se_gr_tx, se_qr_tmx, se_qr_tx, x_ax)
}

##### 3.3.3 Simulations *************************************************** ####

# Unlike the simple quantile, linear, and gamma regressions, the mediation
# analysis takes quite some time to compute, so we save the computations.

# Compute

plan(multisession, workers = 5)

fn_med <- function(Y) {
  mediate(M, Y, boot = T, treat = 'target', mediator = 'topic_switch')
}

M <- lm(topic_switch ~ target + log_prev_mean_ncs5 + log_seniority, data = cpn)

fmla <- mean_ncs5 ~ target + topic_switch + log_prev_mean_ncs5 + log_seniority

Y_mean <- glm(fmla, family = Gamma(link = 'log'), data = cpn)
Y_10 <- rq(fmla, tau = 0.1 , data = cpn)
Y_50 <- rq(fmla, tau = 0.5 , data = cpn)
Y_90 <- rq(fmla, tau = 0.9 , data = cpn)

mediation <- future_lapply(list(Y_mean, Y_10, Y_50, Y_90), fn_med)
saveRDS(mediation, 'mediation')

rm(M, Y_mean, Y_10, Y_50, Y_90, mediation, fn_med, fmla)

# Inspect 

med  <- readRDS('mediation')

coefs <- data.frame(
  term = rep(c('Mean', paste0(c('10%', '50%', '90%'))), 3),
  type = rep(c('Mediation effect', 'Direct effect', 'Total effect'), each = 4),
  estimate = c(
    sapply(med[1:4], \(x) x[['d0']]),
    sapply(med[1:4], \(x) x[['z0']]),
    sapply(med[1:4], \(x) x[['tau.coef']])
  ),
  conf.low = c(
    sapply(med[1:4], \(x) x[["d.avg.ci"]][1]),
    sapply(med[1:4], \(x) x[["z.avg.ci"]][1]),
    sapply(med[1:4], \(x) x[["tau.ci"]][1])
  ),
  conf.high = c(
    sapply(med[1:4], \(x) x[["d.avg.ci"]][2]),
    sapply(med[1:4], \(x) x[["z.avg.ci"]][2]),
    sapply(med[1:4], \(x) x[["tau.ci"]][2])
  )
)

plot_full <- coefs |> 
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(type ~ ., scales = "free_y", switch = "y") +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90, hjust = 0.5, margin = margin(r = 25))
  ) +
  labs(y = NULL, x = NULL)

plot_full

plot_lean <- coefs |> 
  filter(type == 'Mediation effect') |> 
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(y = NULL, x = NULL)
  
plot_lean

ggsave('../results/mediation.png', height = 1.5, width = 6)

rm(med, coefs, plot_lean, plot_full)

# **************************************************************************** #
#******************************************************************************#
### 4. Supplementary                                                        ####
#******************************************************************************#
# **************************************************************************** #

# Here we do the robust analysis: Changing cutoff for minimum number of
# publications with mesh and citation data to 5 and 10.

# Create a folder in current wd called 'robust' and then change wd so we don't
# need to change the output file location in the function. 

cpn_full <- read_feather('cpn_ts7.feather') |> 
  mutate(target = ifelse(target, 0, 1)) |> 
  select(cpn, pi_id, target, mean_ncs5, topic_switch, log_prev_mean_ncs5, log_seniority, prev_npub, pi_prev_npub_w_mesh, npub_w_ci, npub_w_mesh)

setwd('robust')

th = 5
cpn <- cpn_full |> filter(prev_npub >= th & pi_prev_npub_w_mesh >= th & npub_w_ci >= th & npub_w_mesh >= th)
print(nrow(cpn))
fn_robust(cpn)

th = 10
cpn <- cpn_full |> filter(prev_npub >= th & pi_prev_npub_w_mesh >= th & npub_w_ci >= th & npub_w_mesh >= th)
print(nrow(cpn))
fn_robust(cpn)

# Do the mediation analysis manually afterwards. Can't capture that in the
# function, because it errors because of the future.lapply in combination with
# mediation, so isn't included in `fn_robust`

