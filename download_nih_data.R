
library(rvest)
library(readxl)
library(jsonlite)
library(rentrez)
library(dplyr)
library(stringr)
library(data.table)

setwd("C:/Users/au544242/OneDrive - Aarhus universitet/Ph.d/Projekter/Target/Code/parts") # Change to your prefered working directory

# **************************************************************************** #
#******************************************************************************#
### 1. Data, download                                                       ####
#******************************************************************************#
# **************************************************************************** #

# This script was very quickly written in March 2024, because I had to download
# data before it was potentially removed due to the Trump administrations
# DEI-policy. Hence the mess. 

# **************************************************************************** #
#### 2.1 Calls                                                              ####
# **************************************************************************** #

# The calls are downloaded as .csv files from
# https://grants.nih.gov/funding/nih-guide-for-grants-and-contracts
# Here they are cleaned and prepared.

# Clean data on NIH calls

t1 <- fread('NIH_Guide_Results_91-09.csv')
t2 <- fread('NIH_Guide_Results_10-19.csv')
t3 <- fread('NIH_Guide_Results_20-25_27mar.csv')

calls <- bind_rows(t1, t2, t3) %>% rename_with(tolower)

#Add deleted calls (Diversity, Equity)
t1 <- read_excel('2_21_2024-AllGuideResultsReport.xlsx') %>% rename_with(tolower)
t2 <- setdiff(t1$document_number, calls$document_number)
t3 <- t1 %>% filter(document_number %in% t2)

calls <- bind_rows(calls, t3)

saveRDS(calls, 'calls.rds')

#Scraping calls text
calls <- fread('call_urls.txt') %>%
  mutate(
    txt = NA,
    exp_y = str_extract(expired_date, '\\d{4}$') %>% as.integer()
  ) %>% 
  select(-expired_date)

for (i in 1:nrow(calls)) {
  
  if (i %% 100 == 0) cat(i, '\t', format(Sys.time(), '%H:%M:%S'), '\n')
  
  url <- calls$url[i]
  
  txt <- tryCatch(
    {
      read_html(url) %>% html_text2()
    },
    error = function(e) NA_character_
  )
  
  calls$txt[i] <- txt
  
  Sys.sleep(1)
}

write_feather(calls, 'calls_txt.feather')

# **************************************************************************** #
#### 2.2 Projects                                                           ####
# **************************************************************************** #

# This section downloades data on each project (grant) that each call has
# sponsored. When the data was downloaded, for unknown reasons, data on all
# calls was not fetched. Therefore the download proceeds in 3 iterations.

##### 2.2.1 Iteration 1 *************************************************** ####

#Request parameters
dns <- readRDS('calls.rds') %>% pull(document_number)

url <- "https://api.reporter.nih.gov/v2/projects/search"
user <- "Emil D. Alnor, Aarhus University, ea@ps.au.dk"

results <- list()
tls <- Sys.time()
save_is <- numeric(0)

for (i in 1:ceiling(length(dns)/20)) {
  
  if (i %% 20 == 0) cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  #Opportunity numbers fetched in current iteration
  min <- (i - 1)*20 + 1
  max <- min + 19
  if (max > length(dns)) max <- length(dns)
  dns_it <- dns[min:max]
  
  #Create request and fetch
  payload <- list(
    criteria = list(opportunity_numbers = dns_it),
    limit = 500
  )
  
  response <- POST(url, body = payload, encode = "json", user_agent(user)) 
  
  proj <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  Sys.sleep(1.01)
  
  #If no projects are sponsored by the calls, proceed
  if (length(proj) == 0) next
  
  #When number of projects exceed 500, paginate 
  np <- nrow(proj)
  os <- 499
  j <- 1
  projs <- list()
  
  while (np == 500) {
    
    payload <- list(
      criteria = list(opportunity_numbers = dns_it),
      limit = 500,
      offset = os
    )
    
    response <- POST(url, body = payload, encode = "json", user_agent(user)) 
    
    projs[[j]] <- response %>%
      content(as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      `[[`("results")
    
    #Update
    np <- nrow(projs[[j]])
    
    os=os+500
    j=j+1
    
    Sys.sleep(1.01)
  }
  
  results[[i]] <- bind_rows(proj, projs %>% bind_rows())
  
  #Save the results every 30 minutes
  if (difftime(Sys.time(), tls, units = "mins") > 30) {
    saveRDS(results, paste0('projects', i, '.rds'))
    results <- list()
    save_is <- c(save_is, i)
    tls <- Sys.time()
    gc()
  }
  #No need to sleep, since already slept before / inside while-loop
}

saveRDS(results, paste0('projects', i, '.rds'))
saveRDS(save_is, 'save_is.rds')

##### 2.2.2 Iteration 2 *************************************************** ####

#Determine calls not fetched

proj_ids <- list()

for (i in save_is) {
  proj_ids[[i]] <- readRDS(paste0('projects', i, '.rds')) %>% bind_rows() %>% 
    select(appl_id, project_num, project_serial_num, opportunity_number, core_project_num)
}
proj_ids_it1 <- proj_ids %>% bind_rows()
saveRDS(proj_ids_it1, 'proj_ids_it1.rds')

ons <- proj_ids_it1 %>% pull(opportunity_number) %>% unique()

sd <- setdiff(dns, ons)

rm(proj_ids, proj_id, ons, proj_ids_it1, dns)

#Try again

results <- list()
tls <- Sys.time()
save_is <- numeric(0)

for (i in 1:ceiling(length(sd)/20)) {
  
  if (i %% 20 == 0) cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  #Opportunity numbers fetched in current iteration
  min <- (i - 1)*20 + 1
  max <- min + 19
  if (max > length(sd)) max <- length(sd)
  dns_it <- sd[min:max]
  
  #Create request and fetch
  payload <- list(
    criteria = list(opportunity_numbers = dns_it),
    limit = 500
  )
  
  response <- POST(url, body = payload, encode = "json", user_agent(user)) 
  
  proj <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  Sys.sleep(1.01)
  
  #If no projects are sponsored by the calls, proceed
  if (length(proj) == 0) next
  
  #When number of projects exceed 500, paginate 
  np <- nrow(proj)
  os <- 499
  j <- 1
  projs <- list()
  
  while (np == 500) {
    
    payload <- list(
      criteria = list(opportunity_numbers = dns_it),
      limit = 500,
      offset = os
    )
    
    response <- POST(url, body = payload, encode = "json", user_agent(user)) 
    
    projs[[j]] <- response %>%
      content(as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      `[[`("results")
    
    #Update
    np <- nrow(projs[[j]])
    
    os=os+500
    j=j+1
    
    Sys.sleep(1.01)
  }
  
  results[[i]] <- bind_rows(proj, projs %>% bind_rows())
}

saveRDS(results, paste0('projects_it2.rds'))

#All was fetched within 30 mins
proj_ids_it2 <- results %>% bind_rows()

ons2 <- proj_ids_it2 %>% pull(opportunity_number) %>% unique()
ons1 <- readRDS('proj_ids_it1.rds') %>% pull(opportunity_number) %>% unique()

ons <- c(ons1, ons2)
sd <- setdiff(readRDS('calls.rds') %>% pull(document_number), ons)

##### 2.2.3 Iteration 3 *************************************************** ####

#Try again

results <- list()

for (i in 1:ceiling(length(sd)/500)) {
  
  if (i %% 1000 == 0) cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  #Opportunity numbers fetched in current iteration
  min <- (i - 1)*500 + 1
  max <- min + 499
  if (max > length(sd)) max <- length(sd)
  dns_it <- sd[min:max]
  
  #Create request and fetch
  payload <- list(
    criteria = list(opportunity_numbers = dns_it),
    limit = 500
  )
  
  response <- POST(url, body = payload, encode = "json", user_agent(user)) 
  
  proj <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  cat('Iteration: ', i, '   ', '#Projects: ', length(proj), '\n')
  
  Sys.sleep(1.01)
  
  #If no projects are sponsored by the calls, proceed
  if (length(proj) == 0) next
  
  #When number of projects exceed 500, paginate 
  np <- nrow(proj)
  os <- 499
  j <- 1
  projs <- list()
  
  while (np == 500) {
    
    payload <- list(
      criteria = list(opportunity_numbers = dns_it),
      limit = 500,
      offset = os
    )
    
    response <- POST(url, body = payload, encode = "json", user_agent(user)) 
    
    projs[[j]] <- response %>%
      content(as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      `[[`("results")
    
    #Update
    np <- nrow(projs[[j]])
    
    os=os+500
    j=j+1
    
    Sys.sleep(1.01)
  }
  
  results[[i]] <- bind_rows(proj, projs %>% bind_rows())
}

projects_it3 <- results %>% bind_rows()
saveRDS(projects_it3, 'projects_it3.rds')

##### 2.2.4 Combine files ************************************************* ####

sufx <- c('8', '178', '337', '461', '610', '710', '785', '811', '978', '_it2', '_it3')

projects <- bind_rows(lapply(sufx, \(x) {
  readRDS(paste0('projects', x, '.rds')) %>% bind_rows
}))
saveRDS(projects, 'projects.rds')

#File with no lists, so can be saved in .feather and read in Python

#Save a seperate file just with ID variables, since file size is large
proj_ids <- projects %>% select(appl_id, project_num, project_serial_num, opportunity_number, core_project_num, project_detail_url)
saveRDS(proj_ids, 'proj_ids.rds')

rm(payload, proj, proj_ids, projects, projs, response, results, dns, dns_it, i, j, max, min, np, os, save_is, tls, url, user, on, ons, ons1, ons2, sd, sufx)

# **************************************************************************** #
#### 2.3 Publications                                                       ####
# **************************************************************************** #

# This section downloads PMIDS of publications from each project. As in section
# 2, When the data was downloaded, for unknown reasons, data on all projects was
# not fetched. Therefore the download proceeds in 3 iterations.

url <- "https://api.reporter.nih.gov/v2/publications/search"

headers <- add_headers(
  accept = "application/json",
  `Content-Type` = "application/json"
)

##### 3.1.1 Iteration 1 *************************************************** ####

pns <- readRDS('proj_ids.rds') %>% pull(core_project_num) %>% unique()

results <- list()
problems <- list()
tls <- Sys.time()

for (i in 1:ceiling(length(pns)/20)) {
  
  if (i %% 20 == 0) cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  #Project numbers fetched in current iterations
  min <- (i - 1)*20 + 1
  max <- min + 19
  if (max > length(pns)) max <- length(pns)
  pns_it <- pns[min:max]
  
  #Create request and fetch
  payload <- list(
    criteria = list(core_project_nums = pns_it),
    limit = 500
  ) %>% 
    toJSON(auto_unbox = T)
  
  response <- POST(url, headers, body = payload)
  
  if (response$status_code != 200) {
    problems[[i]] <- pns_it
    print('Response error')
    next
  }
  
  pubs <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  Sys.sleep(1.01)
  
  #If projects have no publications, proceeed
  if (length(pubs) == 0) next
  
  #When number of publications exceed 500 we need to paginate 
  np <- nrow(pubs)
  os <- 499
  j <- 1
  pubs_exceed <- list()
  
  while (np == 500) {
    
    if (j==20) { 
      print('Many pubs')
      problems[[i]] <- pns_it
      break
    } #Some projects have more than 10 000 publications, need manual inspection
    
    payload <- list(
      criteria = list(core_project_nums = pns_it),
      limit = 500,
      offset = os
    ) %>% 
      toJSON(auto_unbox = T)
    
    response <- POST(url, headers, body = payload)
    
    pubs_exceed[[j]] <- response %>%
      content(as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      `[[`("results")
    
    #Update
    np <- nrow(pubs_exceed[[j]])
    if (is.null(np)) break
    
    os=os+500
    j=j+1
    
    Sys.sleep(1.01)
  }
  
  if (j == 20) next
  
  results[[i]] <- bind_rows(pubs, pubs_exceed %>% bind_rows())
  
  #Save the results every 30 minutes
  if (difftime(Sys.time(), tls, units = "mins") > 30) {
    saveRDS(results, paste0('pubs', i, '.rds'))
    saveRDS(problems, paste0('problems', i, '.rds'))
    results <- list()
    problems <- list()
    tls <- Sys.time()
    gc()
  }
  #No need to sleep, since already slept before / inside while-loop
}

#Combine

save_is <- c(27, 719, 1359, 2216, 2501, 2521, 3759, 4727, 5316, 6017, 6926, 7945, 9029, 10228, 11223, 12613, 13159)
files <- paste0('pubs', save_is, '.rds')
pubs <- bind_rows(lapply(files, readRDS)) %>% distinct()
saveRDS(pubs, 'pubs_it1.rds')

pns_fetched <- pubs$coreproject %>% unique()
sd <- setdiff(pns, pns_fetched)
saveRDS(sd, 'not_fetched_pubs_it1.rds')

##### 3.1.2 Iteration 2 *************************************************** ####

pns <- readRDS('not_fetched_pubs_it1.rds')

#Check if any projects actually have publications

has_pubs <- list()
tls <- Sys.time()

for (i in 1:ceiling(length(pns)/500)) {
  
  if (i %% 20 == 0) cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  #Project numbers fetched in current iterations
  min <- (i - 1)*500 + 1
  max <- min + 499
  if (max > length(pns)) max <- length(pns)
  pns_it <- pns[min:max]
  
  #Create request and fetch
  payload <- list(
    criteria = list(core_project_nums = pns_it),
    limit = 500
  ) %>% 
    toJSON(auto_unbox = T)
  
  response <- POST(url, headers, body = payload)
  Sys.sleep(1.01)
  # Manual inspection of the 'project_details_url' of 10 projects in the chunks returning 500 showed that they didnt have any publications. 1 of these may have been removed due to the Trump administrations DEI policy, since it was about sexual minorities: https://reporter.nih.gov/project-details/10360448
  if (response$status_code == 500) next
  
  pubs <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  if (length(pubs) > 0) has_pubs[[i]] <- pns_it
}

# For those chunks that have publications, break down into smaller sizes
pns <- has_pubs %>% unlist()
saveRDS(pns, 'has_pubs_it2.rds')
pns <- readRDS('has_pubs_it2.rds')
has_pubs <- list()

for (i in 1:ceiling(length(pns)/10)) {
  
  if (i %% 20 == 0) cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  min <- (i - 1)*10 + 1
  max <- min + 9
  if (max > length(pns)) max <- length(pns)
  pns_it <- pns[min:max]
  
  payload <- list(
    criteria = list(core_project_nums = pns_it),
    limit = 500
  ) %>% 
    toJSON(auto_unbox = T)
  
  response <- POST(url, headers, body = payload)
  Sys.sleep(1.01)
  
  if (response$status_code == 500) next
  
  pubs <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  if (length(pubs) > 0) has_pubs[[i]] <- pns_it
}

pns <- has_pubs %>% unlist() %>% unique()
saveRDS(pns, 'has_pubs_it2_small.rds')


#Now we can fetch
pns <- readRDS('has_pubs_it2_small.rds')
results <- list()
many_pubs <- list()

for (i in 27:ceiling(length(pns)/5)) {
  
  if (i %% 20 == 0) cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  min <- (i - 1)*5 + 1
  max <- min + 4
  if (max > length(pns)) max <- length(pns)
  pns_it <- pns[min:max]
  
  payload <- list(
    criteria = list(core_project_nums = pns_it),
    limit = 500
  ) %>% 
    toJSON(auto_unbox = T)
  
  response <- POST(url, headers, body = payload)
  Sys.sleep(1.01)
  
  pubs <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  if (length(pubs) == 0) next
  
  np <- nrow(pubs)
  
  if (np < 500) {
    
    results[[i]] <- pubs
    next
    
  } else if (np == 500) {
    
    os <- 499
    j <- 1
    pubs_exceed <- list()
    
    while (np == 500) {
      
      if (j==20) { 
        print('Many pubs')
        many_pubs[[i]] <- pns_it
        break
      } #Some projects have more than 10 000 publications, need manual inspection
      
      payload <- list(
        criteria = list(core_project_nums = pns_it),
        limit = 500,
        offset = os
      ) %>% 
        toJSON(auto_unbox = T)
      
      response <- POST(url, headers, body = payload)
      Sys.sleep(1.01)
      
      pubs_exceed[[j]] <- response %>%
        content(as = "text", encoding = "UTF-8") %>%
        fromJSON(flatten = TRUE) %>%
        `[[`("results")
      
      #Update
      np <- nrow(pubs_exceed[[j]])
      if (is.null(np)) break
      
      os=os+500
      j=j+1
      
      
    }
    results[[i]] <- list(pubs, pubs_exceed)
  }
}

pubs_it2 <- results %>% 
  lapply(function(x) if (is.list(x)) bind_rows(x) else x) %>% 
  bind_rows()
saveRDS(pubs_it2, 'pubs_it2.rds')

not_fetched <- many_pubs %>% unlist()
saveRDS(not_fetched, 'not_fetched_pubs_it2.rds')

##### 3.1.3 Iteration 3 *************************************************** ####

pns <- readRDS('not_fetched_pubs_it2.rds')
results <- list()
many_pubs <- list()

for (i in 1:(length(pns))) {
  
  cat(i, format(Sys.time(), format = "%d-%b %H:%M"), '\n')
  
  pns_it <- pns[i:(i+1)]
  
  payload <- list(
    criteria = list(core_project_nums = pns_it),
    limit = 500
  ) %>% 
    toJSON(auto_unbox = T)
  
  response <- POST(url, headers, body = payload)
  if (response$status_code != 200) next
  Sys.sleep(1.01)
  
  pubs <- response %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    `[[`("results")
  
  if (length(pubs) == 0) next
  
  np <- nrow(pubs)
  
  if (np < 500) {
    
    results[[i]] <- pubs
    next
    
  } else if (np == 500) {
    
    os <- 499
    j <- 1
    pubs_exceed <- list()
    
    while (np == 500) {
      
      if (j==20) { 
        print('Many pubs')
        break
      }
      
      payload <- list(
        criteria = list(core_project_nums = pns_it),
        limit = 500,
        offset = os
      ) %>% 
        toJSON(auto_unbox = T)
      
      response <- POST(url, headers, body = payload)
      Sys.sleep(1.01)
      
      pubs_exceed[[j]] <- response %>%
        content(as = "text", encoding = "UTF-8") %>%
        fromJSON(flatten = TRUE) %>%
        `[[`("results")
      
      #Update
      np <- nrow(pubs_exceed[[j]])
      if (is.null(np)) break
      
      os=os+500
      j=j+1
      
      
    }
    results[[i]] <- list(pubs, pubs_exceed)
  }
}

pubs_it3 <- lapply(results, \(x) if (is.list(x)) bind_rows(x) else x) %>% 
  bind_rows() %>% distinct()
saveRDS(pubs_it3, 'pubs_it3.rds')

##### 3.1.4 Combine files ************************************************* ####

files <- paste0('pubs_it', 1:3, '.rds')

pubs <- bind_rows(lapply(files, readRDS)) %>% distinct()
saveRDS(pubs, 'pubs.rds')