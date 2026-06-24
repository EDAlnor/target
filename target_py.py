
# *************************************************************************** #
# *************************************************************************** #
# %% Prepare data
# *************************************************************************** #
# *************************************************************************** #

import pandas as pd
import numpy as np
import pyarrow.feather as feather
import os
import pyreadr as pr
import joblib
os.chdir(r'C:\Users\au544242\OneDrive - Aarhus universitet\Ph.d\Projekter\Target\Code\parts') # Change to your prefered working directory

%xmode Minimal

# ******************************************************* #
# %%% Initials
# ******************************************************* #

# Create projects id dataframes for 1) application IDs and 2) Core Project Numbers.
# First, remove duplicates on application id, which are there due to downloading process
apid_ids = pr.read_r('proj_ids.rds')[None]
apid_ids = apid_ids[~apid_ids.duplicated('appl_id')]
feather.write_feather(apid_ids, 'apid_ids.feather')

# Remove duplicates on core_project_num (there are several application pr. CPN)
m = apid_ids.duplicated('core_project_num')
cpn_ids = apid_ids[~m].drop(columns='appl_id')
feather.write_feather(cpn_ids, 'cpn_ids.feather')

# Number of core_project_numbers (projects) pr. opportunity number (call)
ncpn = (
    cpn_ids['opportunity_number']
    .value_counts()
    .reset_index().rename(columns={'count': 'ncpn'})
)
feather.write_feather(ncpn, 'ncpn.feather')

# Number of publications pr. core_project_number (project)
npubs = (
    pr.read_r('pubs.rds')[None]['coreproject']
    .value_counts()
    .reset_index().rename(columns={'count': 'npub'})
)
feather.write_feather(npubs, 'npubs.feather')

del ncpn, npubs, apid_ids, m, cpn_ids

# ******************************************************* #
# %%% Calls
# ******************************************************* #

# This section:
# 1) Adds number of sponsored projects and number of resulting publications
# to each call
# 2) Creates 'calls_clean' which only has RFA or PA calls that are R01.

calls = pr.read_r('calls.rds')[None].astype({
    'activity_code': 'category',
    'document_type': 'category'
})

# Add number of core_project_numbers (projects). Note that this also includes
# project with 0 publications.
calls = pd.merge(
    calls, pd.read_feather('ncpn.feather'),
    left_on='document_number', right_on='opportunity_number'
).drop(columns=['opportunity_number'])

# Add number of publications. 'npubs' only has 'core_project_number' so we need
# to merge it with 'cpn_ids' to get 'opportunity_number'. Note that this
# removes calls with 0 zero publications.
npubs_opnum = pd.merge(
    pd.read_feather('cpn_ids.feather'), pd.read_feather('npubs.feather'),
    left_on='core_project_num', right_on='coreproject'
).groupby('opportunity_number', as_index=False)['npub'].agg('sum')

calls = pd.merge(
    calls, npubs_opnum,
    left_on='document_number', right_on='opportunity_number'
).drop(columns=['opportunity_number'])

# Clean: Only activity codes R01, R03, R21, and U01, and only PA and RFA.
calls['r01-r03-r21-u01'] = (
    calls['activity_code']
    .isin(['R01', 'R21', 'U01', 'R03', 'R01,R21', 'R01,R03,R21', 'R01,R03'])
)

calls['pa-rfa'] = calls['document_type'].isin(['PA', 'RFA'])

# In the analysis we only use R01
calls['r01'] = calls['activity_code'] == 'R01'

feather.write_feather(calls, 'calls.feather')

# Calls used in the analysis
calls_clean = calls[calls['pa-rfa'] & calls['r01']][['title', 'release_date',                                                     'expired_date', 'document_number', 'document_type', 'url', 'ncpn', 'npub']]

open_calls = ['PA-10-067', 'PA-07-070', 'PA-19-091', 'PA-19-055', 'PA-19-056', 'PA-18-484', 'PA-18-345', 'PA-16-160', 'PA-13-302', 'PA-11-260', 'PA-20-185', 'PA-20-184', 'PA-20-183']
calls_clean['free'] = calls['document_number'].isin(open_calls)

feather.write_feather(calls_clean, 'calls_clean.feather')

del calls, calls_clean, npubs_opnum, open_calls

# ******************************************************* #
# %%% Grants
# ******************************************************* #

# ************************* #
# %%%% IDs
# ************************* #

# Create a dataframe with core-project-numbers of those projects which are sponsored by PA/RFA R01 calls.

cpn = pd.read_feather('cpn_ids.feather')
call_ids = pd.read_feather('calls_clean.feather')['document_number']

cpn_clean = (cpn
  .query('opportunity_number in @call_ids')
  .rename(columns={'core_project_num': 'cpn', 'opportunity_number': 'opnum'})
)

feather.write_feather(cpn_clean, 'cpn_clean1.feather')

del call_ids, cpn, cpn_clean

# ************************* #
# %%%% Full data
# ************************* #

# In R, columns containing lists / dataframes are removed from the raw
# downloaded project file. This is necessary to do in R because otherwise the
# file cannot be passed onto Python via .feather. We now continue working with
# that in Python.
# We name the object 'apid' because we work on this "level" of the projects. In # the end, we want a project-file at the 'core-project-number' (cpn) level.
apid = pd.read_feather('projects_applid_clean.feather')

# Remove projects which are not sponsored by RFA/PA+R01
m1 = apid['core_project_num'].isin(pd.read_feather('cpn_clean1.feather')['cpn'])

aplid_clean = (
  apid[m1] #Filter
  .rename(columns={'core_project_num': 'cpn', 'appl_id': 'apid', 'project_serial_num': 'psn', 'opportunity_number': 'opnum', 'project_detail_url': 'url', 'project_num': 'pnum'})
  .merge(pd.read_feather('npubs.feather'), left_on='cpn', right_on='coreproject') #Add publication count
  [['apid', 'pnum', 'cpn', 'psn', 'opnum', 'project_num_split.support_year', 'project_num_split.activity_code', 'url', 'n_pi', 'contact_pi_name', 'pi_lastName_firstNames', 'pi_id', 'npub', 'award_amount', 'project_start_year']] #Save clean set of columns
  .rename(columns={
      'project_num_split.support_year': 'sy',
      'project_num_split.activity_code': 'ac'
  })
  .astype({ #Downcast
      'award_amount': 'Int32',
      'sy': int,
      'project_start_year': 'Int16',
      'pi_id': int
  })
)

feather.write_feather(aplid_clean, 'aplid_clean.feather')

# CPN clean data
cpn = (
  aplid_clean
  .drop_duplicates(subset=['cpn', 'opnum'])
  .merge(pd.read_feather('calls_clean.feather')[['free', 'document_number']],
         left_on='opnum', right_on='document_number')
  .drop(columns=['apid', 'pnum', 'psn', 'sy', 'ac', 'award_amount', 'document_number'])
)
cpn.to_feather('cpn_clean.feather')

del apid, aplid_clean, cpn, m1

# ******************************************************* #
# %%% Publications
# ******************************************************* #

# ************************* #
# %%%% Prepare for CWTS matching
# ************************* #

# All
pubs = pr.read_r('pubs.rds')[None]
feather.write_feather(pubs, 'pubs.feather')

# R01 and PA/RFA
pubs = pd.read_feather('pubs.feather')
cpn_clean = pd.read_feather('cpn_clean.feather')

pubs_clean = pubs[pubs['coreproject'].isin(cpn_clean['cpn'])]
pubs_clean.to_feather('pubs_clean.feather')

# Get all IDs for publications
pubs_clean_ids = (
  pubs_clean.drop(columns='applid')
  .merge(cpn_clean, left_on='coreproject', right_on='cpn')
  [['pmid', 'cpn', 'opnum']]
)
pubs_clean_ids.to_feather('pubs_clean_ids.feather')

# Add PI for pubs
pubs_clean = pd.read_feather('pubs_clean.feather')
pi = pd.read_feather('aplid_clean.feather')

pi = pi.loc[pi.groupby('cpn')['sy'].idxmin()]

pmids_pi = (
  pd.merge(
    pubs_clean[['pmid', 'coreproject']],
    pi[['cpn', 'pi_lastName_firstNames', 'pi_id']],
    left_on='coreproject', right_on='cpn'
  )
  [['pmid', 'pi_lastName_firstNames', 'pi_id']]
  .rename(columns={'pi_lastName_firstNames': 'pi'})
)

# Lowercase and remove hyphens etc.
pmids_pi['pi'] = (
    pmids_pi['pi']
    .str.lower()
    .str.replace(r'\([^)]*\)|\'[^\']*\'|\"[^\"]*\"|“[^”]*”', '', regex=True)
    .str.replace(r'[.,]', '', regex=True)
    .str.replace(r'\s+', ' ', regex=True)
    .str.strip()
)

feather.write_feather(pmids_pi, 'pmids_pi.feather')

pmids_pi2 = pmids_pi['pmid'].drop_duplicates()
pmids_pi2.to_csv('pmids_pi2.txt', sep='\t', index=False)

pmids_pi.to_csv('pmids_pi.txt', sep='\t', index=False)

del cpn_clean, pi, pmids_pi, pmids_pi2, pubs, pubs_clean, pubs_clean_ids

# ************************* #
# %%%% Citation impact
# ************************* #

# Here we do some downcasting and cleanup, and create two dataframes:

# 1.: For determining MeSH of calls, keep all publications

pmids = pd.read_csv(
    'eda_pmid_pi_indicators.txt', sep='\t',
    dtype={
        'eda_pmid': 'int32',
        'pub_year': 'Int16',
        'doc_type_id': 'Int8',
        'meso_cluster_id': 'Int16',
        'micro_cluster_id': 'Int16',
        'cs_5':    'Int32',
        'cs_10':   'Int32',
        'ncs_5':   'object',
        'ncs_10':  'object',
        'top5_5':  'object',
        'top5_10': 'object',
        'top1_5':  'object',
        'top1_10': 'object'
    }).rename(columns={'eda_pmid': 'pmid'}).drop_duplicates(subset='pmid').drop(columns=['ut', 'cwts_pmid'])

ci_cols = ['ncs_5', 'ncs_10', 'top5_5', 'top5_10', 'top1_5', 'top1_10']

pmids[ci_cols] = pmids[ci_cols].replace(',', '.', regex=True).astype('float64')

feather.write_feather(pmids, 'pmids_full.feather')

# 2.: For citation impact analysis, remove 1) publications without ncs_5, 2) publication that were less than 5 years old at time of data retrieval, 3) 11 publications that somehow have ncs_5 (all 0) but not meso_cluster_id
pmids = (
  pd.read_feather('pmids_full.feather')
  .query('ncs_5.notna() and pub_year < 2020 and meso_cluster_id.notna()', engine='python')
)

# Convert continous topX% to binary
np.random.seed(123)
for col in ['top1_5', 'top1_10', 'top5_5', 'top5_10']:
    pmids[f'{col}_b'] = np.random.binomial(1, pmids[col]).astype(bool)

pmids = pmids.astype({
    'cs_5': 'int32',
    'cs_10': 'int32',
    'pub_year': 'int16',
    'doc_type_id': 'int8',
    'meso_cluster_id': 'int16',
    'micro_cluster_id': 'int32'
})

feather.write_feather(pmids, 'pmids_citationImpact.feather')

del pmids, col, ci_cols

# ************************* #
# %%%% MeSH
# ************************* #

mesh = pd.read_csv(
  'eda_pmid_pi_mesh.txt', sep='\t', na_values='NaN',
  usecols=['pmid', 'descriptor_ui', 'descriptor_major', 'qualifier_major'],
  dtype={
    'pmid': 'int32',
    'descriptor_ui': str,
    'descriptor_major': bool,
    'qualifier_major': str
  }
)

mesh_pmids = mesh['pmid'].drop_duplicates()
mesh_pmids.to_csv('mesh_pmids.csv', index=False)

mesh['qmr'] = mesh['qualifier_major'].eq('True')

mesh['mjr'] = mesh['descriptor_major'] | mesh['qmr']
mesh = (
  mesh[['pmid', 'descriptor_ui', 'mjr']]
  .groupby(['pmid', 'descriptor_ui'])
  .agg({'mjr': 'any'}).reset_index()
)

# Remove check tags 'Male' 'Female'
mesh = mesh[~mesh['descriptor_ui'].isin(['D005260', 'D008297'])]

mesh.to_feather('mesh_grants.feather')

del mesh_pmids, mesh

# ******************************************************* #
# %%% Authors (PIs)
# ******************************************************* #

# ************************* #
# %%%% Match to CWTS-WoS cluster-ids
# ************************* #

# %%%%%% Prepare name formats

aut = (
  pd.read_csv('eda_pmid_pi_author.txt', sep='\t')
  .rename(columns={
    'pi': 'pi_nih', 
    'full_name': 'full_name_wos', 
    'first_name': 
      'first_names_wos'
  })
)

# 1) WoS names

# First names: Remove dots and hyphens . - and replace accent letters
aut['first_names_wos'] = (
  aut['first_names_wos']
  .str.replace('.', '', regex=False).str.strip()
  .str.replace('-', ' ')
  .str.replace('á|à|ä|â|ã|å|ā', 'a', regex=True)
  .str.replace('é|è|ë|ê|ē', 'e', regex=True)
  .str.replace('í|ì|ï|î|ī', 'i', regex=True)
  .str.replace('ó|ò|ö|ô|õ|ō', 'o', regex=True)
  .str.replace('ú|ù|ü|û|ū', 'u', regex=True)
  .str.replace('ç', 'c')
  .str.replace('ñ', 'n')
  .str.replace('ß', 'ss')
)

# LastName, initials: Replace accent letters
aut['full_name_wos'] = (
  aut['full_name_wos']
    .str.replace('á|à|ä|â|ã|å|ā', 'a', regex=True)
    .str.replace('é|è|ë|ê|ē', 'e', regex=True)
    .str.replace('í|ì|ï|î|ī', 'i', regex=True)
    .str.replace('ó|ò|ö|ô|õ|ō', 'o', regex=True)
    .str.replace('ú|ù|ü|û|ū', 'u', regex=True)
    .str.replace('ç', 'c')
    .str.replace('ñ', 'n')
    .str.replace('ß', 'ss')
)

# Extract last name
aut['last_name_wos'] = (
  aut['full_name_wos']
  .str.extract(r'(\S+)(?=,)', expand=False) # Extract from before comma
  .str.replace('-', ' ')  # replace hyphen with blankspace
  .str.split().str[-1] # In case there was a hyphen, this takes the last part 
)

# Extract first name
aut['first_nam_wos'] = aut['first_names_wos'].str.extract(r'^(\S+)')

# Extract middle names
aut['middle_names_wos'] = aut['first_names_wos'].str.extract(
    r'^\S+\s+(.*)').fillna('')

# If first name abbreviated in NIH, abbreviate, else keep as is.
aut['first_nam_wos2'] = np.where(
    aut['pi_nih'].str.split().str[0].str.match(r'^[a-z]$'),
    aut['first_nam_wos'].str[:1],
    aut['first_nam_wos']
)

# Now create WoS name with similar structure as NIH
aut['pi_wos'] = (
    aut['first_nam_wos2'] + ' ' +
    np.where(  # If there is a middle name, add it + a blankspace, else add nothing
        aut['middle_names_wos'].str.len() > 0,
        aut['middle_names_wos'] + ' ',
        '') +
    aut['last_name_wos']
)

# First name abbreviated + last name
aut['fnab_wos'] = aut['full_name_wos'].str.extract(r',\s*(.)')
aut['fnab_ln_wos'] = aut['fnab_wos'] + ' ' + aut['last_name_wos']

# 2) NIH-PI name

aut['pi_nih'] = aut['pi_nih'].str.replace('-', ' ')

# Extract last name
aut['last_name_nih'] = aut['pi_nih'].str.split().str[-1]

# Extract first name and abbreviate
aut['fnab_nih'] = aut['pi_nih'].str[0]

# First name abbreviated + last name
aut['fnab_ln_nih'] = aut['fnab_nih'] + ' ' + aut['last_name_nih']

feather.write_feather(aut, 'aut.feather')

del aut

# %%%%%% Levenshtein distance and match

import Levenshtein

aut = pd.read_feather('aut.feather')

aut['ld_fn'] = aut.apply(lambda x: Levenshtein.distance(
    str(x['pi_nih']), str(x['pi_wos'])), axis=1)
aut['ld_fnabln'] = aut.apply(lambda x: Levenshtein.distance(
    str(x['fnab_ln_nih']), str(x['fnab_ln_wos'])), axis=1)

aut['fnab_match'] = aut['fnab_nih'] == aut['fnab_wos']

aut = aut[['pi_nih', 'pi_wos', 'ld_fn', 'fnab_ln_nih', 'fnab_ln_wos', 'ld_fnabln','fnab_match', 'cluster_id', 'eda_pmid']].sort_values(by=['pi_nih', 'ld_fnabln', 'ld_fn'])

# Most likely match based on...

# Full name

# First name abbreviated + last name
m1 = aut['ld_fnabln'] == 0
m2 = aut['ld_fnabln'] == 1 & aut['fnab_match']

mat_ab = aut[m1 | m2].drop_duplicates(subset=['pi_nih', 'cluster_id'])

# Combine
mat = mat_ab
# mat = pd.concat([mat_fn, mat_ab], ignore_index=True).drop_duplicates(subset=['pi_nih', 'cluster_id'])

# 173 Cluster IDs have more than 1 PI NIH
cids2_t1 = mat.groupby('cluster_id')['pi_nih'].nunique(
).reset_index().query('pi_nih > 1')['cluster_id']
cids2 = mat.query('cluster_id in @cids2_t1').sort_values(by='cluster_id')

# From these keep PI NIH with minum Levenshtein and where PI WOS is not NA
good_t1 = cids2[cids2["ld_fn"] == cids2.groupby(
    "cluster_id")["ld_fn"].transform("min")]
good_t2 = good_t1[good_t1['pi_wos'].notna()]
good = good_t2.drop_duplicates(subset='cluster_id', keep=False)

# Remove the cluster-ids with +1 PI NIH
cleaned_t1 = pd.merge(mat, cids2, on=list(
    mat.columns), how='outer', indicator=True)
cleaned = cleaned_t1.query('_merge == "left_only"')

# And add the good ones
mat2 = pd.concat([cleaned, good])

# Check stats and save
npi = pd.read_feather('pmids_pi.feather')['pi'].nunique()

npi
mat2['pi_nih'].nunique()
round(mat2['pi_nih'].nunique() / npi, 3)

feather.write_feather(mat2, 'authors_matched.feather')

cids = mat2['cluster_id']
cids.to_csv('pi_cluster_id.txt', index=False, header=False)

del aut, cids, cids2, cids2_t1, cleaned, cleaned_t1, good, good_t1, good_t2, m1, m2, mat, mat2, mat_ab, npi

# ************************* #
# %%%% Prepare data
# ************************* #

# %%%%%% Publications

# Citation impact of publications by NIH PIs, prepare data on. 

apubs = pd.read_csv(
  'eda_PI_indicators.txt',
  sep='\t',
  dtype={
    'pmid':        'int32',
    'pub_year':    'int16',
    'doc_type_id': 'Int8',
    'ut':           object,
    'meso_cluster_id': 'Int16',
    'micro_cluster_id': 'Int16',
    'cs_5':   'Int16',
    'cs_10':  'Int16',
    'ncs_5':   object,
    'ncs_10':  object,
    'top5_5':  object,
    'top5_10': object,
    'top1_5':  object,
    'top1_10': object
    }
).rename(columns={'pub_year': 'year'})

ci_cols = ['ncs_5', 'ncs_10', 'top5_5', 'top5_10', 'top1_5', 'top1_10']
apubs[ci_cols] = apubs[ci_cols].replace(',', '.', regex=True).astype(float)

feather.write_feather(apubs, 'apubs_full.feather')

# Resolve duplicates and clean columns
apubs = pd.read_feather('apubs_full.feather')

apubs['na'] = apubs.isna().sum(axis=1)
apubs_clean = apubs.sort_values(by='na').drop_duplicates(subset='pmid', keep='first').drop(columns=['ut', 'na', 'pool'])

feather.write_feather(apubs_clean, 'apubs_clean.feather')

del apubs, ci_cols, apubs_clean

# %%%%%% MeSH

# MeSH terms of publications by NIH PIs, prepare data on. 

amesh = pd.read_csv(
  'eda_pi_mesh.txt', sep='\t',
  usecols=['pmid', 'descriptor_ui', 'descriptor_major', 'qualifier_major'],
  dtype={
    'pmid': 'int32',
    'descriptor_ui': str,
    'descriptor_major': str,
    'qualifier_major': str
  }
)
amesh['qmjr'] = np.where(amesh['qualifier_major'] == '1', True, False)
amesh['dmjr'] = np.where(amesh['descriptor_major'] == '1', True, False)

amesh['mjr'] = amesh['qmjr'] | amesh['dmjr']
amesh = (
  amesh[['pmid', 'descriptor_ui', 'mjr']]
  .groupby(['pmid', 'descriptor_ui'])
  .agg({'mjr': 'any'}).reset_index()
)

# Remove check tags 'Male' 'Female'
amesh = amesh[~amesh['descriptor_ui'].isin(['D005260', 'D008297'])]

amesh.to_feather('amesh.feather')

del amesh

# %%%%%% Key from WoS-id to NIH-id

# Authors were matched based on strings, but we want the integer id
# Here we create a key between the integer id in NIH and WoS
authors_matched = (
  pd.read_feather('authors_matched.feather')[['pi_nih', 'cluster_id']]
  .rename(columns={'pi_nih': 'pi_name_nih', 'cluster_id': 'pi_id_wos'})
)

send2jpa = (
  pd.read_feather('pmids_pi.feather')[['pi', 'pi_id']]
  .rename(columns={'pi': 'pi_name_nih', 'pi_id': 'pi_id_nih'})
  .drop_duplicates('pi_id_nih')
)
send2jpa['pi_name_nih'] = send2jpa['pi_name_nih'].str.replace('-', ' ') #In authors_matched hyphens are replaced with blankspace so we need to do that in the original file also
send2jpa.drop_duplicates(subset='pi_name_nih', keep=False, inplace=True) #~70 authors have more than 1 pi_id. In some cases it is clearly a mistake by NIH (same reasearch area, same institution, same email) in other cases its more ambigious. To be objective, we just remove them all.

key = pd.merge(authors_matched, send2jpa, on='pi_name_nih')[['pi_id_wos', 'pi_id_nih']]

# Match to projects
pubaut = (
  pd.read_csv('eda_pi_pubauthor.txt', sep = '\t')
  .rename(columns={'pi_id': 'pi_id_wos'})
  .drop_duplicates()
)

pub_pi_key = pd.merge(pubaut, key, on='pi_id_wos')
pub_pi_key.to_feather('pub_pi_key.feather')

del authors_matched, send2jpa, key, pubaut, pub_pi_key

# %%%%%% List of names and projects

nih_pis = (
  pd.read_feather('aplid_clean.feather')
  .drop_duplicates(subset=['cpn', 'pi_id'])
  [['pi_lastName_firstNames', 'pi_id', 'cpn', 'url']]
  .rename(columns={'pi_lastName_firstNames': 'pi_name_nih'})
)
nih_pis['pi_name_nih'] = (
  nih_pis['pi_name_nih']
  .str.strip()
  .str.replace(r'\s+', ' ', regex=True)
  .str.replace('.', '')
  .str.title()
)
nih_pis.sort_values(by='pi_name_nih', inplace=True)

feather.write_feather(nih_pis, 'nih_pis.feather')

del nih_pis

# %%%%%% Positions

key = (
  pd.read_csv('eda_pi_pubauthor.txt', sep = '\t')
  .rename(columns={'pi_id': 'cluster_id'})
  .drop_duplicates()
)

ut = pd.read_csv(
  'eda_PI_indicators.txt',
  sep='\t',
  usecols=['pmid', 'ut'],
  dtype={'pmid': int, 'ut': str}
  ).drop_duplicates()

pi_ut = key.merge(ut, on='pmid')
pi_ut.to_csv('pi_ut.txt', sep='\t', index=False)

del key, ut, pi_ut

# ******************************************************* #
# %%% Grants 2
# ******************************************************* #

# With the previous publications of the PI in hand (made in 'Authors'), we can now feature engineer following columns 1: degree of topic switching, 2: citation impact of project, 3: citation impact of PI before the project, 4: Seniority of PI

# ************************* #
# %%%% Clean up
# ************************* #

cpn = pd.read_feather('cpn_clean.feather')

# We can only use projects fulfulling the following criteria: 

# 1) has only 1 PI at any time
m1 = cpn['n_pi'] == 1

# 2) PI is the same across project duration 
pi_same_cpns = (
  cpn.groupby('cpn', as_index=False)
  ['pi_id'].nunique()
  .query('pi_id == 1')
  ['cpn']
)
m2 = cpn['cpn'].isin(pi_same_cpns)

# 3) is sponsored by 1 type of call
cpn_1type_call = (
  cpn.groupby('cpn', as_index=False)['free']
  .nunique()
  .query('free == 1')
  ['cpn']
)
m3 = cpn['cpn'].isin(cpn_1type_call)

# Apply criteria and clean columns
cpn_ts1 = (
  cpn[m1 & m2 & m3]
  .drop(columns=['n_pi', 'contact_pi_name', 'pi_lastName_firstNames', 'project_start_year'])
  .drop_duplicates(subset='cpn')
)
cpn_ts1.to_feather('cpn_ts1.feather')

del cpn, cpn_1type_call, cpn_ts1, m1, m2, m3, pi_same_cpns

# ************************* #
# %%%% Grant publications' year and citation impact
# ************************* #

cpn = pd.read_feather('cpn_ts1.feather')

pubs = (
  pd.read_feather('pubs_clean.feather')
  .rename(columns={'coreproject': 'cpn'})
  [['pmid', 'cpn']]
)

# Add year of publication
years = (
  pd.read_feather('pmids_full.feather')
  .query('pub_year.notna()', engine='python')
  .merge(pubs, on='pmid')
  [['cpn', 'pmid', 'pub_year']]
)

# Add year of first publication
year_first_pub = (
  years
  .groupby('cpn', as_index=False)['pub_year'].min()
  .rename(columns={'pub_year': 'year_first_pub'})
)

# Number of publications outside citation data years
npubs_os_ci_years = (
  years
  .groupby('cpn', as_index=False)
  .apply(lambda g: ((g['pub_year'] < 2000) | (g['pub_year'] > 2019)).sum())
  .rename(columns={None: 'n_outside_range'})
)

# Add citation impact...
ci = (
  pubs
  .merge(
    pd.read_feather('pmids_citationImpact.feather')[['pmid', 'ncs_5', 'top5_5']],
    on='pmid'
   )
)

# 1) for all publications
ci_all = (
  ci.groupby('cpn', as_index=False)
  .agg(
    mean_ncs5=  ('ncs_5', 'mean'),
    npub_w_ci=  ('cpn', 'size')
  )
)

# 2) for publications with PI as co-author.

# First add PI-id to each publication
ci2 = ci.merge(cpn[['cpn', 'pi_id']], on='cpn')

# Get the list of publications for each PI
key = ( 
  pd.read_feather('pub_pi_key.feather')  [['pmid',  'pi_id_nih']]
  .rename(columns={'pi_id_nih': 'pi_id'})
)

pubs_pi = pd.merge(ci2, key, on=['pmid', 'pi_id'])

# Then group by CPN to get citation impact
ci_pi = (
  pubs_pi.groupby('cpn', as_index=False)
  .agg(
    pi_mean_ncs5=  ('ncs_5', 'mean'),
    pi_npub_w_ci=  ('cpn', 'size')
  )
)

# Now add the information
cpn_ts2 = (
  cpn
  .merge(year_first_pub, on='cpn')
  .merge(npubs_os_ci_years, on='cpn')
  .merge(ci_all, on='cpn')
  .merge(ci_pi, on='cpn', how='left') 
)

cpn_ts2.to_feather('cpn_ts2.feather')
del cpn, ci, ci2, ci_all, ci_pi, key, pubs_pi, years, pubs, cpn_ts2, npubs_os_ci_years, year_first_pub

# ************************* #
# %%%% PIs previous publications' citation impact
# ************************* #

cpn = pd.read_feather('cpn_ts2.feather')

pubs = pd.merge(
  pd.read_feather('apubs_clean.feather')[['pmid', 'year', 'ncs_5']],
  pd.read_feather('pub_pi_key.feather')[['pmid', 'pi_id_nih']],
  on='pmid'
)

# Only take citation impact of publications in the period up to 10 years before the 1st publication of the project. Only take projects where the PI has at least 3 previous publications.
ci = (
  pd.merge(
    cpn[['cpn', 'pi_id', 'year_first_pub']], pubs, 
    left_on='pi_id', right_on='pi_id_nih'
  )
  .query('year < year_first_pub & ncs_5.notna()', engine='python')
  .query('year >= (year_first_pub - 10)', engine = 'python')
  .groupby('cpn', as_index=False).agg(
    prev_mean_ncs5= ('ncs_5', 'mean'),
    pi_id=          ('pi_id', 'first'),
    prev_npub=      ('cpn', 'size')
  )
  .query('prev_npub >= 3')
  .drop(columns=['pi_id'])
)

cpn_ts3 = pd.merge(cpn, ci, on='cpn')
cpn_ts3['dif_mean_ncs5'] = cpn_ts3['mean_ncs5'] - cpn_ts3['prev_mean_ncs5']

cpn_ts3.to_feather('cpn_ts3.feather')

del cpn, ci, cpn_ts3, pubs

# ************************* #
# %%%% Compute topic switching
# ************************* #

# %%%%% PIs previous publications' MeSH data 

cpn = pd.read_feather('cpn_ts3.feather')

pis = cpn['pi_id'].drop_duplicates()
apubs = pd.read_feather('apubs_clean.feather')[['pmid', 'year']]
pub_pi_key = (
  pd.read_feather('pub_pi_key.feather')[['pmid', 'pi_id_nih']]
  .rename(columns={'pi_id_nih': 'pi_id'})
  .query('pi_id in @pis')
)
pmids_w_pi = pub_pi_key['pmid'].drop_duplicates()

# Prepare MeSH ----------------------------------------------------------------

mesh = (
  pd.read_feather('amesh.feather')
  .query('pmid in @pmids_w_pi')
  .rename(columns={'descriptor_ui': 'muid'})
  .merge(pd.read_feather('mesh_ic.feather')[['muid', 'ic']], on='muid')
)

# Add weights
mesh['p'] = np.where(mesh['mjr'], 3, 1) 
mesh['sum_p'] = mesh.groupby('pmid')['p'].transform('sum')
mesh['w'] = mesh['p'] / mesh['sum_p']
mesh['w_ic'] = mesh['w'] * mesh['ic']
mesh.drop(columns=['p', 'sum_p', 'mjr', 'w', 'ic'], inplace=True)

# Add ...
mesh = (
  mesh
  .merge(apubs, on='pmid') # ... year
  .merge(pub_pi_key, on='pmid') # ... PI-id
)

# Count number of publications with MeSH for each PI for each year. We use this to speed up the loop by removing PIs that don't meet the treshold criteria of minimum 3 MeSH publications. The '_cum'-column was computed after this code had run, and in hinsight should have been used to filter.
npubs_w_mesh = (
  mesh[['pmid', 'year', 'pi_id']]
  .drop_duplicates()
  .groupby(['pi_id', 'year'], as_index=False).size()
  .rename(columns={'size': 'npub_w_mesh_year'})
)

all_years = np.arange(
  npubs_w_mesh['year'].min(),
  npubs_w_mesh['year'].max() + 1
)

pi_year_grid = (
  npubs_w_mesh[['pi_id']].drop_duplicates()
  .assign(key=1)
  .merge(pd.DataFrame({'year': all_years, 'key': 1}), on='key')
  .drop(columns='key')
)

npubs_w_mesh = (
  pi_year_grid
  .merge(npubs_w_mesh, on=['pi_id', 'year'], how='left')
  .fillna({'npub_w_mesh_year': 0})
  .sort_values(['pi_id', 'year'])
)

npubs_w_mesh['npub_w_mesh_cum'] = (
  npubs_w_mesh
  .groupby('pi_id')['npub_w_mesh_year']
  .cumsum()
)

npubs_w_mesh = npubs_w_mesh.query('npub_w_mesh_cum > 0')
npubs_w_mesh.to_feather('npubs_w_mesh.feather')

# Create the knowledge vector of PI -------------------------------------------

# Compute sum of knowledge points recieved in each MeSH for each PI for each year
mesh_pi_year = (
  mesh
  .groupby(['pi_id', 'year', 'muid'], as_index=False)
  ['w_ic'].sum()
)

# Determine what years should we create knowledge vectors for. These are 1 year before the year of the first publication of each grant.
years = cpn[['pi_id', 'year_first_pub', 'cpn']].drop_duplicates()
years['year'] = years['year_first_pub'] - 1
years.drop(columns='year_first_pub', inplace=True)

# Now we do 2 things for each year, for each PI: 1) compute cumulatative
# knowledge points for each MeSH. 2) compute number of publications with MeSH
# in the years before and use this to remove PIs with less than 3 such
# publications.

years_pi = mesh_pi_year['year'].unique()
years_iterate = np.sort(years_pi[np.isin(years_pi, years['year'])])

del apubs, mesh, pis, pmids_w_pi, pub_pi_key, years_pi # Free RAM

for y in years_iterate:
  print(y, pd.Timestamp.now().strftime('%H:%M:%S'))
  
  # Compute cumulative knowledge points for each MeSH for each PI 
  mesh_pi_cum_year = (
    mesh_pi_year               
    .query('year <= @y & year >= (@y - 10)')
    .groupby(['pi_id', 'muid'], as_index=False)
    ['w_ic'].sum()
  )
  
  # Remove uneccesary PI-years
  mesh_pi_cum_year['year'] = y
  mesh_pi_cum_year = mesh_pi_cum_year.merge(years, on=['pi_id', 'year'])
  
  # Only keep PIs that had minimum 3 publications with MeSH in the years before
  # the year of te first publication of their grant. 
  all_pis = mesh_pi_cum_year['pi_id'].drop_duplicates()
  
  valid_pis = (
    npubs_w_mesh.query('year <= @y & year >= (@y - 10) & pi_id in @all_pis')
    .groupby('pi_id', as_index=False)['npub_w_mesh_year'].sum()
    .query('npub_w_mesh_year >= 3')
    ['pi_id']
  )
  
  final = mesh_pi_cum_year.query('pi_id in @valid_pis')
  
  # Save
  final.to_feather(f'mesh_pi_cumkp_{y}.feather')

# Combine to single dataframe
files = [f'mesh_pi_cumkp_{y}.feather' for y in years_iterate]
df_list = [pd.read_feather(file) for file in files]
pym = pd.concat(df_list, ignore_index=True).rename(columns={'w_ic': 'w_pi'})

pym.to_feather('pi_year_mesh.feather')

# Remove CPNs where PI did not fulfill 3-MeSH criteria
valid_cpns = pym['cpn'].unique() 
cpn4 = cpn.query('cpn in @valid_cpns')

cpn4.to_feather('cpn_ts4.feather')

del files, df_list, pym, y, years_iterate, mesh_pi_cum_year, mesh_pi_year, years, all_pis, final, npubs_w_mesh, valid_pis, cpn, cpn4, valid_cpns, all_years, apid, pi_year_grid

# %%%%% Grant publications' MeSH data 

cpn = pd.read_feather('cpn_ts4.feather')

# Prepare MesH and their weights
mesh = (
  pd.read_feather('mesh_grants.feather')
  .rename(columns={'descriptor_ui': 'muid'})
  .merge(pd.read_feather('mesh_ic.feather')[['muid', 'ic']], on='muid')
)
mesh['p'] = np.where(mesh['mjr'], 3, 1) 
mesh['sum_p'] = mesh.groupby('pmid')['p'].transform('sum')
mesh['w'] = mesh['p'] / mesh['sum_p']
mesh['w_ic'] = mesh['w'] * mesh['ic']
mesh.drop(columns=['p', 'sum_p', 'mjr', 'w', 'ic'], inplace=True)

# Add MeSH to publications
pubs = (
  pd.read_feather('pubs_clean.feather')
  .rename(columns={'coreproject': 'cpn'})
  [['cpn', 'pmid']]
  .merge(cpn[['cpn', 'pi_id', 'year_first_pub']], on='cpn') # Remove irrelevant publications for more speed
  .merge(mesh, on='pmid') # Add MeSH terms
)

# Add MeSH weights for projects
cpn_mesh_weights = (
  pubs.groupby(['cpn', 'muid'], as_index=False).agg(
    w_cpn = ('w_ic', 'sum'),
    pi_id = ('pi_id', 'first')
  )
)
cpn_mesh_weights.to_feather('cpn_mesh_weights.feather')

# Compute number of publications with MeSH terms for projects
npubs_w_mesh = (
  pubs.drop_duplicates(subset=['cpn', 'pmid'])
  .groupby('cpn', as_index=False).size()
)

cpn5 = pd.merge(cpn, npubs_w_mesh, on='cpn').rename(columns={'size': 'npub_w_mesh'})
cpn5.to_feather('cpn_ts5.feather')

del cpn, cpn5, cpn_mesh_weights, mesh, npubs_w_mesh, pubs

# %%%%% Prepare vector dataframes

cpn_mesh = pd.read_feather('cpn_mesh_weights.feather')

# Remove PI Mesh weights were CPN had no MeSH pubs
cpns = cpn_mesh['cpn'].drop_duplicates()
pi_mesh = (
  pd.read_feather('pi_year_mesh.feather')
  .drop(columns='year')
  .query('cpn in @cpns')
)

cpn_pi_mesh_vectors = pd.merge(cpn_mesh, pi_mesh, on = ['pi_id', 'cpn', 'muid'], how='outer').fillna(0)
cpn_pi_mesh_vectors.to_feather('cpn_pi_mesh_vectors.feather')

# Create a dict for faster subsetting
vecs = cpn_pi_mesh_vectors.drop(columns='pi_id')
vecs_grouped = {}
for c, g in vecs.groupby('cpn'):
    vecs_grouped[c] = g.drop(columns='cpn')

# Control that all PIs and CPNs actually have MeSH weights
for c in vecs_grouped.keys():
  if (vecs_grouped[c][['w_pi', 'w_cpn']].sum(axis=0) == 0).any(): print('zero', c)

# Save
joblib.dump(vecs_grouped, 'vecs_grouped.joblib')

del cpn_mesh, cpns, pi_mesh, cpn_pi_mesh_vectors, vecs, vecs_grouped, c, g

# %%%%% Loop

vecs = joblib.load('vecs_grouped.joblib')

sm = pd.read_feather('sm.feather')
sm.index = sm.columns

pi_cpn_sim = []

for i, c in enumerate(vecs.keys()):
  
  # Get MeSH vectors of project 
  cpn = vecs[c]
  
  # Subset S
  S = sm.loc[cpn['muid'], cpn['muid']].values

  # Calculate soft cosine
  a = cpn['w_cpn'].values
  b = cpn['w_pi'].values

  sc = (
    (a @ S @ b) /
    (np.sqrt(a @ S @ a) * np.sqrt(b @ S @ b))
  )
  
  pi_cpn_sim.append([c, sc])
  
  #Monitor progress of loop and save results
  if (i % 250 == 0): print(c, i, pd.Timestamp.now().strftime('%H:%M:%S'))

# Save Soft Cosine in a dataframe
pi_cpn_sim_df = pd.DataFrame(pi_cpn_sim, columns=['cpn', 'soft_cosine'])
pi_cpn_sim_df['topic_switch'] = 1 - pi_cpn_sim_df['soft_cosine']
pi_cpn_sim_df.to_feather('pi_cpn_sim_df.feather')

# Add topic_switch to CPN dataframe
cpn6 = (
  pd.read_feather('cpn_ts5.feather')
  .merge(pi_cpn_sim_df[['cpn', 'topic_switch']], on='cpn')
  .query('npub_w_mesh > 2 and npub_w_ci > 2') # Remove project with less than 3 MeSH publications or less than 3 publications with citation impact
)

cpn6.to_feather('cpn_ts6.feather')

del vecs, sm, pi_cpn_sim, c, i, pi_cpn_sim_df, cpn6, a, b, cpn, S, sc

# ************************* #
# %%%% Final preperation
# ************************* #

cpn = pd.read_feather('cpn_ts6.feather')

# Add log of PI pre-MNCS
cpn['log_prev_mean_ncs5'] = np.log(cpn['prev_mean_ncs5'])

# Add seniority (year of first publication of CPN - year of first pub of PI)
pis = cpn['pi_id'].unique()

pub_pi_key = (
  pd.read_feather('pub_pi_key.feather')[['pmid', 'pi_id_nih']]
  .rename(columns={'pi_id_nih': 'pi_id'})
  .query('pi_id in @pis')
)

pmids_year = pd.read_feather('apubs_clean.feather')[['pmid', 'year']]

year_first_pub_pi = (
  pd.merge(pub_pi_key, pmids_year, on='pmid')
  .groupby('pi_id', as_index=False)
  ['year'].min()
  .rename(columns={'year': 'year_first_pub_pi'})
)

cpn = cpn.merge(year_first_pub_pi, on='pi_id')
cpn['seniority'] = cpn['year_first_pub'] - cpn['year_first_pub_pi']
cpn['log_seniority'] = np.log(cpn['seniority'])
cpn.drop(columns=['year_first_pub_pi'], inplace=True)

# Add PI number of previous publications with MeSH.
cpn['year'] = cpn['year_first_pub'] - 1
cpn = (
  cpn
  .merge(pd.read_feather('npubs_w_mesh.feather'), on = ['year', 'pi_id'])
  .rename(columns={'npub_w_mesh_cum': 'pi_prev_npub_w_mesh'})
  .drop(columns=['year', 'npub_w_mesh_year'])
)

# Recode grant type
coded = (
  pd.read_excel('../inspect/calls_coded.xlsx')
  [['note', 'document_number', 'ncpn', 'title', 'url']]
  .rename(columns={'document_number': 'opnum'})
)
coded["target"] = ~coded["note"].isin([
    "discipline",
    "diversity of PI",
    "open",
    "Only collaboration",
    "Only multidisciplinary",
    "risky"
])
call_type = coded[['target', 'opnum']]

cpn = cpn.drop(columns='free').merge(call_type, on='opnum')

# Finalize
cpn.to_feather('cpn_ts7.feather')

del cpn, pis, pmids_year, pub_pi_key, year_first_pub_pi, coded

# *************************************************************************** #
# *************************************************************************** #
# %% Analyze
# *************************************************************************** #
# *************************************************************************** #

import pandas as pd
import numpy as np
import pyarrow.feather as feather
import os
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import seaborn as sns

%xmode Minimal

os.chdir(r'C:\Users\au544242\OneDrive - Aarhus universitet\Ph.d\Projekter\Target\Code\parts') # Change to your prefered working directory

cpn = pd.read_feather('cpn_ts7.feather')

percs = [0.10, 0.5, 0.9]

# ******************************************************* #
# %%% Descriptive stats 
# ******************************************************* #

# Stats -----------------------------------------------------------------------

# Calls

cpn['opnum'].nunique()
cpn[cpn['target']]['opnum'].nunique()
round(cpn[cpn['target']]['opnum'].nunique() / cpn['opnum'].nunique(), 3) #how many % of calls are targeted

# Grants

len(cpn) - cpn['target'].sum()
1 - (cpn['target'].sum() / len(cpn)).round(3)
sum(cpn['mean_ncs5'] == 0) #grant MNCS is strictly positive

cpn['pi_id'].nunique()

desc = (
  cpn[['mean_ncs5','topic_switch','log_prev_mean_ncs5','log_seniority']]
  .describe(percentiles=percs)
  .round(3)
  .T
  .drop(columns='count')
  .assign(Variable=['Grant MNCS', 'Topic switch', 'ln(PI pre-MNCS)', 'ln(Seniority)'])
)
desc.insert(0, 'Variable', desc.pop('Variable'))

desc
desc.to_excel('../results/desc.xlsx', index=False)

cpn['prev_mean_ncs5'].describe(percentiles=percs)

# ******************************************************* #
# %%% Histograms
# ******************************************************* #

# Truncate data and compute percentiles/mean for top plot
p99 = np.percentile(cpn['mean_ncs5'], 99)
truncated = cpn.query('mean_ncs5 < @p99')['mean_ncs5']
percentiles = [10, 50, 90]
perc_values = np.percentile(cpn['mean_ncs5'], percentiles)
mean_value = cpn['mean_ncs5'].mean()

# Colormap and markers
colors = cm.viridis(np.linspace(0, 0.8, len(percentiles) + 1))
markers = ['v', 's', '^']
grey = '0.95'

# Create mosaic layout
fig, ax = plt.subplot_mosaic(
  [['top', 'top', 'top'],
   ['left', 'middle', 'right']],
  constrained_layout=True,
  figsize=(10, 6)
)

# Top: Grant MNCS 
sns.histplot(truncated, ax=ax['top'], color=grey)
tick_height = 0.1 * ax['top'].get_ylim()[1]
for i, val in enumerate(perc_values):
    ax['top'].plot(
        [val]*2, [0, tick_height],
        color=colors[i],
        linewidth=5,
        marker=markers[i],
        markevery=[1],
        markersize=12,
        label=f"{percentiles[i]}th percentile"
    )
ax['top'].plot(
  [mean_value]*2, [0, tick_height],
  color=colors[-1],
  linewidth=5,
  marker='o',
  markevery=[1],
  markersize=12,
  label="Mean"
)
ax['top'].set_xlabel("Grant MNCS (truncated at 99th percentile)")
ax['top'].set_ylabel('')
ax['top'].legend()

# Bottom left: topic_switch 
sns.histplot(cpn['topic_switch'], ax=ax['left'], color=grey)
ax['left'].set_xlabel("Topic Switch")
ax['left'].set_ylabel("")

# Bottom middle: prev_mean_ncs5 
sns.histplot(cpn['log_prev_mean_ncs5'], ax=ax['middle'], color=grey)
ax['middle'].set_xlabel("ln(PI pre-MNCS)")
ax['middle'].set_ylabel("")

# Bottom right: ln(Seniority)
sns.histplot(cpn['log_seniority'], ax=ax['right'], color=grey)
ax['right'].set_xlabel("ln(Seniority)")
ax['right'].set_ylabel("")

plt.show()

del ax, colors, fig, grey, i, markers, mean_value, p99, perc_values, percentiles, tick_height, truncated, val

# ******************************************************* #
# %%% Bivariate: Descriptive stats by grant type 
# ******************************************************* #

# Table -----------------------------------------------------------------------
pd.set_option('display.max_columns', 10)

pt = (
  cpn.groupby('target')[['mean_ncs5', 'topic_switch', 'log_prev_mean_ncs5', 'log_seniority']]
  .describe(percentiles=percs)
  .stack(level=0)          
  .reset_index()           
  .rename(columns={'level_1':'Variable'})
  .round(3)
  .sort_values('Variable')
)
pt['Grant type'] = np.where(pt['target'], 'Targeted', 'Non-targeted')
pt = pt[['Variable', 'Grant type', 'mean', '10%', '50%', '90%']]
pt['Variable'] = pt['Variable'].replace({
    'mean_ncs5': 'Grant MNCS',
    'topic_switch': 'Topic switch',
    'log_prev_mean_ncs5': 'ln(PI pre-MNCS)',
    'log_seniority': 'ln(Seniority)'
})

pt['Variable'] = pd.Categorical(
  pt['Variable'],
  categories=['Grant MNCS', 'Topic switch', 'ln(PI pre-MNCS)', 'ln(Seniority)'],
  ordered=True
)
pt = pt.sort_values(['Variable', 'Grant type'])

pt.to_excel('../results/grant_type.xlsx', index=False)

sum(cpn['target'])
sum(~cpn['target'])

del pt

# ******************************************************* #
# %%% Fig: Topic switch by grant type
# ******************************************************* #

cpn['target_label'] = np.where(cpn['target'], 'Targeted', 'Non-targeted')
fig, ax = plt.subplots(figsize=(6, 4))

sns.kdeplot(
  data=cpn,
  x='topic_switch',
  hue='target_label',
  common_norm=False,
  ax=ax
)

ax.set_xlabel('Topic Switch')
ax.set_ylabel('Density')
ax.get_legend().set_title('')

plt.show()

# ******************************************************* #
# %%% Various information written in main text of paper
# ******************************************************* #

# Number of MeSH terms for publications
mesh = (
  pd.concat([
    pd.read_feather('mesh_grants.feather')[['pmid', 'mjr']],
    pd.read_feather('amesh.feather')[['pmid', 'mjr']]
  ])
  .groupby('pmid')['mjr']
  .agg(['size', 'sum'])
)

nmesh = mesh['size'].mean().round(0)
nmjr = mesh['sum'].mean().round(0)

print(nmesh, nmjr)

# Average number of publications for PI and grants
cpn['npub_w_ci'].mean().round(1)
cpn['npub_w_ci'].median()
cpn['prev_npub'].mean().round(1)
cpn['prev_npub'].median()

# Clustering
cpn['pi_id'].nunique()

pi_counts = cpn.groupby('pi_id').size()
pi_counts.mean().round(2)
pd.DataFrame({
  'n': pi_counts .value_counts(),
  'p': (pi_counts .value_counts(normalize=True) * 100).round(1)
})
  
n_dups = cpn[cpn['pi_id'].isin(pi_counts[pi_counts > 1].index)].shape[0]
n_dups
round(n_dups / len(cpn) * 100, 1)

# Number of observations and clusters in robustness analysis
for c in [5, 10]:
  rob = cpn.query('npub_w_ci >= @c & npub_w_mesh >= @c & prev_npub >= @c & pi_prev_npub_w_mesh >= @c')
  print(c, len(rob), rob['pi_id'].nunique())
