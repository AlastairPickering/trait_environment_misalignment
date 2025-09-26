# This code reads in the individual forest datasets for each state in the United States
# and Canada, calculates the Basal Area proportions of each species per plot and
# creates Community Weighted Trait Mean (CWM) values using 3 trait databases.

# Any missing trait data is imputed using a nearest neighbour phylogenetic look-up function
# while FIA community group equivalents are imputed for Canadian plots. Finally, Biome 
# information is added to the dataframe and it is exported for merging with climate
# data from extract_climate_data.R


rm(list=ls())


# libraries
library(tidyverse)
library(arrow)
library(randomForest)
library(devtools)
library(V.PhyloMaker)
library(e1071)
library(sf)

# read in FIA forest type codes
FIA_forest_code <- read_csv("data/metadata/FIA_forest_types.csv") %>% 
  dplyr::select(VALUE, group_label, TYPGRPCD, MEANING)

# read in NE state-level plot data removing unnecessary data add in FIA code
ri_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_RI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ri_plot <- merge(ri_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING)

ma_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ma_plot <- merge(ma_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING)

me_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_ME.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
me_plot <- merge(me_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING)

ct_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_CT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ct_plot <- merge(ct_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nh_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_NH.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nh_plot <- merge(nh_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

vt_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_VT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
vt_plot <- merge(vt_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

# PNW states
ak_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_AK.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ak_plot <- merge(ak_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ca_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_CA.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ca_plot <- merge(ca_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

id_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_ID.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
id_plot <- merge(id_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

or_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_OR.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
or_plot <- merge(or_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wa_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_WA.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wa_plot <- merge(wa_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

# Add in rest of States

al_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_AL.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
al_plot <- merge(al_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ar_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_AR.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ar_plot <- merge(ar_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

as_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_AS.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
as_plot <- merge(as_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

az_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_AZ.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
az_plot <- merge(az_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

co_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_CO.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
co_plot <- merge(co_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

de_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_DE.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
de_plot <- merge(de_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

fl_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_FL.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
fl_plot <- merge(fl_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

fm_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_FM.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
fm_plot <- merge(fm_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ga_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_GA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ga_plot <- merge(ga_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

gu_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_GU.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
gu_plot <- merge(gu_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

hi_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_HI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
hi_plot <- merge(hi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ia_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_IA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ia_plot <- merge(ia_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

il_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_IL.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
il_plot <- merge(il_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

in_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_IN.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
in_plot <- merge(in_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ks_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_KS.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ks_plot <- merge(ks_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ky_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_KY.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ky_plot <- merge(ky_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

la_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_LA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
la_plot <- merge(la_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

md_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MD.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
md_plot <- merge(md_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mh_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MH.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mh_plot <- merge(mh_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mi_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mi_plot <- merge(mi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mn_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MN.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mn_plot <- merge(mn_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mo_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MO.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mo_plot <- merge(mo_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mp_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MP.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mp_plot <- merge(mp_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ms_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MS.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ms_plot <- merge(ms_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mt_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_MT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mt_plot <- merge(mt_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nc_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_NC.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nc_plot <- merge(nc_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nd_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_ND.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nd_plot <- merge(nd_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ne_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_NE.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ne_plot <- merge(ne_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nj_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_NJ.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nj_plot <- merge(nj_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nm_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_NM.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nm_plot <- merge(nm_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nv_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_NV.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nv_plot <- merge(nv_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ny_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_NY.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ny_plot <- merge(ny_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

oh_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_OH.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
oh_plot <- merge(oh_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ok_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_OK.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ok_plot <- merge(ok_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

pa_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_PA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
pa_plot <- merge(pa_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

pr_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_PR.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
pr_plot <- merge(pr_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

pw_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_PW.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
pw_plot <- merge(pw_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

sc_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_SC.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
sc_plot <- merge(sc_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

sd_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_SD.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
sd_plot <- merge(sd_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

tn_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_TN.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
tn_plot <- merge(tn_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

tx_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_TX.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
tx_plot <- merge(tx_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ut_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_UT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ut_plot <- merge(ut_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

va_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_VA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
va_plot <- merge(va_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

vi_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_VI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
vi_plot <- merge(vi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wi_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_WI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wi_plot <- merge(wi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wv_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_WV.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wv_plot <- merge(wv_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wy_plot<- read_feather("data/forest_plots/US/cleaned_state_data/FIA_WY.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  dplyr::select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wy_plot <- merge(wy_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

# state codes
state_codes <- c("ma", "me", "vt", "ct", "nh", "ri", "ak", "al", "ar", "as", "az", "ca", 
                 "co", "de", "fl", "fm", "ga", "gu", "hi", "ia", "id", "il", "in", "ks", 
                 "ky", "la", "md", "mh", "mi", "mn", "mo", "mp", "ms", "mt", "nc", "nd", 
                 "ne", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "pr", "pw", "sc", 
                 "sd", "tn", "tx", "ut", "va", "vi", "wa", "wi", "wv", "wy")

# List of all dataframes
plot_dfs <- list(ma_plot, me_plot, vt_plot, ct_plot, nh_plot, ri_plot, ak_plot, al_plot, ar_plot, as_plot, az_plot,
                 ca_plot, co_plot, de_plot, fl_plot, fm_plot, ga_plot, gu_plot, hi_plot, ia_plot, id_plot, il_plot, 
                 in_plot, ks_plot, ky_plot, la_plot, md_plot, mh_plot, mi_plot, mn_plot, mo_plot, mp_plot, ms_plot, 
                 mt_plot, nc_plot, nd_plot, ne_plot, nj_plot, nm_plot, nv_plot, ny_plot, oh_plot, ok_plot, or_plot, 
                 pa_plot, pr_plot, pw_plot, sc_plot, sd_plot, tn_plot, tx_plot, ut_plot, va_plot, vi_plot, wa_plot, 
                 wi_plot, wv_plot, wy_plot)

# Add state code column to each dataframe
for (i in seq_along(plot_dfs)) {
  plot_dfs[[i]]$state_code <- state_codes[i]
}

# Merge all dataframes
plot_df_master <- Reduce(function(x, y) merge(x, y, all = TRUE), plot_dfs)

# Remove rows with NA
plot_df_master <- na.omit(plot_df_master)

# Calculate total unique plots per FIA_group
plot_df_master_group<- plot_df_master %>% 
  group_by(group_label) %>% 
  summarise(total_plots = n_distinct(pid))
sum(plot_df_master_group$total_plots)

# Filter by STDAGE and nonstocked
plot_df_master <- plot_df_master %>% 
  group_by(group_label) %>% 
  filter(STDAGE>= quantile(STDAGE,0.5)) %>% 
  filter(!group_label == "Nonstocked") 

# remove plots to free up memory
plots <- ls(pattern = "_plot")
rm(list=plots)

# Read in Canada plot data which already has basal_area calculations
canada_plot <- read_csv("data/forest_plots/Canada/canada_plot_df_2024.csv")
canada_plot$pid <- as.character(canada_plot$pid)
canada_plot$state_code <- "canada"
canada_plot <- na.omit(canada_plot)

# 1b. Prepare trait data and labels

# read in imputed traits
imputed_df<- read_csv("data/traits/physiological_traits.csv")

# drop any species whose traits aren't in imputed_traits
plot_df_master <- plot_df_master %>% 
  filter(accepted_bin %in% imputed_df$accepted_bin)

canada_plot <- canada_plot %>% 
  filter(accepted_bin %in% imputed_df$accepted_bin)

# Read in tree species metadata
metadata_df<- read_csv("data/metadata/taxonomic_info.csv")

# filter imputed traits to just relevant species from US and Canada dfs
traits_df<- imputed_df %>% 
  filter(accepted_bin %in% plot_df_master$accepted_bin | accepted_bin %in% canada_plot$accepted_bin)

traits_df <- mutate(traits_df, trait_short = tolower(trait_short))
traits_df <- mutate(traits_df, trait_short = gsub(" ", "_", trait_short))

# create new wider df with imputed values for each trait per species
traits_df <-traits_df %>% 
  filter(traits_df$fit =="phy") %>% 
  group_by(accepted_bin) %>% 
  dplyr::select(accepted_bin,pred_value, trait_short) %>% 
  pivot_wider(
    names_from = trait_short,
    values_from = pred_value
  )

# select just the traits and remove NAs
traits_df <- na.omit(traits_df)

# Read in myco trait metadata
myco <- read_csv("data/traits/myco_trait.csv")

# Align accepted_binomials using SPCD codes
myco$accepted_bin2 <- plot_df_master$accepted_bin[match(myco$SPCD,plot_df_master$SPCD)]
traits_df$myco_association <- myco$myco_association[match(traits_df$accepted_bin, myco$accepted_bin2)]
traits_df$myco_association <- as.numeric(traits_df$myco_association)

# read in trait syndromes and filter to relevant syndromes and species
syndromes_df<- read_csv("data/traits/trait_syndromes.csv") %>% 
  dplyr::select(accepted_bin,shade,drought,cold,water,fire_tol) %>% 
  filter(accepted_bin %in% plot_df_master$accepted_bin)

# Adjust cold tolerance to max higher values representative of higher tolerance
syndromes_df$cold <- abs(syndromes_df$cold)

# merge syndromes with traits
traits_df<- left_join(traits_df,syndromes_df,by = 'accepted_bin')
syndrome_species <- traits_df %>% 
  filter(is.na(shade))

# Read in full phylo tree
full_tree <- V.PhyloMaker::GBOTB.extended
full_tree$tip.label <- gsub("_", " ", full_tree$tip.label)

# Create cophenetic matrix
coph_matrix <-cophenetic(full_tree)

# Subset matrix to missing syndrome species (rows) and Rueda syndrome species (cols)
target_species <- syndrome_species$accepted_bin
target_indices <- match(target_species, rownames(coph_matrix))
target_distances <- coph_matrix[target_indices,]
rownames(target_distances) <- target_species
accepted_species <- syndromes_df$accepted_bin
accepted_indices <- match(accepted_species,colnames(target_distances))
subset_coph_matrix <- target_distances[,accepted_indices]
colnames(subset_coph_matrix) <- accepted_species

# Find rows with at least one non-NA value
rows_to_keep <- apply(!is.na(subset_coph_matrix), 1, any)

# Create the neighbour column
neighbours <- apply(subset_coph_matrix[rows_to_keep, , drop = FALSE], 1, function(row) {
  colnames(subset_coph_matrix)[which.min(row)]
})

# Add the neighbour column to the subset_coph_matrix
subset_coph_matrix <- cbind(subset_coph_matrix, neighbour = NA)  # Initialize the column
subset_coph_matrix[rows_to_keep, "neighbour"] <- neighbours

missing_species <- as.data.frame(subset_coph_matrix)
missing_species <- missing_species %>% mutate(accepted_bin = rownames(missing_species))
missing_species$genus <- metadata_df$genus[match(missing_species$accepted_bin, metadata_df$accepted_bin)]

# Function to fill missing values in the "neighbour" column
fill_missing_neighbours <- function(df) {
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "neighbour"])) {
      # Get the genus of the current row
      current_genus <- df[i, "genus"]
      
      # Find other rows with the same genus
      same_genus_rows <- df[df$genus == current_genus, "neighbour"]
      
      if (length(same_genus_rows) > 0) {
        # Select the first available non-NA value from other rows with the same genus
        df[i, "neighbour"] <- same_genus_rows[!is.na(same_genus_rows)][1]
      } else {
        # If no other genus member is available, put 0
        df[i, "neighbour"] <- 0
      }
    }
  }
  return(df)
}

# Apply the function to fill missing values in the neighbour column
missing_species <- fill_missing_neighbours(missing_species)

# Syndromes to be replaced
syndromes_to_replace <- c("shade", "drought", "cold", "myco_association","water","fire_tol")

# Function to impute trait syndromes from neighbour species
impute_traits_from_neighbour <- function(traits_df, missing_species) {
  for (i in 1:nrow(missing_species)) {
    # Get the current species and its neighbour
    current_species <- rownames(missing_species)[i]
    neighbour_species <- missing_species[i, "neighbour"]
    
    # Find the index of the neighbour species in traits_df
    neighbour_index <- which(traits_df$accepted_bin == neighbour_species)
    
    # If the neighbour species is found in traits_df
    if (length(neighbour_index) > 0) {
      # Impute trait values from the neighbour to the current species for specific syndromes
      for (syndrome in syndromes_to_replace) {
        traits_df[traits_df$accepted_bin == current_species, syndrome] <- traits_df[neighbour_index, syndrome]
      }
    }
  }
  
  return(traits_df)
}

# Apply the function to impute trait syndromes from neighbour species
traits_df <- impute_traits_from_neighbour(traits_df, missing_species)

myco_na <- traits_df %>% 
  filter(is.na(myco_association)) %>% 
  dplyr::select(accepted_bin, myco_association)

sum(is.na(traits_df$myco_association))
traits_df <- na.omit(traits_df)

# check skewedness of traits
threshold <- 1

for(col in names(traits_df[-1])) {
  sk <- skewness(traits_df[[col]])
  if(abs(sk)>threshold){
    print(paste("Trait",col, "skewed")) 
  } else {
    print(paste("Trait",col, "not skewed")) 
  }
}

# create new log transformed trait_df transforming only those traits that are skewed
traits_transformed <- traits_df
traits_transformed$root_depth <- log(traits_transformed$root_depth)
traits_transformed$stem_diameter <- log(traits_transformed$stem_diameter)
traits_transformed$leaf_thickness <- log(traits_transformed$leaf_thickness)
traits_transformed$crown_diameter <- log(traits_transformed$crown_diameter)
traits_transformed$tree_height <- log(traits_transformed$tree_height)
traits_transformed$leaf_area <- log(traits_transformed$leaf_area)
traits_transformed$bark_thickness <- log(traits_transformed$bark_thickness)
traits_transformed$seed_dry_mass <- log(traits_transformed$seed_dry_mass)
traits_transformed$water <- log(traits_transformed$water)

# check how many traits still skewed
for(col in names(traits_transformed[-1])) {
  sk <- skewness(traits_transformed[[col]])
  if(abs(sk)>threshold){
    print(paste("Trait",col, "skewed")) 
  } else {
    print(paste("Trait",col, "not skewed")) 
  }
}

# create plot df grouped by plot_id including species and basal area for US
plot_df <- plot_df_master %>% 
  dplyr::select(pid, accepted_bin, DIA, TPA_UNADJ, SPCD)%>% 
  group_by(pid, accepted_bin) %>%  # Group by pid and accepted_bin
  summarise(basal_area = sum((pi*(DIA/2)^2)*TPA_UNADJ)) %>% 
  dplyr::select(pid, accepted_bin, basal_area) # basal area per species per plot

# Add Canadian plots to this new df as their basal area has already been calculated
plot_df_canada <- rbind(plot_df, canada_plot) %>% dplyr::select(-state_code)

# Summarise basal area for each combination of pid and accepted_bin
plot_df_canada <- plot_df_canada %>% 
  pivot_wider(
    names_from = accepted_bin,
    values_from = basal_area,
    values_fill = list(basal_area = 0) # Fill missing values with 0
  ) %>%
  ungroup() # Ungroup the data

# create new df combining trait values and plot info
plot_traits_df <- plot_df_canada %>% 
  pivot_longer(cols = -pid, names_to = "species", values_to = "basal_area")

# Merge plot_traits_df with trait_df based on species names
plot_traits_df <- merge(plot_traits_df, traits_transformed, by.x = "species", by.y = "accepted_bin", all.x = TRUE)
plot_traits_df [is.na(plot_traits_df)] <- 0 # Replace missing trait values with 0
plot_traits_df <- plot_traits_df %>% filter(basal_area>0)

plot_traits_df <- plot_traits_df %>% # Aggregate trait values for each plot into a single vector
  group_by(pid) %>%
  summarise(across(-c(species, basal_area), ~ weighted.mean(., basal_area, na.rm = TRUE))) %>%
  ungroup()

# check how many traits still skewed
for(col in names(plot_traits_df[-1])) {
  sk <- skewness(plot_traits_df[[col]])
  if(abs(sk)>threshold){
    print(paste("Trait",col, "skewed")) 
  } else {
    print(paste("Trait",col, "not skewed")) 
  }
}

fia_code <- plot_df_master %>% 
  group_by(pid) %>% 
  dplyr::select (pid, group_label,state_code) %>% 
  unique()
plot_traits_df$FIA_group <- fia_code$group_label[match(plot_traits_df$pid,fia_code$pid)] 

# Check for missing FIA_group values
summary(plot_traits_df$FIA_group)

# Separate the data into rows with and without FIA_group
train_df <- plot_traits_df %>% filter(!is.na(FIA_group))
test_df <- plot_traits_df %>% filter(is.na(FIA_group))

# Ensure FIA_group is a factor
train_df$FIA_group <- as.factor(train_df$FIA_group)

# Train a Random Forest model to predict FIA_group based on the traits
set.seed(123)  # For reproducibility
rf_model <- randomForest(FIA_group ~ ., data = train_df[, -which(names(train_df) == "pid")], importance = TRUE)

# Predict the missing FIA_group values for the test data
test_df$FIA_group <- predict(rf_model, newdata = test_df[, -which(names(test_df) %in% c("FIA_group", "pid"))])

# Combine the train and test data back together
plot_traits_df <- bind_rows(train_df, test_df)


# Add in static geological and physical enviromental traits
llid <- read_csv("data/metadata/plot_ll_id_north_america.csv")
plot_traits_df$ll_id <- llid$ll_id[match(plot_traits_df$pid, llid$pid)]
env_df <- read_csv("data/metadata/composite_static_layers_NA.csv")
ll_id <- env_df$ll_id
env_df <- as.data.frame(env_df)
env_df$ll_id <- ll_id
plot_traits_df <- merge(plot_traits_df, env_df, by ="ll_id")


plot_traits_df <- plot_traits_df %>% 
  dplyr::select(-ll_id) %>% 
  na.omit()

num_species <- plot_df_canada %>% 
  pivot_longer(cols = -pid, names_to = "species", values_to = "basal_area") %>% 
  filter(!is.na(basal_area) & basal_area > 0) %>%  # Include only species with non-NA and positive basal area
  group_by(pid) %>% 
  summarise(num_species = n_distinct(species))  # Summarise to count unique species per pid

managed_code <- plot_df_master %>% 
  group_by(pid) %>% 
  dplyr::select (pid, managed, OWNCD, STDAGE) %>% 
  unique()
plot_traits_df$managed <- managed_code$managed[match(plot_traits_df$pid,managed_code$pid)] 
plot_traits_df$OWNCD <- managed_code$OWNCD[match(plot_traits_df$pid,managed_code$pid)] 
plot_traits_df$STDAGE <- managed_code$STDAGE[match(plot_traits_df$pid,managed_code$pid)] 
plot_traits_df$num_species <- num_species$num_species[match(plot_traits_df$pid,num_species$pid)] 

sum(is.na(num_species$num_species))

# Load the WWF Terrestrial Ecoregions shapefile
ecoregions <- st_read("data/metadata/ecoregions//wwf_terr_ecos.shp")

# Check and fix invalid geometries in the ecoregions shapefile
if (any(!st_is_valid(ecoregions))) {
  ecoregions <- st_make_valid(ecoregions)
  message("Invalid geometries found and repaired.")
}
# Convert  dataframe with lat/lon to an sf object
plot_traits_df <- plot_traits_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove=FALSE)  # WGS84 coordinate system

# Perform spatial join to assign ecoregion
plot_traits_df <- st_join(plot_traits_df, ecoregions, join = st_intersects)

#  extract a specific column like 'ECO_NAME' for the ecoregion name
plot_traits_df <- plot_traits_df %>%
  mutate(ecoregion = ECO_NAME)
plot_traits_df <- st_drop_geometry(plot_traits_df)

# Create a lookup table for Biome codes and labels
biome_lookup <- data.frame(
  BIOME = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
  BIOME_LABEL = c(
    "Tropical & Subtropical Moist Broadleaf Forests",
    "Tropical & Subtropical Dry Broadleaf Forests",
    "Tropical & Subtropical Coniferous Forests",
    "Temperate Broadleaf & Mixed Forests",
    "Temperate Conifer Forests",
    "Boreal Forests/Taiga",
    "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    "Temperate Grasslands, Savannas & Shrublands",
    "Flooded Grasslands & Savannas",
    "Montane Grasslands & Shrublands",
    "Tundra",
    "Mediterranean Forests, Woodlands & Scrub",
    "Deserts & Xeric Shrublands",
    "Mangroves"
  )
)

# Join the biome labels onto the plot_traits_df based on the BIOME code
plot_traits_df <- plot_traits_df %>%
  left_join(biome_lookup, by = "BIOME")

# Replace the BIOME column with the BIOME_LABEL
plot_traits_df <- plot_traits_df %>%
  mutate(BIOME = BIOME_LABEL) %>%
  dplyr::select(-BIOME_LABEL)  # Optionally remove the temporary BIOME_LABEL column

write_csv(plot_traits_df,"data/precomputed/forest_trait_means.csv")
