library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(RPostgreSQL)
library(RColorBrewer)
library(classInt)

# read puma data -----------------------------------

puma_units <- st_read(proj_con, c("eitc","puma_state_geography")) %>%
  rmapshaper::ms_simplify(keep_shapes = TRUE) %>%
  left_join(dbReadTable(proj_con, c("eitc", "HighLevelTest_EITCGap SummaryQualTaxUnits")), by = c("geoid10"="GEOID")) %>%
  select(geoid10, name10, single, single_nonfam, single_parent, married, EstEITC_QTU)

# read demographic data ----------------------------------------------------------------------------------------------
taxsites <- st_read(proj_con, c("eitc", "taxsite_state_01302018")) %>% mutate(id = as.character(id)) %>% filter(state == "CO")
charities <- st_read(proj_con, c("eitc", "community_facilities"))
demograph <- st_read(proj_con, c("public", "tract_state_geography_2010"))[,"geoid10"] %>%
  rmapshaper::ms_simplify(keep_shapes = TRUE) %>%
  inner_join(dbReadTable(proj_con, c("eitc", "language"))[,2:9], by="geoid10") %>%
  inner_join(dbReadTable(proj_con, c("eitc", "plurality"))[,2:11], by="geoid10") %>%
  inner_join(dbReadTable(proj_con, c("eitc", "family_type"))[,2:6] %>% mutate(married_per = married/total_family, single_per = single/total_family), by="geoid10") %>%
  inner_join(dbReadTable(proj_con, c("eitc", "median_family_income_children"))[,2:6], by="geoid10") %>%
  inner_join(dbReadTable(proj_con, c("eitc", "median_family_income_size"))[,2:9], by="geoid10")
tract_geo <- st_read(proj_con, c("public", "tract_state_geography_2010")) %>% select(geometry) %>% rmapshaper::ms_simplify(keep_shapes = TRUE)
st_geometry(demograph) <- NULL
demograph[demograph<0] <- NA
demograph[demograph$others_per>100, "others_per"] <- NA

# map layers index ---------------------------------------------------------------------------------------------------
  #report buider ------------------------------------------------------
  rpt_lst <- list()
  rpt_lst[[1]] <- st_read(proj_con,c("eitc","zip_state_eitc_rpt")) %>% rmapshaper::ms_simplify(keep_shapes = TRUE)
  rpt_lst[[2]] <- st_read(proj_con,c("eitc","county_state_eitc_rpt")) %>% rmapshaper::ms_simplify(keep_shapes = TRUE)
  rpt_lst[[3]] <- st_read(proj_con,c("eitc","hdist_state_eitc_rpt")) %>% rmapshaper::ms_simplify(keep_shapes = TRUE)
  rpt_lst[[4]] <- st_read(proj_con,c("eitc","sdist_state_eitc_rpt")) %>% rmapshaper::ms_simplify(keep_shapes = TRUE)
  names(rpt_lst) <- c("Zip Code","County","State House District","State Senate District")
  
  rpt_index <- as.data.frame(
    cbind(c("Total Households","Households (Income < 50K)","Total Families","Families (Income < 50K with Own Children)",
            "Total Tax Filers","# EITC Claims","% EITC Claims","EITC Amount",
            "Total Households","Households (Income < 50K)","Total Families","Families (Income < 50K with Own Children)",
            "Total Tax Filers","# EITC Claims","% EITC Claims","EITC Amount",
            "Total Households","Households (Income < 50K)","Total Families","Families (Income < 50K with Own Children)",
            "Total Households","Households (Income < 50K)","Total Families","Families (Income < 50K with Own Children)"),
          c("ttl_hh","qual_hh","ttl_fam","qual_fam",
            "total_return","eitc_return","eitc_share","eitc_amnt",
            "ttl_hh","qual_hh","ttl_fam","qual_fam",
            "total_return","eitc_return","eitc_share","eitc_amnt",
            "ttl_hh","qual_hh","ttl_fam","qual_fam",
            "ttl_hh","qual_hh","ttl_fam","qual_fam"),
          c("Zip Code","Zip Code","Zip Code","Zip Code","Zip Code","Zip Code","Zip Code","Zip Code",
            "County","County","County","County","County","County","County","County",
            "State House District","State House District","State House District","State House District",
            "State Senate District","State Senate District","State Senate District","State Senate District"))
  )
  colnames(rpt_index) <- c("dropdown", "column","geography")
  
  # rpt_col_lst <- list()
  # rpt_col_lst[[1]] <- list(
  #   "Total Families"=leaflet::colorBin(
  #     palette = brewer.pal(7, "Blues"),
  #     bins = classIntervals(rpt_lst[[1]]$ttl_fam, 7, style = "fisher")$brks,
  #     na.color = "#808080"
  #   ),
  #   "# EITC Qualified Families"=leaflet::colorBin(
  #     palette = brewer.pal(7, "Reds"),
  #     bins = classIntervals(rpt_lst[[1]]$qual_fam, 7, style = "fisher")$brks,
  #     na.color = "#808080"
  #   )
  # )
  # rpt_col_lst[[2]] <- list(
  #   "Total Families"=leaflet::colorBin(
  #     palette = brewer.pal(7, "Reds"),
  #     bins = classIntervals(rpt_lst[[2]]$ttl_fam, 7, style = "fisher")$brks,
  #     na.color = "#808080"
  #   ),
  #   "# EITC Qualified Families"=leaflet::colorBin(
  #     palette = brewer.pal(7, "Reds"),
  #     bins = classIntervals(rpt_lst[[2]]$qual_fam, 7, style = "fisher")$brks,
  #     na.color = "#808080"
  #   )
  # )
  
  
  #demograph zoom in --------------------------------------------    
demo_ind <- c(
  "Language Spoken at Home",
  "Language Spoken at Home",
  "Language Spoken at Home",
  "Race & Ethnicity",
  "Race & Ethnicity",
  "Race & Ethnicity",
  "Race & Ethnicity",
  "Race & Ethnicity",
  "Race & Ethnicity",
  "Race & Ethnicity",
  "Family Type",
  "Family Type",
  "Family Type"
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income",
  # "Median Family Income"
  )
demo_layers <- c(
  "% English Speakers",
  "% Spanish Speakers",
  "% Other Languages Speakers",
  "Total Population",
  "Plurality",
  "Non Hispanic or Latino White",
  "Hispanic or Latino",
  "Black or African American",
  "American Indian and Alaska Native",
  "Asian and Native Hawaiian and Other Pacific Islanders",
  "Total Families",
  "% Families of Married Couple",
  "% Families of Single Householder"
  # "All Families",
  # "Size 2",
  # "Size 3",
  # "Size 4",
  # "Size 5",
  # "Size 6",
  # "Size 7 and Above",
  # "Married Couple with Own Children",
  # "Single Father Families",
  # "Single Mother Families"
  )
demo_column <- c(
  "english_per",
  "spanish_per",
  "others_per",
  "b02001_001e",
  "group",
  "nonlatwhite",
  "his_lat",
  "black",
  "native",
  "asian_islanders",
  "total_family",
  "married_per",
  "single_per"
  # "all_family",
  # "size2",
  # "size3",
  # "size4",
  # "size5",
  # "size6",
  # "size7above",
  # "married_own_kid",
  # "single_fa",
  # "single_mo"
)

demo_index <- as.data.frame(cbind(demo_ind,demo_layers,demo_column), stringsAsFactors = FALSE) 

# color ramp -----------------------------------------------------------------------------
  #tract level demograph ramp --------------------------------
english_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(demograph$english_per, 7, style = "fisher")$brks,
  na.color = "#808080"
)

spanish_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(demograph$spanish_per, 7, style = "fisher")$brks,
  na.color = "#808080"
)

others_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(demograph$others_per, 7, style = "fisher")$brks,
  na.color = "#808080"
)

pop_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(demograph$b02001_001e, 7, style = "fisher")$brks,
  na.color = "#808080"
)

plu_ramp <- leaflet::colorFactor(
  palette = brewer.pal(4, "Set1"),
  domain = demograph$group,
  na.color = "#808080"
)

white_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greys"),
  bins = classIntervals(demograph$nonlatwhite, 7, style = "fisher")$brks,
  na.color = "#808080"
)

hislat_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(demograph$his_lat, 7, style = "fisher")$brks,
  na.color = "#808080"
)

black_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(demograph$black, 7, style = "fisher")$brks,
  na.color = "#808080"
)

native_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(demograph$native, 7, style = "fisher")$brks,
  na.color = "#808080"
)

asian_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(demograph$asian_islanders, 7, style = "fisher")$brks,
  na.color = "#808080"
)

asian_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greys"),
  bins = classIntervals(demograph$asian_islanders, 7, style = "fisher")$brks,
  na.color = "#808080"
)

family_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(demograph$total_family, 7, style = "fisher")$brks,
  na.color = "#808080"
)

married_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(100*demograph$married_per, 7, style = "fisher")$brks,
  na.color = "#808080"
)

single_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(100*demograph$single_per, 7, style = "fisher")$brks,
  na.color = "#808080"
)

median_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(demograph$all_family, 7, style = "fisher")$brks,
  na.color = "#808080"
)

size2_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(demograph$size2, 7, style = "fisher")$brks,
  na.color = "#808080"
)

size3_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(demograph$size3, 7, style = "fisher")$brks,
  na.color = "#808080"
)

size4_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(demograph$size4, 7, style = "fisher")$brks,
  na.color = "#808080"
)

size5_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(demograph$size5, 7, style = "fisher")$brks,
  na.color = "#808080"
)

size6_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(demograph$size6, 7, style = "fisher")$brks,
  na.color = "#808080"
)

size7_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greys"),
  bins = classIntervals(demograph$size7above, 7, style = "fisher")$brks,
  na.color = "#808080"
)

kids_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(demograph$married_own_kid, 7, style = "fisher")$brks,
  na.color = "#808080"
)

father_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(demograph$single_fa, 7, style = "fisher")$brks,
  na.color = "#808080"
)

mother_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(demograph$single_mo, 7, style = "fisher")$brks,
  na.color = "#808080"
)

irs_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(zip_irs$qual_share*100, 7, style = "fisher")$brks,
  na.color = "#808080"
)
  #legistalive boundary ramp ---------------------------------------------
  pums_ramp <- leaflet::colorBin(
    palette = brewer.pal(7, "Reds"),
    bins = classIntervals(puma_units$EstEITC_QTU, 7, style = "fisher")$brks,
    na.color = "#808080"
  )
    #zip----
zip_ttlfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(rpt_lst[[1]]$ttl_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
zip_qualfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[1]]$qual_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
zip_qualshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(rpt_lst[[1]]$qual_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
zip_ttlrtn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(rpt_lst[[1]]$total_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
zip_eitcreturn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(rpt_lst[[1]]$eitc_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
zip_eitcshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(rpt_lst[[1]]$eitc_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
zip_eitcamnt_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[1]]$eitc_amnt, 7, style = "fisher")$brks,
  na.color = "#808080"
)
    #county----
county_ttlfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[2]]$ttl_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
county_qualfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[2]]$qual_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
county_qualshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(rpt_lst[[2]]$qual_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
county_ttlrtn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(rpt_lst[[2]]$total_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
county_eitcreturn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(rpt_lst[[2]]$eitc_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
county_eitcshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(rpt_lst[[2]]$eitc_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
county_eitcamnt_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[2]]$eitc_amnt, 7, style = "fisher")$brks,
  na.color = "#808080"
)
    #hdist----
hdist_ttlfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[3]]$ttl_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
hdist_qualfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[3]]$qual_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
hdist_qualshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(rpt_lst[[3]]$qual_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
hdist_ttlrtn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(rpt_lst[[3]]$total_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
hdist_eitcreturn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(rpt_lst[[3]]$eitc_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
hdist_eitcshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(rpt_lst[[3]]$eitc_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
hdist_eitcamnt_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[3]]$eitc_amnt, 7, style = "fisher")$brks,
  na.color = "#808080"
)
    #sdist----
sdist_ttlfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[4]]$ttl_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
sdist_qualfam_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[4]]$qual_fam, 7, style = "fisher")$brks,
  na.color = "#808080"
)
sdist_qualshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Greens"),
  bins = classIntervals(rpt_lst[[4]]$qual_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
sdist_ttlrtn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Purples"),
  bins = classIntervals(rpt_lst[[4]]$total_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
sdist_eitcreturn_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Oranges"),
  bins = classIntervals(rpt_lst[[4]]$eitc_return, 7, style = "fisher")$brks,
  na.color = "#808080"
)
sdist_eitcshare_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Blues"),
  bins = classIntervals(rpt_lst[[4]]$eitc_share, 7, style = "fisher")$brks,
  na.color = "#808080"
)
sdist_eitcamnt_ramp <- leaflet::colorBin(
  palette = brewer.pal(7, "Reds"),
  bins = classIntervals(rpt_lst[[4]]$eitc_amnt, 7, style = "fisher")$brks,
  na.color = "#808080"
)
# charities icons---------------------------------------------------------------------------

charity_house <- icons(
  iconUrl = "charity_icon/house.png",
  iconWidth = 15, iconHeight = 15
)

charity_bene <- icons(
  iconUrl = "charity_icon/bene.png",
  iconWidth = 15, iconHeight = 15
)

# map viz -------------------------------------------------------------------------------
tm_shape(zip_irs) +
  tm_polygons("eitc_share")

tm_shape(df) + tm_dots(col = "projectname")
