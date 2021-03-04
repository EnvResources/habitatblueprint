make_hsd = function(expr, score){
  function(x) score[unlist(lapply(expr, eval, envir = environment()))]  
}

classify_ta = function(ta){
  ta.windows = list(
    "optimal growth" = expression(x >= 14 & x <= 18),
    "positive growth" = expression(x < 14 | (x >= 18 & x <= 21)),
    "negative/no growth" = expression(x > 21 & x <= 25),
    "unsuitable" = expression(x > 25)
  )
  ta_hsd = make_hsd(ta.windows, names(ta.windows))
  factor(sapply(ta, ta_hsd), levels = names(ta.windows))
}

classify_sa = function(sa){
  sa.windows = list(
    "freshwater" = expression(x < 10),
    "isotonic" = expression(x >= 10 & x <= 15),
    "brackish" = expression(x > 15 & x <= 28),
    "marine" = expression(x > 28)
  )
  sa_hsd = make_hsd(sa.windows, names(sa.windows))
  factor(sapply(sa, sa_hsd), levels = names(sa.windows))
}

classify_oa = function(oa){
  oa.windows = list(
    "minimal impairment" = expression(x >= 6),
    "some impairment" = expression(x >= 4 & x < 6),
    "severe impairment" = expression(x >= 3 & x < 4),
    "unsuitable" = expression(x < 3)
  )
  oa_hsd = make_hsd(oa.windows, names(oa.windows))
  factor(sapply(oa, oa_hsd), levels = names(oa.windows))
}

classify_freshwater = function(ta, sa, oa){
  hab_cat = function(xs){
    if(all(xs %in% c("optimal growth", "positive growth", "minimal impairment",   
      "isotonic", "freshwater")))
      return("optimal")
    if(all(xs %in% c("negative/no growth", "minimal impairment", "freshwater", 
      "isotonic")))
      return("growth limited")
    if(all(xs %in% c("optimal growth", "positive growth", "some impairment",   
      "severe impairment", "isotonic", "freshwater")))
      return("impaired")
    if(all(xs %in% c("optimal growth", "positive growth", "minimal impairment",   
      "brackish")))
      return("energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", 
      "severe impairment", "isotonic", "freshwater")))
      return("growth limited, impaired")
    if(all(xs %in% c("negative/no growth", "minimal impairment", "brackish")))
      return("growth limited, energy demanding")
    if(all(xs %in% c("optimal growth", "positive growth", "some impairment",   
      "severe impairment", "brackish")))
      return("impaired, energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", 
      "severe impairment", "brackish")))
      return("growth limited, impaired, energy demanding")
    if(any(xs %in% c("unsuitable", "marine")))
      return("unsuitable")
    stop("no category for ", paste(c("ta", "oa", "sa"), xs, sep = " = ", 
      collapse = ", "))
  }
  factor(apply(data.frame(ta, oa, sa), 1, hab_cat),
    levels = c("optimal", "growth limited", "impaired", "energy demanding", 
      "growth limited, impaired", "growth limited, energy demanding", 
      "impaired, energy demanding", 
      "growth limited, impaired, energy demanding", 
      "unsuitable"))
}

classify_saltwater = function(ta, oa, sa){
  hab_cat = function(xs){
    if(all(xs %in% c("optimal growth", "positive growth", "minimal impairment",   
      "isotonic", "brackish", "freshwater")))
      return("optimal")
    if(all(xs %in% c("negative/no growth", "minimal impairment", "freshwater", 
      "brackish", "isotonic")))
      return("growth limited")
    if(all(xs %in% c("optimal growth", "positive growth", "some impairment",   
      "severe impairment", "isotonic", "freshwater", "brackish")))
      return("impaired")
    if(all(xs %in% c("optimal growth", "positive growth", "minimal impairment",   
      "marine")))
      return("energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", 
    "severe impairment", "isotonic", "freshwater", "brackish")))
      return("growth limited, impaired")
    if(all(xs %in% c("negative/no growth", "minimal impairment", "marine")))
      return("growth limited, energy demanding")
    if(all(xs %in% c("optimal growth", "positive growth", "some impairment",   
      "severe impairment", "marine")))
      return("impaired, energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", 
      "severe impairment", "marine")))
      return("growth limited, impaired, energy demanding")
    if(any(xs %in% c("unsuitable")))
      return("unsuitable")
    stop("no category for ", paste(c("ta", "oa", "sa"), xs, sep = " = ", 
      collapse = ", "))
  }
  factor(apply(data.frame(ta, oa, sa), 1, hab_cat),
    levels = c("optimal", "growth limited", "impaired", 
      "energy demanding", "growth limited, impaired",
      "growth limited, energy demanding", "impaired, energy demanding", 
      "growth limited, impaired, energy demanding", 
      "unsuitable"))
}


data(grids)
habgrids = grids
habgrids["ta.qual"] = classify_ta(habgrids$ta)
habgrids["sa.qual"] = classify_sa(habgrids$sa)
habgrids["oa.qual"] = classify_oa(habgrids$oa)
habgrids["habitat.fwa"] = classify_freshwater(habgrids$ta.qual, habgrids$sa.qual, habgrids$oa.qual)
habgrids["habitat.swa"] = classify_saltwater(habgrids$ta.qual, habgrids$sa.qual, habgrids$oa.qual)
# add closure meta data
data(closures)
habgrids["code"] = "O"
habgrids["days.since.closure"] = NA
for(d in unique(habgrids$date)){
  ind = which(d >= closures$start & d <= closures$end)
  if(length(ind) > 0){
    habgrids[habgrids$date == d, "code"] = as.character(closures[ind, "code"])
    if(as.character(closures[ind, "code"]) == "C")
      habgrids[habgrids$date == d, "days.since.closure"] = 
        habgrids[habgrids$date == d, "date"] - 
        closures$start[max(which(d >= closures$start))]
  }
}
habgrids["code"] = factor(habgrids$code)
# add ctd meta data
data(ctdmeta)
gridid = as.character(interaction(habgrids$date, habgrids$id))
metaid = as.character(interaction(strftime(ctdmeta$start, format = "%Y-%m-%d", 
  tz = "US/Pacific"), ctdmeta$id))
for(i in seq(nrow(ctdmeta))){
  habgrids[gridid == metaid[i], "numcasts"] = ctdmeta[i, "numcasts"]  
}

# usethis::use_data(habgrids, overwrite = TRUE)