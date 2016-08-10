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
      "isotonic", "freshwater")))
      return("impaired")
    if(all(xs %in% c("optimal growth", "positive growth", "severe impairment",   
      "isotonic", "freshwater")))
      return("severely impaired")
    if(all(xs %in% c("optimal growth", "positive growth", "minimal impairment",   
      "brackish")))
      return("energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", "isotonic", 
      "freshwater")))
      return("growth limited, impaired")
    if(all(xs %in% c("negative/no growth", "severe impairment", "isotonic", 
      "freshwater")))
      return("growth limited, severely impaired")
    if(all(xs %in% c("negative/no growth", "minimal impairment", "brackish")))
      return("growth limited, energy demanding")
    if(all(xs %in% c("optimal growth", "positive growth", "some impairment",   
      "brackish")))
      return("impaired, energy demanding")
    if(all(xs %in% c("optimal growth", "positive growth", "severe impairment",   
      "brackish")))
      return("severely impaired, energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", "brackish")))
      return("growth limited, impaired, energy demanding")
    if(all(xs %in% c("negative/no growth", "severe impairment", "brackish")))
      return("growth limited, severely impaired, energy demanding")
    if(any(xs %in% c("unsuitable", "marine")))
      return("unsuitable")
    stop("no category for ", paste(c("ta", "oa", "sa"), xs, sep = " = ", 
      collapse = ", "))
  }
  factor(apply(data.frame(ta, oa, sa), 1, hab_cat),
    levels = c("optimal", "growth limited", "impaired", 
      "severely impaired", "energy demanding", "growth limited, impaired",
      "growth limited, severely impaired", "growth limited, energy demanding", "impaired, energy demanding", "severely impaired, energy demanding", 
      "growth limited, impaired, energy demanding", 
      "growth limited, severely impaired, energy demanding", "unsuitable"))
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
      "isotonic", "freshwater", "brackish")))
      return("impaired")
    if(all(xs %in% c("optimal growth", "positive growth", "severe impairment",   
      "isotonic", "freshwater", "brackish")))
      return("severely impaired")
    if(all(xs %in% c("optimal growth", "positive growth", "minimal impairment",   
      "marine")))
      return("energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", "isotonic", 
      "freshwater", "brackish")))
      return("growth limited, impaired")
    if(all(xs %in% c("negative/no growth", "severe impairment", "isotonic", 
      "freshwater", "brackish")))
      return("growth limited, severely impaired")
    if(all(xs %in% c("negative/no growth", "minimal impairment", "marine")))
      return("growth limited, energy demanding")
    if(all(xs %in% c("optimal growth", "positive growth", "some impairment",   
      "marine")))
      return("impaired, energy demanding")
    if(all(xs %in% c("optimal growth", "positive growth", "severe impairment",   
      "marine")))
      return("severely impaired, energy demanding")
    if(all(xs %in% c("negative/no growth", "some impairment", "marine")))
      return("growth limited, impaired, energy demanding")
    if(all(xs %in% c("negative/no growth", "severe impairment", "marine")))
      return("growth limited, severely impaired, energy demanding")
    if(any(xs %in% c("unsuitable")))
      return("unsuitable")
    stop("no category for ", paste(c("ta", "oa", "sa"), xs, sep = " = ", 
      collapse = ", "))
  }
  factor(apply(data.frame(ta, oa, sa), 1, hab_cat),
    levels = c("optimal", "growth limited", "impaired", 
      "severely impaired", "energy demanding", "growth limited, impaired",
      "growth limited, severely impaired", "growth limited, energy demanding", "impaired, energy demanding", "severely impaired, energy demanding", 
      "growth limited, impaired, energy demanding", 
      "growth limited, severely impaired, energy demanding", "unsuitable"))
}
