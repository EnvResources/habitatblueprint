make_hsd = function(expr, score){
  function(x) score[unlist(lapply(expr, eval, envir = environment()))]  
}

classify_ta = function(ta){
  ta.windows = list(
    "optimal" = expression(x >= 14 & x <= 18),
    "suitable" = expression(x < 14 | (x >= 18 & x <= 21)),
    "stressful" = expression(x > 21 & x <= 25),
    "unsuitable" = expression(x > 25)
  )
  ta_hsd = make_hsd(ta.windows, names(ta.windows))
  factor(sapply(ta, ta_hsd), levels = names(ta.windows))
}

classify_sa = function(sa){
  sa.windows = list(
    "ion importing" = expression(x < 10),
    "ion neutral" = expression(x >= 10 & x <= 15),
    "induced ion exporting" = expression(x > 15 & x <= 28),
    "ion exporting" = expression(x > 28)
  )
  sa_hsd = make_hsd(sa.windows, names(sa.windows))
  factor(sapply(sa, sa_hsd), levels = names(sa.windows))
}

classify_oa = function(oa){
  oa.windows = list(
    "no impairment" = expression(x >= 6),
    "suitable" = expression(x >= 4 & x < 6),
    "limited" = expression(x >= 3 & x < 4),
    "unsuitable" = expression(x < 3)
  )
  oa_hsd = make_hsd(oa.windows, names(oa.windows))
  factor(sapply(oa, oa_hsd), levels = names(oa.windows))
}

classify_overall = function(ta, sa, oa){
  hab_cat = function(xs){
    if(any(xs %in% c("unsuitable", "ion exporting")))
      "unsuitable"
    else if(all(xs %in% c("optimal", "no impairment", "ion importing")))
      "optimal"  
    else if(xs[[1]] == "suitable" && all(xs[2:3] %in% c("no impairment", "ion importing")))
      "sub-optimal (T)"
    else if(xs[[1]] == "stressful" && all(xs[2:3] %in% c("no impairment", "ion importing")))
      "stressful (T)"
    else if(xs[[2]] %in% c("suitable", "limited") && all(xs[c(1,3)] %in% c("optimal", "ion importing")))
      "sub-optimal (DO)"
    else if(all(xs[1:2] %in% c("optimal", "no impairment")) && xs[[3]] %in% c("ion neutral", "induced ion exporting"))
      "sub-optimal (S)"
    else if(all(xs[1:2] %in% c("suitable", "limited")) && xs[[3]] %in% c("ion importing"))
      "sub-optimal (T, DO)"
    else if(all(xs[c(1,3)] %in% c("suitable", "ion neutral", "induced ion exporting")) && xs[[2]] %in% c("no impairment"))
      "sub-optimal (T, S)"
    else if(all(xs[2:3] %in% c("suitable", "limited", "ion neutral", "induced ion exporting")) && xs[[1]] %in% c("optimal"))  
      "sub-optimal (DO, S)" 
    else if(xs[[1]] %in% c("stressful") && xs[[2]] %in% c("suitable", "limited") && xs[[3]] %in% c("ion importing"))
      "stressful sub-optimal (T, DO)"
    else if(all(xs[c(1, 3)] %in% c("stressful", "ion neutral", "induced ion exporting")) && xs[[2]] %in% c("no impairment"))
      "stressful sub-optimal (T, S)"
    else if(all(xs[2:3] %in% c("suitable", "limited", "ion neutral", "induced ion exporting")) && xs[[1]] %in% c("suitable"))
      "sub-optimal (T, DO, S)"
    else if(all(xs[2:3] %in% c("suitable", "limited", "ion neutral", "induced ion exporting")) && xs[[1]] %in% c("stressful"))
      "stressful sub-optimal (T, DO, S)"
    else
      stop("no category for ", paste(c("ta", "oa", "sa"), xs, sep = " = ", collapse = ", "))
  }
  factor(apply(data.frame(ta, oa, sa), 1, hab_cat),
    levels = c("optimal", "sub-optimal (T)", "sub-optimal (DO)", 
      "sub-optimal (S)", "sub-optimal (T, DO)", "sub-optimal (T, S)",
      "sub-optimal (DO, S)", "sub-optimal (T, DO, S)", "stressful (T)", 
      "stressful sub-optimal (T, DO)", "stressful sub-optimal (T, S)", 
      "stressful sub-optimal (T, DO, S)", "unsuitable"))
}
