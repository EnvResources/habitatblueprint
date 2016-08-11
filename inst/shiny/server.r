data(ctdmeta)
data(inflows)
data(tides)
data(wll)
data(habgrids)

shinyServer(function(input, output, session){

reactivehab = reactiveValues(grid = habgrids)
  observe({
    if(input$navbar == "stop")
      stopApp()
  })

########## Explore Transect ###################################################  

########## side bar ###########

  # drop-down selection box for transect date
  output$transect_date = renderUI({
    selectInput("transect_date", "Select transect", size = 10, selected = 1, 
      selectize = FALSE, choices = setNames(seq(nrow(ctdmeta)), 
        paste(strftime(ctdmeta$start, "%Y-%m-%d"), 
          paste0(strftime(ctdmeta$start, "%H:%M"), "--",  
            strftime(ctdmeta$end, "%H:%M")), 
          paste0("(", ctdmeta$code,")")))
    )
  })

 # prep for flow/tide/wll plots
  intervalstart = reactive(ctdmeta[input$transect_date, "start"] - 3.5*86400)
  intervalend = reactive(ctdmeta[input$transect_date, "start"] + 3.5*86400)
  x.scale = reactive(scale_x_datetime("", limits = c(intervalstart(), 
    intervalend())))
  transect.lines = reactive({
    d = data.frame(date = as.numeric(ctdmeta[input$transect_date, "start"]),
      availability = ifelse(ctdmeta[input$transect_date, "numcasts"] < 12, 
        "partial", "complete"))
    list(
      geom_vline(data = d, aes(xintercept = date, linetype = availability)),
      scale_linetype_manual(values = c("complete" = "solid", 
        "partial" = "dashed"), guide = FALSE)
    )
  })
  
  # plot the flows
  inflowdata = reactive(filter(inflows, datetime >= intervalstart(),
    datetime <= intervalend(), gauge == "russian river"))
  output$transect_flows = renderPlot({
    ggplot(inflowdata(), aes(x = datetime, y = flow)) + 
    geom_line(color = "#377eb8") + ggtitle("Russian River Flow") +
    transect.lines() + ylab("inflow (m3/s)") + x.scale()
  })
  
  # plot the tides
  tidedata = reactive(filter(tides, datetime >= intervalstart(),
    datetime <= intervalend()))
  output$transect_tides = renderPlot({
    ggplot(tidedata(), aes(x = datetime, y = height)) + 
    geom_line(color = "#377eb8") + ggtitle("Point Reyes Tide Height") +
    transect.lines() + ylab("tide height (MLLW, m)") + x.scale() 
  })    
  
  # plot the water level
  wlldata = reactive(filter(wll, mtime >= intervalstart(),
    mtime <= intervalend(), site == "jenner"))
  output$transect_wll = renderPlot({
    ggplot(wlldata(), aes(x = mtime, y = depth)) + 
    geom_line(color = "#377eb8") + ggtitle("Water Depth at Jenner") +
    transect.lines() + ylab("water depth (m)") + x.scale()
  })    

########## main panel ##########

  # get the grid for selected transect
  transectdate = reactive(strftime(ctdmeta[input$transect_date, "start"], 
    "%Y-%m-%d", tz = "US/Pacific"))
  transectid = reactive(ctdmeta[input$transect_date, "id"])
  griddata = reactive(mutate_(filter(reactivehab$grid, date == transectdate(), 
    id == transectid()), habitat = input$habitat_type))

  # plot settings
  plot.settings = list(
    xlim(min(habgrids$dist), max(habgrids$dist)),
    ylim(min(habgrids$elev), max(habgrids$elev)),
    ylab("elevation above NAVD29 (m)"),
    xlab("distance from river mouth (m)")
  )
  overall.colors = c(
    "optimal" = "#1f78b4",
    "growth limited" = "#a6cee3",
    "impaired" = "#33a02c",
    "severely impaired" = "#6a3d9a",
    "energy demanding" = "#fb9a99",
    "growth limited, impaired" = "#fdbf6f",
    "growth limited, severely impaired" = "#b2df8a",
    "growth limited, energy demanding" = "#cab2d6",
    "impaired, energy demanding" = "#ffff99",
    "severely impaired, energy demanding" = "#e31a1c",
    "growth limited, impaired, energy demanding" = "#ff7f00",
    "growth limited, severely impaired, energy demanding" = "#b15928",   
    "unsuitable" = "#000000"
  )
  ta.colors = setNames(brewer.pal(4, "RdYlGn"), c("unsuitable", 
    "negative/no growth", "positive growth", "optimal growth"))
  sa.colors = setNames(c("#7b3294", "#c2a5cf", "#92c5de", "#0571b0"), 
    c("marine", "brackish", "isotonic", "freshwater"))
  oa.colors = setNames(brewer.pal(4, "BrBG"), c("unsuitable", 
    "severe impairment", "some impairment", "minimal impairment"))
  depth.colors = setNames(brewer.pal(5, "BuPu"), c("littoral",
    "surface limnetic", "epibenthic", "subsurface limnetic", "profundal")
)
  habitat.colors = reactive({
    switch(input$habitat_type, 
      "habitat.fwa" = scale_fill_manual("Overall habitat quality", 
        values = overall.colors, drop = FALSE),
      "habitat.swa" = scale_fill_manual("Overall habitat quality", 
        values = overall.colors, drop = FALSE),
      "ta.qual" = scale_fill_manual("Temperature quality", 
        values = ta.colors, drop = FALSE),
      "sa.qual" = scale_fill_manual("Salinity quality", 
        values = sa.colors, drop = FALSE),
      "oa.qual" = scale_fill_manual("Dissolved oxygen quality", 
        values = oa.colors, drop = FALSE),
      "ta" = scale_fill_distiller("Temperature\n", type = "div", 
        palette = "RdYlBu", guide = "colourbar"),
      "sa" = scale_fill_distiller("Salinity\n", type = "div", 
        palette = "PRGn", guide = "colourbar"),
      "oa" = scale_fill_distiller("Dissolved Oxygen\n", type = "div", 
        palette = "BrBG", direction = 1, guide = "colourbar")
    )
  })
  
  # plot the main grid
  habitat.plot = reactive(ggplot(griddata(), aes(x = dist, y = elev, 
      fill = habitat)) + geom_raster() + 
      plot.settings + habitat.colors() + 
      theme(legend.position = "none")  
  )
  output$grid_plot = renderPlot({
    habitat.plot()
  })
  
  # plot the categories
  cat.settings = list(
    scale_y_continuous("total volume (m3)\n", labels = comma),
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(), 
      axis.ticks.x = element_blank())
  )

  catdata = reactive(summarize(group_by(griddata(), date, habitat), 
    volume = sum(volume.total)))

  cat.plot = reactive(ggplot(arrange(catdata(), -as.numeric(habitat)), 
    aes(x = date, y = volume, fill = habitat)) + 
    geom_bar(stat = "identity", position = "stack") + 
    habitat.colors() + cat.settings + theme(legend.position = "left")
  )
  output$category_bar = renderPlot({
    cat.plot()
  })

  # overall volume
  depthvoldata = reactive({
    depthvoldata = gather(summarize(group_by(griddata(), date, habitat), 
      volume.littoral = sum(volume.littoral),
      volume.limnetic = sum(volume.limnetic), 
      volume.epibenthic = sum(volume.epibenthic),
      volume.sublimnetic = sum(volume.sublimnetic),
      volume.profundal = sum(volume.profundal)
      ), depth.cat, volume, -habitat, -date)
  depthvoldata["depth.cat"] = factor(depthvoldata$depth.cat, 
      levels = c("volume.littoral", "volume.limnetic", "volume.epibenthic",
        "volume.sublimnetic", "volume.profundal"), 
      labels = c("littoral", "surface limnetic", "epibenthic", 
        "subsurface limnetic", "profundal"))
  depthvoldata
  })
  vol.depth = reactive(ggplot(depthvoldata(), aes(x = date, y = volume, 
    fill = depth.cat)) + geom_bar(stat = "identity") + 
    scale_fill_manual("depth category", values = depth.colors, 
      drop = FALSE) + cat.settings +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(), 
      axis.ticks.y = element_blank())
      
  )
  output$depth_vol = renderPlot({
    vol.depth()
  })
  
  # plot by depth
  depthdata = reactive({
    depthdata = summarize(group_by(gather(griddata(), depth.zone, 
      volume, volume.littoral, volume.limnetic, volume.epibenthic, 
      volume.sublimnetic, volume.profundal), date, depth.zone, habitat), 
      volume = sum(volume))
    depthdata["depth.zone"] = factor(depthdata$depth.zone, 
      levels = c("volume.littoral", "volume.limnetic", "volume.epibenthic",
        "volume.sublimnetic", "volume.profundal"), 
      labels = c("littoral", "surface limnetic", "epibenthic", 
        "subsurface limnetic", "profundal"))
    depthdata
  })
      

  cat.depth = reactive(ggplot(arrange(depthdata(), -as.numeric(habitat)), 
    aes(x = date, y = volume, fill = habitat)) + 
    geom_bar(stat = "identity", position = "stack") + 
    habitat.colors() + cat.settings + theme(legend.position = "none") + 
    facet_wrap(~ depth.zone, nrow = 1) + theme(axis.title.y = element_blank())
  )
  
  output$depth_cat = renderPlot({
    cat.depth()
  })
    
  
########### Explore Periods ###################################################

########## side bar ###########

  output$period = renderUI({
    selectInput("period_date", "Select transects", size = 10, selected = c(1,2), 
      multiple = TRUE, selectize = FALSE, choices = setNames(seq(nrow(ctdmeta)), 
        paste(strftime(ctdmeta$start, "%Y-%m-%d"), 
          paste0(strftime(ctdmeta$start, "%H:%M"), "--",  
            strftime(ctdmeta$end, "%H:%M")), 
          paste0("(", ctdmeta$code,")")))
    )
  })
  
 # prep for flow/tide/wll plots
  periodrange = reactive(sort(input$period_date))
  periodstart = reactive(ctdmeta[periodrange()[1], "start"] - 3.5*86400)
  periodend = reactive(ctdmeta[rev(periodrange())[1], "start"] + 3.5*86400)
  periodinflow = reactive(filter(inflows, datetime >= periodstart(),
    datetime <= periodend(), gauge == "russian river"))
  periodtide = reactive(filter(tides, datetime >= periodstart(),
    datetime <= periodend()))
  periodwll = reactive(filter(wll, mtime >= periodstart(),
    mtime <= periodend(), site == "jenner"))
  period.x.scale = reactive(scale_x_datetime("", limits = c(periodstart(), 
    periodend())))
  
  region.setting = reactive({
    d = data.frame(date = as.numeric(ctdmeta[periodrange(), "start"]),
      availability = ifelse(ctdmeta[periodrange(), "numcasts"] < 12, 
        "partial", "complete"))
    list(
      geom_vline(data = d, aes(xintercept = date, linetype = availability)),
      scale_linetype_manual(values = c("complete" = "solid", 
        "partial" = "dashed"), guide = FALSE)
    )
  })
  
  # plot the flows
  output$period_flows = renderPlot({
    ggplot(periodinflow(), aes(x = datetime, y = flow)) + 
    geom_line(color = "#377eb8") + ggtitle("Russian River Flow") +
    ylab("inflow (m3/s)") + period.x.scale() + region.setting()
  })
  
  # plot the tides
  output$period_tides = renderPlot({
    ggplot(periodtide(), aes(x = datetime, y = height)) + 
    geom_line(color = "#377eb8") + ggtitle("Point Reyes Tide Height") +
    ylab("tide height (MLLW, m)") + period.x.scale() + region.setting() 
  })    
  
  # plot the water level
  output$period_wll = renderPlot({
    ggplot(periodwll(), aes(x = mtime, y = depth)) + 
    geom_line(color = "#377eb8") + ggtitle("Water Depth at Jenner") +
    ylab("water depth (m)") + period.x.scale() + region.setting()
  })
  
########## main panel ##########

  # get the grid for selected period
  perioddate = reactive(strftime(ctdmeta[periodrange(), "start"], 
    "%Y-%m-%d", tz = "US/Pacific"))
  periodtime = reactive(ctdmeta[periodrange(), "start"])
  periodid = reactive(ctdmeta[periodrange(), "id"])
  periodgrids = reactive(mutate_(filter(reactivehab$grid, date %in% as.Date(perioddate()), 
    id %in% periodid()), habitat = input$period_habitat_type))

    period.habitat.colors = reactive({
    switch(input$period_habitat_type, 
      "habitat.fwa" = overall.colors,
      "habitat.swa" = overall.colors,
      "ta.qual" = ta.colors,
      "sa.qual" = sa.colors,
      "oa.qual" = oa.colors
    )
  })
    
  # overall habitat plots
  overall.gathered = reactive({
    hablevels = levels(periodgrids()$habitat)
    overall = summarize(group_by(periodgrids(), date, id, 
      habitat, code, days.since.closure), 
      volume = sum(volume.total))
    ovlevels = as.character(unique(overall$habitat))
    overall.spread = spread_(overall, "habitat", "volume", fill = 0)
    overall.spread["total.volume"] = rowSums(overall.spread[ovlevels])
    overall.gathered = gather_(overall.spread, "habitat", "volume", 
      gather_cols = ovlevels)
    overall.gathered["habitat"] = factor(overall.gathered$habitat, 
      levels = hablevels)
    overall.gathered["volume.frac"] = overall.gathered$volume/
                                      overall.gathered$total.volume
    overall.gathered = left_join(overall.gathered, 
      unique(periodgrids()[c("date", "id", "wse")]), by = c("date", "id"))
    overall.gathered = left_join(overall.gathered,
      data.frame(date = as.Date(perioddate()), id = periodid(), 
        datetime = periodtime()), by = c("date", "id"))
    overall.gathered
  })
  output$period_overall = renderPlot({
    if(input$plot_type == "stacked area"){
      ggplot(arrange(overall.gathered(), -as.numeric(habitat)), 
        aes(x = datetime, y = volume, fill = habitat)) + 
        geom_area(position = "stack") + scale_x_datetime("") +
        scale_fill_manual("", values = period.habitat.colors(), drop = FALSE) +
        theme(legend.position = "left") +
        scale_y_continuous(name = "Volume (m3)", labels = comma) + 
        region.setting()
    } else{
      ggplot(arrange(overall.gathered(), -as.numeric(habitat)), 
        aes(x = factor(datetime), y = volume, fill = habitat)) + xlab("") +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual("", values = period.habitat.colors(), drop = FALSE) +
        theme(legend.position = "left") +
        scale_y_continuous(name = "Volume (m3)", labels = comma)    
    }
  })
  
  # depth plots
  alldepth = reactive({
    alldepth = summarize(group_by(gather(periodgrids(), depth.zone, 
      volume, volume.littoral, volume.limnetic, volume.epibenthic, 
      volume.sublimnetic, volume.profundal), date, id, 
      depth.zone, code, days.since.closure), 
      volume = sum(volume))
    alldepth["depth.zone"] = factor(alldepth$depth.zone, ordered = TRUE,
      levels = c("volume.littoral", "volume.limnetic", "volume.epibenthic",
        "volume.sublimnetic", "volume.profundal"), 
      labels = c("littoral", "surface limnetic", "epibenthic", 
        "subsurface limnetic", "profundal"))    
    alldepth = left_join(alldepth, 
      unique(periodgrids()[c("date", "id", "wse")]), by = c("date", "id"))
    alldepth = left_join(alldepth,
      data.frame(date = as.Date(perioddate()), id = periodid(), 
        datetime = periodtime()), by = c("date", "id"))
    alldepth
  })
  
  output$period_alldepth = renderPlot({
    if(input$plot_type == "stacked area"){
      ggplot(arrange(alldepth(), -as.numeric(depth.zone)), 
        aes(x = datetime, y = volume, fill = depth.zone)) + 
        geom_area(position = "stack") + scale_x_datetime("") +
        scale_fill_manual("", values = depth.colors, drop = FALSE) +
        theme(legend.position = "left") +
        scale_y_continuous(name = "Volume (m3)", labels = comma) + 
        region.setting()
    } else{
      ggplot(arrange(alldepth(), -as.numeric(depth.zone)), 
        aes(x = factor(datetime), y = volume, fill = depth.zone)) + xlab("") +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual("", values = depth.colors, drop = FALSE) +
        theme(legend.position = "left") +
        scale_y_continuous(name = "Volume (m3)", labels = comma)    
    }
  })
  
  # stratified habitat plots
  bydepth.gathered = reactive({
    hablevels = levels(periodgrids()$habitat)
    bydepth = summarize(group_by(gather(periodgrids(), depth.zone, 
      volume, volume.littoral, volume.limnetic, volume.epibenthic, 
      volume.sublimnetic, volume.profundal), date, id, 
      habitat, depth.zone, code, days.since.closure), 
      volume = sum(volume))
    ovlevels = as.character(unique(bydepth$habitat))
    bydepth.spread = spread_(bydepth, "habitat", "volume", fill = 0)
    bydepth.spread["total.volume"] = rowSums(bydepth.spread[ovlevels])
    bydepth.gathered = gather_(bydepth.spread, "habitat", "volume", 
      gather_cols = ovlevels)
    bydepth.gathered["habitat"] = factor(bydepth.gathered$habitat, 
      levels = hablevels)
    bydepth.gathered["volume.frac"] = bydepth.gathered$volume/
                                      bydepth.gathered$total.volume
    bydepth.gathered = left_join(bydepth.gathered, 
      unique(periodgrids()[c("date", "id", "wse")]), by = c("date", "id"))
    bydepth.gathered = left_join(bydepth.gathered,
      data.frame(date = as.Date(perioddate()), id = periodid(), 
        datetime = periodtime()), by = c("date", "id"))
    bydepth.gathered["depth.zone"] = factor(bydepth.gathered$depth.zone, 
      levels = c("volume.littoral", "volume.limnetic", "volume.epibenthic",
        "volume.sublimnetic", "volume.profundal"), 
      labels = c("littoral", "surface limnetic", "epibenthic", 
        "subsurface limnetic", "profundal"))
    bydepth.gathered
  })
  output$period_bydepth = renderPlot({
    if(input$plot_type == "stacked area"){
      ggplot(arrange(bydepth.gathered(), -as.numeric(habitat)), 
        aes(x = datetime, y = volume, fill = habitat)) + 
        geom_area(position = "stack") + scale_x_datetime("") +
        scale_fill_manual("", values = period.habitat.colors(), drop = FALSE) +
        theme(legend.position = "none") +
        scale_y_continuous(name="Volume (m3)", labels = comma) + 
        region.setting() + facet_wrap(~ depth.zone, ncol = 1, scales = "free_y")
    } else{
      ggplot(arrange(bydepth.gathered(), -as.numeric(habitat)), 
        aes(x = factor(datetime), y = volume, fill = habitat)) + xlab("") +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual("", values = period.habitat.colors(), drop = FALSE) +
        theme(legend.position = "none") +
        scale_y_continuous(name="Volume (m3)", labels = comma) + 
        facet_wrap(~ depth.zone, ncol = 1, scales = "free_y")
    }
  })

########## Perturbate Transect ###################################################  

########## main panel ##########
  output$perturb_date = renderUI({
    selectInput("perturb_date", "Select transect", size = 10, selected = 1, 
      selectize = FALSE, choices = setNames(seq(nrow(ctdmeta)), 
        paste(strftime(ctdmeta$start, "%Y-%m-%d"), 
          paste0(strftime(ctdmeta$start, "%H:%M"), "--",  
            strftime(ctdmeta$end, "%H:%M")), 
          paste0("(", ctdmeta$code,")")))
    )
  })

  # get the grid for selected transect
  perturbdate = reactive(strftime(ctdmeta[input$perturb_date, "start"], 
    "%Y-%m-%d", tz = "US/Pacific"))
  perturbid = reactive(ctdmeta[input$perturb_date, "id"])
  perturbdata = reactive({
    res = mutate_(filter(reactivehab$grid, date == perturbdate(), 
    id == perturbid()), habitat = input$perturb_var)
    updateSliderInput(session, "window_dist", 
      min = min(res$dist), max = max(res$dist), step = 100,
      value = c(min(res$dist), max(res$dist)))
    updateSliderInput(session, "window_elev", 
      min = min(res$elev), max = max(res$elev), step = 0.1,
      value = c(min(res$elev), max(res$elev)))
    res
    })
  
  perturb.habitat.colors = reactive({
    switch(input$perturb_var, 
      "ta" = scale_fill_distiller("Temperature", type = "div", 
        palette = "RdYlBu", guide = "colourbar"),
      "sa" = scale_fill_distiller("Salinity", type = "div", 
        palette = "PRGn", guide = "colourbar"),
      "oa" = scale_fill_distiller("Dissolved Oxygen", type = "div", 
        palette = "RdYlGn", guide = "colourbar", direction = -1)
    )
  })

  perturb.plot = reactive(ggplot(perturbdata(), aes(x = dist, y = elev, 
      fill = habitat)) + geom_raster() + 
      plot.settings + perturb.habitat.colors() 
  )
  
  perturb.window = reactive({
    d = data.frame(dist = rep(input$window_dist, each = 2),
      elev = c(input$window_elev, rev(input$window_elev)))
    geom_polygon(data = d, fill = NA, color = "black")
  })
  
  output$perturb_plot = renderPlot({
    perturb.plot() + perturb.window()
  })

  observeEvent(input$perturb_action, {
    perturbmask = ( reactivehab$grid$date == perturbdate() ) & 
      ( reactivehab$grid$id == perturbid() ) &
      ( reactivehab$grid$dist >= min(input$window_dist) ) & 
      ( reactivehab$grid$dist <= max(input$window_dist) ) &
      ( reactivehab$grid$elev >= min(input$window_elev) ) & 
      ( reactivehab$grid$elev <= max(input$window_elev) )
    reactivehab$grid[perturbmask, input$perturb_var] = reactivehab$grid[[input$perturb_var]][perturbmask] + input$perturb_val
    classfun = switch(input$perturb_var, 
      sa = habitatblueprint:::classify_sa, 
      ta = habitatblueprint:::classify_ta, 
      oa = habitatblueprint:::classify_oa)
    reactivehab$grid[perturbmask, paste0(input$perturb_var, ".qual")] = classfun(reactivehab$grid[[input$perturb_var]][perturbmask])
    reactivehab$grid[perturbmask, "habitat.fwa"] = habitatblueprint:::classify_freshwater(reactivehab$grid$ta.qual[perturbmask], 
      reactivehab$grid$sa.qual[perturbmask], reactivehab$grid$oa.qual[perturbmask])
    reactivehab$grid[perturbmask, "habitat.swa"] = habitatblueprint:::classify_saltwater(reactivehab$grid$ta.qual[perturbmask], 
      reactivehab$grid$sa.qual[perturbmask], reactivehab$grid$oa.qual[perturbmask])
  })

  observeEvent(input$perturb_reset, {
    reactivehab$grid = habgrids
  })

})
