shinyServer(function(input, output){
  data(ctdmeta)
  data(habgrids)
  data(inflows)
  data(tides)
  data(wll)

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
  inflowdata = reactive(filter(inflows, datetime >= intervalstart(),
    datetime <= intervalend(), gauge == "russian river"))
  tidedata = reactive(filter(tides, datetime >= intervalstart(),
    datetime <= intervalend()))
  wlldata = reactive(filter(wll, mtime >= intervalstart(),
    mtime <= intervalend(), site == "jenner"))
  x.scale = reactive(scale_x_datetime("", limits = c(intervalstart(), 
    intervalend())))
  # plot the flows
  output$transect_flows = renderPlot({
    ggplot(inflowdata(), aes(x = datetime, y = flow)) + 
    geom_line(color = "#377eb8") + ggtitle("Russian River Flow") +
    geom_vline(xintercept = as.numeric(ctdmeta[input$transect_date, "start"]), 
      linetype = "dashed") + ylab("inflow (m3/s)") + x.scale()
  })
  
  # plot the tides
  output$transect_tides = renderPlot({
    ggplot(tidedata(), aes(x = datetime, y = height)) + 
    geom_line(color = "#377eb8") + ggtitle("Point Reyes Tide Height") +
    geom_vline(xintercept = as.numeric(ctdmeta[input$transect_date, "start"]), 
      linetype = "dashed") + ylab("tide height (MLLW, m)") + x.scale() 
  })    
  
  # plot the water level
  output$transect_wll = renderPlot({
    ggplot(wlldata(), aes(x = mtime, y = depth)) + 
    geom_line(color = "#377eb8") + ggtitle("Water Depth at Jenner") +
    geom_vline(xintercept = as.numeric(ctdmeta[input$transect_date, "start"]), 
      linetype = "dashed") + ylab("water depth (m)") + x.scale()
  })    

########## main panel ##########

  # get the grid for selected transect
  transectdate = reactive(strftime(ctdmeta[input$transect_date, "start"], 
    "%Y-%m-%d"))
  transectid = reactive(ctdmeta[input$transect_date, "id"])
  griddata = reactive(filter(habgrids, date == transectdate(), 
    id == transectid()))

  # plot settings
  plot.settings = list(
    xlim(min(habgrids$dist), max(habgrids$dist)),
    ylim(min(habgrids$elev), max(habgrids$elev)),
    ylab("elevation above NAVD29 (m)"),
    xlab("distance from river mouth (m)")
  )
  habitat.colors = c(
    "optimal" = "#1f78b4",
    "sub-optimal (T)" = "#a6cee3",
    "sub-optimal (DO)" = "#33a02c",
    "sub-optimal (S)" = "#6a3d9a",
    "sub-optimal (T, DO)" = "#fb9a99",
    "sub-optimal (T, S)" = "#fdbf6f",
    "sub-optimal (DO, S)" = "#b2df8a",
    "sub-optimal (T, DO, S)" = "#cab2d6",
    "stressful (T)" = "#ffff99",
    "stressful sub-optimal (T, DO)" = "#e31a1c",
    "stressful sub-optimal (T, S)" = "#ff7f00",
    "stressful sub-optimal (T, DO, S)" = "#b15928",   
    "unsuitable" = "#000000"
  )
  ta.colors = setNames(brewer.pal(4, "RdYlGn"), c("unsuitable", 
    "stressful", "suitable", "optimal"))
  sa.colors = setNames(brewer.pal(4, "RdYlGn"), c("ion.exporting", 
    "induced ion exporting", "ion neutral", "ion importing"))
  oa.colors = setNames(brewer.pal(4, "RdYlGn"), c("unsuitable", "limited", 
    "suitable", "no impairment"))
  depth.colors = setNames(brewer.pal(4, "BuPu"), c("littoral", 
    "surface limnetic", "subsurface limnetic", "profundal") 
)

  # plot the main grid
  habitat.plot = reactive(ggplot(griddata(), aes(x = dist, y = elev, 
      fill = habitat)) + geom_raster() + 
      plot.settings + scale_fill_manual("", values = habitat.colors) + 
      theme(legend.position = "none")  
  )
  output$grid_plot = renderPlot({
    habitat.plot()
  })
  
  # plot the categories
  catdata = reactive(summarize(group_by(griddata(), habitat), 
    volume = sum(volume)))
  cat.settings = list(
    scale_y_continuous("total volume (m3)", labels = comma),
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(), 
      axis.ticks.x = element_blank())
  )
  cat.plot = reactive(ggplot(catdata(), aes(x = habitat, y = volume, 
    fill = habitat)) + geom_bar(stat = "identity") + 
    scale_fill_manual("overall habitat quality", values = habitat.colors, 
      drop = FALSE) + cat.settings + theme(legend.position = "left")
  )
  output$category_bar = renderPlot({
    cat.plot()
  })

  # plot by depth
  depthdata = reactive(summarize(group_by(griddata(), depth.zone, habitat), 
    volume = sum(volume)))
#  depth.plot = reactive(ggplot(griddata(), aes(x = dist, y = elev, 
#      fill = depth.qual)) + geom_raster() + 
#      plot.settings + scale_fill_manual("", values = depth.colors) + 
#      theme(legend.position = "bottom")  
#  )
#  output$depth_plot = renderPlot({
#    depth.plot()
#  })

  cat.depth = reactive(ggplot(depthdata(), aes(x = habitat, y = volume, 
    fill = habitat)) + geom_bar(stat = "identity") + 
    scale_fill_manual("overall habitat quality", values = habitat.colors, 
      drop = FALSE) + cat.settings + theme(legend.position = "none") + 
    facet_wrap(~ depth.zone) + theme(axis.title.y = element_blank())
  )
  
  output$depth_cat = renderPlot({
    cat.depth()
  })
  
  # overall volume
#  depthvoldata = reactive(gather(summarize(group_by(griddata(), habitat), 
#    volume.total = sum(volume.total), volume.littoral = sum(volume.littoral),
#    volume.surface = sum(volume.surface), 
#    volume.subsurface = sum(volume.subsurface),
#    volume.profundal = sum(volume.profundal)
#  )), depth.cat, volume, -habitat)
#  vol.depth = reactive(ggplot(depthvoldata(), aes(x = depth.cat, y = volume, 
#    fill = depth.cat)) + geom_bar(stat = "identity", position = "stack") + 
#    scale_fill_manual("depth category", values = depth.colors, 
#      drop = FALSE) + cat.settings + theme(legend.position = "none") 
#  )
#  output$depth_vol = renderPlot({
#    vol.depth()
#  })
#  hab.depth.cat = reactive(ggplot(depthvoldata(), aes(x = habitat, y = volume, 
#    fill = habitat)) + geom_bar(stat = "identity", position = "stack") + 
#    scale_fill_manual("depth category", values = depth.colors, 
#      drop = FALSE) + cat.settings + theme(legend.position = "none") + 
#    facet_wrap(~ depth.cat, nrow = 1)
#  )
#  output$depth_cat_hab = renderPlot({
#    hab.depth.cat()
#  })
  
  
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
    geom_vline(xintercept = as.numeric(ctdmeta[periodrange(), "start"]), 
      linetype = "dashed", color = "black")
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
    "%Y-%m-%d"))
  periodtime = reactive(ctdmeta[periodrange(), "start"])
  periodid = reactive(ctdmeta[periodrange(), "id"])
  periodgrids = reactive(filter(habgrids, date %in% as.Date(perioddate()), 
    id %in% periodid()))
  
  # overall habitat plots
  overall.gathered = reactive({
    hablevels = levels(periodgrids()$habitat)
    overall = summarize(group_by(periodgrids(), date, id, 
      habitat, code, days.since.closure), 
      volume = sum(volume))
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
        scale_fill_manual("", values = habitat.colors) +
        theme(legend.position = "none") +
        scale_y_continuous(name="Volume (m3)", labels = comma) + 
        region.setting()
    } else{
      ggplot(arrange(overall.gathered(), -as.numeric(habitat)), 
        aes(x = factor(datetime), y = volume, fill = habitat)) + xlab("") +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual("", values = habitat.colors, drop = FALSE) +
        theme(legend.position = "none") +
        scale_y_continuous(name="Volume (m3)", labels = comma)    
    }
  })
  
  # stratified habitat plots
  bydepth.gathered = reactive({
    hablevels = levels(periodgrids()$habitat)
    bydepth = summarize(group_by(periodgrids(), date, id, 
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
    bydepth.gathered
  })
  output$period_bydepth = renderPlot({
    if(input$plot_type == "stacked area"){
      ggplot(arrange(bydepth.gathered(), -as.numeric(habitat)), 
        aes(x = datetime, y = volume, fill = habitat)) + 
        geom_area(position = "stack") + scale_x_datetime("") +
        scale_fill_manual("", values = habitat.colors, drop = FALSE) +
        theme(legend.position = "left") +
        scale_y_continuous(name="Volume (m3)", labels = comma) + 
        region.setting() + facet_wrap(~ depth.zone, ncol = 1)
    } else{
      ggplot(arrange(bydepth.gathered(), -as.numeric(habitat)), 
        aes(x = factor(datetime), y = volume, fill = habitat)) + xlab("") +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual("", values = habitat.colors, drop = FALSE) +
        theme(legend.position = "left") +
        scale_y_continuous(name="Volume (m3)", labels = comma) + 
        facet_wrap(~ depth.zone, ncol = 1)
    }
  })
})
