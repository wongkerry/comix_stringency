bs_group <- function(dt, sims, prop = 1.0, samp_type = "adult", 
                     area_ = "All") {
  
  # subset by region --------------------------------------------------------
  areaname <- area_
  if(area_ == "All"){
    area_ <- c("at","dk","es","fr","it","pl","pt")
  }

  dt <- dt[area %in% area_]
  bs_list <- list()
  for(i in 1:sims){
    pids <- unique(dt$part_id)
    nsamp <- length(pids)*prop
    df_samp <- data.table(part_id = sample(pids, replace = TRUE, size = nsamp))
    samp1 <- merge(df_samp, dt, by = "part_id")
    bs_list[[i]] <- samp1[, .(
      N = .N,
      area = areaname,
      iteration = i, 
      panel = panel, 
      # Home = weighted.mean(n_cnt_home,  w = dayweight),
      # `Work/Educ` = weighted.mean(n_cnt_workschool,  w = dayweight),
      # Other = weighted.mean(n_cnt_other,  w = dayweight),
      # Physical = weighted.mean(n_cnt_phys,  w = dayweight),
      # Inside = weighted.mean(n_cnt_inside,  w = dayweight),
      # Outside = weighted.mean(n_cnt_outside,  w = dayweight),
      # `Other house` = weighted.mean(n_cnt_other_house,  w = dayweight),
      # `Supermarket` = weighted.mean(n_cnt_supermarket,  w = dayweight),
      # `Bar restaurant` = weighted.mean(n_cnt_bar_rest,  w = dayweight),
      weighted = weighted.mean(n_cnt, w = dayweight * genderageweight, na.rm = TRUE),
      unweighted = weighted.mean(n_cnt, w = dayweight, na.rm = TRUE)
    ),
    by = .(start_date, mid_date, end_date, survey_round)
    ]
  }
  rbindlist(bs_list)
}  
