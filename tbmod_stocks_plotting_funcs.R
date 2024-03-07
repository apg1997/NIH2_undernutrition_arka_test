# 1. Set-up
suppressPackageStartupMessages({
  model = new.env()
  library(here)
  library(fst)
  library(tools)
  library(data.table)
  library(dplyr) 
  library(tidyverse) 
  library(testthat) 
  library(reshape2) 
  library(assertthat)
  library(stringi)
  library(Matrix)
  library(ggplot2)
  library(RColorBrewer)
  library(cowplot)
  options(scipen = 999)
})


filter.stocks <- function(
    filter.table = NULL){
  
  # Ensure an object has been passed to filter.table:
  assert_that(!(is.null(filter.table)), msg = "Provide a valid filter table")
  # List of colnames to include in the filter.table:
  valid_colnames <- c("filter.number", "stocks.file.path", "country.code", 
                      "tb.filter", "hiv.filter", "ses.filter", "risk.filter", "vx.filter", "age.from.filter", "age.thru.filter", "denominator.row", "multiplier"
                      #"den.tb.filter", "den.hiv.filter", "den.ses.filter", "den.risk.filter", "den.vx.filter", "den.age.from.filter", "den.age.thru.filter"
                      )
  
  # Check these columns are present within the filter.table:
  error_msg <- paste0("Ensure colnames in the filter.table are all in: ", valid_colnames)
  assert_that(isTRUE(unique(colnames(filter.table) %in% valid_colnames)), msg = error_msg)
  # Check that the filter.table has > 0 rows:
  assert_that(nrow(filter.table) > 0, msg = "The filter.table does not have any rows")
  
  dt_list <- list()
  dt_list2 <- list()
  pop_list <- list()
  
  for(i in 1:nrow(filter.table)){
   #i <- 1
   #cat("Numerator filter ", i,"\n")
   #cat("\n")
   focal_row <- filter.table[i,]
   #print(focal_row)
   focal_path <- focal_row$stocks.file.path[[1]]
   assert_that(file_ext(focal_path) %in% c("fst", "txt"), msg = "Ensure that the filter.table is pointing at an fst or txt file")
   if(file_ext(focal_path) == "txt"){
     focal_dt <- as.data.table(file = fread(focal_path))
   }else if(file_ext(focal_path) == "fst"){
     focal_dt <- as.data.table(read_fst(path = focal_path))
   }else{
     stop("The file path provided pointed at a file that was neither fst or txt")
   }
   
   
   # Check that all the values within the filter.table are contained within the focal_df
   # Unique tb states:
   tb_uniq <- paste(unlist(focal_row$tb.filter), collapse = ",")
   tb_uniq <- gsub(" ", "", tb_uniq)
   tb_uniq <- unique(strsplit(tb_uniq, split = ",")[[1]])
   #print(tb_uniq)
   assert_that(isTRUE(unique(tb_uniq %in% unique(focal_dt$TB))), msg = paste0("Not all TB stages provided in the filter are present within the TB column of stocks output; filter row: ", i))
   
   # Unique hiv states:
   hiv_uniq <- paste(unlist(focal_row$hiv.filter), collapse = ",")
   hiv_uniq <- gsub(" ", "", hiv_uniq)
   hiv_uniq <- unique(strsplit(hiv_uniq, split = ",")[[1]])
   #print(hiv_uniq)
   assert_that(isTRUE(unique(hiv_uniq %in% unique(focal_dt$HIV))), msg = paste0("Not all HIV stages provided in the filter are present within the HIV column of stocks output; filter row: ", i))
   
   # Unique ses states:
   ses_uniq <- paste(unlist(focal_row$ses.filter), collapse = ",")#
   ses_uniq <- gsub(" ", "", ses_uniq)
   ses_uniq <- unique(strsplit(ses_uniq, split = ",")[[1]])
   #print(ses_uniq)
   assert_that(isTRUE(unique(ses_uniq %in% unique(focal_dt$SES))), msg = paste0("Not all SES stages provided in the filter are present within the SES column of stocks output; filter row: ", i))
     
   # Unique risk states:
   risk_uniq <- paste(unlist(focal_row$risk.filter), collapse = ",")#
   risk_uniq <- gsub(" ", "", risk_uniq)
   risk_uniq <- unique(strsplit(risk_uniq, split = ",")[[1]])
   #print(risk_uniq)
   assert_that(isTRUE(unique(risk_uniq %in% unique(focal_dt$RISK))), msg = paste0("Not all RISK stages provided in the filter are present within the RISK column of stocks output; filter row: ", i))
   
   # Unique vx states:
   vx_uniq <- paste(unlist(focal_row$vx.filter), collapse = ",")
   vx_uniq <- gsub(" ", "", vx_uniq)
   vx_uniq <- unique(strsplit(vx_uniq, split = ",")[[1]])
   #print(vx_uniq)
   assert_that(isTRUE(unique(vx_uniq %in% unique(focal_dt$VXa))), msg = paste0("Not all VXa stages provided in the filter are present within the VXa column of stocks output; filter row: ", i))
   
   # Unique lower ages:
   age_lower_uniq <- paste(unlist(focal_row$age.from.filter), collapse = ",")
   age_lower_uniq <- gsub(" ", "", age_lower_uniq)
   age_lower_uniq <- as.numeric(unique(strsplit(age_lower_uniq, split = ",")[[1]]))
   #print(age_lower_uniq)
   assert_that(isTRUE(unique(age_lower_uniq %in% unique(focal_dt$age_from))), msg = paste0("Not all lower ages provided in the filter are present within the age_from column of stocks output; filter row: ", i))
   
   # Unique upper ages:
   age_upper_uniq <- paste(unlist(focal_row$age.thru.filter), collapse = ",")
   age_upper_uniq <- gsub(" ", "", age_upper_uniq)
   age_upper_uniq <- as.numeric(unique(strsplit(age_upper_uniq, split = ",")[[1]]))
   #print(age_upper_uniq)
   assert_that(isTRUE(unique(age_upper_uniq %in% unique(focal_dt$age_thru))), msg = paste0("Not all upper ages provided in the filter are present within the age_thru column of stocks output; filter row: ", i))
   
   # Filter stocks output:
   focal_dt <- focal_dt[focal_dt$TB %in% tb_uniq,]
   focal_dt <- focal_dt[focal_dt$HIV %in% hiv_uniq,]
   focal_dt <- focal_dt[focal_dt$SES %in% ses_uniq,]
   focal_dt <- focal_dt[focal_dt$RISK %in% risk_uniq,]
   focal_dt <- focal_dt[focal_dt$VXa %in% vx_uniq,]
   focal_dt <- focal_dt[focal_dt$age_from %in% age_lower_uniq,]
   focal_dt <- focal_dt[focal_dt$age_thru %in% age_upper_uniq,]
   

   # Step to ensure that, if multiple age boundaries are provided, the 0-99 group is removed, as this is a re-count:
   if(length(age_lower_uniq) == 1 & length(age_upper_uniq) == 1 & (0 %in% age_lower_uniq) & (99 %in% age_upper_uniq)){
     focal_dt <- focal_dt
   }else {
     focal_dt <- focal_dt[!(focal_dt$age_from == 0 & focal_dt$age_thru == 99),]
   }
   
   dt_list[[i]] <- focal_dt
   
   focal_breakdown <- focal_dt %>%
     group_by(country, year) %>%
     summarize(pop_total = sum(value))
   
   pop_list[[i]] <- focal_breakdown
   
   cat("\n")

  }
  
  # Loop going through each filter row, and dividing the corresponding dt_list entry by the appropriate populations from pop_list:
  for(i in 1:nrow(filter.table)){
   
   #cat("Filter row: ", i,"\n")
   
   #i <- 1
   focal_row <- filter.table[i,]
   focal_denominator <- focal_row$denominator.row
   focal_multiplier <- focal_row$multiplier
   
   # Assertion statements to ensure that the focal denominator is within the number of rows present within the filter table
   # Also ensure that the focal denominator row is either NA or a positive integer
    
   focal_dt <- dt_list[[i]]
   focal_pop <- pop_list[[focal_denominator]]
   #print(focal_pop)
   
   if(!(is.na(focal_denominator))){
     
     # Assertion statement to ensure that the number of unique time points in the numerator and denominator are the same:
     assert_that(isTRUE(unique(unique(focal_dt$year) %in% unique(focal_pop$year))), msg = "The number of time points in the numerator and denominator are different")
     
     # Adding the total population of the denominator at each time point to the dt as a new column:
     focal_dt$den.annual.pop <- focal_pop$pop_total[match(focal_dt$year, focal_pop$year)]
     
     # Assertion statement ensuring that an appropriate multiplier has been provided, i.e., an integer from 1 to number of rows in filter.table 
     #assert_that(isTRUE(unique()), msg = "")
     
     #focal_dt$new.value <- ((focal_dt$value/focal_dt$den.annual.pop) * focal_multiplier)
     focal_dt$value <- ((focal_dt$value/focal_dt$den.annual.pop) * focal_multiplier)
     focal_dt$multi <- focal_multiplier
     
     focal_dt <- focal_dt[, year := floor(year)]
     dt_list2[[i]] <- focal_dt
     
   }else{
     focal_dt <- focal_dt[, year := floor(year)]
     dt_list2[[i]] <- focal_dt
   }
     
  }

  return(dt_list2)
   
}



dimension_names <- c("TB", "HIV", "RISK", "SES", "VXa", "age_from", "age_thru")

plot.stocks <- function(stocks.dt.list = NULL
                        ){
   
  assert_that(isTRUE(!(is.null(stocks.dt.list))), msg = "A list of data tables is required")
  # assertion statements on the structure of the stocks.dt
  
  # assertion statements ensuring that that 'type' has a length of 1 and is either 'proportion' or 'count'
  #assert_that(isTRUE(length(type) == 1), msg = "Provide a single 'type' item")
  #assert_that(type %in% c("count", "proportion"), msg = "Ensure that type is either 'count' or 'proportion'")
  
  # Create a plot list
  plot_list <- list()
  
  for(i in 1:length(stocks.dt.list)){
    
    #i <- 1
    focal_dt <- stocks.dt.list[[i]]
    #focal_dt_subset <- focal_dt[, .SD, .SDcols = dimension_names]
    #unique_combinations <- (unique(focal_dt_subset))
    
    per_year <- focal_dt %>%
      group_by(year) %>%
      summarise(total = sum(value))
    
    #filter_summary <- paste0(
    #  "Country code:  ", unique(focal_dt[,country]), "\n",
    #  "TB strata:  ", paste(unlist(unique(focal_dt[, .(TB)])), collapse = ","), "\n",
    #  "HIV strata:  ", paste(unlist(unique(focal_dt[, .(HIV)])), collapse = ","), "\n",
    #  "VXa strata:  ", paste(unlist(unique(focal_dt[, .(VXa)])), collapse = ","), "\n",
    #  "SES strata:  ", paste(unlist(unique(focal_dt[, .(SES)])), collapse = ","), "\n",
    #  "RISK strata:  ", paste(unlist(unique(focal_dt[, .(RISK)])), collapse = ","), "\n",
    #  "Ages from:  ", paste(unlist(unique(focal_dt[, .(age_from)])), collapse = ","), "\n",
    #  "Ages thru:  ", paste(unlist(unique(focal_dt[, .(age_thru)])), collapse = ","), "\n"
    #  )
    #print(filter_summary)
    #print(focal_dt)
    
    if("den.annual.pop" %in% colnames(focal_dt)){
      if(unique(focal_dt$multi) == 1){
        y_lab <- paste0("Rate")
        y_max_lim <- 1
        y_seq <- seq(from = 0, to = 1, by = 0.1)
      }else{
        y_lab <- paste0("Rate (per ", unique(focal_dt$multi), ")")
        y_max_lim <- NA
      }
    }else{
      y_lab <- "Count"
      y_max_lim <- NA
    }
    
    
    focal_plot <- ggplot() +
      theme_bw() +
      xlab("Year") +
      ylab(y_lab) +
      ylim(0, y_max_lim) +
      #annotate("text", label = filter_summary, x = min(per_year$year), y = 0, size = 4, hjust = 0, vjust = 0) +
      geom_line(data = per_year, aes(x = year, y = total), size = 1) #+
      
    
    #plot(focal_plot)
    
    plot_list[[i]] <- focal_plot
    
  }
  
  return(plot_list)
}


