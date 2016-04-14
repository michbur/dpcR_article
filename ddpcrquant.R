library(modeest)

#ddpcrquant.read("./amplitude_testfiles")

path <- "./amplitude_testfiles/"
threshold.int = 0.9995
reps = 10
blocks = 150
threshold.manual = FALSE
assay = 1
saveplots = FALSE
outputfile = FALSE
ci.conc.method ="wilson" 
ci.conc= 0.95
vol.mix = 20
vol.temp = 4



all_files <- list.files(path)
csv_files <- all_files[grepl("csv$", all_files)]
raw_data_files <- csv_files[grepl("Amplitude", all_files)]

summary_file <- read.csv(file = paste0(path, "/", csv_files[!grepl("Amplitude", all_files)]), 
                         header = TRUE, row.names  = NULL)
summary_file[, c("Well","Sample","TypeAssay","Assay")]

##possibly redundant
head_filtered <- summary_file[, c("Well","Sample","TypeAssay","Assay")]


#possibly redundant
list_assays <- lapply(unique(summary_file[["Assay"]]), function(single_assay)
  summary_file[summary_file[["Assay"]] == single_assay, c("Well","Sample","TypeAssay","Assay")]
)

#possibly redundant
assays <- unique(head_filtered[["Assay"]])


set.seed(1553)
split <- blocks #number of blocks
cutoff.quantile = threshold.int

tmp_ntc <- raw_data_files

lapply(unique(summary_file[["Assay"]]), function(single_assay) {
  dat <- summary_file[summary_file[["Assay"]] == single_assay, c("Well","Sample","TypeAssay","Assay")]
  
  channel <- as.numeric(substr(as.character(unique(dat[["TypeAssay"]])), 3, 3))
  
  ntc_wells <- as.character(dat[grepl("NTC", dat[["Sample"]], ignore.case = TRUE), "Well"])
  ntc_processed <- lapply(raw_data_files[sapply(ntc_wells, function(single_well) grep(single_well, raw_data_files))],
                            function(single_file) {
                              fluo_dat <- read.csv(file = paste0(path, "/", single_file), header = TRUE, row.names  = NULL)[, channel]
                              corr.factor <- hsm(fluo_dat)
                              list(fluo_dat = fluo_dat - corr.factor,
                                   corr.factor = corr.factor)
                            })
  
  ntc_file <- unlist(lapply(ntc_processed, function(i) i[["fluo_dat"]]))
  corr.factor <- unlist(lapply(ntc_processed, function(i) i[["corr.factor"]]))
  quantgev <- sapply(1L:reps, function(dummy_variable) {
    ### block creation
    random_ntc_file <- sample(ntc_file)
    
    ### determine maxima of subsamples
    signal.maxima <- sapply(base::split(random_ntc_file, ceiling(seq_along(ntc_file)/(length(ntc_file)/split))), 
                            function(single_split)
                              max(single_split))
    
    ## fit the GEV model using ML
    droplet.fit <- try(fgev(signal.maxima), silent=TRUE)
    
    quantgev_value <- try(qgev(cutoff.quantile, droplet.fit[["estimate"]][1],
                               droplet.fit[["estimate"]][2], droplet.fit[["estimate"]][3]), silent = TRUE)
    unname(ifelse(quantgev_value > max(random_ntc_file) + 3000, NA, quantgev_value))
  })
  
  threshold <- if(threshold.manual == FALSE) {
    #calculate final threshold
    mean(quantgev, na.rm = TRUE)
  } else { 
    threshold.manual
  }
  
  lapply()

  sample_wells <- sapply(as.character(dat[["Well"]]), function(single_well) grep(single_well, raw_data_files))
  lapply(raw_data_files[sample_wells],
                function(single_file) {
                  fluo_dat <- read.csv(file = paste0(path, "/", single_file), header = TRUE, row.names  = NULL)[, channel]
                  window <- subset(fluo_dat, 
                                   fluo_dat < threshold + corr_factor) 
                  window.mode <- hsm(window)
                  if(is.na(window.mode)){window.mode = 0}
                })
  
  
  browser()
})



