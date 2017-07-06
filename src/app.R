require("shiny")
require("parallel")

output$ui.action <- renderUI({
  if (is.null(input$file1)) return()
  actionButton("run", "Run ColocalizeR!")
})


observeEvent(input$run, {
  #remove leftover .zip(s) and other files first
  file.remove(list.files(pattern = ".zip$|.lif$|.pdf$|.txt$"))
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)

  
  #ColocalizeR starts here
  #set up parallel computing for image reading optimization
  total_cores <- detectCores() - 1
  cl <- makeCluster(total_cores)
  
  #set bools to false so they don't break the script
  oif_bool <- FALSE
  lif_bool <- FALSE
  lsm_bool <- FALSE
  tif_bool <- FALSE
  #unzipping only required for .oif files (also zipped .lif files), not zipped .lif files should be handled accordingly
  input_dir <- file_path_sans_ext(inFile$name)
  if (grepl(pattern = ".lif$", inFile$name) == TRUE) {
    withProgress(message = "Reading images", {
      images_list <- read.image(inFile$datapath)
      lif_meta <- list()
      for (i in seq_along(images_list)) {
        lif_meta[[i]] <- seriesMetadata(images_list[[i]])
      }
    })
    lif_bool <- TRUE
  } else if (grepl(pattern = ".zip$", inFile$name) == TRUE) {
    input_dir <- file_path_sans_ext(inFile$name)
    unzip(inFile$datapath)
    input_files <- paste(input_dir, "/", dir(input_dir, pattern = ".oif$|.lif$|.lsm$|.tif$"), sep = "")
    withProgress(message = "Reading images", {
      if (sum(grepl(".oif$", input_files)) / length(input_files) == 1) {
        images_list <- parLapply(cl, input_files, read.image)
        oif_meta <- list()
        for (i in seq_along(images_list)) {
          oif_meta[[i]] <- globalMetadata(images_list[[i]])
        }
        oif_bool <- TRUE
        showNotification("Import complete, .oif files detected. Starting processing...", type = "message")
      } else if (sum(grepl(".lif$", input_files)) / length(input_files) == 1) {
        images_list <- read.image(input_files)
        lif_meta <- list()
        for (i in seq_along(images_list)) {
          lif_meta[[i]] <- seriesMetadata(images_list[[i]])
        }
        lif_bool <- TRUE
        showNotification("Import complete, .lif files detected. Starting processing...", type = "message")
      } else if (sum(grepl(".lsm$", input_files)) / length(input_files) == 1) {
        images_list <- parLapply(cl, input_files, read.image)
        lsm_meta <- list()
        for (i in seq_along(images_list)) {
          lsm_meta[[i]] <- seriesMetadata(images_list[[i]])
        }
        lsm_bool <- TRUE
        showNotification("Import complete, .lsm files detected. Starting processing...", type = "message")
      } else if (sum(grepl(".tif$", input_files)) / length(input_files) == 1) {
        images_list <- parLapply(cl, input_files, read.image)
        tif_meta <- list()
        for (i in seq_along(images_list)) {
          tif_meta[[i]] <- basename(file_path_sans_ext(input_files[[i]]))
        }
        tif_bool <- TRUE
        showNotification("Import complete, .tif files detected. Starting processing...", type = "message")
      } else {
        showNotification("Only upload .lif, .oif, .lsm or .tif files seperately. Refresh page to retry.", type = "error", duration = NULL)
      }
    })
  }

  
  #save fluorochrome info to variables
  fluo1 <- input$selectfluor1
  fluo2 <- input$selectfluor2
  
  #make progress object to track progress
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Processing images", value = 0)

      colocmainloop <- function(i) {
      require("rJava")
      require("devtools")
      require("EBImage")
      require("RBioFormats")
      require("tools")
      require("data.table")
      require("shiny")
      
      progress$inc(1/(length(images_list)/total_cores))
      
      if (oif_bool == TRUE) {
        img_name <- file_path_sans_ext(oif_meta[[i]]$`[File Info] DataName`)
      } else if (lif_bool == TRUE) {
        img_name <- lif_meta[[i]]$`Image name`
      } else if (lsm_bool == TRUE) {
        img_name <- lsm_meta[[i]]$`Recording Name #1`
      } else if (tif_bool == TRUE) {
        img_name <- tif_meta[[i]]
      }
      
      #Start pdf, plots will go in here
      pdf(paste(input_dir, "_", i, "_", img_name, ".pdf", sep = ""), paper = "a4")
      
      chan1_dt <- data.table(getFrame(images_list[[i]], as.numeric(input$selectchan1)))
      chan2_dt <- data.table(getFrame(images_list[[i]], as.numeric(input$selectchan2)))
      
      #Change pixel intensity scaling based on checkbox values
      if (input$bitdepth == "8-bit"){
        chan1_dt_16bit <- chan1_dt * 256
        chan2_dt_16bit <- chan2_dt * 256
      } else if (input$bitdepth == "12-bit"){
        chan1_dt_16bit <- chan1_dt * 4096
        chan2_dt_16bit <- chan2_dt * 4096
      } else if (input$bitdepth == "16-bit"){
        chan1_dt_16bit <- chan1_dt * 65536
        chan2_dt_16bit <- chan2_dt * 65536
      }
      
      #subtract 10000 from Airyscan processing images
      if (input$airyscan == 1){
        chan1_dt_16bit <- chan1_dt_16bit - 10000
        chan2_dt_16bit <- chan2_dt_16bit - 10000
      }
      
      #Change pixel intensity scale from 0 to 1 to 0 to 4096
      # if (max(chan1_dt, na.rm = TRUE) <= 1) {
      #   chan1_dt_16bit <- chan1_dt * 4096
      #   chan2_dt_16bit <- chan2_dt * 4096
      # }
      
      #Plot histograms to show distributions of intensities
      #Data frames cannot be plotted as histograms, hence as.matrix
      hist(as.matrix(chan1_dt_16bit), main = "Pre-normalization", xlab = paste(fluo1, "intensity"))
      hist(as.matrix(chan2_dt_16bit), main = "Pre-normalization", xlab = paste(fluo2, "intensity"))
      
      #Scatter plot of un-normalized data
      #Scatter plot takes numeric vectors as input, this line also converts the data frames
      smoothScatter(as.vector(as.matrix(chan1_dt_16bit)), as.vector(as.matrix(chan2_dt_16bit)), main = "Pre-normalization", xlab = fluo1, ylab = fluo2)
      
      #Convert data frames to vectors for further processing
      chan1_vect <- as.vector(as.matrix(chan1_dt_16bit))
      chan2_vect <- as.vector(as.matrix(chan2_dt_16bit))
      
      #Exclude pixels that are < 20% of max in both channels
      chan1_vect[chan1_vect < (0.20 * max(chan1_vect)) & chan2_vect < (0.20 * max(chan2_vect))] <- NA
      
      #Plot new histograms
      hist(chan1_vect, main = "Filter < 200 AU", xlab = paste(fluo1, "intensity"))
      hist(chan2_vect, main = "Filter < 200 AU", xlab = paste(fluo2, "intensity"))
      
      #Normalize channels based on mean fluorescent intensity 
      #Plot histograms, scatter plot, regression line (-1 to force line through origin)
      if (input$normalizevals == 1){
        chan1_relative <- chan1_vect / mean(chan1_vect, na.rm = TRUE)
        chan2_relative <- chan2_vect / mean(chan2_vect, na.rm = TRUE)
        hist(chan1_relative, main = "Normalized", xlab = paste(fluo1, "intensity"))
        hist(chan2_relative, main = "Normalized", xlab = paste(fluo2, "intensity"))
        smoothScatter(chan1_relative, chan2_relative, main = "Pre-Y axis transformation", xlab = fluo1, ylab = fluo2)
        abline(lm(chan2_relative ~ chan1_relative - 1), col = "red")
      } else {
        chan1_relative <- chan1_vect
        chan2_relative <- chan2_vect
        hist(chan1_relative, main = "Final", xlab = paste(fluo1, "intensity"))
        hist(chan2_relative, main = "Final", xlab = paste(fluo2, "intensity"))
        smoothScatter(chan1_relative, chan2_relative, main = "Pre-Y axis transformation", xlab = fluo1, ylab = fluo2)
        abline(lm(chan2_relative ~ chan1_relative - 1), col = "red")
      }
    
      #Calculate how Y-axis (mch_trans) needs to be transformed to reach X=Y
      regression_norm <- lm(chan2_relative ~ chan1_relative - 1)
      chan2_trans <- chan2_relative * (1 / regression_norm$coefficients[1])
      
      #Place both vectors in data frame, plot new scatter plot
      scatter_dt <- data.table(chan1_relative, chan2_trans)
      smoothScatter(scatter_dt, xlim = c(0, max(scatter_dt$chan1_relative, na.rm = TRUE)), ylim = c(0, max(scatter_dt$chan2_trans, na.rm = TRUE)), main = "Final", xlab = fluo1, ylab = fluo2)
      abline(lm(chan2_trans ~ chan1_relative - 1, data = scatter_dt), col = "red")
      
      #Calculate fineal linear regression
      final_model <- lm(chan2_trans ~ chan1_relative - 1, data = scatter_dt)
      dev.off()
      #Create .txt file to output regression results
      sink(file = paste(input_dir, "_", i, "_", img_name, ".txt", sep = ""), type = "output")
      print(summary(final_model))
      sink()
      #Print to console to keep track of progress
      print(i)
    }
    #ColocalizeR ends here

    #mclapply(seq_along(images_list), colocmainloop, mc.cores = total_cores)
    lapply(seq_along(images_list), colocmainloop)

    stopCluster(cl)
    
    #zip all files
    to_zip <- list.files(pattern = paste(input_dir, "..", sep = ""), include.dirs = FALSE, recursive = FALSE)
    zip(paste(file_path_sans_ext(input_dir), "_output", sep = ""), files = to_zip)
    #Download button
    output$output_file <- downloadHandler(
      filename <- function() {
        paste(file_path_sans_ext(input_dir), "_output", ".zip", sep = "")
      },
      
      content <- function(file) {
        file.copy(paste(file_path_sans_ext(input_dir), "_output", ".zip", sep = ""), file)
      },
      contentType = "application/zip"
    )
    
    
    session$sendCustomMessage("download_ready", list(fileSize = floor(runif(1) * 10000)))
    
    #clean up files
    file.remove(to_zip)
    unlink(input_dir, recursive = TRUE)
    unlink("__MACOSX", recursive = TRUE)
    
    

})