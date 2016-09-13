#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load necessary libraries, increase max upload size
library(shiny)
library(tools)
library(devtools)
library(EBImage)
library(RBioFormats)
library(parallel)
library(data.table)
options(shiny.maxRequestSize = 1024 * 1024 ^ 2)

# Define UI for application that measures colocalization
ui <- shinyUI(fluidPage(
    singleton(tags$head(HTML( #disable download button
    '
      <script type="text/javascript">
        $(document).ready(function() {
          // disable download at startup, output_file is the id of the download button
          $("#output_file").attr("disabled", "true").attr("onclick", "return false;");

          Shiny.addCustomMessageHandler("download_ready", function(message) {
            $("#output_file").removeAttr("disabled").removeAttr("onclick").html(
              "<i class=\\"fa fa-download\\"></i>Download output!");
            });
          })
      </script>
    '
    ))),


    # Application title
    titlePanel("ColocalizeR"),

    # Sidebar with
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', label = "Choose .zip or .lif file", accept = "application/zip|application/octet-stream"),
        selectInput('selectchan1', label = "Select first channel index for analysis", choices = 1:10, selected = 1),
        selectInput('selectfluor1', label = "Select first fluorochome for analysis", choices = readLines("src/fluorlist.txt")),
        selectInput('selectchan2', label = "Select second channel index for analysis", choices = 1:10, selected = 2),
        selectInput('selectfluor2', label = "Select second fluorochome for analysis", choices = readLines("src/fluorlist.txt")),
        actionButton('run', "Run ColocalizeR!"),
        downloadButton('output_file', "Download output!")),

      # Additional info
      mainPanel(
        h3("Welcome to ColocalizeR!"),
        ("ColocalizeR currently only accepts files from Olympus microscopy software (.oif extension) in .zip format and .lif files (confirmed working with Leica SP8, SP5 unsupported but might work)."),
        ("ColocalizeR pulls the name of each image from the metadata"),
        br(),
        br(),
        strong("ColocalizeR plots pixel intensities per channel in a scatter plot and derives R-squared values as a goodness-of-fit measure."),
        ("ColocalizeR filters out pixels that are < 20% max intensity in both channels"),
        h4("Instructions:"),
        p("* Upload .lif file or .zip of .oif files using the sidebar."),
        p("* Pick the channel numbers corresponding to your fluorochromes of interest."),
        p("* Press run."),
        p("* Download .zip with output files."),
        br(),
        br(),
        h5("To do:"),
        p("* Add ROI selection")


      )
    )
  )
  )
# Define server logic required to measure colocalization based on ColocalizeR
server <- shinyServer(function(input, output, session) {
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
     input_files <- paste(input_dir, "/", dir(input_dir, pattern = ".oif$|.lif$"), sep = "")
     withProgress(message = "Reading images", {
      if (sum(grepl(".oif$", input_files)) / length(input_files) == 1) {
       images_list <- parLapply(cl, input_files, read.image)
       oif_meta <- list()
       for (i in seq_along(images_list)) {
         oif_meta[[i]] <- globalMetadata(images_list[[i]])
       }
       oif_bool <- TRUE
     } else if (sum(grepl(".lif$", input_files)) / length(input_files) == 1) {
       images_list <- read.image(input_files)
       lif_meta <- list()
       for (i in seq_along(images_list)) {
         lif_meta[[i]] <- seriesMetadata(images_list[[i]])
       }
       lif_bool <- TRUE
     } else {
       message("Only upload .lif or .oif files seperately.")
     }
     })
   }
   #stop cluster to free up memory
   stopCluster(cl)

   #save fluorochrome info to variables
   fluo1 <- input$selectfluor1
   fluo2 <- input$selectfluor2

   withProgress(message = "Processing images", detail = (paste("Image", 1)), value = 0, {
   #Turn images into matrices, this will become a for loop for automated processing of all files
   #Channel 1 = GFP, Channel 2 = mCh, this could become a user input thing once I figure out how that works
   for (i in seq_along(images_list)) {
     incProgress(1/seq_along(images_list), detail = paste("Image", i))

     if (oif_bool == TRUE) {
       img_name <- file_path_sans_ext(oif_meta[[i]]$`[File Info] DataName`)
     } else if (lif_bool == TRUE) {
       img_name <- lif_meta[[i]]$`Image name`
     }

     #Start pdf, plots will go in here
     pdf(paste(input_dir, "_", img_name, ".pdf", sep = ""), paper = "a4")

     chan1_dt <- data.table(getFrame(images_list[[i]], as.numeric(input$selectchan1)))
     chan2_dt <- data.table(getFrame(images_list[[i]], as.numeric(input$selectchan2)))

     #Change pixel intensity scale from 0 to 1 to 0 to 4096
     if (max(chan1_dt, na.rm = TRUE) <= 1) {
       chan1_dt_16bit <- chan1_dt * 4096
       chan2_dt_16bit <- chan2_dt * 4096
     }

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
     chan1_relative <- chan1_vect / mean(chan1_vect, na.rm = TRUE)
     chan2_relative <- chan2_vect / mean(chan2_vect, na.rm = TRUE)

     #Plot histograms, scatter plot, regression line (-1 to force line through origin)
     hist(chan1_relative, main = "Normalized", xlab = paste(fluo1, "intensity"))
     hist(chan2_relative, main = "Normalized", xlab = paste(fluo2, "intensity"))
     smoothScatter(chan1_relative, chan2_relative, main = "Pre-Y axis transformation", xlab = fluo1, ylab = fluo2)
     abline(lm(chan2_relative ~ chan1_relative - 1), col = "red")

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
     sink(file = paste(input_dir, "_", img_name, ".txt", sep = ""), type = "output")
     print(summary(final_model))
     sink()
     #Print to console to keep track of progress
     print(i)
   }
   #ColocalizeR ends here

   #zip all files
   to_zip <- list.files(pattern = paste(input_dir, "..", sep = ""), include.dirs = FALSE, recursive = FALSE)
   zip(paste(input_dir, "_output", sep = ""), files = to_zip)
   #Download button
   output$output_file <- downloadHandler(
     filename <- function() {
       paste(input_dir, "_output", ".zip", sep = "")
     },

     content <- function(file) {
       file.copy(paste(input_dir, "_output", ".zip", sep = ""), file)
     },
     contentType = "application/zip"
   )
   session$sendCustomMessage("download_ready", list(fileSize = floor(runif(1) * 10000)))

   #clean up files
   file.remove(to_zip)
   unlink(input_dir, recursive = TRUE)
   unlink("__MACOSX", recursive = TRUE)


   })
  })
})
# Run the application
shinyApp(ui = ui, server = server)
