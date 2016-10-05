#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
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
      #actionButton('run', "Run ColocalizeR!"),
      uiOutput('ui.action'),
      tags$hr(),
      downloadButton('output_file', "Download output!")),

    # Additional info
    mainPanel(
      h3("Welcome to ColocalizeR!"),
      ("ColocalizeR currently accepts compressed .lsm, .oif and .tif (as .zip) and .lif files."),
      p("ColocalizeR pulls the name of each image from the metadata, or from the filename of .tif files."),
      br(),
      br(),
      strong("ColocalizeR plots pixel intensities per channel in a scatter plot and derives R-squared values as a goodness-of-fit measure."),
      p("ColocalizeR filters out pixels that are < 20% max intensity in both channels."),
      h4("Instructions:"),
      tags$ol(
        tags$li("Upload files using the sidebar."),
        tags$li("Select channel numbers and fluorochromes of interest."),
        tags$li("Press run."),
        tags$li("Download .zip with output files.")
      ),
      br(),
      h5("Changelog:"),
      h6("v0.6"),
      tags$ul(
        tags$li("Added .lsm and .tif support"),
        tags$li("Improved naming of output files, resolved bug with duplicate file names in datasets.")
      ),
      h6("v0.5"),
      tags$ul(
        tags$li("Implemented parallel CPU processing -- huge speed increase!"),
        tags$li("Improved notifications & error handling")
      )
    )
  ),
  hr(),
  em("Colocalizer v0.6. PL 2016."),em(a("Membrane Trafficking in Immune Cells Lab.", href = "http://membranetrafficking.com")),
  br(),
  em("The software is provided \"as is\" and is free for personal use. Official GitHub repo of the underlying script is available "), em(a("here.", href = "https://github.com/plinders/ColocalizeR"))
  )
)
