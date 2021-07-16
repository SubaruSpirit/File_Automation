library(tidyverse)
library(shiny)
library(pdftools)
library(tesseract)
library(tidytext)
library(reactable)

list1 = read_csv("raw.csv")

ui <- shinyUI(fluidPage(
  
  titlePanel("Testing File upload"),
  
  sidebarLayout(
    sidebarPanel(
      titlePanel("Demo"),
      fileInput("file_import", "Upload Files ( . pdf format only)",
                multiple = T, accept = ".pdf"),
      tableOutput("files"),
      selectInput("type", label = "Document Type",
                  choices = c(list1[["document_type"]],""),selected = ""),
      textInput("trial_number", "Trial Number"),
      textInput("spec_id", "LRA Spec ID"),
      textInput("lot_number", "PMD Orderable Lot Number"),
      textInput("date", "Date"),
      selectInput("vial_kit", label = "Kit or Vial?",
                  choices = c("Kit","Vial",""), selected = ""),
      actionButton("approve","Approve"),
      textOutput("feedback"),
      actionButton("next_pdf","Next PDF"),
      textOutput("end")
    ),
    
    mainPanel(
      uiOutput("pdfview"),
      reactableOutput("test")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  # table of the imported file
  output$files <- renderTable({input$file_import})
  
  ### display the pdf ########################################################
  x = reactiveVal(1)
  
  observeEvent(input$file_import,{
    
    file.rename(input$file_import$datapath[x()], "0.pdf")
    file.copy("0.pdf","www", overwrite = T)
    
    output$pdfview <- renderUI({
      tags$iframe(style="height:1200px; width:100%", src="0.pdf")
    })
  })
  
  ### OCR ####################################################################
  pngfile1 <- reactive({pdftools::pdf_convert("0.pdf",
                                              dpi = 300)})
  text1 <- reactive({tesseract::ocr(pngfile1())})
  df1 = reactive({as_tibble(text1())})
  r1 = reactive({df1() %>%
      unnest_tokens(word, value, to_lower = F)})
  
  # document type
  observeEvent(input$file_import,{
    updateSelectInput(session, "type", selected = 
                        if(paste(r1()[3,],r1()[4,],r1()[5,])==
                           "Label Specification Approval")
                        {"Label Specification Approval Form (ie variable text approval)"}else{""})
  })
  
  # trial number
  observeEvent(input$file_import,{
    updateTextInput(session, "trial_number", value = 
                      as.character(r1()[which(grepl("Trial", r1()[["word"]]))+2,]))
  })
  
  # spec id
  observeEvent(input$file_import,{
    updateTextInput(session, "spec_id", value = 
                      paste(as.character(r1()[which(grepl("Spec", r1()[["word"]]))[2]+2,]),
                            "-",
                            as.character(r1()[which(grepl("Spec", r1()[["word"]]))[2]+3,]),
                            sep=""
                      )
    )
  })
  
  # lot number
  observeEvent(input$file_import,{
    updateTextInput(session, "lot_number", value = 
                      as.character(r1()[which(grepl("Lot", r1()[["word"]]))+2,])
    )
  })
  
  # date
  observeEvent(input$file_import,{
    updateTextInput(session, "date", value = 
                      paste(as.character(r1()[which(grepl("QA", r1()[["word"]]))[2]+1,]),
                            "-",
                            as.character(r1()[which(grepl("QA", r1()[["word"]]))[2]+2,]),
                            "-",
                            as.character(r1()[which(grepl("QA", r1()[["word"]]))[2]+3,]),
                            sep=""
                      )
    )
  })
  
  # vial or kit
  observeEvent(input$file_import,{
    updateSelectInput(session, "vial_kit", selected = 
                        if(length(which(grepl("PRIMARY", r1()[["word"]])))==1){
                          "Vial"
                        } else{"Kit"})
  })
  
  
  ### To be deleted #############################################################  
  # put the critical info into the dataframe
  observeEvent(input$file_import,{
    output$feedback = renderText("")
    
    df2 = reactive({
      tibble(
        pdf_name = "0.pdf",
        protocol = input$trial_number,
        report_type = input$type,
        label_spec_id = input$spec_id,
        pmd_lot_number = input$lot_number,
        date = input$date,
        vial_kit = input$vial_kit
      )
    })
    
    df3 = reactive({
      mutate(df2(), pdf_name=paste(
        df2()[["protocol"]],"Label Specification Approval",
        df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
        df2()[["vial_kit"]],df2()[["date"]],
        ".pdf"
      ))
    })
    
    # display the dataframe (will be deleted once live)
    output$test <- renderReactable({
      reactable(df3())
    })
  })
  
  ###############################################################################
  
  
  observeEvent(input$approve, {
    output$feedback = renderText("Oh Yeah!")
    
    df2 = reactive({
      tibble(
        pdf_name = "0.pdf",
        protocol = input$trial_number,
        report_type = input$type,
        label_spec_id = input$spec_id,
        pmd_lot_number = input$lot_number,
        date = input$date,
        vial_kit = input$vial_kit
      )
    })
    
    df3 = reactive({
      mutate(df2(), pdf_name=paste(
        df2()[["protocol"]],"Label Specification Approval",
        df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
        df2()[["vial_kit"]],df2()[["date"]],
        ".pdf"
      ))
    })
    
    file.rename("0.pdf",
                df3()[["pdf_name"]]
    )
    file.copy(df3()[["pdf_name"]], "final")
    
    # append to csv
    write.table(df3(),"test.csv", append = T, col.names = F, row.names = F ,
                sep = ",")
    
    file.copy("test.csv", "final", overwrite = T)
    
    
  })
  
  
  observeEvent(input$next_pdf, {
    
    if(x()<length(input$file_import$datapath)){
      x(x()+1)
      file.rename(input$file_import$datapath[x()], "0.pdf")
      file.copy("0.pdf","www", overwrite = T)
      
      output$pdfview <- renderUI({
        tags$iframe(style="height:1200px; width:100%", src="0.pdf")
      })
      
      pngfile1 <- pdftools::pdf_convert("0.pdf",dpi = 300)
      text1 <- tesseract::ocr(pngfile1)
      df1 = as_tibble(text1)
      r1 = df1 %>%
        unnest_tokens(word, value, to_lower = F)
      
      
      # document type
      updateSelectInput(session, "type", selected = 
                          if(paste(r1[3,],r1[4,],r1[5,])==
                             "Label Specification Approval")
                          {"Label Specification Approval Form (ie variable text approval)"}else{""})
      
      
      # trial number
      updateTextInput(session, "trial_number", value = 
                        as.character(r1[which(grepl("Trial", r1[["word"]]))+2,]))
      
      
      # spec id
      updateTextInput(session, "spec_id", value = 
                        paste(as.character(r1[which(grepl("Spec", r1[["word"]]))[2]+2,]),
                              "-",
                              as.character(r1[which(grepl("Spec", r1[["word"]]))[2]+3,]),
                              sep=""
                        )
      )
      
      
      # lot number
      updateTextInput(session, "lot_number", value = 
                        as.character(r1[which(grepl("Lot", r1[["word"]]))+2,])
      )
      
      
      # date
      updateTextInput(session, "date", value = 
                        paste(as.character(r1[which(grepl("QA", r1[["word"]]))[2]+1,]),
                              "-",
                              as.character(r1[which(grepl("QA", r1[["word"]]))[2]+2,]),
                              "-",
                              as.character(r1[which(grepl("QA", r1[["word"]]))[2]+3,]),
                              sep=""
                        )
      )
      
      
      # vial or kit
      updateSelectInput(session, "vial_kit", selected = 
                          if(length(which(grepl("PRIMARY", r1[["word"]])))==1){
                            "Vial"
                          } else{"Kit"})
    } else {output$end = renderText("You have reached the last PDF!")}

    
    
    

    
 
    
    
  })
  
  
  
  
})

shinyApp(ui = ui, server = server)
