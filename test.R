library(tidyverse)
library(shiny)
library(pdftools)
library(tesseract)
library(tidytext)
library(reactable)
library(shinyFeedback)
library(shinyjs)

list1 = read_csv("raw.csv")

ui <- shinyUI(fluidPage(
  useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  titlePanel("Testing File upload"),
  
  sidebarLayout(
    sidebarPanel(
      titlePanel("Demo"),
      fileInput("file_import", "Upload Files ( . pdf format only)",
                multiple = T, accept = ".pdf"),
      disabled(actionButton("ocr_button","OCR (click this when nothing shows up)",
                   class = "btn-danger", icon=icon("warning sign"))),
      tableOutput("files"),
      selectInput("type", label = "Document Type",
                  choices = c(list1[["document_type"]],""),selected = ""),
      uiOutput("trial_number"),
      uiOutput("spec_id"),
      uiOutput("label_template_id"),
      uiOutput("lot_number"),
      uiOutput("reg_qa"),
      uiOutput("vial_kit"),
      uiOutput("date"),
      fluidRow(
        column(2,
               actionButton("approve","Approve")
        ),
        column(2,
               disabled(actionButton("next_pdf","Next PDF"))
        ),
        column(4,
               ""
        ),
        column(2,
               actionButton("report_issues","Report Issues")
        )
      ),
      fluidRow(
        column(2,
               textOutput("end")
        ),
        column(2,
               ""
        ),
        column(4,
               ""
        ),
        column(2,
               textOutput("report_feedback")
        )
      ),
      h5(tags$b("PDF Name")),
      verbatimTextOutput("name_table")
    ),
    
    mainPanel(
      uiOutput("pdfview"),
      reactableOutput("test")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  # table of the imported file
  #output$files <- renderTable({input$file_import})
  
  ### issues log #############################################################
  observeEvent(input$report_issues,{
    output$report_feedback = renderText("Thanks!")
    file.copy("0.pdf", file.path("issues", paste0(gsub("[[:punct:]]", " ",
                                                       Sys.time()), ".pdf")))
  })
  
  ### display the pdf ########################################################
  x = reactiveVal(1)
  
  observeEvent(input$file_import,{
    enable("ocr_button")
    output$report_feedback = renderText("")
    
    file.rename(input$file_import$datapath[x()], "0.pdf")
    file.copy("0.pdf","www", overwrite = T)
    
    output$pdfview <- renderUI({
      tags$iframe(style="height:1200px; width:100%", src="0.pdf")
    })
    
    ### pdf_text ###########################################################
    text1 <- reactive({pdf_text("0.pdf")})
    df1 = reactive({as_tibble(text1())})
    r1 = reactive({df1() %>%
        unnest_tokens(word, value, to_lower = F)})
    
    ########################################################################
    
    # document type
    updateSelectInput(session, "type", selected = 
                        if((which(grepl("\\<Label\\>", r1()[["word"]]))[1] +1 ==
                            which(grepl("\\<Specification\\>", r1()[["word"]]))[1]) &
                           (which(grepl("\\<Specification\\>", r1()[["word"]]))[1] +1 ==
                            # %in% TRUE: return FALSE when it's NA
                            which(grepl("\\<Approval\\>", r1()[["word"]]))[1])%in% TRUE)
                        {"Label Specification Approval Form (ie variable text approval)"}
                      else if ((which(grepl("\\<P\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<L\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<L\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<Plan\\>", r1()[["word"]]))[1])%in% TRUE){
                        "Pack and Label Plan"}
                      else if ((which(grepl("\\<Label\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<Proof\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<Proof\\>", r1()[["word"]]))[1] +1 ==
                                # %in% TRUE: return FALSE when it's NA
                                which(grepl("\\<Request\\>", r1()[["word"]]))[1])%in% TRUE)
                      {"Label Proof Request"}
                      else if ((which(grepl("\\<LABEL\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<SPECIFICATION\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<SPECIFICATION\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<COVER\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<COVER\\>", r1()[["word"]]))[1] +1 ==
                                # %in% TRUE: return FALSE when it's NA
                                which(grepl("\\<SHEET\\>", r1()[["word"]]))[1]) %in% TRUE) {
                        "Label Proof"
                      }
                      else if ((which(grepl("\\<LINKS\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<REPORT\\>", r1()[["word"]]))[1]) %in% TRUE){
                        "Links Report"
                      }
                      else if ((which(grepl("\\<RANDOMIZATION\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<REPORT\\>", r1()[["word"]]))[1]) %in% TRUE){
                        "Randomization Report"
                      }
                      else if ((which(grepl("\\<Kit\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<List\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<List\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<Specification\\>", r1()[["word"]]))[1])%in% TRUE){
                        "Medication Kit List Specification"
                      }
    )
    
    
    # trial number
    output$trial_number = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      } else if (input$type=="Pack and Label Plan"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      } else if (input$type=="Label Proof Request"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      } else if (input$type=="Label Proof"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      } else if (input$type=="Links Report"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      } else if (input$type=="Randomization Report"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      } else if (input$type=="Medication Kit List Specification"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      }
    })
    
    # spec id
    output$spec_id = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("spec_id", label = "LRA Spec ID",
                  value = 
                    paste(as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))[1]+2,]),
                          "-",
                          as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))[1]+3,]),
                          sep=""
                    )
        )
      } else{""}
    })
    
    # label template id
    output$label_template_id = renderUI({
      if(input$type=="Label Proof Request"){
        textInput("label_template_id", label = "Label Template ID",
                  value = 
                    gsub(",","",as.character(r1()[which(grepl("\\<LRA\\>", r1()[["word"]]))[1]+3,]))
        )
      } else if(input$type=="Label Proof"){
        textInput("label_template_id", label = "Label Template ID",
                  value = 
                    as.character(r1()[which(grepl("Client", r1()[["word"]]))[1]+2,])
        )
      }
    })
    
    # lot number
    output$lot_number = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    as.character(r1()[which(grepl("^A[0-9]{5}$", r1()[["word"]]))[which(which(grepl("^A[0-9]{5}$", r1()[["word"]]))-15 <last(which(grepl("PMD", r1()$word))) &
                                                                                        which(grepl("^A[0-9]{5}$", r1()[["word"]]))>last(which(grepl("PMD", r1()$word))))],])
        )
      } else if (input$type=="Pack and Label Plan"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))[1]+1,])
        )
      } else if (input$type=="Links Report"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    paste(unique(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))+2,
                                    ])[["word"]], collapse = " ")
        )
      } 
    })
    
    # reg or qa
    output$reg_qa = renderUI({
      if(input$type=="Label Proof Request"){
        
        selectInput("reg_qa", label = "Reg or QA",
                  choices = c("Reg","QA",""),
                  selected = "Reg")
      } else {""}
    })
    
    # date
    output$date = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("date", label = "Date",
                  value = 
                    paste(as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+1,]),
                          "-",
                          as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+2,]),
                          "-",
                          as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+3,]),
                          sep=""
                    )
        )
      } else if(input$type=="Pack and Label Plan"){
        textInput("date", label = "Date",
                  value = 
                    paste(
                      as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                       (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))],]),
                      as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                           (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))]+1,]),
                      as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                           (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))]+2,]),
                      sep = "-"
                    )
        )
      } else if(input$type=="Label Proof Request"){
        
        ### OCR for last page to extract Transperfect and date #################
        text2 = reactive({pdf_ocr_text("0.pdf", pages = pdf_length("0.pdf"), dpi=300)})
        df2 = reactive({as_tibble(text2())})
        r2 = reactive({df2() %>%
            unnest_tokens(word, value, to_lower = F)})
        ########################################################################
        
        textInput("date", label = "Date",
                  value = if(length(which(grepl("transperfect", r2()[["word"]],
                                                 ignore.case = T)))>=1){
                    paste(
                      if(grepl("^[0-9]{1}$", r2()[last(which(grepl("Signature", r2()[["word"]])))-2,])){
                        paste(0, r2()[last(which(grepl("Signature", r2()[["word"]])))-2,], sep="")
                      } else {r2()[last(which(grepl("Signature", r2()[["word"]])))-2,]},
                      r2()[last(which(grepl("Signature", r2()[["word"]])))-3,],
                      r2()[last(which(grepl("Signature", r2()[["word"]])))-1,],
                      sep="-"
                    )
                  } else{""} # ask the QA to fill in the date when signing
        )
      } else if(input$type=="Label Proof"){
        textInput("date", label = "Date",
                  value = 
                    paste(
                      as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+2,]),
                      as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+3,]),
                      as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+4,]),
                      sep="-"
                    )
        )
      } else if(input$type=="Links Report"){
        textInput("date", label = "Date",
                  value = paste(
                    r1()[which(r1()[["word"]] %in% month.abb)[4]-1,],
                    r1()[which(r1()[["word"]] %in% month.abb)[4],],
                    r1()[which(r1()[["word"]] %in% month.abb)[4]+1,],
                    sep="-"
                  )
                    
        )
      } else if(input$type=="Randomization Report"){
        textInput("date", label = "Date",
                  value = paste(
                    r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4]-1,],
                    r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4],],
                    r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4]+1,],
                    sep="-"
                  )
                  
        )
      } else if(input$type=="Medication Kit List Specification"){
        textInput("date", label = "Date",
                  value = paste(
                    r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")))-1,],
                    r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))),],
                    r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")))+1,],
                    sep="-"
                  )
                  
        )
      }
    })
    
    # vial or kit
    output$vial_kit = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                    selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))>=1){
                      "Vial"
                    } else{"Kit"}
        )
      } else if (input$type=="Label Proof Request"){
        selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                    selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))>=1){
                      "Vial"
                    } else{"Kit"}
        )
      } else if (input$type=="Label Proof") {
        selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                    selected = if(as.character(r1()[which(grepl("Container", r1()[["word"]]))[1]+2,])=="Vial"){
                      "Vial"
                    } else{"Kit"}
        )
      }
      
    })
    
    # put info into df and display
    
    df2 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = input$spec_id,
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = "",
          reg_qa = ""
        )
      } else if (input$type=="Pack and Label Plan"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if (input$type=="Label Proof Request"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = input$label_template_id,
          reg_qa = input$reg_qa
        )
      } else if(input$type=="Label Proof") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = input$label_template_id,
          reg_qa = ""
        )
      } else if(input$type=="Links Report") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if(input$type=="Randomization Report") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if(input$type=="Medication Kit List Specification") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      }
    })
    
    df3 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Specification Approval",
          df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
          df2()[["vial_kit"]],df2()[["date"]],
          ".pdf"
        ))
      } else if (input$type=="Pack and Label Plan"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"PL Plan", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Label Proof Request"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Proof Request", df2()[["label_template_id"]],
          paste(df2()[["reg_qa"]], df2()[["vial_kit"]], sep = "_"),
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Label Proof") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Proof", df2()[["label_template_id"]],
          df2()[["vial_kit"]],df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Links Report") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Links Report", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Randomization Report") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Sub Rand", df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Medication Kit List Specification") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Kit List Specification", df2()[["date"]], ".pdf"
        ))
      }
    })
    
    # display the dataframe (will be deleted once live)
    output$test <- renderReactable({
      reactable(df3())
    })
    
    output$name_table <- renderText({
      df3()[["pdf_name"]]
    })
    
    
  })
  
  
  ### approve ################################################################
  observeEvent(input$approve, {
    disable("approve")
    enable("next_pdf")
    output$report_feedback = renderText("")
    
    df2 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = input$spec_id,
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = "",
          reg_qa = ""
        )
      } else if (input$type=="Pack and Label Plan"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if (input$type=="Label Proof Request"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = input$label_template_id,
          reg_qa = input$reg_qa
        )
      } else if(input$type=="Label Proof") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = input$label_template_id,
          reg_qa = ""
        )
      } else if(input$type=="Links Report") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if(input$type=="Randomization Report") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if(input$type=="Medication Kit List Specification") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      }
    })
    
    df3 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Specification Approval",
          df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
          df2()[["vial_kit"]],df2()[["date"]],
          ".pdf"
        ))
      } else if (input$type=="Pack and Label Plan"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"PL Plan", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Label Proof Request"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Proof Request", df2()[["label_template_id"]],
          paste(df2()[["reg_qa"]], df2()[["vial_kit"]], sep = "_"),
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Label Proof") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Proof", df2()[["label_template_id"]],
          df2()[["vial_kit"]],df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Links Report") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Links Report", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Randomization Report") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Sub Rand", df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Medication Kit List Specification") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Kit List Specification", df2()[["date"]], ".pdf"
        ))
      }
    })
    
    file.copy("0.pdf", file.path("final", df3()[["pdf_name"]]))
    
    # append to csv
    write.table(df3(),"test.csv", append = T, col.names = F, row.names = F ,
                sep = ",")
    
    file.copy("test.csv", "final", overwrite = T)
    
    
  })
  
  
  ### next pdf ##############################################################
  observeEvent(input$next_pdf, {
    enable("approve")
    disable("next_pdf")
    output$report_feedback = renderText("")
    
    if(x()<length(input$file_import$datapath)){
      x(x()+1)
      file.rename(input$file_import$datapath[x()], "0.pdf")
      file.copy("0.pdf","www", overwrite = T)
      
      output$pdfview <- renderUI({
        tags$iframe(style="height:1200px; width:100%", src="0.pdf")
      })
      
      ### OCR ###########################################################
      text1 <- reactive({pdf_text("0.pdf")})
      df1 = reactive({as_tibble(text1())})
      r1 = reactive({df1() %>%
          unnest_tokens(word, value, to_lower = F)})
      ########################################################################
      
      # document type
      updateSelectInput(session, "type", selected = 
                          if((which(grepl("\\<Label\\>", r1()[["word"]]))[1] +1 ==
                              which(grepl("\\<Specification\\>", r1()[["word"]]))[1]) &
                             (which(grepl("\\<Specification\\>", r1()[["word"]]))[1] +1 ==
                              # %in% TRUE: return FALSE when it's NA
                              which(grepl("\\<Approval\\>", r1()[["word"]]))[1])%in% TRUE)
                          {"Label Specification Approval Form (ie variable text approval)"}
                        else if ((which(grepl("\\<P\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<L\\>", r1()[["word"]]))[1]) &
                                 (which(grepl("\\<L\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<Plan\\>", r1()[["word"]]))[1])%in% TRUE){
                          "Pack and Label Plan"}
                        else if ((which(grepl("\\<Label\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<Proof\\>", r1()[["word"]]))[1]) &
                                 (which(grepl("\\<Proof\\>", r1()[["word"]]))[1] +1 ==
                                  # %in% TRUE: return FALSE when it's NA
                                  which(grepl("\\<Request\\>", r1()[["word"]]))[1])%in% TRUE)
                        {"Label Proof Request"}
                        else if ((which(grepl("\\<LABEL\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<SPECIFICATION\\>", r1()[["word"]]))[1]) &
                                 (which(grepl("\\<SPECIFICATION\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<COVER\\>", r1()[["word"]]))[1]) &
                                 (which(grepl("\\<COVER\\>", r1()[["word"]]))[1] +1 ==
                                  # %in% TRUE: return FALSE when it's NA
                                  which(grepl("\\<SHEET\\>", r1()[["word"]]))[1]) %in% TRUE) {
                          "Label Proof"
                        }
                        else if ((which(grepl("\\<LINKS\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<REPORT\\>", r1()[["word"]]))[1]) %in% TRUE){
                          "Links Report"
                        }
                        else if ((which(grepl("\\<RANDOMIZATION\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<REPORT\\>", r1()[["word"]]))[1]) %in% TRUE){
                          "Randomization Report"
                        }
                        else if ((which(grepl("\\<Kit\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<List\\>", r1()[["word"]]))[1]) &
                                 (which(grepl("\\<List\\>", r1()[["word"]]))[1] +1 ==
                                  which(grepl("\\<Specification\\>", r1()[["word"]]))[1])%in% TRUE){
                          "Medication Kit List Specification"
                        }
      )
      
      
      # trial number
      output$trial_number = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
        } else if (input$type=="Pack and Label Plan"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
        } else if (input$type=="Label Proof Request"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
        } else if (input$type=="Label Proof"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
        } else if (input$type=="Links Report"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
        } else if (input$type=="Randomization Report"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
        } else if (input$type=="Medication Kit List Specification"){
          textInput("trial_number", label = "Trial Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
        }
      })
      
      # spec id
      output$spec_id = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("spec_id", label = "LRA Spec ID",
                    value = 
                      paste(as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))[1]+2,]),
                            "-",
                            as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))[1]+3,]),
                            sep=""
                      )
          )
        } else{""}
      })
      
      # label template id
      output$label_template_id = renderUI({
        if(input$type=="Label Proof Request"){
          textInput("label_template_id", label = "Label Template ID",
                    value = 
                      gsub(",","",as.character(r1()[which(grepl("\\<LRA\\>", r1()[["word"]]))[1]+3,]))
          )
        } else if(input$type=="Label Proof"){
          textInput("label_template_id", label = "Label Template ID",
                    value = 
                      as.character(r1()[which(grepl("Client", r1()[["word"]]))[1]+2,])
          )
        }
      })
      
      # lot number
      output$lot_number = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("lot_number", label = "PMD Orderable Lot Number",
                    value = 
                      as.character(r1()[which(grepl("^A[0-9]{5}$", r1()[["word"]]))[which(which(grepl("^A[0-9]{5}$", r1()[["word"]]))-15 <last(which(grepl("PMD", r1()$word))) &
                                                                                            which(grepl("^A[0-9]{5}$", r1()[["word"]]))>last(which(grepl("PMD", r1()$word))))],])
          )
        } else if (input$type=="Pack and Label Plan"){
          textInput("lot_number", label = "PMD Orderable Lot Number",
                    value = 
                      as.character(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))[1]+1,])
          )
        } else if (input$type=="Links Report"){
          textInput("lot_number", label = "PMD Orderable Lot Number",
                    value = 
                      paste(unique(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))+2,
                      ])[["word"]], collapse = " ")
          )
        } 
      })
      
      # reg or qa
      output$reg_qa = renderUI({
        if(input$type=="Label Proof Request"){
          
          selectInput("reg_qa", label = "Reg or QA",
                      choices = c("Reg","QA",""),
                      selected = "Reg")
        } else {""}
      })
      
      # date
      output$date = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          textInput("date", label = "Date",
                    value = 
                      paste(as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+1,]),
                            "-",
                            as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+2,]),
                            "-",
                            as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+3,]),
                            sep=""
                      )
          )
        } else if(input$type=="Pack and Label Plan"){
          textInput("date", label = "Date",
                    value = 
                      paste(
                        as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                             (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))],]),
                        as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                             (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))]+1,]),
                        as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                             (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))]+2,]),
                        sep = "-"
                      )
          )
        } else if(input$type=="Label Proof Request"){
          
          ### OCR for last page to extract Transperfect and date #################
          text2 = reactive({pdf_ocr_text("0.pdf", pages = pdf_length("0.pdf"), dpi=300)})
          df2 = reactive({as_tibble(text2())})
          r2 = reactive({df2() %>%
              unnest_tokens(word, value, to_lower = F)})
          ########################################################################
          
          textInput("date", label = "Date",
                    value = if(length(which(grepl("transperfect", r2()[["word"]],
                                                  ignore.case = T)))>=1){
                      paste(
                        if(grepl("^[0-9]{1}$", r2()[last(which(grepl("Signature", r2()[["word"]])))-2,])){
                          paste(0, r2()[last(which(grepl("Signature", r2()[["word"]])))-2,], sep="")
                        } else {r2()[last(which(grepl("Signature", r2()[["word"]])))-2,]},
                        r2()[last(which(grepl("Signature", r2()[["word"]])))-3,],
                        r2()[last(which(grepl("Signature", r2()[["word"]])))-1,],
                        sep="-"
                      )
                    } else{""} # ask the QA to fill in the date when signing
          )
        } else if(input$type=="Label Proof"){
          textInput("date", label = "Date",
                    value = 
                      paste(
                        as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+2,]),
                        as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+3,]),
                        as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+4,]),
                        sep="-"
                      )
          )
        } else if(input$type=="Links Report"){
          textInput("date", label = "Date",
                    value = paste(
                      r1()[which(r1()[["word"]] %in% month.abb)[4]-1,],
                      r1()[which(r1()[["word"]] %in% month.abb)[4],],
                      r1()[which(r1()[["word"]] %in% month.abb)[4]+1,],
                      sep="-"
                    )
                    
          )
        } else if(input$type=="Randomization Report"){
          textInput("date", label = "Date",
                    value = paste(
                      r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4]-1,],
                      r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4],],
                      r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4]+1,],
                      sep="-"
                    )
                    
          )
        } else if(input$type=="Medication Kit List Specification"){
          textInput("date", label = "Date",
                    value = paste(
                      r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")))-1,],
                      r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))),],
                      r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")))+1,],
                      sep="-"
                    )
                    
          )
        }
      })
      
      # vial or kit
      output$vial_kit = renderUI({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                      selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))>=1){
                        "Vial"
                      } else{"Kit"}
          )
        } else if (input$type=="Label Proof Request"){
          selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                      selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))>=1){
                        "Vial"
                      } else{"Kit"}
          )
        } else if (input$type=="Label Proof") {
          selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                      selected = if(as.character(r1()[which(grepl("Container", r1()[["word"]]))[1]+2,])=="Vial"){
                        "Vial"
                      } else{"Kit"}
          )
        }
        
      })
      
      # put info into df and display
      
      df2 = reactive({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = input$spec_id,
            pmd_lot_number = input$lot_number,
            date = input$date,
            vial_kit = input$vial_kit,
            label_template_id = "",
            reg_qa = ""
          )
        } else if (input$type=="Pack and Label Plan"){
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = "",
            pmd_lot_number = input$lot_number,
            date = input$date,
            vial_kit = "",
            label_template_id = "",
            reg_qa = ""
          )
        } else if (input$type=="Label Proof Request"){
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = "",
            pmd_lot_number = "",
            date = input$date,
            vial_kit = input$vial_kit,
            label_template_id = input$label_template_id,
            reg_qa = input$reg_qa
          )
        } else if(input$type=="Label Proof") {
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = "",
            pmd_lot_number = "",
            date = input$date,
            vial_kit = input$vial_kit,
            label_template_id = input$label_template_id,
            reg_qa = ""
          )
        } else if(input$type=="Links Report") {
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = "",
            pmd_lot_number = input$lot_number,
            date = input$date,
            vial_kit = "",
            label_template_id = "",
            reg_qa = ""
          )
        } else if(input$type=="Randomization Report") {
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = "",
            pmd_lot_number = "",
            date = input$date,
            vial_kit = "",
            label_template_id = "",
            reg_qa = ""
          )
        } else if(input$type=="Medication Kit List Specification") {
          tibble(
            pdf_name = "0.pdf",
            protocol = input$trial_number,
            report_type = input$type,
            label_spec_id = "",
            pmd_lot_number = "",
            date = input$date,
            vial_kit = "",
            label_template_id = "",
            reg_qa = ""
          )
        }
      })
      
      df3 = reactive({
        if(input$type=="Label Specification Approval Form (ie variable text approval)"){
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"Label Specification Approval",
            df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
            df2()[["vial_kit"]],df2()[["date"]],
            ".pdf"
          ))
        } else if (input$type=="Pack and Label Plan"){
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"PL Plan", df2()[["pmd_lot_number"]],
            df2()[["date"]], ".pdf"
          ))
        } else if (input$type=="Label Proof Request"){
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"Label Proof Request", df2()[["label_template_id"]],
            paste(df2()[["reg_qa"]], df2()[["vial_kit"]], sep = "_"),
            df2()[["date"]], ".pdf"
          ))
        } else if (input$type=="Label Proof") {
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"Label Proof", df2()[["label_template_id"]],
            df2()[["vial_kit"]],df2()[["date"]], ".pdf"
          ))
        } else if (input$type=="Links Report") {
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"Links Report", df2()[["pmd_lot_number"]],
            df2()[["date"]], ".pdf"
          ))
        } else if (input$type=="Randomization Report") {
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"Sub Rand", df2()[["date"]], ".pdf"
          ))
        } else if (input$type=="Medication Kit List Specification") {
          mutate(df2(), pdf_name=paste(
            df2()[["protocol"]],"Kit List Specification", df2()[["date"]], ".pdf"
          ))
        }
      })
      
      # display the dataframe (will be deleted once live)
      output$test <- renderReactable({
        reactable(df3())
      })
      
      output$name_table <- renderText({
        df3()[["pdf_name"]]
      })
      
      
    } else {output$end = renderText("You have reached the last PDF!")}
    
    
  })
  
  
  ### click ocr button if pdf_text failed ###############
  observeEvent(input$ocr_button, {
    ### pdf_text ###########################################################
    text1 <- reactive({pdf_ocr_text("0.pdf", dpi = 300)})
    df1 = reactive({as_tibble(text1())})
    r1 = reactive({df1() %>%
        unnest_tokens(word, value, to_lower = F)})
    
    ########################################################################
    
    # document type
    updateSelectInput(session, "type", selected = 
                        if((which(grepl("\\<Label\\>", r1()[["word"]]))[1] +1 ==
                            which(grepl("\\<Specification\\>", r1()[["word"]]))[1]) &
                           (which(grepl("\\<Specification\\>", r1()[["word"]]))[1] +1 ==
                            # %in% TRUE: return FALSE when it's NA
                            which(grepl("\\<Approval\\>", r1()[["word"]]))[1])%in% TRUE)
                        {"Label Specification Approval Form (ie variable text approval)"}
                      else if ((which(grepl("\\<P\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<L\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<L\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<Plan\\>", r1()[["word"]]))[1])%in% TRUE){
                        "Pack and Label Plan"}
                      else if ((which(grepl("\\<Label\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<Proof\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<Proof\\>", r1()[["word"]]))[1] +1 ==
                                # %in% TRUE: return FALSE when it's NA
                                which(grepl("\\<Request\\>", r1()[["word"]]))[1])%in% TRUE)
                      {"Label Proof Request"}
                      else if ((which(grepl("\\<LABEL\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<SPECIFICATION\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<SPECIFICATION\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<COVER\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<COVER\\>", r1()[["word"]]))[1] +1 ==
                                # %in% TRUE: return FALSE when it's NA
                                which(grepl("\\<SHEET\\>", r1()[["word"]]))[1]) %in% TRUE) {
                        "Label Proof"
                      }
                      else if ((which(grepl("\\<LINKS\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<REPORT\\>", r1()[["word"]]))[1]) %in% TRUE){
                        "Links Report"
                      }
                      else if ((which(grepl("\\<RANDOMIZATION\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<REPORT\\>", r1()[["word"]]))[1]) %in% TRUE){
                        "Randomization Report"
                      }
                      else if ((which(grepl("\\<Kit\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<List\\>", r1()[["word"]]))[1]) &
                               (which(grepl("\\<List\\>", r1()[["word"]]))[1] +1 ==
                                which(grepl("\\<Specification\\>", r1()[["word"]]))[1])%in% TRUE){
                        "Medication Kit List Specification"
                      }
    )
    
    
    # trial number
    output$trial_number = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      } else if (input$type=="Pack and Label Plan"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      } else if (input$type=="Label Proof Request"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Trial\\>", r1()[["word"]]))[1]+2,]))
      } else if (input$type=="Label Proof"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      } else if (input$type=="Links Report"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      } else if (input$type=="Randomization Report"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      } else if (input$type=="Medication Kit List Specification"){
        textInput("trial_number", label = "Trial Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Protocol\\>", r1()[["word"]]))[1]+1,]))
      }
    })
    
    # spec id
    output$spec_id = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("spec_id", label = "LRA Spec ID",
                  value = 
                    paste(as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))[1]+2,]),
                          "-",
                          as.character(r1()[which(grepl("\\<Spec\\>", r1()[["word"]]))[1]+3,]),
                          sep=""
                    )
        )
      } else{""}
    })
    
    # label template id
    output$label_template_id = renderUI({
      if(input$type=="Label Proof Request"){
        textInput("label_template_id", label = "Label Template ID",
                  value = 
                    gsub(",","",as.character(r1()[which(grepl("\\<LRA\\>", r1()[["word"]]))[1]+3,]))
        )
      } else if(input$type=="Label Proof"){
        textInput("label_template_id", label = "Label Template ID",
                  value = 
                    as.character(r1()[which(grepl("Client", r1()[["word"]]))[1]+2,])
        )
      }
    })
    
    # lot number
    output$lot_number = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    as.character(r1()[which(grepl("^A[0-9]{5}$", r1()[["word"]]))[which(which(grepl("^A[0-9]{5}$", r1()[["word"]]))-15 <last(which(grepl("PMD", r1()$word))) &
                                                                                          which(grepl("^A[0-9]{5}$", r1()[["word"]]))>last(which(grepl("PMD", r1()$word))))],])
        )
      } else if (input$type=="Pack and Label Plan"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    as.character(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))[1]+1,])
        )
      } else if (input$type=="Links Report"){
        textInput("lot_number", label = "PMD Orderable Lot Number",
                  value = 
                    paste(unique(r1()[which(grepl("\\<Lot\\>", r1()[["word"]]))+2,
                    ])[["word"]], collapse = " ")
        )
      } 
    })
    
    # reg or qa
    output$reg_qa = renderUI({
      if(input$type=="Label Proof Request"){
        
        selectInput("reg_qa", label = "Reg or QA",
                    choices = c("Reg","QA",""),
                    selected = "Reg")
      } else {""}
    })
    
    # date
    output$date = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        textInput("date", label = "Date",
                  value = 
                    paste(as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+1,]),
                          "-",
                          as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+2,]),
                          "-",
                          as.character(r1()[last(which(grepl("\\<QA\\>", r1()[["word"]])))+3,]),
                          sep=""
                    )
        )
      } else if(input$type=="Pack and Label Plan"){
        textInput("date", label = "Date",
                  value = 
                    paste(
                      as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                           (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))],]),
                      as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                           (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))]+1,]),
                      as.character(r1()[which(grepl("^[0-9]{2}$", r1()[["word"]]))[which((which(grepl("^[0-9]{2}$", r1()[["word"]])) -7 < which(grepl("Approved", r1()[["word"]]))) &
                                                                                           (which(grepl("^[0-9]{2}$", r1()[["word"]])) > which(grepl("Approved", r1()[["word"]]))))]+2,]),
                      sep = "-"
                    )
        )
      } else if(input$type=="Label Proof Request"){
        
        ### OCR for last page to extract Transperfect and date #################
        text2 = reactive({pdf_ocr_text("0.pdf", pages = pdf_length("0.pdf"), dpi=300)})
        df2 = reactive({as_tibble(text2())})
        r2 = reactive({df2() %>%
            unnest_tokens(word, value, to_lower = F)})
        ########################################################################
        
        textInput("date", label = "Date",
                  value = if(length(which(grepl("transperfect", r2()[["word"]],
                                                ignore.case = T)))>=1){
                    paste(
                      if(grepl("^[0-9]{1}$", r2()[last(which(grepl("Signature", r2()[["word"]])))-2,])){
                        paste(0, r2()[last(which(grepl("Signature", r2()[["word"]])))-2,], sep="")
                      } else {r2()[last(which(grepl("Signature", r2()[["word"]])))-2,]},
                      r2()[last(which(grepl("Signature", r2()[["word"]])))-3,],
                      r2()[last(which(grepl("Signature", r2()[["word"]])))-1,],
                      sep="-"
                    )
                  } else{""} # ask the QA to fill in the date when signing
        )
      } else if(input$type=="Label Proof"){
        textInput("date", label = "Date",
                  value = 
                    paste(
                      as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+2,]),
                      as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+3,]),
                      as.character(r1()[which(grepl("CUSTOMER", r1()[["word"]]))+4,]),
                      sep="-"
                    )
        )
      } else if(input$type=="Links Report"){
        textInput("date", label = "Date",
                  value = paste(
                    r1()[which(r1()[["word"]] %in% month.abb)[4]-1,],
                    r1()[which(r1()[["word"]] %in% month.abb)[4],],
                    r1()[which(r1()[["word"]] %in% month.abb)[4]+1,],
                    sep="-"
                  )
                  
        )
      } else if(input$type=="Randomization Report"){
        textInput("date", label = "Date",
                  value = paste(
                    r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4]-1,],
                    r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4],],
                    r1()[which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))[4]+1,],
                    sep="-"
                  )
                  
        )
      } else if(input$type=="Medication Kit List Specification"){
        textInput("date", label = "Date",
                  value = paste(
                    r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")))-1,],
                    r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))),],
                    r1()[last(which(r1()[["word"]] %in% c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")))+1,],
                    sep="-"
                  )
                  
        )
      }
    })
    
    # vial or kit
    output$vial_kit = renderUI({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                    selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))>=1){
                      "Vial"
                    } else{"Kit"}
        )
      } else if (input$type=="Label Proof Request"){
        selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                    selected = if(length(which(grepl("PRIMARY", r1()[["word"]])))>=1){
                      "Vial"
                    } else{"Kit"}
        )
      } else if (input$type=="Label Proof") {
        selectInput("vial_kit", label = "Vial or Kit?", choices = c("Kit","Vial",""),
                    selected = if(as.character(r1()[which(grepl("Container", r1()[["word"]]))[1]+2,])=="Vial"){
                      "Vial"
                    } else{"Kit"}
        )
      }
      
    })
    
    # put info into df and display
    
    df2 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = input$spec_id,
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = "",
          reg_qa = ""
        )
      } else if (input$type=="Pack and Label Plan"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if (input$type=="Label Proof Request"){
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = input$label_template_id,
          reg_qa = input$reg_qa
        )
      } else if(input$type=="Label Proof") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = input$vial_kit,
          label_template_id = input$label_template_id,
          reg_qa = ""
        )
      } else if(input$type=="Links Report") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = input$lot_number,
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if(input$type=="Randomization Report") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      } else if(input$type=="Medication Kit List Specification") {
        tibble(
          pdf_name = "0.pdf",
          protocol = input$trial_number,
          report_type = input$type,
          label_spec_id = "",
          pmd_lot_number = "",
          date = input$date,
          vial_kit = "",
          label_template_id = "",
          reg_qa = ""
        )
      }
    })
    
    df3 = reactive({
      if(input$type=="Label Specification Approval Form (ie variable text approval)"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Specification Approval",
          df2()[["label_spec_id"]],df2()[["pmd_lot_number"]],
          df2()[["vial_kit"]],df2()[["date"]],
          ".pdf"
        ))
      } else if (input$type=="Pack and Label Plan"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"PL Plan", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Label Proof Request"){
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Proof Request", df2()[["label_template_id"]],
          paste(df2()[["reg_qa"]], df2()[["vial_kit"]], sep = "_"),
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Label Proof") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Label Proof", df2()[["label_template_id"]],
          df2()[["vial_kit"]],df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Links Report") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Links Report", df2()[["pmd_lot_number"]],
          df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Randomization Report") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Sub Rand", df2()[["date"]], ".pdf"
        ))
      } else if (input$type=="Medication Kit List Specification") {
        mutate(df2(), pdf_name=paste(
          df2()[["protocol"]],"Kit List Specification", df2()[["date"]], ".pdf"
        ))
      }
    })
    
    # display the dataframe (will be deleted once live)
    output$test <- renderReactable({
      reactable(df3())
    })
    
    output$name_table <- renderText({
      df3()[["pdf_name"]]
    })
    
  })
  
  
  
  
  
  
})

shinyApp(ui = ui, server = server)
