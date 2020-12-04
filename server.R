# Server functions

server <- function(input, output, session) {
  
  # user session$userData to store user data that will be needed throughout
  # the Shiny application
  session$userData$db_trigger <- reactiveVal(0)
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$signup, {
    if(saveData(input)) {
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      resetForm(session)      
    }
  })
  
  # Load participants each time data changes (using db_trigger variable to indicate it)
  participants <- reactive({
    session$userData$db_trigger() # Reactive to update when data updates.
    out <- loadData()
    out
  })
  
  # Show the  participants in a reactive table ----
  output$participants <- renderDT({
    out <- participants()
    datatable(out[ , c("first_name", "last_name", "department", "email")], 
              rownames = FALSE,
              colnames = c("First Name", "Last Name", "Department", "Email Address")) 
  })
  
  
  #---- Admin tab functions ----
  manage_participants_prep <- reactiveVal(NULL)
  
  observeEvent(session$userData$db_trigger(), {
    out <- participants()
    
    #Create action buttons
    emails <- out$email
    actions <- purrr::map_chr(emails, function(email) {
      paste0(
        '<button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', email, ' style="margin: 0"><i class="fa fa-trash-alt"></i></button>'
      )
    })
    
    # Set the Action Buttons row to the first column of the `risks` table
    out <- cbind(
      tibble(" " = actions),
      out
    )
    
    if (is.null(manage_participants_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      manage_participants_prep(out)
    } else {
      # manually hide the tooltip from the row so that it doesn't get stuck
      # when the row is deleted
      shinyjs::runjs("$('.btn-sm').tooltip('hide')")
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(manage_participants_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
  })
  
  output$manage_participants <- renderDT({
    req(manage_participants_prep())
    out <- manage_participants_prep()
    
    datatable(
      out,
      rownames = FALSE,
      colnames = c('First Name', 'Last Name', 'Email', 'Department', 'Signup Date'),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      extensions = c("Buttons"),
      options = list(
        scrollX = TRUE,
        dom = 'Bftip',
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("participants-", Sys.Date()),
            exportOptions = list(
              columns = 1:(length(out) - 1)
            )
          )
        ),
        columnDefs = list(
          list(targets = 0, orderable = FALSE)
        )
      )
    )
    
  })
  
  manage_participants_proxy <- DT::dataTableProxy('manage_participants')
  
  participant_to_delete <- eventReactive(input$participant_email_to_delete, {
    participants() %>%
      filter(email == input$participant_email_to_delete) %>%
      as.list()
  })
  
  callModule(
    participant_delete_module,
    "delete_participant",
    modal_title = "Delete Participant",
    participant_to_delete = participant_to_delete,
    modal_trigger = reactive({input$participant_email_to_delete})
  )
  

  observeEvent(input$pair, {
    
    out <- participants()
    
    out <- match_people(out, avoid_same = "department")
    
    output$meetings_table <- renderDT({
      
      # Number of participants
      n <- length(grep("V", names(out)))

      datatable(
        out[ , c("meeting", "first_name", "last_name", "department", "email", paste0("V", 1:n)), with=F],
        rownames = FALSE,
        # colnames = c("Meeting", "First Name", "Last Name", "Department", "Email Address", paste0("Colleage", 1:n)),
        selection = "none",
        class = "compact stripe row-border nowrap",
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          dom = 'Bftip',
          buttons = "excel"
        )
      )
    })
    
  })
  

}