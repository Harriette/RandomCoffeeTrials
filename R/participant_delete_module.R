require(shinyFeedback)


participant_delete_module <- function(input, output, session, modal_title, participant_to_delete, modal_trigger) {
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {

    # Authorize who is able to access particular buttons (here, modules)
    #******** DON'T LIKE FAILURE WITHOUT MESSAGE - SHOULD FIX ********************
#    req(session$userData$email == 'myname@me.com')
  
    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2('Are you sure you want to remove the following participant?'),
          h2(participant_to_delete()$email)
        ),
        br(),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("delete_button"),
            "Delete Participant",
            class = "btn-danger",
            style = "color: #FFF;"
          )
        )
      )
    )
  })
  
  
  
  observeEvent(input$delete_button, {
    req(modal_trigger())
    
    removeModal()
    
    hold <- participant_to_delete()

    tryCatch({    
      data <- loadData()
      data <- data %>%
        filter(email != hold$email)
      saveRDS(object = data, 
              file = file.path(saveDir, dataFile))
      
      session$userData$db_trigger(session$userData$db_trigger() + 1)
      showToast("success", "Risk Successfully Deleted")
    }, error = function(error) {
      
      showToast("error", "Error Deleting Risk")
      
      print(error)
    })   
    
  })
}