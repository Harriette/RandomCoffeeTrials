# UI Functions

ui <- bootstrapPage(
  
  includeCSS("www/app.css"),
  
  useShinyalert(),
  shinyjs::useShinyjs(),
  
  tags$nav(class="navbar",
           a(class="navbar-brand", href="#",
             img(src = "UKPnI.png", height='20px') 
           )
  ),
  
  div(class="container",
      div(class="jumbotron", id="test",
          div(class="container content",
              h1(class="display-1", "Randomised Coffee Trials")
          )
      ),
      
      
      shiny::tabsetPanel(id="my_tabs",
                         shiny::tabPanel(title = "Sign Up",
                                         
                                         div(class="col-sm-12 col-md-6 col-lg-6",
                                             div(id = "more",
                                                 br(),
                                                 p("Randomised Coffee Trials is our initiative to help people from across the UK Club meet, talk and share ideas."),
                                                 p("Sign up below and each month you will be paired with a new, ‘random’ person from the UK Club to meet over coffee (alternative drinks are available!).  The algorithm will try to pair you with someone from outside of your team if possible."),
                                                 p("You can talk about whatever you want over these coffees. They’re about making new friends, sharing ideas, and developing a better understanding of different teams and roles."),
                                                 p("Sign up and we’ll e-mail you your new pairing every month. We’ll leave you to organise your meet up sometime over the following month, after which we’ll send out your next pairing."),
                                                 p("There are no requirements regarding the topics discussed and you don’t have to report back (although any feedback is welcome), just enjoy the chance to meet and talk with someone new!"),
                                                 p("For any queries, or to stop participating, please email:",
                                                   a( href="mailto:randomcoffee@thomasmiller.com", "randomcoffee@thomasmiller.com")
                                                 )
                                             ),
                                             hr()
                                         ),
                                         
                                         div(class="col-sm-12 col-md-6 col-lg-6",
                                             br(),
                                             div(id = "form", class="panel panel-default",
                                                 div(class="panel-heading", 
                                                     h4(
                                                       icon("pen-nib", class = NULL, lib = "font-awesome"),
                                                       "  Sign up now...")
                                                 ),
                                                 div(class="panel-body",
                                                     textInput("first_name", label = "First name"),
                                                     textInput("last_name", label = "Last name"),
                                                     textInput("email", label = "Email address"),
                                                     selectInput("department", label = "Team", choices = departments),
                                                     actionButton("signup", "I'm in!", class = "btn-primary btn-lg")
                                                 )
                                             )
                                         )
                         ),
                         
                         shiny::tabPanel(title =  "Participants", 
                                         br(),
                                         DT::DTOutput("participants") 
                         ),
                         
                         shiny::tabPanel(title =  "Admin", 
                                         br(),
                                         passwordInput("password", "Enter password"),
                                         br(),
                                         br(),
                                         conditionalPanel(
                                           condition = paste0("input.password == '", admin_password, "'"), 
                                           h2("Manage Participant List"),
                                           br(),
                                           DTOutput("manage_participants"),
                                           br(),
                                           h2("Organise Meetings"),
                                           br(),
                                           actionButton("pair", "Pair up participants", class = "btn-primary btn-lg"),
                                           br(),
                                           br(),
                                           DTOutput("meetings_table"),
                                           br(),
                                           
                                           tags$script(src = "manage_participants.js"),
                                           tags$script(paste0("manage_participants_js('')"))
                                           
                                         )                         
                        )
                         

                         
                         
      )
      
      
  )
  
  
)



