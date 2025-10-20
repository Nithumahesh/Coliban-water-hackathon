library(shiny)
library(shinyquiz)
library(shinyauthr)
library(tibble)
library(sodium)
library(bslib)
library(shinyWidgets)

sm_ui_quiz_complete_no_restart <- function(store, ns, messages) {
  # verify
  shinyquiz:::verify_messages_structure(messages)
  # determine which message to show
  all_correct <- shinyquiz:::sm_is_all_correct(store)
  is_skipped <- shinyquiz:::sm_get_state(store, variable = "quiz-skipped")

  if (is_skipped) {
    html_content <- htmltools::tagList(
      htmltools::br(),
      shinyquiz:::add_message_skipped(messages@message_skipped)
    )
  } else if (all_correct) {
    html_content <- htmltools::tagList(
      htmltools::br(),
      shinyquiz:::add_message_correct(messages@message_correct),
      shinyquiz:::add_confetti()
    )
  } else {
    html_content <- htmltools::tagList(
      htmltools::br(),
      shinyquiz:::add_message_wrong(messages@message_wrong)
    )
  }

  # render the grade / report
  grade_report <- shinyquiz:::sm_ui_complete_report(store)

  # combine — note here we **do not** add the restart button

  html_content <- htmltools::tagList(
    html_content,
    grade_report,
    htmltools::br(), htmltools::br(), htmltools::hr(), htmltools::br()
  )

  html_content

}

# After loading shinyquiz, override the internal function

# So that when the package calls it, it uses ours

environment(sm_ui_quiz_complete_no_restart) <- asNamespace("shinyquiz")
assignInNamespace("sm_ui_quiz_complete", sm_ui_quiz_complete_no_restart, ns = "shinyquiz")

# Source your quiz definition

source("quiz_hackathon.R")

# User base

user_base <- data.frame(
  user = c("Amy", "Jake"),
  password = c("pass1", "pass2"),
  permissions = c("admin", "user"),
  stringsAsFactors = FALSE
)

users <- data.frame(
  name = c("Amy", "Jake", "Nithu", "Peter"),
  quiz1_score = c(50, 10, 80, 60),
  quiz2_score = c(60, 30, 90, 80),
  quiz3_score = c(80, 40, 70, 90),
  total_score = c(190, 80, 240, 230), # Sum of all quiz scores
  max_score = c(300, 300, 300, 300)    # Total possible score (100 per quiz)
)

# UI

ui <- fluidPage(
  tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
  tags$head(
    tags$title("IWN Hackathon 2025"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      * { font-family: Arial, Helvetica, sans-serif; }

      .link-buttons a {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        gap: 8px;
        margin-bottom: 12px;
        font-size: 2rem;
        border-radius: 6px;
        box-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }

      .link-buttons a .fa { font-size: 1.4em; }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus,

      .navbar .nav-link.active { color: #0d6efd !important; }

      .quiz-container {
        background-color: #f0f8ff;
        border-radius: 10px;
        padding: 30px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        max-width: 800px;
        margin: auto;
        font: 'Bree Serif Regular';
      }

      button {
        background-color: #6c5ce7;
        color: white;
        border: none;
        padding: 10px 20px;
        margin-top: 20px;
        border-radius: 5px;
        font-size: 16px;
      }

      button:hover {
        background-color: #5a4bcf;
      }

      #logout button {
        background-color: #e74c3c;
        padding: 6px 12px;
        font-size: 14px;
        border-radius: 4px;
        color: white;
      }

      #logout button:hover {
        background-color: #c0392b;
      }

     .top-bar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 10px 20px;
        background-color: #f5f5f5;
        border-bottom: 1px solid #ddd;
        margin-bottom: 10px;
      }
 
.icon-btn {
        font-size: 20px;
        color: #333;
      }

      .usage-stats {
        text-align: center;
        font-size: 14px;
        margin-top: 10px;
      }

      .toggle-btns {
        display: flex;
        flex-direction: column;
        align-items: center;
        margin-top: 30px;
        padding: 0 20px;
      }

      .switch-row {
        width: 90%;
        max-width: 350px;
        margin-bottom: 15px;
      }

      .bootstrap-switch {
        width: 100% !important;
        display: flex !important;
        justify-content: space-between !important;
      }

      .bootstrap-switch .bootstrap-switch-label {
        flex-grow: 1;
        text-align: left;
      }

    "))

  ),

  # Login UI

  shinyauthr::loginUI(id = "login"),

  uiOutput("navbarPage"),

  conditionalPanel(
    condition = "input.main_tabs == 'quiz'",  # Ensure quiz content only shows in quiz tab
    uiOutput("quiz_content")
  ))

# Server

server <- function(input, output, session) {

  user_progress <- reactiveValues()

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  # Start quiz

  output$navbarPage <- renderUI({

    req(credentials()$user_auth)

    if  (credentials()$info$user == "Amy") {

    tagList(
      navbarPage(
        id = "main_tabs",
        title = NULL,
        tabPanel(

          title = "", value = "home",
          icon = icon("home", class = "fa-1x", title = "Home"),
          actionButton("logout", "Logout", class = "btn btn-danger"),
          tags$div(style = "text-align:center; margin-top:20px;",
                   tags$img(src = "avatar.png", style = "width:120px;border-radius:12px;"),
                   br(),
                   tags$div(style = "font-size: 35px; font-style: bold;",
                            p(paste("Welcome ", credentials()$info$user, "!"))),
                   br(),
                   actionButton("btn_profile", "Profile", class = "btn-primary btn-lg btn-block"),
                   br(),
                   actionButton("btn_usage", "Usage", class = "btn-primary btn-lg btn-block"),
                   br(),
                   actionButton("btn_quiz", "Quiz", class = "btn-primary btn-lg btn-block"),
                   br(),
                   actionButton("btn_contact", "Contact", class = "btn-primary btn-lg btn-block")
          )
        ),

        tabPanel(

          title = "", value = "profile",
          icon = icon("user", class = "fa-1x", title = "User Profile"),
          tags$div(style="text-align:center; margin-top:20px;", tags$img(src = "Progress-Bar.png", style = "width:500px;border-radius:12px;")),
          tags$div(style = "text-align:center; margin-top:20px;",
                   tags$img(src = "avatar.png", style = "width:200px;border-radius:12px;"),
                   br(),
                   tags$div(style = "font-size: 35px; font-style: bold;",
                            p(paste(credentials()$info$user))),
                   page_fillable(
                     layout_columns(
                       tags$div(tags$img(src = "Button-level-blue.png", style = "width: 200px;border-radius:12px;")),
                       tags$div(tags$img(src = "Button-use-grey.png", style = "width: 200px;border-radius:12px;")),
                       tags$div(tags$img(src = "Button-friends-grey.png", style = "width: 200px;border-radius:12px;")),
                       tags$div(tags$img(src = "Button-streak-blue.png", style = "width:200px;border-radius:12px;"))
                     )
                   ),

                   br())
        ),

        tabPanel(

          title = "", value = "usage",
          icon = icon("chart-line", class = "fa-1x", title = "Water consumption"),
          h3("October 2025", align = "center"),

          # Graph

          plotOutput("mockPlot", height = "280px"),

          # Usage stats

          div(class = "usage-stats",
              "Total use = 2971L (95.8 L/day)", tags$br(),
              "Predicted total = 4702L (151.7 L/day)"
          ),

          # Toggle switches - centered and wide

          div(class = "toggle-btns",
              div(class = "switch-row",
                  switchInput("actual", "Actual usage", value = TRUE, onLabel = "", offLabel = "")
              ),

              div(class = "switch-row",
                  switchInput("lastYear", "Same time last year", value = FALSE, onLabel = "", offLabel = "")
              ),

              div(class = "switch-row",
                  switchInput("rankings", "Water saving rankings", value = FALSE, onLabel = "", offLabel = "")
              )
        )),

        tabPanel(

          title = "", value = "quiz",
          icon = icon("circle-question", class = "fa-1x", title = "Water quiz"),
          h3("Weekly quiz", style = "color: #248bcb; text-align: center;"),
          tags$div(br(), br(),
                   actionButton("btn_quiz1", "Level 1",class = "btn btn-primary btn-lg btn-block" ),
                   br(),
                   actionButton("btn_quiz2", "Level 2",class = "btn btn-primary btn-lg btn-block" ),
                   br(),
                   actionButton("btn_quiz3", "Level 3",class = "btn btn-primary btn-lg btn-block" ),
                   br(),
                   actionButton("btn_quiz4", "Level 4",class = "btn btn-primary btn-lg btn-block" ),
                   br(),
                   actionButton("btn_quiz5", "Level 5",class = "btn btn-primary btn-lg btn-block" ))
        ),
 
        tabPanel(

          title = div(tags$img(src = "cw-logo.png", class = "cw-logo", height = 20, width = 20, style = "margin-right:5px;")),
          value = "about",
          h3("Contact & Links", style = "color: #248bcb; text-align: center;"),
          tags$div(br(),br(),
                   class = "link-buttons",
                   a(href = "https://coliban.com.au", class = "btn btn-primary btn-lg btn-block",
                     icon("globe"), " coliban.com.au"),
                   a(href = "https://connect.coliban.com.au", class = "btn btn-primary btn-lg btn-block",
                     icon("link"), " connect.coliban.com.au"),
                   a(href = "mailto:coliban@coliban.com.au", class = "btn btn-primary btn-lg btn-block",
                     icon("envelope"), " coliban@coliban.com.au"),
                   a(href = "tel:1300363200", class = "btn btn-primary btn-lg btn-block",
                     icon("phone"), " 1300 363 200"),
                   a(href = "https://www.facebook.com/ColibanWater", class = "btn btn-primary btn-lg btn-block",
                     icon("facebook"), " Coliban Water")
          )
        ),

        tabPanel(

          title = "", value = "admin",
          icon = icon("gear", class = "fa-1x", title = "Admin screen"),
          h3("Admin screen (demo only)")
        )
    )
    )
  }

    else {

      tagList(
        navbarPage(
          id = "main_tabs",
          title = NULL,
          tabPanel(
            title = "", value = "home",
            icon = icon("home", class = "fa-1x", title = "Home"),
            actionButton("logout", "Logout", class = "btn btn-danger"),
            tags$div(style = "text-align:center; margin-top:20px;",
                     tags$img(src = "Avatar2.png", style = "width:120px;border-radius:12px;"),
                     br(),
                     tags$div(style = "font-size: 35px; font-style: bold;",
                              p(paste("Welcome ", credentials()$info$user, "!"))),
                     br(),
                     actionButton("btn_profile", "Profile", class = "btn-primary btn-lg btn-block"),
                     br(),
                     actionButton("btn_usage", "Usage", class = "btn-primary btn-lg btn-block"),
                     br(),
                     actionButton("btn_quiz", "Quiz", class = "btn-primary btn-lg btn-block"),
                     br(),
                     actionButton("btn_contact", "Contact", class = "btn-primary btn-lg btn-block")
            )
          ),

          tabPanel(
            title = "", value = "profile",
            icon = icon("user", class = "fa-1x", title = "User Profile"),
            tags$div(style="text-align:center; margin-top:20px;", tags$img(src = "Progress-Bar.png", style = "width:500px;border-radius:12px;")),
            tags$div(style = "text-align:center; margin-top:20px;",
                     tags$img(src = "Avatar2.png", style = "width:200px;border-radius:12px;"),

                     br(),

                     tags$div(style = "font-size: 35px; font-style: bold;",
                              p(paste(credentials()$info$user))),

                     page_fillable(

                       layout_columns(
                         tags$div(tags$img(src = "Button-level-blue.png", style = "width: 200px;border-radius:12px;")),
                         tags$div(tags$img(src = "Button-use-grey.png", style = "width: 200px;border-radius:12px;")),
                         tags$div(tags$img(src = "Button-friends-grey.png", style = "width: 200px;border-radius:12px;")),
                         tags$div(tags$img(src = "Button-streak-blue.png", style = "width:200px;border-radius:12px;"))
                       )
                     ),

                     br())

          ),

          tabPanel(

            title = "", value = "usage",
            icon = icon("chart-line", class = "fa-1x", title = "Water consumption"),
            h3("This page will contain water consumption data")
          ),

          tabPanel(

            title = "", value = "quiz",
            icon = icon("circle-question", class = "fa-1x", title = "Water quiz"),
            h3("Weekly quiz", style = "color: #248bcb; text-align: center;"),
            tags$div(br(), br(),
                     actionButton("btn_quiz1", "Level 1",class = "btn btn-primary btn-lg btn-block" ),
                     br(),
                     actionButton("btn_quiz2", "Level 2",class = "btn btn-primary btn-lg btn-block" ),
                     br(),
                     actionButton("btn_quiz3", "Level 3",class = "btn btn-primary btn-lg btn-block" ),
                     br(),
                     actionButton("btn_quiz4", "Level 4",class = "btn btn-primary btn-lg btn-block" ),
                     br(),
                     actionButton("btn_quiz5", "Level 5",class = "btn btn-primary btn-lg btn-block" ))

          ),

          tabPanel(

            title = div(tags$img(src = "cw-logo.png", class = "cw-logo", height = 20, width = 20, style = "margin-right:5px;")),

            value = "about",

            h3("Contact & Links", style = "color: #248bcb; text-align: center;"),

            tags$div(br(),br(),
                     class = "link-buttons",
                     a(href = "https://coliban.com.au", class = "btn btn-primary btn-lg btn-block",
                       icon("globe"), " coliban.com.au"),
                     a(href = "https://connect.coliban.com.au", class = "btn btn-primary btn-lg btn-block",
                       icon("link"), " connect.coliban.com.au"),
                     a(href = "mailto:coliban@coliban.com.au", class = "btn btn-primary btn-lg btn-block",
                       icon("envelope"), " coliban@coliban.com.au"),
                     a(href = "tel:1300363200", class = "btn btn-primary btn-lg btn-block",
                       icon("phone"), " 1300 363 200"),
                     a(href = "https://www.facebook.com/ColibanWater", class = "btn btn-primary btn-lg btn-block",
                       icon("facebook"), " Coliban Water")
            )
          ),
          tabPanel(
            title = "", value = "admin",
            icon = icon("gear", class = "fa-1x", title = "Admin screen"),
            h3("Admin screen (demo only)")
          )
        )
      )
    }})

  observeEvent(input$btn_profile, {
    updateNavbarPage(session, inputId = "main_tabs", selected = "profile")
  })

  observeEvent(input$btn_usage, {
    updateNavbarPage(session, inputId = "main_tabs", selected = "usage")
  })

  observeEvent(input$btn_quiz, {
    updateNavbarPage(session, inputId = "main_tabs", selected = "quiz")
  })

  observeEvent(input$btn_contact, {
    updateNavbarPage(session, inputId = "main_tabs", selected = "about")
  })

  observeEvent(input$logout, {
    logout_init(TRUE)
  })

  observeEvent(input$btn_quiz1, {

    output$quiz_content <- renderUI(
      showModal(modalDialog(
        footer = modalButton("Close"),
        quiz_ui(quiz1),
        easyClose = TRUE
    )))

    quiz_server(quiz1)

    updateActionButton(session, "btn_quiz1", icon = icon('star'), disabled = TRUE)
  })

  observeEvent(input$btn_quiz2, {

    output$quiz_content <- renderUI(
      showModal(modalDialog(
        footer = modalButton("Close"),
        quiz_ui(quiz2),
        easyClose = TRUE
      )))

    quiz_server(quiz2)

    updateActionButton(session, "btn_quiz2", icon = icon('star'), disabled = TRUE)
  })
 
observeEvent(input$btn_quiz3, {

    output$quiz_content <- renderUI(
      showModal(modalDialog(
        footer = modalButton("Close"),
        quiz_ui(quiz3),
        easyClose = TRUE
      )))

    quiz_server(quiz3)

  updateActionButton(session ,"btn_quiz3", icon = icon('star'), disabled = TRUE)
  })

  output$mockPlot <- renderPlot({

    # Mock data for water usage

    bar_values <- c(150, 170, 160, 140, 155, 165, 175, 180, 190, 185)
    barplot(bar_values,
            col = "#3399ff",
            names.arg = paste0(seq(2, 20, 2), " Oct"),
            ylim = c(0, 250),
            ylab = "Litres/day",
            main = "Daily Water Usage")
  })
}

shinyApp(ui = ui, server = server)
 