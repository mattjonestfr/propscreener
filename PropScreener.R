library(shiny)
library(googlesheets4)
library(dplyr)
library(reshape2)
library(odds.converter)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(magick)
library(reactablefmtr)
library(png)
library(grid)
library(gt)
library(gtable)
library(shinythemes)
#### lfakf; ####
gs4_auth(cache = ".secrets")
options(gargle_oauth_email = "mattjonestfr@gmail.com")
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

# pull in sheet from Google

# pull in archives
karc <- read_sheet(sheet = "KArc",
                   ss = "https://docs.google.com/spreadsheets/d/1mqQzr18_4HhGzBcdElbsifi-3p8i-5OV4nfQ1AidAQ4/edit#gid=616461185")

karc$Player <- iconv(karc$Player, to='ASCII//TRANSLIT')
karc$Player <- gsub("'", '', karc$Player)
karc$`Prop Type` <- "Strikeouts"

karc <- karc %>%
  arrange(DateId) %>%
  filter(!is.na(Result)) %>%
  group_by(Player) %>%
  select("DateId" = DateId, "Player" = Player, "Team" = Team, "Opp" = Opp, `Prop Type` , "Line" = `PB Line`, "Over Price" = `O$...21`, "Under Price" = `U$...22`,
         Actual, Result) %>%
  mutate(Order = dense_rank(DateId))

# make opponent df
opp <- karc %>%
  select(DateId, Opp, Line, `Over Price`, `Under Price`, Actual, Result, Order)

opp$Result <- ifelse(is.na(opp$Line), "-", opp$Result)

team <- read_sheet(sheet = "Team",
                   skip = 0,
                   ss = "https://docs.google.com/spreadsheets/d/1mqQzr18_4HhGzBcdElbsifi-3p8i-5OV4nfQ1AidAQ4/edit#gid=616461185")

team <- team %>% 
  select("Team" = `Home Team`, Abbreviation) %>%
  filter(Team != "Cleveland Indians")

# pull in sheet from Google
pb <- read_sheet(sheet = "TeamSheet",
                 skip = 1,
                 ss = "https://docs.google.com/spreadsheets/d/1mqQzr18_4HhGzBcdElbsifi-3p8i-5OV4nfQ1AidAQ4/edit#gid=616461185")

# rename duplicated columns
pb <- pb %>%
  select("Timestamp" = `...2`, "Player Name" = `Player Name`,`Prop Type`,
         "Line" = ...7, "Over" = ...8, "Under" = ...9, "Team" = TEAM, "Opponent" = OPP) %>%
  filter(`Prop Type` == "Strikeouts(231)")

pb$`Player Name` <- iconv(pb$`Player Name`, to='ASCII//TRANSLIT')
pb$`Player Name` <- gsub("'", '', pb$`Player Name`)


# split time and date
pb$Date <- as.Date(pb$Timestamp)
pb$Time <- format(as.POSIXct(pb$Timestamp), 
                  format = "%H:%M:%S")

#add today's date for filtering
today <- Sys.Date()

pitchers <- karc

pitchers <- pitchers %>% select("Name" = Player, Team)

pitchers <- pitchers[!duplicated(pitchers$Name) , ]

#### ####
ui <- shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  fluidRow(
    
    # Application title
    titlePanel("Prop Screener"),
    
    # Show a plot of the generated distribution
    wellPanel(fluidRow(
      column(
        width = 12,
        selectizeInput("player",
                       "Player:",
                       choices = pb$`Player Name`)
      ),
      
      hr(),
      

      column(
        width = 12,
        plotOutput("main")
        ),
      column(
        width = 4,
        gt_output("data"),
      ),
      column(
        width = 8,
        gt_output("opp")
      )
      
    )
    )
  )))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectizeInput(session,"player", options = list(), selected = sample(pb$`Player Name`,1))
  
  output$main <- renderPlot({
    
    filt.res <- karc %>%
      filter(Player == input$player)
    
    filt.res$`Over Price` <- as.numeric(filt.res$`Over Price`)
    filt.res$`Under Price` <- as.numeric(filt.res$`Under Price`)
    filt.res$`Over Price` <- round(filt.res$`Over Price`, 0)
    filt.res$`Under Price` <- round(filt.res$`Under Price`, 0)
    
    
    pb$Time <- format(as.POSIXct(pb$Timestamp), 
                      format = "%H:%M:%S")
    
    filt.pb <- pb %>%
      filter(`Player Name` == input$player)
    
    max_time <- max(filt.pb$Time)
    
    filt.pb <- filt.pb %>% 
      filter(Time == max_time)
    
    current_line <- filt.pb$Line
    over <- odds.dec2us(filt.pb$Over)
    over_fix <- as.numeric(over)
    over_fix <- round(over_fix, 0)
    over_fix <- ifelse(over_fix > 0, paste0("+",over_fix), over_fix)
    under <- odds.dec2us(filt.pb$Under)
    under_fix <- as.numeric(under)
    under_fix <- round(under_fix, 0)
    under_fix <- ifelse(under_fix > 0, paste0("+",under_fix), under_fix)

    
    p <- filt.res %>%
      ggplot(aes(x = Order)) + 
      xlab(label = "Start #") +
      geom_col(aes(y=Actual, fill = Result)) +
      geom_label(aes(y=0, label = paste0("Closing Price - Over: ",`Over Price`, " / Under: ", `Under Price`))) +
      scale_fill_manual(values = c("Over" = "green", "Under" = "red")) +
      geom_hline(yintercept = current_line) +
      ggtitle(label = paste0(input$player," Results for 2022"),
              subtitle = paste0("Current Line: ", current_line, " | Over :", over_fix,
                                " Under: ", under_fix)) +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(face = "bold", size = 15, hjust = 0.5),
            axis.title = element_text(size = 8, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 8, face = "bold", hjust = 0.5),
            strip.text.x = element_text(size = 12, face = "italic", hjust = 0.5),
            legend.position = "none") 
    
    p
    
    # add logo
    # ggdraw() +
    #   draw_image("https://bettheprop.com/wp-content/uploads/2018/08/cropped-WebBannersmall-6.png",
    #              scale = 1,
    #              halign = 1,
    #              valign = 1) +
    #   draw_plot(p)
    
    
    
  })
  
  output$data <- render_gt({
    
    filt.res <- karc %>%
      filter(Player == input$player) %>%
      ungroup() %>%
      select(Opp, Line, `Over Price`, `Under Price`, Actual, Result)
    
    filt.res$`Over Price` <- round(filt.res$`Over Price`, 0)
    filt.res$`Under Price` <- round(filt.res$`Under Price`, 0)
    
    filt.res %>%
      gt() %>%
      tab_header(title = paste(input$player, " Results")) %>%
      cols_align(align = "center") %>%
      data_color(
        columns = c(`Over Price`, `Under Price`),
        colors = scales::col_numeric(
          # custom defined values - notice that order matters!
          palette = c("#93ccea", "#007AFF"),
          domain = NULL)) %>%
      data_color(
        columns = Result,
        colors = scales::col_factor(
          # custom defined values - notice that order matters!
          palette = c("white", "#BD2B2B",  "#29891A"),
          domain = c("Over", "Under", "-")))
  })
  
  output$opp <- render_gt({
    
    filt.pb <- pb %>%
      filter(`Player Name` == input$player)
    
    pb_opponent <- filt.pb$Opponent
    pb_opponent <- as.data.frame(pb_opponent)
    pb_opponent <- pb_opponent[!duplicated(pb_opponent$pb_opponent) , ]
    
    opp_gt <- opp %>%
      filter(Opp == pb_opponent) %>%
      arrange(desc(DateId)) %>%
      ungroup() 
    
    opp_gt$Order <- NULL
    opp_gt$DateId <- NULL
    
    
    opp_gt$`Under Price` <- round(opp_gt$`Under Price`, 0)
    opp_gt$`Over Price` <- round(opp_gt$`Over Price`, 0)
    
    opp_gt %>%
      gt() %>%
      tab_header(title = "Opponent Result") %>%
      cols_align(align = "center") %>%
      data_color(
        columns = c(`Over Price`, `Under Price`),
        colors = scales::col_numeric(
          # custom defined values - notice that order matters!
          palette = c("#93ccea", "#007AFF"),
          domain = NULL)) %>%
      data_color(
        columns = Result,
        colors = scales::col_factor(
          # custom defined values - notice that order matters!
          palette = c("white", "#BD2B2B",  "#29891A"),
          domain = c("Over", "Under", "-")))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
