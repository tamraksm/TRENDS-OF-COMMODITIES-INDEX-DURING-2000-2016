###############################################
###############################################
###############################################
# A shiny app always needs shiny
library(shiny)

# Load any support libraries and data outside of shinyServer function
library(tidyverse)
library(gtable)
library(grid)
library(shinythemes)

### Define UI for application
ui <- fluidPage(theme = shinytheme("united"),
                # Application title
                titlePanel(title = "Behaviour of Different Commodities/Index"),
                sidebarLayout(
                  
                  # Sidebar typically used to house input controls
                  sidebarPanel(
                    radioButtons("inputid", label = "Select Your Commodity/Index",
                                 choices = c("","Gold", "Crude", "Dollar", "SNP500"),selected=NULL),
                    
                    checkboxInput("rline", "Show Recession Line", TRUE),
                    
                    selectInput(inputId = "var1",
                                label = "What do you like to compare?",
                                choices = c("None","Gold & Crude", "Gold & Dollar", "Gold & SNP500", "Crude & Dollar",
                                            "Crude & SNP500", "Dollar & SNP500")),
                    br(),
                    h3("COLOR INFORMATION"),
                    h4("GOLD : ORANGE" ,style = "color:orange"),
                    h4("CRUDE OIL: RED",style = "color:red"),
                    h4("DOLLAR INDEX : GREEN",style = "color:green"),
                    h4("SNP 500 : BLUE",style = "color:blue")
                  ),
                  
                  # Main panel typically used to display outputs
                  mainPanel(
                    plotOutput(outputId = "Pl1")
                  )
                  
                )
)


### Define server behavior for application here and make plots for each of the choices
server <- function(input, output) {
  
  pl1<- ggplot()+
    geom_line(aes(x=Date,y=Price),color="orange",data=gold_final,size=1)+
    labs(title = "Weekly Gold Price from 2000 - 2016 ",
         x = "Years", y = "Price of Gold ") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_y_continuous(limit = c(0, 2000))+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 17),
          title = element_text(size = 20))
  
  pl2<- ggplot()+
    geom_line(aes(x=Date,y=Price),color="red",data=crude_final,size=1)+
    labs(title = "Weekly Crude Oil Price from 2000 - 2016 ",
         x = "Years", y = "Price of Crude Oil ") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_y_continuous(limit = c(0, 150))+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 17),
          title = element_text(size = 20))
  
  pl3<-ggplot()+
    geom_line(aes(x=Date,y=Price),color="green",data=dollar_final,size=1)+
    labs(title = "Weekly US Dollar Index Price from 2000 - 2016 ",
         x = "Years", y = "Price of US Dollar Index ") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_y_continuous(limit = c(50, 150))+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 17),
          title = element_text(size = 20))
  
  pl4<- ggplot()+
    geom_line(aes(x=Date,y=Price),color="blue",data=snp_final,size=1)+
    labs(title = "Weekly SNP 500 Index from 2000 - 2016 ",
         x = "Years", y = "Price of SNP 500 Index ") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_y_continuous(limit = c(500, 2500))+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 17),
          title = element_text(size = 20))
  
  
  
  output$Pl1 <- renderPlot({
    if(input$inputid == "Gold" & input$rline == FALSE){
      
      pl1
      
    }else if (input$inputid == "Crude" & input$rline == FALSE) {
      
      pl2
      
    }else if (input$inputid == "Dollar" & input$rline == FALSE) {
      
      pl3
      
    }else if (input$inputid == "SNP500" & input$rline == FALSE) {
      
      pl4 
      
    }else if (input$inputid == "Gold" & input$rline == TRUE) {
      ql1<- pl1+
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)
      ql1
      
    }else if (input$inputid == "Crude" & input$rline == TRUE) {
      ql2<- pl2+
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)
      ql2
      
    }else if (input$inputid == "Dollar" & input$rline == TRUE) {
      ql3<- pl3+
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)
      ql3
      
    }else if (input$inputid == "SNP500" & input$rline == TRUE) {
      
      ql4<- pl4+
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)
      ql4
      
    }else if ( input$var1== "Gold & Crude" ){
      p1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="orange",data=gold_final,size=1)+
        labs(title = "Weekly Price of Gold and Crude from 2000 - 2016 ",
             x = "Years", y = "Price") +
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(0, 2000))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      f1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="red",data=crude_final,size=1)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(0, 150))+
        theme(plot.title = element_text(hjust = 0.5))%+replace% 
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      g1 <- ggplot_gtable(ggplot_build(p1))
      g2 <- ggplot_gtable(ggplot_build(f1))
      
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      grid.draw(g)
      
      
    }else if ( input$var1== "Gold & Dollar" ){
      p1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="orange",data=gold_final,size=1)+
        labs(title = "Weekly Price of Gold and Dollar from 2000 - 2016 ",
             x = "Years", y = "Price") +
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(0, 2000))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      f1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="green",data=dollar_final,size=1)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(50, 150))+
        theme(plot.title = element_text(hjust = 0.5))%+replace% 
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      g1 <- ggplot_gtable(ggplot_build(p1))
      g2 <- ggplot_gtable(ggplot_build(f1))
      
      
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      grid.draw(g)
      
      
    }else if ( input$var1== "Gold & SNP500" ){
      p1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="orange",data=gold_final,size=1)+
        labs(title = "Weekly Price of Gold and SNP500 from 2000 - 2016 ",
             x = "Years", y = "Price") +
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(0, 2000))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      f1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="blue",data=snp_final,size=1)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(500, 2500))+
        theme(plot.title = element_text(hjust = 0.5))%+replace% 
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      g1 <- ggplot_gtable(ggplot_build(p1))
      g2 <- ggplot_gtable(ggplot_build(f1))
      
      
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      
      grid.draw(g)
      
    }else if ( input$var1== "Crude & Dollar" ){
      p1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="red",data=crude_final,size=1)+
        labs(title = "Weekly Price of Crude and Dollar from 2000 - 2016 ",
             x = "Years", y = "Price") +
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(0, 150))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      f1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="green",data=dollar_final,size=1)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(50, 150))+
        theme(plot.title = element_text(hjust = 0.5))%+replace% 
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      g1 <- ggplot_gtable(ggplot_build(p1))
      g2 <- ggplot_gtable(ggplot_build(f1))
      
      
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      grid.draw(g)
      
      
    }else if ( input$var1== "Crude & SNP500" ){
      p1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="red",data=crude_final,size=1)+
        labs(title = "Weekly Price of Crude and SNP500 from 2000 - 2016 ",
             x = "Years", y = "Price") +
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(0, 150))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      f1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="blue",data=snp_final,size=1)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(500, 2500))+
        theme(plot.title = element_text(hjust = 0.5))%+replace% 
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      g1 <- ggplot_gtable(ggplot_build(p1))
      g2 <- ggplot_gtable(ggplot_build(f1))
      
      
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      
      grid.draw(g)
      
      
    }else if ( input$var1== "Dollar & SNP500" ){
      p1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="green",data=dollar_final,size=1)+
        labs(title = "Weekly Price of Dollar and SNP500 from 2000 - 2016 ",
             x = "Years", y = "Price") +
        geom_vline(aes(xintercept=as.numeric(gold_final$Date[c(414,496)])),
                   linetype=4, colour="brown",size=2)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(0, 150))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      f1<- ggplot()+
        geom_line(aes(x=Date,y=Price),color="blue",data=snp_final,size=1)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        scale_y_continuous(limit = c(500, 2500))+
        theme(plot.title = element_text(hjust = 0.5))%+replace% 
        theme(panel.background = element_rect(fill = NA),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 17),
              title = element_text(size = 20))
      
      
      g1 <- ggplot_gtable(ggplot_build(p1))
      g2 <- ggplot_gtable(ggplot_build(f1))
      
      
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      
      grid.draw(g)
      
      
    }
  })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)
