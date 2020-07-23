library(devtools)
library(gganimate)
library(ggplot2)
library(shiny)
library(shinyanimate)

#
# Presidents
#
pres.df = read.table(textConnection(
  "name,       start,      end,       party,         inits
'Eisenhower', 1955-07-01, 1961-01-20, Republican,    'DDE'
'Kennedy',    1961-01-20, 1963-11-22, Democrat,      'JFK'
'Johnson',    1963-11-22, 1969-01-20, Democrat,      'LBJ'
'Nixon',      1969-01-20, 1974-08-09, Republican,    'RMN'
'Ford',       1974-08-09, 1977-01-20, Republican,    'GRF'
'Carter',     1977-01-20, 1981-01-20, Democrat,      'JEC'
'Reagan',     1981-01-20, 1989-01-20, Republican,    'RWR'
'BushSr',     1989-01-20, 1993-01-20, Republican,    'GHWB'
'Clinton',    1993-01-20, 2001-01-20, Democrat,      'WJC'
'BushJr',     2001-01-20, 2009-01-20, Republican,    'GWB'
'Obama',      2009-01-20, 2017-01-20, Democrat,      'BHO'
'Trump',      2017-01-20, 2019-02-01, Republican,    'DJT'"), sep=',',
  colClasses=c('character', 'Date', 'Date', 'factor', 'factor'), header=TRUE)

var_n<- c("GDP Growth", 
          "Top Tax", 
          "Debt/GDP", 
          "Fed Funds", 
          "Ch In Fed Funds",
          "Unemployment",
          "Ch In Top Tax", 
          "GDP Per Capita",
          "Forex")   

n_vars <- length(var_n)
var_list <- 1:n_vars
names(var_list) <- var_n
var_list_p <- var_list
var_list_p[["None"]] <- n_vars+1

#
# Read Data
#
debt    <- read.csv('/Users/nevinaltaras/Downloads/gdebttogdp.csv',       header=T)
# 1/1939 yearly
tax     <- read.csv('/Users/nevinaltaras/Downloads/toptaxrate.csv',       header=T)
# 1/1913 yeary
ch_tax  <- read.csv('/Users/nevinaltaras/Downloads/delta_toptaxrate.csv', header=T)
# 1/1914 yeary
ff      <- read.csv('/Users/nevinaltaras/Downloads/fedfunds.csv',         header=T)
# 7/1954 monthly
ch_ff   <- read.csv('/Users/nevinaltaras/Downloads/delta_fedfunds.csv',   header=T)
# 7/1955 monthly
gdp     <- read.csv('/Users/nevinaltaras/Downloads/gdprate.csv',          header=T)
#/1947 quarterly
capgdp  <- read.csv('/Users/nevinaltaras/Downloads/percapitagdprate.csv', header=T)
# 1/1948 quarterly
fx      <- read.csv('/Users/nevinaltaras/Downloads/forex.csv',            header=T)
# 1/1973 yearly
ur      <- read.csv('/Users/nevinaltaras/Downloads/UNRATE.csv',           header=T)
# /1/1948 monthly

#
# Latest Starting Series Ch Fed Funds, in 7/1955
# Clean up data: eliminate before 7/1955, interpolate to make monthly
#
pres_factor  <- factor(pres.df$name,  levels=unique(pres.df$name))
party_factor <- factor(pres.df$party, levels=unique(pres.df$party))

debt_cl   <-approx (as.Date(debt$DATE), debt$GFDGDPA188S, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
tax_cl    <-approx (as.Date(tax$DATE), tax$IITTRHB, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
ch_tax_cl <-approx (as.Date(ch_tax$DATE), ch_tax$IITTRHB_CH1, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
gdp_cl    <-approx (as.Date(gdp$DATE), gdp$GDPC1_PC1, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
ff_cl     <-approx (as.Date(ff$DATE), ff$FEDFUNDS, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
capgdp_cl <-approx (as.Date(capgdp$DATE), capgdp$A939RX0Q048SBEA_PC1, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
ur_cl     <-approx (as.Date(ur$DATE), ur$UNRATE, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
fx_cl     <-approx (as.Date(fx$DATE), fx$TWEXMANL, 
                           xout=as.Date(ch_ff$DATE),   method = "linear", rule=2)
pres_indx <-(approx (as.Date(pres.df$start), pres_factor,
                            xout=as.Date(ch_ff$DATE),  method = "constant", rule=2))$y
pres_cl   <- pres.df$name[pres_indx]
inits_cl  <- trimws(pres.df$inits[pres_indx])

party_indx<-(approx (as.Date(pres.df$start), party_factor, 
                            xout=as.Date(ch_ff$DATE),  method = "constant", rule=2))$y
party_cl     <- pres.df$party[party_indx]
#
# Make montly data from presidential periods
#
inp <- as_tibble(data.frame (
  "gdp"   =gdp_cl$y,
  "tax"   =tax_cl$y,
  "debt"  =debt_cl$y, 
  "ff"    =ff_cl$y,
  "ch_ff" =ch_ff$FEDFUNDS_CH1,
  "ur"    =ur_cl$y,
  "ch_tax"=ch_tax_cl$y,  
  "capgdp"=capgdp_cl$y,
  "fx"    =fx_cl$y,
  "date"  =as.Date(ch_ff$DATE),   
  "pres"  =pres_cl,
  "inits" =inits_cl,
  "party" =party_cl))
#
# Aggregate monthly data by President - for Presidential averages
#
inp$pres  <- factor(inp$pres, levels=unique(inp$pres))

agg_inp <- aggregate(inp, by=list(inp$pres), FUN=mean)
agg_pres <- bind_cols (pres.df, as.data.frame(agg_inp))
agg_pres <- transmute (agg_pres, gdp, tax, debt, ff, ch_ff, ur, ch_tax, capgdp, fx, 
                       name, inits, start, end, party)

#
# Functions
# Date Management
#
sub_data <- function (window_begin, window_end, inp){

  sub_inp <- subset (inp, date >=window_begin & date <= window_end)
  
  sub_agg <- subset (agg_pres, end >=window_begin & start <= window_end)
  sub_agg$start[1] <- window_begin
  sub_agg$end[length(sub_agg$end)] <- window_end
  
  sub_rec <- subset(recessions.df, Trough >= window_begin & Peak <= window_end)
  if (length(sub_rec[[1]])>0){
    if (sub_rec$Peak[1] < window_begin)
      sub_rec$Peak[1] <-window_begin
    
    if (sub_rec$Trough[length(sub_rec$Trough)] > window_end)
      sub_rec$Trough[length(sub_rec$Trough)] <-window_end
  }

  sub_list <- list (sub_inp=sub_inp, sub_agg=sub_agg, sub_rec=sub_rec)

  return (sub_list)
}
#
# Plot 1
#
plot1 <- function (var1_indx, var2_indx, rint, pint, sub_list){
  
  sub_inp <- sub_list$sub_inp
  sub_agg <- sub_list$sub_agg
  sub_rec <- sub_list$sub_rec
  
  colnames(sub_inp)[var1_indx] <- "var1"
  if (var2_indx < n_vars+1 & var2_indx != var1_indx)
    colnames(sub_inp)[var2_indx]   <- "var2"
  
  colnames(sub_agg)[var1_indx] <- "var1"   

  p1 <- ggplot (sub_inp) + theme_gray() + 
    geom_line(aes(x=date, y=var1/100), color="black") +
    geom_rect(data=sub_agg, aes (xmin=start, xmax=end, ymin=-Inf, ymax=+Inf), 
      fill=ifelse(trimws(sub_agg$party)=="Democrat", "blue", "red" ), alpha=pint) +
    geom_hline(yintercept=0,color="black", size=0.3) +
    geom_vline(data=sub_agg,                       aes(xintercept=start), size=0.2) +
    geom_vline(data=sub_agg[length(sub_agg$end),], aes(xintercept=end),   size=0.2) +
    geom_text(data=sub_agg, aes(x=start, y=-Inf, label=trimws(inits)),
      size = 4.5, vjust=1, hjust=0, nudge_x=0, check_overlap =TRUE, angle=90) +
    labs(y=var_n[var1_indx], title=paste("Historical ", var_n[var1_indx], "Rate")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    scale_y_continuous(labels=scales::percent)         

    if (length(sub_rec[[1]]>0)){
      p1 <- p1 + geom_rect(data=sub_rec,
        aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='black', alpha=rint) 
    }
  
#   Superimpose second variable if any
    if (var2_indx < n_vars+1 & var2_indx != var1_indx){
#     Rescale factor for second variable, second axis      
      div <- abs (max(sub_inp$var1)- min(sub_inp$var1))/abs (max(sub_inp$var2)- min(sub_inp$var2))
    
      p1 <- p1 + geom_line (aes(x=date, y=var1/100), color="blue") +
        geom_line (aes(x=date, y=var2/100*div), color="red") +
        labs(y=var_n[var1_indx], title=paste("Historical ", var_n[var1_indx], "and ", var_n[var2_indx], " Rates")) +
        theme(axis.title.y = element_text(color="blue"), axis.title.y.right=element_text(color="red")) +
        scale_y_continuous(labels=scales::percent,
          sec.axis = sec_axis(~ ./div, name=paste(var_n[var2_indx], " Rate"), labels=scales::percent)) 
    }
# Prepare animation
    a1 <- p1 +transition_reveal(date) + ease_aes('linear') +
      geom_line(aes(x= date, y=var1/100), color = ifelse(var2_indx==n_vars+1, "black", "blue"))
  
      return (list(p1=p1, a1=a1))
}
#
# Plot 2
#
plot2 <- function (var1_indx, var2_indx, sub_list){

  sub_agg <- sub_list$sub_agg
  
  colnames(sub_agg)[var1_indx] <- "var1"   

  avg_label <- round(sub_agg$var1, ifelse(abs(max(sub_agg$var1)) < 10, 1, 0))
  
  p2 <- ggplot (data=sub_agg) +
    geom_rect(data=sub_agg, aes(xmin=start, xmax=end, ymin=0, ymax=var1/100), 
      fill=ifelse(trimws(sub_agg$party)=="Democrat", "blue", "red"), alpha=1) +
    geom_hline(yintercept=0,color="black", size=0.3) +
    geom_segment(data=sub_agg, aes(x=start, y=-Inf, xend=start, yend=Inf), size=0.2) +
    geom_segment(data=sub_agg[length(sub_agg$end),], aes(x=end, y=-Inf, xend=end, yend=Inf), size=0.2) + 
    geom_text(data=sub_agg, aes(x=start, y=0, label=name), size = 5, vjust = 1.1, hjust = 0, angle=90) +
    theme_gray() + theme (axis.title.x = element_blank()) +
    labs(y=var_n[var1_indx], caption=paste("Average ", var_n[var1_indx],  "Rate By President")) +
    theme(plot.caption = element_text(size = 12, hjust = 0)) +
    scale_y_continuous(labels=scales::percent) 
  if (min(sub_agg$var1)<0){
    p2 <- p2 + geom_text(data=sub_agg, aes(x=start, y=ifelse (var1 <0, var1/100, 0),label=avg_label, vjust=ifelse(var1>0, +1.5, +1.5)), hjust=0) 
  }
  if(min(sub_agg$var1)>=0){
    p2 <- p2 + geom_text(data=sub_agg, aes(x=start, y=var1/100,label=avg_label, vjust=ifelse(var1>0, -0.5, +1.5)), hjust=0) 
  }

  if (var2_indx < n_vars+1 & var2_indx != var1_indx){
    p2 <- p2 + scale_y_continuous(labels=scales::percent,
      sec.axis = sec_axis(~ ./1 , name=paste(var_n[var1_indx]), labels=scales::percent))
  }
# Prepare animation  
  a2 <- p2+transition_states(start, wrap=FALSE)+shadow_mark()+enter_grow()+enter_fade()      

  return (list(p2=p2, a2=a2))
}
#
# Plot Correlation
#
plotcorr <- function (var1_indx, var2_indx, sub_list){
  
  if (var2_indx < n_vars+1 & var2_indx != var1_indx){
 
    sub_inp <- sub_list$sub_inp

    colnames(sub_inp)[var1_indx]   <- "var1"
    colnames(sub_inp)[var2_indx]   <- "var2"

    corr_coeff <- round(cor (sub_inp$var1, sub_inp$var2), 2)

    pcorr <- ggplot(sub_inp, aes(x=var1/100, y=var2/100)) +
    geom_point(color="purple") + labs(x = var_n[var1_indx], y = var_n[var2_indx], 
      title=paste(var_n[var1_indx], " vs ", var_n[var2_indx],  
      "- ", sub_inp$date[1], "To", sub_inp$date[length(sub_inp$date)]), 
      caption=paste("Correlation Coefficient=", corr_coeff)) + 
    theme(plot.caption = element_text(size = 18, hjust = 0)) +
    scale_x_continuous(labels=scales::percent) +        
    scale_y_continuous(labels=scales::percent) +        
    geom_smooth(method="lm", color="black") 

    return (pcorr)    
  }
}
#
# SHINY
#
# Define UI ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(

      sliderInput("pdecimal", "Party Intensity:",
                  min = 0, max = 1,
                  value = 0.4, step = 0.1),
      sliderInput("rdecimal", "Recession Intensity:",
                  min = 0, max = 1,
                  value = 0.3, step = 0.1),
      radioButtons("var1", 
                   label = "Macro Parameter",
                   choices = var_list,
                   selected=1),
     selectInput ("var2",                          
                   label = "Superimpose Parameter",
                   choices = var_list_p,
                   selected=n_vars+1),
     dateRangeInput("dates", 
                    label = "Date Range",
                    start = inp$date[1],
                    end   = inp$date[length(inp$date)],
                    min   = inp$date[1],
                    max   = inp$date[length(inp$date)]), 
     actionButton("do",      "Enter Dates", width = 100),
     br(),
     actionButton("animate", "Animate", width = 100)
    ),

      mainPanel(textOutput('button'),
                plotOutput('plot1'),
                plotOutput('plot2'),
                textOutput("corr"),
                plotOutput('plot3'),
                imageOutput('anim1'),
                imageOutput('anim2'))
  )
)

# Define server logic ----
server <- function(input, output, session) {
  observeEvent(input$do, {
    sub_list <- sub_data (as.Date(input$dates[1]), as.Date(input$dates[2]), inp)
# Plot1 Top Graph
    output$plot1 <- renderPlot({
      pl1 <- plot1 (as.numeric(input$var1), as.numeric(input$var2), input$rdecimal, input$pdecimal, sub_list)
      print (pl1$p1)
    }) 
# Plot2 Presidential Averages Graph
    output$plot2 <- renderPlot({
      
      pl2 <- plot2 (as.numeric(input$var1), as.numeric(input$var2), sub_list)
      print (pl2$p2)
    }) 
# Plot3 Correlation Graph If Overlay
    output$plot3 <- renderPlot({
      if (as.numeric(input$var2) < n_vars+1 & as.numeric(input$var2) != as.numeric(input$var1)){
        pcorr <- plotcorr (as.numeric(input$var1), as.numeric(input$var2), sub_list)
        print (pcorr)
      }
    })
  }) # End Input Action Enter Dates   
# Anim1 & Anim2 Animation Areas
  observeEvent(input$animate,{
    sub_list <- sub_data (as.Date(input$dates[1]), as.Date(input$dates[2]), inp)
    
    pl1 <- plot1 (as.numeric(input$var1), as.numeric(input$var2), input$rdecimal, input$pdecimal, sub_list)   
    pl2 <- plot2 (as.numeric(input$var1), as.numeric(input$var2), sub_list)
    
    output$anim1 <- renderImage({
      anim_save("outfile1.gif", animate (pl1$a1, fps=5, renderer=gifski_renderer(loop=FALSE)) ) 
      list(src = "outfile1.gif",contentType = 'image/gif',width = "600px",height = "400px")
    }) 
    output$anim2 <- renderImage({
      anim_save("outfile2.gif", animate (pl2$a2, fps=5, renderer=gifski_renderer(loop=FALSE)) ) 
      list(src = "outfile2.gif",contentType = 'image/gif',width = "600px",height = "400px")
    }) 
  }) 

}

# Run the app ----
shinyApp(ui = ui, server = server)  


