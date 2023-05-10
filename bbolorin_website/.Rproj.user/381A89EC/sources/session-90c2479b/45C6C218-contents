#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(bslib)

# Cleaning the data
energy = read.csv("energy_data_set.csv", header = TRUE)
colnames(energy) = c("country", "Year", "Region", "co2_fuel_com", "avg_co2",
                     "co2_gdp", "tot_energy_prod", "tot_energy_con", "perc_renew",
                     "elec_demand", "oil_dom_con", "ref_oil_prod", "gas_prod",
                     "gas_dom_con", "con_gdp", "elec_prod", "elec_dom_con", 
                     "coal_dom_con", "perc_wind_sol", "crud_oil_prod", "coal_prod")

energy$Region = factor(energy$Region, 
                       levels = c("Arab States", "South/Latin America", 
                                  "Asia & Pacific", "Europe", "North America",
                                  "Middle east", "Africa"))

energy$gas_prod = as.numeric(energy$gas_prod)
energy$coal_dom_con = as.numeric(energy$coal_dom_con)
energy$perc_wind_sol = as.numeric(energy$perc_wind_sol)
energy$coal_prod = as.numeric(energy$coal_prod)

energy = energy[complete.cases(energy), ]

# Sub data frames by region
data_arab = energy[energy$Region == "Arab States", ]
data_south = energy[energy$Region == "South/Latin America", ]
data_asia = energy[energy$Region == "Asia & Pacific", ]
data_europe = energy[energy$Region == "Europe", ]
data_north = energy[energy$Region == "North America", ]
data_middle = energy[energy$Region == "Middle east", ]
data_africa = energy[energy$Region == "Africa", ]

# Yearly Averages

year = c(1990:2020)

    # Domestic Oil Consumption
avg_oil_con_africa = NULL
for(i in 1:nrow(data_africa)){
  for(y in 1:31){
    avg_oil_con_africa[y] = mean(data_africa$oil_dom_con[data_africa$Year == year[y]])
  }
}

avg_oil_con_arab = NULL
for(i in 1:nrow(data_arab)){
  for(y in 1:31){
    avg_oil_con_arab[y] = mean(data_arab$oil_dom_con[data_arab$Year == year[y]])
  }
}

avg_oil_con_asia = NULL
for(i in 1:nrow(data_asia)){
  for(y in 1:31){
    avg_oil_con_asia[y] = mean(data_asia$oil_dom_con[data_asia$Year == year[y]])
  }
}

avg_oil_con_europe = NULL
for(i in 1:nrow(data_europe)){
  for(y in 1:31){
    avg_oil_con_europe[y] = mean(data_europe$oil_dom_con[data_europe$Year == year[y]])
  }
}

avg_oil_con_middle = NULL
for(i in 1:nrow(data_middle)){
  for(y in 1:31){
    avg_oil_con_middle[y] = mean(data_middle$oil_dom_con[data_middle$Year == year[y]])
  }
}

avg_oil_con_north = NULL
for(i in 1:nrow(data_north)){
  for(y in 1:31){
    avg_oil_con_north[y] = mean(data_north$oil_dom_con[data_north$Year == year[y]])
  }
}

avg_oil_con_south = NULL
for(i in 1:nrow(data_south)){
  for(y in 1:31){
    avg_oil_con_south[y] = mean(data_south$oil_dom_con[data_south$Year == year[y]])
  }
}

    # Domestic Oil Consumption Plot

oil.plot <- plot_ly(x = year, y = avg_oil_con_africa, name = "Africa",
                    type = "scatter", mode = "lines", 
                    line = list(color = "#AC3D1F", width = 2))
oil.plot <- oil.plot %>% add_trace(y = avg_oil_con_arab, name = "Arab States",
                                   line = list(color = "#34393C", width = 2))
oil.plot <- oil.plot %>% add_trace(y = avg_oil_con_asia, name = "Asia & Pacific",
                                   line = list(color = "#EBA423", width = 2))
oil.plot <- oil.plot %>% add_trace(y = avg_oil_con_europe, name = "Europe",
                                   line = list(color = "#46B5D3", width = 2))
oil.plot <- oil.plot %>% add_trace(y = avg_oil_con_middle, name = "Middle east",
                                   line = list(color = "#DB494C", width = 2))
oil.plot <- oil.plot %>% add_trace(y = avg_oil_con_north, name = "North America",
                                   line = list(color = "#207288", width = 2))
oil.plot <- oil.plot %>% add_trace(y = avg_oil_con_south, name = "South/Latin America",
                                   line = list(color = "#86AA6D", width = 2))
oil.plot <- oil.plot %>% layout(title = "Average Domestic Oil Consumption vs Year",
                                xaxis = list(title = "Year"),
                                yaxis = list(title = "Average domestic oil 
       consumption (Mt)"))
oil.plot <- oil.plot %>% layout(margin = list(l = 50, r = 50, b = 100, t = 50),
                                annotations = list(x = 1, y = -0.3, text = "Mt = million tons",
                                                   xref='paper', yref='paper', showarrow = F, 
                                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                   font = list(size = 10)))

    # Domestic Natural Gas Consumption
avg_gas_con_africa = NULL
for(i in 1:nrow(data_africa)){
  for(y in 1:31){
    avg_gas_con_africa[y] = mean(data_africa$gas_dom_con[data_africa$Year == year[y]])
  }
}

avg_gas_con_arab = NULL
for(i in 1:nrow(data_arab)){
  for(y in 1:31){
    avg_gas_con_arab[y] = mean(data_arab$gas_dom_con[data_arab$Year == year[y]])
  }
}

avg_gas_con_asia = NULL
for(i in 1:nrow(data_asia)){
  for(y in 1:31){
    avg_gas_con_asia[y] = mean(data_asia$gas_dom_con[data_asia$Year == year[y]])
  }
}

avg_gas_con_europe = NULL
for(i in 1:nrow(data_europe)){
  for(y in 1:31){
    avg_gas_con_europe[y] = mean(data_europe$gas_dom_con[data_europe$Year == year[y]])
  }
}

avg_gas_con_middle = NULL
for(i in 1:nrow(data_middle)){
  for(y in 1:31){
    avg_gas_con_middle[y] = mean(data_middle$gas_dom_con[data_middle$Year == year[y]])
  }
}

avg_gas_con_north = NULL
for(i in 1:nrow(data_north)){
  for(y in 1:31){
    avg_gas_con_north[y] = mean(data_north$gas_dom_con[data_north$Year == year[y]])
  }
}

avg_gas_con_south = NULL
for(i in 1:nrow(data_south)){
  for(y in 1:31){
    avg_gas_con_south[y] = mean(data_south$gas_dom_con[data_south$Year == year[y]])
  }
}

    # Domestic Natural Gas Consumption Plot
gas.plot <- plot_ly(x = year, y = avg_gas_con_africa, name = "Africa",
                    type = "scatter", mode = "lines", 
                    line = list(color = "#AC3D1F", width = 2))
gas.plot <- gas.plot %>% add_trace(y = avg_gas_con_arab, name = "Arab States",
                                   line = list(color = "#34393C", width = 2))
gas.plot <- gas.plot %>% add_trace(y = avg_gas_con_asia, name = "Asia & Pacific",
                                   line = list(color = "#EBA423", width = 2))
gas.plot <- gas.plot %>% add_trace(y = avg_gas_con_europe, name = "Europe",
                                   line = list(color = "#46B5D3", width = 2))
gas.plot <- gas.plot %>% add_trace(y = avg_gas_con_middle, name = "Middle east",
                                   line = list(color = "#DB494C", width = 2))
gas.plot <- gas.plot %>% add_trace(y = avg_gas_con_north, name = "North America",
                                   line = list(color = "#207288", width = 2))
gas.plot <- gas.plot %>% add_trace(y = avg_gas_con_south, name = "South/Latin America",
                                   line = list(color = "#86AA6D", width = 2))
gas.plot <- gas.plot %>% layout(title = "Average Domestic Natural Gas \n Consumption vs Year",
                                xaxis = list(title = "Year"),
                                yaxis = list(title = "Average domestic natural gas
       consumption (bcm)", standoff = 100))
gas.plot <- gas.plot %>% layout(margin = list(l = 50, r = 50, b = 100, t = 50),
                                annotations = list(x = 1, y = -0.3, text = "bcm = billion cubic meters",
                                                   xref='paper', yref='paper', showarrow = F, 
                                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                   font = list(size = 10)))
    # Domestic Coal Consumption
avg_coal_africa = NULL
for(i in 1:nrow(data_africa)){
  for(y in 1:31){
    avg_coal_africa[y] = mean(data_africa$coal_dom_con[data_africa$Year == year[y]])
  }
}

avg_coal_arab = NULL
for(i in 1:nrow(data_arab)){
  for(y in 1:31){
    avg_coal_arab[y] = mean(data_arab$coal_dom_con[data_arab$Year == year[y]])
  }
}

avg_coal_asia = NULL
for(i in 1:nrow(data_asia)){
  for(y in 1:31){
    avg_coal_asia[y] = mean(data_asia$coal_dom_con[data_asia$Year == year[y]])
  }
}

avg_coal_europe = NULL
for(i in 1:nrow(data_europe)){
  for(y in 1:31){
    avg_coal_europe[y] = mean(data_europe$coal_dom_con[data_europe$Year == year[y]])
  }
}

avg_coal_middle = NULL
for(i in 1:nrow(data_middle)){
  for(y in 1:31){
    avg_coal_middle[y] = mean(data_middle$coal_dom_con[data_middle$Year == year[y]])
  }
}

avg_coal_north = NULL
for(i in 1:nrow(data_north)){
  for(y in 1:31){
    avg_coal_north[y] = mean(data_north$coal_dom_con[data_north$Year == year[y]])
  }
}

avg_coal_south = NULL
for(i in 1:nrow(data_south)){
  for(y in 1:31){
    avg_coal_south[y] = mean(data_south$coal_dom_con[data_south$Year == year[y]])
  }
}

    # Domestic Coal Consumption Plot
coal.plot <- plot_ly(x = year, y = avg_coal_africa, name = "Africa",
                     type = "scatter", mode = "lines", 
                     line = list(color = "#AC3D1F", width = 2))
coal.plot <- coal.plot %>% add_trace(y = avg_coal_arab, name = "Arab States",
                                     line = list(color = "#34393C", width = 2))
coal.plot <- coal.plot %>% add_trace(y = avg_coal_asia, name = "Asia & Pacific",
                                     line = list(color = "#EBA423", width = 2))
coal.plot <- coal.plot %>% add_trace(y = avg_coal_europe, name = "Europe",
                                     line = list(color = "#46B5D3", width = 2))
coal.plot <- coal.plot %>% add_trace(y = avg_coal_middle, name = "Middle east",
                                     line = list(color = "#DB494C", width = 2))
coal.plot <- coal.plot %>% add_trace(y = avg_coal_north, name = "North America",
                                     line = list(color = "#207288", width = 2))
coal.plot <- coal.plot %>% add_trace(y = avg_coal_south, name = "South/Latin America",
                                     line = list(color = "#86AA6D", width = 2))
coal.plot <- coal.plot %>% layout(title = "Average Domestic Coal Consumption vs Year",
                                  xaxis = list(title = "Year"),
                                  yaxis = list(title = "Average domestic coal
       consumption (Mt)", standoff = 100))
coal.plot <- coal.plot %>% layout(margin = list(l = 50, r = 50, b = 100, t = 50),
                                  annotations = list(x = 1, y = -0.3, text = "Mt = million tons",
                                                     xref='paper', yref='paper', showarrow = F, 
                                                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                     font = list(size = 10)))

    # Percent Renewable Electricity
avg_perc_renew_africa = NULL
for(i in 1:nrow(data_africa)){
  for(y in 1:31){
    avg_perc_renew_africa[y] = mean(data_africa$perc_renew[data_africa$Year == year[y]])
  }
}

avg_perc_renew_arab = NULL
for(i in 1:nrow(data_arab)){
  for(y in 1:31){
    avg_perc_renew_arab[y] = mean(data_arab$perc_renew[data_arab$Year == year[y]])
  }
}

avg_perc_renew_asia = NULL
for(i in 1:nrow(data_asia)){
  for(y in 1:31){
    avg_perc_renew_asia[y] = mean(data_asia$perc_renew[data_asia$Year == year[y]])
  }
}

avg_perc_renew_europe = NULL
for(i in 1:nrow(data_europe)){
  for(y in 1:31){
    avg_perc_renew_europe[y] = mean(data_europe$perc_renew[data_europe$Year == year[y]])
  }
}

avg_perc_renew_middle = NULL
for(i in 1:nrow(data_middle)){
  for(y in 1:31){
    avg_perc_renew_middle[y] = mean(data_middle$perc_renew[data_middle$Year == year[y]])
  }
}

avg_perc_renew_north = NULL
for(i in 1:nrow(data_north)){
  for(y in 1:31){
    avg_perc_renew_north[y] = mean(data_north$perc_renew[data_north$Year == year[y]])
  }
}

avg_perc_renew_south = NULL
for(i in 1:nrow(data_south)){
  for(y in 1:31){
    avg_perc_renew_south[y] = mean(data_south$perc_renew[data_south$Year == year[y]])
  }
}

    # Percent Renewable Electricity Plot
renew.plot <- plot_ly(x = year, y = avg_perc_renew_africa, name = "Africa",
                      type = "scatter", mode = "lines", 
                      line = list(color = "#AC3D1F", width = 2))
renew.plot <- renew.plot %>% add_trace(y = avg_perc_renew_arab, name = "Arab States",
                                       line = list(color = "#34393C", width = 2))
renew.plot <- renew.plot %>% add_trace(y = avg_perc_renew_asia, name = "Asia & Pacific",
                                       line = list(color = "#EBA423", width = 2))
renew.plot <- renew.plot %>% add_trace(y = avg_perc_renew_europe, name = "Europe",
                                       line = list(color = "#46B5D3", width = 2))
renew.plot <- renew.plot %>% add_trace(y = avg_perc_renew_middle, name = "Middle east",
                                       line = list(color = "#DB494C", width = 2))
renew.plot <- renew.plot %>% add_trace(y = avg_perc_renew_north, name = "North America",
                                       line = list(color = "#207288", width = 2))
renew.plot <- renew.plot %>% add_trace(y = avg_perc_renew_south, name = "South/Latin America",
                                       line = list(color = "#86AA6D", width = 2))
renew.plot <- renew.plot %>% layout(title = "Average Percentage of Renewable Electricity vs Year",
                                    xaxis = list(title = "Year"),
                                    yaxis = list(title = "Average percent of total electricity 
       production that is renewable", standoff = 100))



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Theme
  theme = bs_theme(version = 4, "united"),

    # Application title
    titlePanel(h3("Domestic Consumption of Different Fuel Types Around the World From 1990-2020")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("fuel",
                        "Fuel type",
                        choices = c("Oil",
                                    "Natural Gas",
                                    "Coal",
                                    "Renewable Electricity")),
            helpText("Hover over traces on the plot to reveal the values for fuel consumption and year."),
        br(),
        helpText("Select the traces in the legend to either remove or add traces to the plot."),
        br(),
        helpText("Click and drag on the plot to veiw different time ranges.")),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("fuelPlot"),
           br(),
           textOutput("fuelText")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$fuelPlot <- renderPlotly({
        if(input$fuel == "Oil")
          oil.plot
      else if(input$fuel == "Natural Gas")
        gas.plot
      else if(input$fuel == "Coal")
        coal.plot
      else if(input$fuel == "Renewable Electricity")
        renew.plot
    })
    output$fuelText <- renderText({
      if(input$fuel == "Oil")
        paste("North America has the largest average domestic oil consumption with 
              an average of 440 Mt from 1990 to 2020. Asia & Pacific have increased 
              their domestic oil consumption from 55.3 Mt in 1990 to 119.3 Mt in 2020. 
              The remaining regions had a fairly constant or more subtle changes in domestic 
              oil consumption from 1990 to 2020, all below about 55 Mt.")
      else if(input$fuel == "Natural Gas")
        paste("North America also has the highest average domestic natural gas 
              consumption which has increased from 300.7 bcm in 1990 to 494 bcm 
              in 2020. Average domestic natural gas consumption in the Middle east 
              was 14.5 bcm in 1990, but increased to 140.6 bcm in 2020. The other 
              regions consumed less than about 75 bcm each, but still increased 
              from 1990-2020.")
      else if(input$fuel == "Coal")
        paste("Since 1990, North America has cut down on its average domestic coal 
        consumption. In 1990, North America consumed and average of 434.3 Mt of coal 
        but in 2020, an average of 223.5 Mt were consumed. Europe also reduced their 
        average domestic coal consumption from 113.9 Mt in 1990 to 48.8 Mt in 2020. 
        By contrast, Asia & Pacific have increased their average domestic coal consumption 
        from 146.9 Mt to 503.9 Mt between 1990 and 2020. The remaining regions maintained 
        a roughly constant or a more subtle increase in domestic coal consumption between 1990 and 2020.
")
      else if(input$fuel == "Renewable Electricity")
        paste("The region with the highest average percentage 
              of renewable electricity production from 1990 to 2020 was South/Latin America. 
              However, this average percentage decreased from 58% in 1990 to 50.2% in 2020. 
              From 1990 to 2020, North America, Europe, and Asia & Pacific increased their 
              average percentage of renewable electricity from 37.2% to 43.8%, 17.2% to 35.3%, 
              and 20.1% to 22.7%, respectively. All other regions decreased their average 
              percentage of renewable electricity except for the Arab states which remained 
              below 1%.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
