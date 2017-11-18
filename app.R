library(shiny)
library(shinythemes)
library(fireData)
library(shinyBS)
library(shinydashboard)

# Connect to Firebase
library(jsonlite)
library(httr)
library(qpcR)

# This list houses the IDs of each stop and their nearest opposites.
alternatives <- list()
alternatives[2] <- 14  # StopID 2 is opposite StopID 14
alternatives[14] <- 2
alternatives[3] <- 15
alternatives[15] <- 3
alternatives[4] <- 16
alternatives[16] <- 4
alternatives[6] <- 17
alternatives[17] <- 6
alternatives[7] <- 18
alternatives[18] <- 7
alternatives[8] <- 19
alternatives[19] <- 8
alternatives[20] <- 9
alternatives[9] <- 20
alternatives[10] <- 22
alternatives[22] <- 10
alternatives[11] <- 23
alternatives[23] <- 11
alternatives[24] <- 12
alternatives[12] <- 24
alternatives[13] <- 25
alternatives[25] <- 13
alternatives[21] <- 28
alternatives[28] <- 21

# Get a stop name based on the stopID
getStopNameByID <- function(stopID){
  stops <- GET("https://bt3103-ef12e.firebaseio.com/stops.json")
  stops <- content(stops, as = "parsed")
  stops <- as.list(stops)
  for(stop in stops){
    if(stopID == stop$stopID){
      return(stop$stopName)
    }
  }
}

# Return a list of all the stops
returnStops <- function(){
  stops <- GET("https://bt3103-ef12e.firebaseio.com/stops.json")
  stops <- content(stops,as="parsed")
  stopList <- list()
  i <- 1
  for(stop in stops){
    stopList[i] <- stop$stopName
    i <- i+1
  }
  return (stopList)
}

# Get a stop based on the stopName
getStopByName <- function(stopName){
  stops <- GET("https://bt3103-ef12e.firebaseio.com/stops.json")
  stops <- content(stops, as = "parsed")
  stops <- as.list(stops)
  for(stop in stops){
    if(stopName == stop$stopName){
      return(stop)
    }
  }
}

# Create oracle matrix
updateServiceMatrix <- function(){
  # Initialise Bus Service Matrix
  current<-matrix(ncol=30, nrow=30)
  for(i in 1:ncol(current)){
    for (j in 1:nrow(current)){
      if(i == j){
        current[i,j] = "HELP LA!"
      } else {
        current[i,j] = "Off"
      }
    }
  }
  
  route <- content(GET(paste0("https://bt3103-ef12e.firebaseio.com/latestRoutes.json")), as="parsed")
  busServices <- names(route)
  
  #initialize the data frame to store the bus stops
  currentRoute <- data.frame(matrix(ncol = length(busServices)))
  for (i in length(busServices)){
    names(currentRoute) <- busServices
  }
  
  currentList = list()
  
  #Obtaining the route for each bus service
  for(i in 1:length(busServices)){
    date1 = route[[i]][[1]]$effDate
    date2 = route[[i]][[2]]$effDate
    currentDate <- Sys.Date()
    routeStops = list()
    
    if(date1>date2){
      if(currentDate >= date1){
        numStops <- route[[i]][[1]]$stops[2:length(route[[i]][[1]]$stops)]
        for(j in length(numStops)){
          routeStops[j] <- numStops[[j]]
        }
      }
      else if (date1> currentDate & currentDate >date2){
        numStops <- route[[i]][[2]]$stops[2:length(route[[i]][[2]]$stops)]
        for(j in length(numStops)){
          routeStops[j] <- route[[i]][[2]]$stops[[j]]
        }
      }
    } 
    else {
      if(currentDate >= date2){
        numStops <- route[[i]][[2]]$stops[2:length(route[[i]][[2]]$stops)]
        for(j in length(numStops)){
          routeStops[j] <- numStops[[j]]
        }
      } 
      else if (date2> currentDate & currentDate >date1){
        numStops <- route[[i]][[1]]$stops[2:length(route[[i]][[1]]$stops)]
        for(j in length(numStops)){
          routeStops[j] <- numStops[[j]]
        }
      }
    }
    #initialize the list of stops for both current and future routes
    listStops <-list()
    
    #for current route
    for(k in 1:length(numStops)){
      listStops[k] <- numStops[k]
    }
    if(i == 1){
      currentList <- cbind(currentList, listStops)
    } else {
      currentList <- qpcR:::cbind.na(currentList, listStops)
    }
    
    
  }
  
  #Initialize the dataframe heading
  busColNames<-setNames(data.frame(matrix(ncol = length(busServices), nrow = 0)), c(busServices))
  
  currentList <- rbind(busColNames, currentList)
  colnames(currentList) <- busServices
  
  
  #obtaining the bus stop ID
  stops <- GET(paste0("https://bt3103-ef12e.firebaseio.com/stops.json"))
  stops <- content(stops, as="parsed")
  
  idOfStops <- c()
  namesOfStops <- c()
  counter = 1
  for (i in (stops)){
    namesOfStops[[counter]] <- i$stopName
    idOfStops[[counter]] <- i$stopID
    counter <- counter + 1
  }
  
  #filling in the matrix with the bus services
  serviceNumber = 1
  #View(currentList)
  for(i in 1:length(currentList)){ 
    stops <- c()
    for (j in 1:length(currentList[[i]])){
      if (is.na(currentList[[i]][[j]])){
        #skip if there are no more bus stops
      } else {
        stops <- append(stops, currentList[[i]][[j]])
      }
    }
    busStop <- match(stops, namesOfStops)
    for (m in 1:length(busStop)){
      for (n in 1:length(busStop)){
        if (m>n){ 
          #do nothing
        } 
        else if (m==n){
          #do nothing
        }
        else {
          if(isTRUE(current[busStop[m],busStop[n]] == "Off")){
            current[busStop[m],busStop[n]] <- busServices[serviceNumber]
          } 
          else if(isTRUE(current[busStop[m],busStop[n]] == "HELP LA!")){
            #do nothing
          }
          #if there exist two or more services bteween start and end point
          else if(isTRUE(current[busStop[m],busStop[n]] != "Off") || isTRUE(current[busStop[m],busStop[n]] != "HELP LA!") || isTRUE(current[busStop[m],busStop[n]] != "Opp")){
            if(grepl(busServices[serviceNumber], current[busStop[m],busStop[n]], fixed = TRUE)){
              #do nothing as bus service is pasted inside current
            } else {
              #paste bus service into current
              current[busStop[m],busStop[n]] <- paste(current[busStop[m],busStop[n]],busServices[serviceNumber], sep = ",")
              
            }
          } 
        }
      }
    }
    #change of bus service number
    serviceNumber = serviceNumber + 1
  }
  return (t(current))
}


ui <- shinyUI(fluidPage(
  includeCSS("styles.css"),
  theme = shinytheme("united"),
  img(src="bus_new.jpeg"),
  fluidRow(
    column(width=6,offset=3,selectInput("frommenu","From:",sort(unlist(returnStops())), width = "100%",selectize = FALSE)),
    column(width=6,offset=3,selectInput("tomenu","To:",sort(unlist(returnStops())), width = "100%",selectize = FALSE),br(), br())),
    actionButton("button", "Let's Go", icon("bus")),
    textOutput("text"),
    textOutput("text2")
    )
  )

server <- function(input, output, session) {
  
  
  oracle <- as.data.frame(updateServiceMatrix())
  
  x<-reactive(input$frommenu)
  
  y<-reactive(input$tomenu)
  
  
  observeEvent(input$button,{
    # Obtain bus number
    
    
    # Inserting travel request
    # If start and end is same
    if (y()==x()){
      msgg<-"Please include different start and end locations"
      showModal(modalDialog(title = "No such bus",msgg,easyClose = TRUE))
    }
    
    # if start and end different
      # Case 1: No direct but alternative available
      # Case 2: Both direct but alternative available
    else{
      timing<-format(Sys.time(), format="%Y-%m-%d")
      startlocations<-c(x(), timing)
      endlocations<-c(y(), timing)
      
      msg <- toupper(oracle[[as.numeric(getStopByName(x())$stopID)]][[as.numeric(getStopByName(y())$stopID)]])
      
      # smart Return (191117: updated code from line 287 to 327)
      if(msg == "OFF"){
        
        xStopID <- as.numeric(getStopByName(x())$stopID)
        yStopID <- as.numeric(getStopByName(y())$stopID)
        altXStopID <- alternatives[[as.numeric(getStopByName(x())$stopID)]]
        altYStopID <- alternatives[[as.numeric(getStopByName(y())$stopID)]]
        
        #example: from PGP to EA
        if(is.null(altXStopID)){
          if(is.null(altYStopID)){
            msg <- paste0("There is no direct service available for your destination.")
          } 
          else if (oracle[[xStopID]][[altYStopID]] != "Off"){
            msg <- paste0("Try setting ", getStopNameByID(altYStopID)," as your end location.")
          }
        }
        else if(is.null(altYStopID)){
          if(is.null(altXStopID)){
            msg <- paste0("There is no direct service available for your destination.")
          } 
          else if (oracle[[altXStopID]][[yStopID]] != "Off"){
            msg <- paste0("Try setting ", getStopNameByID(altXStopID)," as your end location.")
          }
        }
        else if (altXStopID == yStopID){
          msg <- paste0("Your chosen end destination is just opposite of you. ")
        }
        else {
          if(oracle[[altXStopID]][[yStopID]] != "Off"){
            msg <- paste0("Try setting ", getStopNameByID(altXStopID)," as your start location.") 
          }
          else if (oracle[[xStopID]][[altYStopID]] != "Off"){
            msg <- paste0("Try setting ", getStopNameByID(altYStopID)," as your end location.")
          }
          else if (oracle[[altXStopID]][[altYStopID]] != "Off"){
            msg <- paste0("Try setting ", getStopNameByID(altXStopID), " and ", getStopNameByID(altYStopID)," as your start and end location.")
          } 
          else {
            msg <- paste0("There is no direct service available for your destination.")
          }
        }
      }
      
      ### Comment if testing
      # Upload travel request
      a<-upload(startlocations, projectURL = "https://bt3103-ef12e.firebaseio.com/", directory="Start Locations")
      b<-upload(endlocations, projectURL = "https://bt3103-ef12e.firebaseio.com/", directory="End Locations")
      
      showModal(modalDialog(title = "Your Bus Is",msg,easyClose = TRUE))
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

