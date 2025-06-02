library(websocket)
library(promises)
library(later)

# Globals
ws_global <- NULL
ws_connected <- FALSE

initialize_ws <- function(url = "ws://localhost:8081") {
  if (!is.null(ws_global) && ws_connected) return(invisible())
  
  ws <- WebSocket$new(url)
  
  ws$onOpen(function(event) {
    ws_connected <<- TRUE
    cat("WebSocket connected\n")
  })
  
  ws$onClose(function(event) {
    ws_connected <<- FALSE
    cat("WebSocket closed\n")
  })
  
  ws$onError(function(event) {
    ws_connected <<- FALSE
    cat("WebSocket error\n")
  })
  
  ws_global <<- ws
  
  # Wait until connected
  waited <- 0
  while (!ws_connected && waited < 5) {
    run_now(timeout = 0.1)
    Sys.sleep(0.1)
    waited <- waited + 0.1
  }
}

send_notification <- function(message) {
  if (is.null(ws_global) || !ws_connected) {
    initialize_ws()
  }
  
  if (ws_connected) {
    ws_global$send(message)
  } else {
    warning("WebSocket not connected")
  }
}
