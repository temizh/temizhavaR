library(httpuv)
library(jsonlite)

clients <- list()

# Start WebSocket server
ws_server <- startServer("0.0.0.0", 8081,  # Use appropriate IP and port
  list(
    # Handling WebSocket connection open event
    onWSOpen = function(ws) {
      cat("Client connected via WebSocket\n")

      id <- as.character(Sys.time())
      clients[[id]] <<- ws

      # Handling incoming messages
      ws$onMessage(function(binary, message) {
        cat("Message received:", message, "\n")
        for (client in clients) {
          client$send(message)
        }
      })

      # Handle WebSocket close event
      ws$onClose(function() {
        cat("Client disconnected\n")
      })
    }
  )
)

cat("WebSocket server running on ws://localhost:8081\n")
