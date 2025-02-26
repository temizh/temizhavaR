# Temiz Hava Hakki: API for retrieving data

## Overview

This is an API built using **RestRserve** in R to serve air quality data. The API allows users to fetch air quality measurements based on various filters such as time range, station, and region. Authentication is enforced using an API key.

## Features

* Retrieve air quality data in **hourly** or **daily** format.
* Filter results by  **region** ,  **station** , and  **date range** .
* Authentication via API key.
* Secured by API key.

## Installation

### Prerequisites

Ensure that you have the following installed:

* **R (>= 4.0)**
* **RestRserve** package
* **RPostgres** and DBI for database connection
* **dotenv** for environment variables management
* **dplyr** and openxlsx for data processing

### Setup

1. Clone the repository:
   ```sh
   git clone <repository_url>
   cd <repository_name>
   ```
2. Install dependencies:
   ```r
   install.packages(c("RestRserve", "RPostgres", "DBI", "dotenv", "dplyr", "openxlsx"))
   ```
3. Configure environment variables in `.env` file:
   ```ini
   POSTGRES_HOST=your_db_host
   TEMIZHAVA_DB=your_db_name
   POSTGRES_TUSER=your_db_user
   POSTGRES_TUSER_PASSWORD=your_db_password
   POSTGRES_PORT=your_db_port
   API_KEY=your_api_key
   API_PORT=your_api_port
   ```

## Project Structure

```
/api
│── main.R                # Main entry point
│── server.R              # Server configuration
│── endpoints.R           # API endpoints
│── middleware.R          # Middleware for authentication
│── db_connect.R          # Database connection script
│── logic.R               # Core logic functions
│── .env                  # Environment variables
│── README.md             # Documentation
```

## API Endpoints

### 1. Fetch Air Quality Data

**Endpoint:** `/get_data`

**Method:** `GET`

**Query Parameters:**

| Parameter      | Type     | Description                                      |
| -------------- | -------- | ------------------------------------------------ |
| `frequency`  | String   | `daily`or `hourly`(default:`daily`)        |
| `parameters` | String[] | List of pollutants (e.g.,`PM10`,`NO2`, etc.) |
| `start_date` | String   | Start date (`YYYY-MM-DD`)                      |
| `end_date`   | String   | End date (`YYYY-MM-DD`)                        |
| `region`     | String   | Region name (optional)                           |
| `station`    | String   | Station name (optional)                          |

**Example Request (cURL):**

```sh
curl -X GET "http://localhost:8080/get_data?frequency=daily&start_date=2024-01-01&end_date=2024-01-31" \
     -H "X-API-Key: your_api_key"
```

### 2. Fetch Air Quality Data by Configuration

**Endpoint:** `/get_data_by_config`

**Method:** `POST`

**Body:**

JSON object with parameters: frequency, parameters, start_date, end_date, region, station. Parameters have the same functionality as in `/get_data` endpoint.

**Example Request (cURL):**

```sh
curl -X POST "http://localhost:8080/get_data_post" \
     -H "Content-Type: application/json" \
     -H "X-API-Key: your_api_key" \
     -d '{
           "frequency": "daily",
           "parameters": ["PM10", "NO2", "CO"],
           "start_date": "2024-01-01",
           "end_date": "2024-01-31",
           "region": "Istanbul",
           "station": "Station A"
         }'
```

### Authentication

All requests require an API key in the request header:

```sh
-H "X-API-Key: your_api_key"
```

If an invalid or missing API key is detected, the server will return:

```json
{
  "error": "Invalid API key."
}
```

## Running the API

To start the API server, run:

```r
source("main.R")
```

The server will start at `http://localhost:8080`, assuming port is defined as 8080 in `.env` file.

## Middleware

* **API Key Middleware** : Verifies API key before processing requests.

## Database Connection

The database connection is managed in `db_connect.R`. The connection details are retrieved from environment variables. Database is postgres database.
