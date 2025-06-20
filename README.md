# Dijkstra's Algorithm Visualization

This project implements Dijkstra's Algorithm in Haskell with a visualization frontend in Elm.

## Prerequisites

- Haskell Stack
- Elm
- Node.js (for serving the frontend)

## Setup

1. Install Haskell dependencies:
```bash
stack build
```

2. Install Elm dependencies:
```bash
cd elm
elm make Main.elm --output=elm.js
```

## Running the Application

1. Start the Haskell server:
```bash
stack run
```

2. Open `http://localhost:3000` in your web browser.

## Project Structure

- `Dijkstra.hs`: Implementation of Dijkstra's Algorithm
- `Server.hs`: Haskell server using Scotty
- `elm/Main.elm`: Elm frontend for visualization
- `elm/index.html`: HTML entry point
- `elm/style.css`: CSS styles
- `elm/elm.js`: Compiled Elm code

## Features

- Interactive graph visualization
- Real-time shortest path calculation
- Distance display for each node
- Error handling and user feedback 