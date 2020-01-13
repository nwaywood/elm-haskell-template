# Elm-Haskell-Template

## Aim 

Reliable, easy to set up template for future projects using elm and haskell. 


## Getting Started

Setup directory for project:

```bash
git clone git@github.com:nwaywood/elm-haskell-template.git <your-project-name>
cd <your-project-name>
rm -rf .git
```

Or use this repository as a github template!

## Application Structure

```
.
├── client                         # Frontend application
│   ├── elm.json                   # Elm file for dependencies
│   ├── static                     # Static resources (html, images etc)
│   └── src                        # Source code
│       ├── Main.elm               # The Main elm file
│       └── Router.elm             # Contains App routes and url logic
├── server                         # Root folder for server code
│   ├── app  
│   │   ├── Main.hs                # Runs application
│   └── src  
│       ├── Core.hs                # Application logic

```

#### Development

From the client folder

`$ elm-live src/Main.elm --proxy-host=http://localhost:3000 --proxy-prefix=/`

Default port is 8000 for elm-live

Note: Node version v10 or greater for elm-live. 

Or:

`$ elm reactor src/Main.elm  --port=3000`

Running either will proxy API calls to the haskell server.

Then run 

`$ stack run` 

From the server folder to start the haskell server

By default the server is listening on port 3000.

#### Production Build 

`$ elm make src/Main.elm --output=static/index.html`

and 

`$ stack run` 

from the server folder. 

