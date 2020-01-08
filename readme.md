# Elm-Haskell-Template

## Usage 

### Client 

#### Dev Mode

From the client folder

`$ elm-live src/Main.elm --proxy-host=http://localhost:3000 --proxy-prefix=/`

Note: Node version v10 or greater for elm-live. 

Or:

`$ elm reactor src/Main.elm  --port=3000`

#### Production Build 

`$ elm make src/Main.elm --output=static/index.html`

### Server 

`$ stack run`
