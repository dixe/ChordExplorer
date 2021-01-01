# Build/Run

`elm-live src/Main.elm -u  --start-page=withPorts.html -- --output=elm.js` for hotload on editor save. -u always server index. html and let elm handle routing. withPorts.html has link to the --output=elm.js file and has setup with port for audio

or

`elm reactor and select src/Mail.elm` for regular load
