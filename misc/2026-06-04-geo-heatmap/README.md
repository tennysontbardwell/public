Hacky, vibcoded script to visualize google timeline location data.

Export from your phone, then pass the resulting file to the appropriate python script.

I believe the android version needs the first key from the json export to be extacted, something vaguely like `cat Timeline.json | jq '.semanticSegments' > timeline2.json`
