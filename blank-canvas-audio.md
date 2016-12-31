# Using Audio in blank-canvas

<!--
TODO:
* add links to blank canvas
* local file path
* AND urls
* need full file path (unless you add it as a data file in cabal) 

Questions:
* Just write for current version?
-->

Say you want to make a game using blank-canvas. Say you want to have a ball in the game.  Sometimes that ball will hit the ground. You might want to add some sound effect of the ball hitting the ground. Or maybe you just want to make a playbar that plays and pauses music. Well, with the `CanvasAudio` type, you can! 

## Basic Example
`CanvasAudio` uses the [HTML5 Audio element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio) as its back end.  A `CanvasAudio` is a handle to an audio element that can be controlled (with `playAudio`,
`pauseAudio`, `setVolumeAudio`, and various other functions) inside a `Canvas` context. 

To begin, let's look at a simple example that loads an audio file and then plays it. Then we'll look at some 
other useful functions, and finally we'll look at using audio with other parts of `blank-canvas`.

### Setup
To first start, we need to set up blank-canvas itself. All this involves is importing the library and calling `blankCanvas` (to initiate the web service) on some port (in this case, 3000). Once the program is running, open up [http://localhost:3000/](http://localhost:3000/) to view the results of your hard work.

Here is a template for this setup:

````Haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank						-- import the blank canvas
import qualified Data.Text as Text			-- blank-canvas uses text for all of its string needs

main :: IO ()
main = blankCanvas 3000 $ \ context -> do	-- start blank canvas on port 3000
	send context $ do						-- send commands to this specific context
		return ()							-- do something
````

### Play Some Sounds

Now to make some sounds! Creating a `CanvasAudio` simply requires calling `newAudio` with 
the source of the audio file (either a file path or a URL). 
This will generate an audio element and return a handle so that you can control it. 
For example, to begin playing the audio, use the `playAudio` function.

````Haskell
main :: IO ()
main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
        send context $ do                 -- send commands to this specific context
          music <- newAudio "http://upload.wikimedia.org/wikipedia/en/d/df/Florence_Foster_Jenkins_H%C3%B6lle_Rache.ogg" -- initialize a new audio context
          playAudio music

````

To pause this audio, simply add `pauseAudio` at some point after `playAudio`.

### More functionality

While the full range of functionality for `CanvasAudio` is available on `blank-canvas`'s [hackage page](http://hackage.haskell.org/package/blank-canvas-0.6/docs/Graphics-Blank.html), let's look at some more examples of
different available functions and how to use them.

### playAudio and pauseAudio
This most basic functions are the self-descriptive `playAudio` and `pauseAudio`.

````Haskell
music <- newAudio "some url/filepath"
playAudio music
pauseAudio music -- immediately pause the audio
````
### Setting playback speed
A basic function is `setPlaybackRateAudio`. This is a multiplier - 1 is the original playback rate 
of the file, 2 is twice as fast, 0.5 is half speed, -1 is reverse playback (if the browser supports
it), etc.

````Haskell
music <- newAudio "some url/filepath"
setPlaybackRateAudio(music,2.0)
playAudio music
````

## Making a Play Bar

## Sound a Bounce
