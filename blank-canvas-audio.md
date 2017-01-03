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

### Play Sounds

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

### (REMOVE) playAudio and pauseAudio
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
For a more involved example, we'll look at the audio portions of a simple playbar that has an updating play/pause indicator and a bar that fills as the audio file is played. The complete code for this example is available [here](https://github.com/ku-fpg/blank-canvas/blob/master/examples/playbar/Main.hs).

### Main
First is the `main` which calls `startLoop` with the path to the audio file and is event-triggered by the mouse down.

````Haskell
main = do
  dat <- getDataDir
  blankCanvas 3000 { events = ["mousedown"]} $ \context ->
    startLoop context "music/sonata.ogg"
````

### StartLoop
`startLoop` divides the program into three threads: one that continuously updates the current time of the audio file (stored in the `curTime` TVar), one that continuously reads mouse input to update the `play` TVar, and the main thread which draws the playbar. 

````Haskell
startLoop :: DeviceContext -> Text.Text -> IO ()
startLoop context filename = do
  music   <- send context $ newAudio filename	-- make new CanvasAudio
  play    <- newTVarIO (Paused)					-- The audio is paused when the page loads
  curTime <- newTVarIO 0						-- The audio starts at the beginning

  forkIO $ getCurTime context music curTime
  forkIO $ loopInput context music play
  loopBar context music play curTime

````

### GetCurTime
`getCurTime` continuously updates the TVar `curTime` with the current time from the audio file. Note that
because `currentTimeAudio :: CanvasAudio -> Canvas Double` you first need to send the currentTimeAudio query to the context to get the value `curTime'`

````Haskell
getCurTime :: DeviceContext -> CanvasAudio -> TVar Double -> IO ()
getCurTime context audio curTime = do
  curTime' <- send context $ currentTimeAudio audio
  atomically $ writeTVar curTime curTime'
  getCurTime context audio curTime
````

### LoopInput

Similarly, `loopInput` continuously updates the TVar `play`. The type `Play` is defined as:

````Haskell
data Play = Playing | Paused
          deriving (Eq,Ord,Show)
````

and the function `swap` is defined as:

````Haskell
-- switch Play to opposite value
swap :: TVar Play -> STM ()
swap play = do
  play' <- readTVar play
  if (play' == Playing)
    then writeTVar play (Paused)
    else writeTVar play (Playing)
````

`loopInput` will only update `play` when there is a mousedown even and it is within the bounds of
the play/pause symbol.

````Haskell
loopInput :: DeviceContext -> CanvasAudio -> TVar Play -> IO ()
loopInput context audio play = do
  play' <- readTVarIO play
  event <- wait context -- waits for event to occur
  case ePageXY event of
    -- if no mouse location, ignore, and loop again
    Nothing -> loopInput context audio play
    -- rework to get proper clicking range
    Just (w,h) -> do
      -- checks to see if the mouse is being clicked on top of the play/pause button
      if (w >= 5 && w <= 28 && h >= 10 && h <= 41) then
        send context $ do
        if (play' == Playing)
          then do
          pauseAudio audio
          else do
          playAudio audio
        else loopInput context audio play

      -- update play
      atomically $ swap play
    
  loopInput context audio play
````

### LoopBar

`loopBar` handles the drawing of the play bar. The with the current delay the animation runs at 25 frames per second.

````Haskell
loopBar :: DeviceContext -> CanvasAudio -> TVar Play -> TVar Double -> IO ()
loopBar context audio play curTime = do
  play'    <- readTVarIO play
  curTime' <- readTVarIO curTime
  playbarDraw context audio play' curTime'
  threadDelay (40 * 1000)
  loopBar context audio play curTime -- draws a playbar that fills up as the current time gets larger
````

