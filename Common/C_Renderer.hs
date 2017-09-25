
module C_Renderer
  (
      draw
  )
  where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


-- draw :: [(Int,Int)] -> IO ()
draw vs = toFile (def) "test.svg" $
  do
      layout_title .= "Title"
      plot (fillBetween "pl1" [ (x,(0,y)) | (x,y) <- vs])


myFileOps = fo_size .~ (100,100)


fillBetween title vs = liftEC $ do
    plot_fillbetween_title .= title
    color <- takeColor
    plot_fillbetween_style .= solidFillStyle color
    plot_fillbetween_values .= vs
