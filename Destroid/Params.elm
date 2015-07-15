module Destroid.Params where


---- Global parameters ----

aspect     = 1.5   -- aspect ratio of the space (game area)
stageScale = 0.8   -- percentage of the screen view occupied by a compactification length

spaceW    = 200 * aspect     -- space width (game units)
spaceH    = 200              -- space height
spaceSize = (spaceW, spaceH) -- space size

debug = True

---- Flight parameters ----

scaleR = 0.006  -- rotation response scale
scaleV = 0.025  -- acceleration response scale
bulV   = 80     -- bullet velocity
astV   = 20     -- asteroid debris velocity

astSizeBig    = spaceH / 15 -- Asteroid radii
astSizeMedium = spaceH / 35
astSizeSmall  = spaceH / 65

---- Visual form parameters ----

shipSize   = spaceH / 50
shipHitR   = 3.0  -- shit hit radius

bulletSize = spaceH / 350
