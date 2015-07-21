module Destroid.Params where

import Color exposing (rgb, rgba)


---- Global parameters ----

aspect     = 1.5   -- aspect ratio of the space (game area)
stageScale = 0.8   -- percentage of the screen view occupied by a compactification length

spaceW    = 200 * aspect     -- space width (game units)
spaceH    = 200              -- space height
spaceSize = (spaceW, spaceH) -- space size

debug = False


---- Color palette ----

gameBG       = rgb  31  42  62
titleBG      = rgb  45  61  91
shipcolor    = rgb  225 250 213
boostercolor = rgb  169 172 146
bulletcolor  = rgb  247 104 147
lifebarcolor = rgba 214 102 220 0.7
clearcolor   = rgb  132 216 208  --teal


---- Flight parameters ----

scaleR = 0.004  -- rotation response scale
scaleV = 0.030  -- acceleration response scale
bulV   = 80     -- bullet velocity
astV   = 10     -- asteroid debris velocity

astSizeBig    = spaceH / 15 -- Asteroid radii
astSizeMedium = spaceH / 35
astSizeSmall  = spaceH / 65


---- Visual form parameters ----

shipSize   = spaceH / 50
shipHitR   = 3.0  -- ship hit radius

bulletSize = spaceH / 350


---- Wormhole animation parameters ----

t_fade = 75
t_sep  = 30


---- Weapon parameters ----

bulletLife = 200


---- Other ----

astDamageScale = 0.1

t_blink = 60

t_death = 200

t_cleared = 400

