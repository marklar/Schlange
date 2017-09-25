{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import           Data.Monoid ((<>))
import           Reflex.Dom
import qualified Svg as Svg

{- | SVG

other shapes:
  ellipse
  polygon ("points", "200,10 250,190 160,210")
  line : x1, y1, x2, y2, style=
  polyline ("points", "200,10 250,190 160,210")
  text
  paths, Bezier curves:
      https://www.w3schools.com/graphics/svg_path.asp

    rectAttrs = 
      mkAttrs [ ("height", "")
              , ("width", "")
              , ("y", "")
              , ("x", "")
              , ("rx", "")  -- radius
              , ("ry", "")  -- radius
              , ("stroke-width", "")
              , ("stroke", "rgb(0,0,255)")
                -- fill-opacity:0.1; stroke-opacity:0.9; opacity: 0.5
              , ("fill", "")
              ]
-}

main ∷ IO ()
main = mainWidget $ do
  aOnDyn ← namedWire "A"
  bOnDyn ← namedWire "B"
  let abOnDyn = nand aOnDyn bOnDyn
  foo $ coloredSvgLine abOnDyn
  return ()


-- | div containing html_button & svg_line
-- Want everything drawn in SVG?
namedWire ∷ MonadWidget t m
          ⇒ Text → m (Dynamic t Bool)
namedWire btnName =
  el "div" $ do
    aClickEv ← button btnName
    dyn ← toggle True aClickEv
    foo $ coloredSvgLine dyn
    return dyn

-- | make an svg of 150 x 20
foo ∷ MonadWidget t m
    ⇒ m () → m ()
foo = svgWdHt 150 20

svgWdHt ∷ MonadWidget t m
        ⇒ Int → Int → m () → m ()
svgWdHt w h =
  Svg.attr "svg" (Svg.mkAttrs [ ("width",  toText w)
                              , ("height", toText h)
                              ])

nand ∷ Reflex t
     ⇒ Dynamic t Bool → Dynamic t Bool → Dynamic t Bool
nand = zipDynWith (\a b → not $ a && b)


coloredSvgLine ∷ MonadWidget t m
               ⇒ Dynamic t Bool → m ()
coloredSvgLine b =
  Svg.dynAttr "line" (attrs <$> b) (return ())
  where
    attrs ∷ Bool → Svg.AttrMap
    attrs b = Svg.mkAttrs
      [ ("x1", toText 10),  ("y1", toText 10)
      , ("x2", toText 140), ("y2", toText 10)
      , case b of
          True →  ("style", "stroke: green; stroke-width: 3")
          False → ("style", "stroke: red;   stroke-width: 3")
      ]


toText ∷ Show α ⇒ α → Text
toText = T.pack . show
