{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Svg where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom

type AttrMap = Map Text Text
type DynAttrs t = Dynamic t AttrMap

mkAttrs ∷ [(Text,Text)] → AttrMap
mkAttrs = Map.fromList

------------------------------------------
-- SVG elements with dynamic attributes.

{-# INLINABLE dynAttr #-}
dynAttr ∷ ∀ t m a. MonadWidget t m
        ⇒ Text → DynAttrs t → m a → m a
dynAttr elementTag attrs child = snd <$> dynAttr' elementTag attrs child

{-# INLINABLE dynAttr' #-}
dynAttr' ∷ ∀ t m a. MonadWidget t m
         ⇒ Text → DynAttrs t → m a → m (El t, a)
dynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

-----------------------------------------
-- SVG elements with static attributes.

{-# INLINABLE attr #-}
attr ∷ ∀ t m a. MonadWidget t m
     ⇒ Text → AttrMap → m a → m a
attr elementTag attrs child = dynAttr elementTag (constDyn attrs) child

{-# INLINABLE attr' #-}
attr' ∷ ∀ t m a. MonadWidget t m
      ⇒ Text → AttrMap → m a → m (El t, a)
attr' elementTag attrs child = dynAttr' elementTag (constDyn attrs) child

-- | Shortcut for attr when you want to set only the 'class' attribute.
svgClass ∷ ∀ t m a. MonadWidget t m
         ⇒ Text → Text → m a → m a
svgClass elementTag c child = attr elementTag ("class" =: c) child


---------------------------------------------------------------
-- SVG elements with neither dynamic attributes nor children.

childAttr ∷ ∀ t m. MonadWidget t m
          ⇒ Text → AttrMap → m ()
childAttr elementTag attrs = dynAttr elementTag (constDyn attrs) (return ())

childAttr' ∷ ∀ t m. MonadWidget t m
           ⇒ Text → AttrMap → m (El t, ())
childAttr' elementTag attrs = dynAttr' elementTag (constDyn attrs) (return ())

-------------------------------------
-- SVG elements without attributes.

{-# INLINABLE svg #-}
svg ∷ ∀ t m a. MonadWidget t m
    ⇒ Text → m a → m a
svg elementTag child = attr elementTag Map.empty child

{-# INLINABLE svg' #-}
svg' ∷ ∀ t m a. MonadWidget t m
     ⇒ Text → m a → m (El t, a)
svg' elementTag child = attr' elementTag (Map.empty ∷ AttrMap) child
