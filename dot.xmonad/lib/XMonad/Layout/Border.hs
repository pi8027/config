
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module XMonad.Layout.Border where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

data Border a = Border Dimension Dimension deriving (Read, Show)

instance LayoutModifier Border Window where
  modifyLayout (Border fwidth nwidth) w r = do
    dpy <- asks display
    case W.stack w of
      Nothing -> return () 
      Just (W.Stack fw uws dws) ->
        io $ mapM_ (uncurry (setWindowBorderWidth dpy))
          ((fw, fwidth) : map (flip (,) nwidth) (uws ++ dws))
    runLayout w r

setBorder :: Dimension -> Dimension -> l a -> ModifiedLayout Border l a
setBorder n m = ModifiedLayout (Border n m)

