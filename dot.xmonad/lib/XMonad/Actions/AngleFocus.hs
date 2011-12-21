
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances,
             PatternGuards,
             DeriveDataTypeable #-}

module XMonad.Actions.AngleFocus where

import Data.Function
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Control.Arrow

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M

angleFocus :: Float -> Float -> X ()
angleFocus a1 a2 = angleSelect a1 a2 >>= maybe (return ()) focus

angleSwap :: Float -> Float -> X ()
angleSwap a1 a2 = angleSelect a1 a2 >>= maybe (return ()) swapFocus

angleSelect :: Float -> Float -> X (Maybe Window)
angleSelect a1 a2 = do
    pos <- getWindowsPos'
    return $ case pos of
        Just (fw, nws) -> fmap fst $ listToMaybe $
            sortBy (on compare (hypotenuse (snd fw) . snd)) $
                filter (inRange a1 a2 (snd fw) . snd) nws
        Nothing -> Nothing

swapFocus :: Window -> X ()
swapFocus w = windows $ \winset ->
    let cur = W.current winset
        ws = W.workspace cur
        stack = W.stack ws in
    winset { W.current = cur { W.workspace = ws { W.stack = f <$> stack }}}
    where
    f s@(W.Stack f u d) = case break (w ==) u of
        (u1, f' : u2) -> W.Stack f u2 (reverse u1 ++ [f'] ++ d)
        _ -> case break (w ==) d of
            (d1, f' : d2) -> W.Stack f (reverse d1 ++ [f'] ++ u) d2
            _ -> s

getWindowsPos :: X (Maybe ((Window, Rectangle), [(Window, Rectangle)]))
getWindowsPos = do
    W.StackSet (W.Screen workspace _ (SD rect)) _ _ floating <- gets windowset
    case W.stack workspace of
        Nothing -> return Nothing
        Just stack' -> do
            (managed, _) <- runLayout workspace rect
            let wpos = managed ++
                    M.toList (M.map (scaleRationalRect rect) floating)
            return $ case partition ((W.focus stack' ==) . fst) wpos of
                ([], _) -> Nothing
                (fw : _, nws) -> Just (fw, nws)

getWindowsPos' :: X (Maybe ((Window, (Int, Int)), [(Window, (Int, Int))]))
getWindowsPos' = fmap (second f *** map (second f)) <$> getWindowsPos where
    f (Rectangle x y x' y') =
        (fromIntegral x + div (fromIntegral x') 2,
        fromIntegral y + div (fromIntegral y') 2)

hypotenuse :: (Int, Int) -> (Int, Int) -> Float
hypotenuse (x1, y1) (x2, y2) =
    sqrt $ fromIntegral $ join (*) (x2 - x1) + join (*) (y2 - y1)

angle :: (Int, Int) -> (Int, Int) -> Maybe Float
angle p1 p2 =
    let h = hypotenuse p1 p2 in
    if h == 0
        then Nothing
        else Just $ (if snd p2 > snd p1 then id else negate)
            (acos (fromIntegral (fst p2 - fst p1) / h))

inRange :: Float -> Float -> (Int, Int) -> (Int, Int) -> Bool
inRange a1 a2 fpos pos =
    case angle fpos pos of
        Nothing -> False
        Just a3 -> a3 - a1 -
            (pi * 2) * (fromIntegral (floor ((a3 - a1) / (pi * 2)))) < a2

