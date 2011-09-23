
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module Main where

import Data.Maybe
import Data.List
import Data.Ratio
import Data.Monoid
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import System.Exit
import System.IO
import System.Directory

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.Named
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

import XMonad.Actions.AngleFocus
import XMonad.Layout.Border

------------------------------------------------------------------------
-- Theme
--

defaultFont :: String
defaultFont = "-misc-fixed-medium-r-normal--12-*"

promptTheme :: XPConfig
promptTheme = XPC {
  font                = defaultFont,
  bgColor             = "#4444ff",
  fgColor             = "#000000",
  fgHLight            = "#ffffff",
  bgHLight            = "#000000",
  borderColor         = "#4444ff",
  promptBorderWidth   = 0,
  promptKeymap        = defaultXPKeymap,
  completionKey       = xK_Tab,
  position            = Bottom,
  height              = 12,
  historySize         = 256,
  historyFilter       = id,
  defaultText         = [],
  autoComplete        = Nothing,
  showCompletionOnTab = False }

tabTheme :: Theme
tabTheme = Theme {
  activeColor         = "#000",
  inactiveColor       = "#000",
  urgentColor         = "#ff0",
  activeBorderColor   = "#fff",
  inactiveBorderColor = "#333",
  urgentBorderColor   = "#0f0",
  activeTextColor     = "#fff",
  inactiveTextColor   = "#666",
  urgentTextColor     = "#F00",
  fontName            = defaultFont,
  decoWidth           = 200,
  decoHeight          = 12 }

gsConfig :: HasColorizer a => GSConfig a
gsConfig = GSConfig {
  gs_cellheight = 30,
  gs_cellwidth = 240,
  gs_cellpadding = 10,
  gs_colorizer = defaultColorizer,
  gs_font = defaultFont,
  gs_navigate = M.fromList [
    ((0, xK_h), \(x, y) -> (x-1, y)),
    ((0, xK_t), \(x, y) -> (x, y+1)),
    ((0, xK_n), \(x, y) -> (x, y-1)),
    ((0, xK_s), \(x, y) -> (x+1, y)),
    ((0, xK_space), const (0, 0)) ],
  gs_originFractX = 1/2,
  gs_originFractY = 1/2 }

wsgrid = ask >>=
  gridselect gsConfig { gs_cellheight = 50, gs_cellwidth = 100 , gs_cellpadding = 45 } .
  map (join (,)) . workspaces . config

------------------------------------------------------------------------
-- Key bindings
--

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf = M.fromList $ [
  -- launcher
  ((modm,   xK_Return    ), spawn $ XMonad.terminal conf),
  ((smodm,  xK_Return    ), spawn "emacs"),
  ((modm,   xK_semicolon ), shellPrompt promptTheme),
  -- prompt
  ((smodm,  xK_semicolon ), sshPrompt promptTheme),
  -- Kill focused window
  ((smodm,  xK_c         ), kill),
  -- Change layout
  ((modm,   xK_space     ), sendMessage NextLayout),
  ((smodm,  xK_space     ), setLayout $ XMonad.layoutHook conf),
  -- Resize viewed windows to the correct size
  ((modm,   xK_quoteleft ), refresh),
  -- move focus
  ((modm,   xK_w         ), windows W.focusDown),
  ((modm,   xK_v         ), windows W.focusUp),
  ((modm,   xK_m         ), windows W.focusMaster),
  ((modm,   xK_h         ), angleFocus (pi * 5 / 6) (pi / 3)),
  ((modm,   xK_t         ), angleFocus (pi * 2 / 6) (pi / 3)),
  ((modm,   xK_n         ), angleFocus (pi * 8 / 6) (pi / 3)),
  ((modm,   xK_s         ), angleFocus (pi * 11 / 6) (pi / 3)),
  -- swap window
  ((smodm,  xK_w         ), windows W.swapDown),
  ((smodm,  xK_v         ), windows W.swapUp),
  ((smodm,  xK_m         ), windows W.swapMaster),
  ((smodm,  xK_h         ), angleSwap (pi * 5 / 6) (pi / 3)),
  ((smodm,  xK_t         ), angleSwap (pi * 2 / 6) (pi / 3)),
  ((smodm,  xK_n         ), angleSwap (pi * 8 / 6) (pi / 3)),
  ((smodm,  xK_s         ), angleSwap (pi * 11 / 6) (pi / 3)),
  -- magnifier
  ((cmodm,  xK_w         ), sendMessage MagnifyMore),
  ((cmodm,  xK_v         ), sendMessage MagnifyLess),
  ((cmodm,  xK_m         ), sendMessage Toggle),
  -- toggle xmobar
  ((cmodm,  xK_z         ), sendMessage ToggleStruts),
  -- Shrink the master area
  ((modm,   xK_comma     ), sendMessage Shrink),
  -- Expand the master area
  ((modm,   xK_period    ), sendMessage Expand),
  -- Increment the number of windows in the master area
  ((smodm,  xK_comma     ), sendMessage (IncMasterN 1)),
  -- Deincrement the number of windows in the master area
  ((smodm,  xK_period    ), sendMessage (IncMasterN (-1))),
  -- Push window back into tiling
  ((smodm,  xK_quoteleft ), withFocused $ windows . W.sink),
  -- Grid select
--  ((modm,   xK_g         ), goToSelected gsConfig),
  ((modm,   xK_z         ), wsgrid >>= maybe (return ()) (windows . W.greedyView)),
  ((smodm,  xK_z         ), wsgrid >>= maybe (return ()) (windows . W.shift)),
  -- (Quit|Restart) xmonad
  ((scmodm, xK_quoteright), io $ exitWith ExitSuccess),
  ((modm,   xK_quoteright), spawn "xmonad --restart"),
  -- reset xrandr
  ((modm,   xK_x         ), spawn "xrandr --output LVDS1 --off"),
  ((smodm,  xK_x         ), spawn "xrandr --output LVDS1 --auto")]
  ++

  [((m .|. modm, k), windows s)
    | (i, k) <- zip (workspaces conf) numsyms
    , (s, m) <- [(W.greedyView i, 0), (W.shift i, shiftMask)]]
  ++

  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_a, xK_o, xK_e] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  where

  modm = modMask conf
  smodm = modm .|. shiftMask
  cmodm = modm .|. controlMask
  scmodm = modm .|. shiftMask .|. controlMask

  numsyms = [xK_exclam, xK_at, xK_numbersign, xK_dollar, xK_percent,
    xK_asciicircum, xK_ampersand, xK_asterisk, xK_parenleft, xK_parenright]

------------------------------------------------------------------------
-- Mouse bindings
--

mouseBindings' :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' (XConfig {XMonad.modMask = modm}) = M.fromList [

  -- mod-button1, Set the window to floating mode and move by dragging
  ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),

  -- mod-button2, Raise the window to the top of the stack
  ((modm, button2), \w -> focus w >> windows W.shiftMaster),

  -- mod-button3, Set the window to floating mode and resize by dragging
  ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)]

  -- you may also bind events to the mouse scroll wheel (button4 and button5)

------------------------------------------------------------------------
-- Layouts
--

layoutHook' =
  avoidStruts $
    setBorder 0 0 (named "Tab" (addTabs shrinkText tabTheme Simplest)) |||
    setBorder 2 1 (magnifier (
      tiled |||
      Mirror tiled |||
      Circle |||
      named "IM" (withIM (1%6) (ClassName "Skype") Grid)))

  where

  tiled = Tall 1 (3/100) (2/3)

------------------------------------------------------------------------
-- Window rules
--

manageHook' :: ManageHook
manageHook' = composeAll [
  className =? "MPlayer"        --> doFloat,
  className =? "Gimp"           --> doFloat,
  resource  =? "desktop_window" --> doIgnore,
  resource  =? "kdesktop"       --> doIgnore ]
  <+> manageDocks

------------------------------------------------------------------------
-- Event handling
--

handleEventHook' :: Event -> X All
handleEventHook' = mempty

------------------------------------------------------------------------
-- Status bars and logging
--

logHook' :: ScreenId -> Handle -> X ()
logHook' n h = dynamicLogWithPP PP {
  ppCurrent         = xmobarColor "#f33" "" . wrap "[" "]",
  ppVisible         = xmobarColor "#cc0" "",
  ppHidden          = id,
  ppHiddenNoWindows = xmobarColor "#666" "",
  ppUrgent          = xmobarColor "#f33" "#ff0",
  ppSep             = wrap " " " " $ xmobarColor "" "#fff" " ",
  ppWsSep           = "",
  ppTitle           = xmobarColor "#0f0" "" . shorten 60,
  ppLayout          = id,
  ppOrder           = id,
  ppSort            = getSortByTag,
  ppExtras          = [],
  ppOutput          = hPutStrLn h }

  where

  sepBy :: String -> [String] -> String
  sepBy sep = intercalate sep . filter (not . null)

  dynamicLogWithPP :: PP -> X ()
  dynamicLogWithPP pp = dynamicLogString pp >>= io . ppOutput pp
  
  dynamicLogString :: PP -> X String
  dynamicLogString pp = do
    winset <- screenWorkspace n >>=
      maybe (gets windowset) ((<$> gets windowset) . W.view)
    urgents <- readUrgents
    sort' <- ppSort pp
    let ld = description . W.layout . W.workspace . W.current $ winset
    let ws = pprWindowSet sort' urgents pp winset
    wt <- maybe (return "") (fmap show . getName) . W.peek $ winset
    extras <- mapM (`catchX` return Nothing) $ ppExtras pp
    return $ encodeOutput . sepBy (ppSep pp) . ppOrder pp $
      [ ws
      , ppLayout pp ld
      , ppTitle  pp wt
      ]
      ++ catMaybes extras

------------------------------------------------------------------------
-- Startup hook
--

startupHook' :: X ()
startupHook' = return ()

------------------------------------------------------------------------
-- Run XMonad
--

main :: IO ()
main = do
  xmobars <- countScreens >>=
    mapM (\n -> (,) (S n) <$> spawnPipe ("xmobar -x " ++ show n)) .
    (\n -> [0 .. n-1])
  xmonad XConfig {
    -- simple stuff
    terminal           = "urxvt",
    focusFollowsMouse  = False,
    borderWidth        = 0,
    modMask            = mod4Mask,
    numlockMask        = mod2Mask,
    workspaces         = [[c] | c <- ['1'..'9']],
    normalBorderColor  = "#666",
    focusedBorderColor = "#f33",
    -- key bindings
    keys               = keys',
    mouseBindings      = mouseBindings',
    -- hooks, layouts
    layoutHook         = layoutHook',
    manageHook         = manageHook',
    handleEventHook    = handleEventHook',
    logHook            = mapM_ (uncurry logHook') xmobars,
    startupHook        = startupHook' }

