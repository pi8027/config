{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module Main where

import Data.Maybe
import Data.List
import Data.Ratio
import Data.Monoid
import qualified Data.Map as M
import Data.Time
import Control.Applicative
import Control.Monad
import System.Exit
import System.IO
import System.Directory

import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.Named
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import XMonad.Actions.AngleFocus
import XMonad.Layout.Border

--------------------------------------------------------------------------------
-- Theme
--

defaultFont, defaultFontL :: String
defaultFont = "-misc-fixed-medium-r-normal--10-*"
defaultFontL = "-misc-fixed-medium-r-normal--20-*"

promptTheme :: XPConfig
promptTheme = XPC {
  font                = defaultFont,
  bgColor             = "#000",
  fgColor             = "#fff",
  fgHLight            = "#000",
  bgHLight            = "#fff",
  borderColor         = "#0f0",
  promptBorderWidth   = 1,
  promptKeymap        = defaultXPKeymap,
  completionKey       = xK_Tab,
  position            = Bottom,
  height              = 14,
  historySize         = 256,
  historyFilter       = id,
  defaultText         = [],
  autoComplete        = Nothing,
  showCompletionOnTab = False }

tabTheme :: Theme
tabTheme = Theme {
  activeColor         = "#fff",
  inactiveColor       = "#000",
  urgentColor         = "#ff0",
  activeBorderColor   = "#000",
  inactiveBorderColor = "#fff",
  urgentBorderColor   = "#0f0",
  activeTextColor     = "#000",
  inactiveTextColor   = "#fff",
  urgentTextColor     = "#F00",
  fontName            = defaultFont,
  decoWidth           = 200,
  decoHeight          = 14,
  windowTitleAddons   = [],
  windowTitleIcons    = [] }

gsConfig :: HasColorizer a => GSConfig a
gsConfig = GSConfig {
  gs_cellheight = 40,
  gs_cellwidth = 240,
  gs_cellpadding = 10,
  gs_colorizer = defaultColorizer,
  gs_font = defaultFontL,
  gs_navigate = gsNavigation,
  gs_originFractX = 1/2,
  gs_originFractY = 1/2 }

gsNavigation :: TwoD a (Maybe a)
gsNavigation =
  makeXEventhandler $ shadowWithKeymap navKeyMap $ const gsNavigation
  where
  navKeyMap = M.fromList [
     ((0,           xK_Escape), cancel),
     ((controlMask, xK_m     ), select),
     ((0,           xK_Return), select),
     ((0,           xK_slash ), substringSearch gsNavigation),
     ((0,           xK_h     ), move (-1,0) >> gsNavigation),
     ((0,           xK_t     ), move (0,1) >> gsNavigation),
     ((0,           xK_n     ), move (0,-1) >> gsNavigation),
     ((0,           xK_s     ), move (1,0) >> gsNavigation),
     ((0,           xK_Tab   ), moveNext >> gsNavigation),
     ((shiftMask,   xK_Tab   ), movePrev >> gsNavigation)]

wsgrid = do
  workspaceList >>= gridselect gsConfig' . map (join (,))
  where
  gsConfig' = gsConfig {
    gs_cellwidth = 150,
    gs_cellpadding = 30 }

--------------------------------------------------------------------------------
-- Topic Space
--

topics :: [Topic]
topics = ["work", "web", "irc", "pdf"]

topicConfig :: TopicConfig
topicConfig = TopicConfig {
  topicDirs = M.fromList [],
  defaultTopicAction = const $ return (),
  defaultTopic = "work",
  maxTopicHistory = 4,
  topicActions = M.fromList [] }

--------------------------------------------------------------------------------
-- Workspace
--

workspaceList :: X [WorkspaceId]
workspaceList = gets $
  (\(W.StackSet c v h _) ->
    map W.tag (h ++ map W.workspace (c : v))) . windowset

addWorkspacePrompt :: XPConfig -> X ()
addWorkspacePrompt c = do
  ws <- workspaceList
  name' <- inputPrompt c "add workspace"
  case name' of
    Just name | name `notElem` ws && not (null name) -> addWorkspace name
    _ -> return ()

--------------------------------------------------------------------------------
-- Screen Snapshot
--

sshot :: String -> X ()
sshot option = do
    ZonedTime (LocalTime day time) _ <- io getZonedTime
    let dir = "ss/" ++ showGregorian day ++ "/"
        filepath = dir ++ show time ++ ".png"
    io $ (doesDirectoryExist dir >>= (`unless` createDirectory dir))
    spawn $ "import " ++ option ++ " " ++ filepath ++
        " && chromium ~/" ++ filepath

sshotCurrent :: X ()
sshotCurrent =
    gets (fmap W.focus . W.stack . W.workspace . W.current . windowset) >>=
        maybe (return ()) (sshot . ("-window " ++) . show)

sshotRoot :: X ()
sshotRoot = sshot "-window root"

sshotSelect :: X ()
sshotSelect = sshot ""

--------------------------------------------------------------------------------
-- Key bindings
--

keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
  -- launcher
  ((modm,   xK_Return    ), spawn "urxvt -e zsh -c tmux"),
  ((cmodm,  xK_Return    ), spawn "urxvt"),
  ((smodm,  xK_Return    ), spawn "emacs"),
  ((modm,   xK_semicolon ), shellPrompt promptTheme),
  ((modm,   xK_q         ),
    submap . M.fromList $ [
      ((0, xK_semicolon), spawn "xlock -bg black -fg white"),
      ((0, xK_q        ), spawn "xrandr --output LVDS1 --off"),
      ((0, xK_j        ), spawn "xrandr --output LVDS1 --auto")]),
  ((modm,   xK_j         ),
    submap . M.fromList $ [
      ((0, xK_semicolon), sshotCurrent),
      ((0, xK_q        ), sshotRoot),
      ((0, xK_j        ), sshotSelect)]),
  -- prompt
  ((smodm,  xK_semicolon ), sshPrompt promptTheme),
  -- Kill focused window
  ((smodm,  xK_c         ), kill),
  -- Change layout
  ((modm,   xK_space     ), sendMessage NextLayout),
  ((smodm,  xK_space     ), setLayout $ layoutHook conf),
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
  ((modm,   xK_z         ), sendMessage ToggleStruts),
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
  -- Workspace
  ((modm,   xK_u         ),
    wsgrid >>= maybe (return ()) (windows . W.greedyView)),
  ((smodm,  xK_u         ), wsgrid >>= maybe (return ()) (windows . W.shift)),
  ((modm,   xK_i         ), Main.addWorkspacePrompt promptTheme),
  ((smodm,  xK_i         ), removeWorkspace),
  -- (Quit|Restart) xmonad
  ((scmodm, xK_quoteright), io $ exitWith ExitSuccess),
  ((modm,   xK_quoteright), spawn "killall trayer ; xmonad --restart")]

  ++

--  [((modm .|. m, k), f i)
--    | (i, k) <- zip [1..] numsyms
--    , (f, m) <- [
--        (switchNthLastFocused topicConfig, 0),
--        (shiftNthLastFocused, shiftMask)]]
--  ++

  [((modm .|. m, k), windows $ f n) |
    (n, k) <- zip topics numsyms,
    (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  ++

  [((modm .|. m, k), screenWorkspace s >>= flip whenJust (windows . f))
    | (s, k) <- zip [0..] [xK_a, xK_o, xK_e]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  where

  modm = modMask conf
  smodm = modm .|. shiftMask
  cmodm = modm .|. controlMask
  scmodm = modm .|. shiftMask .|. controlMask

  numsyms = [xK_exclam, xK_at, xK_numbersign, xK_dollar, xK_percent,
    xK_asciicircum, xK_ampersand, xK_asterisk, xK_parenleft, xK_parenright]

--------------------------------------------------------------------------------
-- Mouse bindings
--

mouseBindings' :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' _ = M.empty

--------------------------------------------------------------------------------
-- Layouts
--

layoutHook' =
  avoidStruts $
    setBorder 0 0 (named "Tab" tabbed) |||
    setBorder 2 1 (magnifier (
      tiled |||
      Mirror tiled |||
      Grid |||
      Circle)) |||
    named "IM" (setBorder 1 1
      (withIM (1%5) (ClassName "Skype") (setBorder 0 0 tabbed ||| Grid)))

  where

  tiled = Tall 1 (3/100) (1/2)
  tabbed = addTabs shrinkText tabTheme Simplest

--------------------------------------------------------------------------------
-- Window rules
--

manageHook' :: ManageHook
manageHook' =
  composeAll [
    resource  =? "desktop_window" --> doIgnore,
    resource  =? "kdesktop"       --> doIgnore,
    className =? "MPlayer"        --> doFloat,
    className =? "Gimp"           --> doFloat,
    className =? "Skype"          -->
      doF (W.modify' rotateUp . W.view "im" . W.shift "im")]
  <+> manageDocks
  where
  rotateUp (W.Stack t [] rs) = W.Stack t (reverse rs) []
  rotateUp (W.Stack t ls rs) = W.Stack t (init ls) (rs ++ [last ls])

--------------------------------------------------------------------------------
-- Event handling
--

handleEventHook' :: Event -> X All
handleEventHook' = mempty

--------------------------------------------------------------------------------
-- Status bars and logging

logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP PP {
  ppCurrent         = xmobarColor "#f33" "",
  ppVisible         = xmobarColor "#cc0" "",
  ppHidden          = id,
  ppHiddenNoWindows = xmobarColor "#666" "",
  ppUrgent          = xmobarColor "#f33" "#ff0",
  ppSep             = wrap " " " " $ xmobarColor "" "#fff" " ",
  ppWsSep           = " ",
  ppTitle           = xmobarColor "#0f0" "" . shorten 60,
  ppLayout          = id,
  ppOrder           = id,
  ppSort            = getSortByTag,
  ppExtras          = [],
  ppOutput          = hPutStrLn h }

--------------------------------------------------------------------------------
-- Startup hook
--

startupHook' :: X ()
startupHook' = return ()

--------------------------------------------------------------------------------
-- Run XMonad
--

main :: IO ()
main = do
  xmobarHandle <- spawnPipe "xmobar"
  spawn "trayer --edge top --align right --width 10 --height 16 \
    \--transparent true --alpha 0 --tint 0x000000 --padding 0 --distance 0"
  xmonad XConfig {
    -- simple stuff
    terminal           = "urxvt",
    focusFollowsMouse  = False,
    borderWidth        = 0,
    modMask            = mod4Mask,
    workspaces         = topics,
    normalBorderColor  = "#666",
    focusedBorderColor = "#f33",
    -- key bindings
    keys               = keys',
    mouseBindings      = mouseBindings',
    -- hooks, layouts
    layoutHook         = layoutHook',
    manageHook         = manageHook',
    handleEventHook    = handleEventHook',
    logHook            = logHook' xmobarHandle,
    startupHook        = startupHook' }
