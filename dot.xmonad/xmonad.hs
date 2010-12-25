
import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf = M.fromList $ [
    ((smodm, xK_Return), spawn $ XMonad.terminal conf),                           -- launch a terminal
    ((modm,  xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\""), -- launch dmenu
    ((smodm, xK_p     ), spawn "gmrun"),                                          -- launch gmrun
    ((smodm, xK_c     ), kill),                                                   -- close focused window
    ((modm,  xK_space ), sendMessage NextLayout),                                 -- Rotate through the available layout algorithms
    ((smodm, xK_space ), setLayout $ XMonad.layoutHook conf),                     --  Reset the layouts on the current workspace to default
    ((modm,  xK_n     ), refresh),                                                -- Resize viewed windows to the correct size
    ((modm,  xK_Tab   ), windows W.focusDown),                                    -- Move focus to the next window
    ((modm,  xK_j     ), windows W.focusDown),                                    -- Move focus to the next window
    ((modm,  xK_k     ), windows W.focusUp),                                      -- Move focus to the previous window
    ((modm,  xK_m     ), windows W.focusMaster),                                  -- Move focus to the master window
    ((modm,  xK_Return), windows W.swapMaster),                                   -- Swap the focused window and the master window
    ((smodm, xK_j     ), windows W.swapDown),                                     -- Swap the focused window with the next window
    ((smodm, xK_k     ), windows W.swapUp),                                       -- Swap the focused window with the previous window
    ((modm,  xK_h     ), sendMessage Shrink),                                     -- Shrink the master area
    ((modm,  xK_l     ), sendMessage Expand),                                     -- Expand the master area
    ((modm,  xK_t     ), withFocused $ windows . W.sink),                         -- Push window back into tiling
    ((modm,  xK_comma ), sendMessage (IncMasterN 1)),                             -- Increment the number of windows in the master area
    ((modm,  xK_period), sendMessage (IncMasterN (-1))),                          -- Deincrement the number of windows in the master area
    ((smodm, xK_q     ), io $ exitWith ExitSuccess),                              -- Quit xmonad
    ((modm,  xK_q     ), spawn "xmonad --recompile; xmonad --restart")]           -- Restart xmonad
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows s)
        | (i, k) <- zip (workspaces conf) numsyms
        , (s, m) <- [(W.greedyView i, 0), (W.shift i, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    where
    modm = modMask conf
    smodm = modm .|. shiftMask
    numsyms = [xK_exclam, xK_at, xK_numbersign, xK_dollar, xK_percent,
        xK_asciicircum, xK_ampersand, xK_asterisk, xK_parenleft, xK_parenright]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
--
myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook
--
myStartupHook = do
    spawn "urxvt"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = xmonad defaultConfig {
      -- simple stuff
        terminal           = "urxvt",
        focusFollowsMouse  = False,
        borderWidth        = 3,
        modMask            = mod4Mask,
        numlockMask        = mod2Mask,
        workspaces         = [[c] | c <- ['1'..'9']],
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

