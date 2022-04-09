-- IMPORTS

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ServerMode

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

import Graphics.X11.ExtraTypes.XF86

-- MY SETTINGS
myTerminal      = "alacritty"
myBrowser       = "qutebrowser"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False 

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 3 

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["<fn=1>\xf15c</fn>¹", -- document icon for writing
                   "<fn=1>\xfa9e</fn>²", -- globe icon for browsing
                   "<fn=1>\xf121</fn>³", -- dev icon for game dev
                   "<fn=1>\xf722</fn>⁴", -- music file icon for composition
                   "<fn=1>\xf1fc</fn>⁵", -- paint icon for art
                   "<fn=1>\xf5aa</fn>⁶", -- blender icon for blender
                   "<fn=1>\xf616</fn>⁷", -- money icon for finances
                   "<fn=1>\xfce8</fn>⁸", -- rice icon for ricing
                   "<fn=1>\xe370</fn>⁹"] -- glitter icon for extra

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#282828"
myFocusedBorderColor = "#458588"

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "ranger" spawnRanger findRanger manageRanger
                , NS "octave" spawnOctave findOctave manageOctave
                , NS "keepassxc" spawnKeepassXC findKeepassXC manageKeepassXC
                , NS "btm" spawnBtm findBtm manageBtm
                , NS "mu4e" spawnMu4e findMu4e manageMu4e
                , NS "helpmenu" spawnHelp findHelp manageHelp
                , NS "nuclear" spawnNuclear findNuclear manageNuclear
                ]
    where
      spawnTerm  = myTerminal ++ " -t scratchpad"
      findTerm   = title =? "scratchpad"
      manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
      spawnRanger = myTerminal ++ " -t ranger -e ranger"
      findRanger = title =? "ranger"
      manageRanger = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
      spawnOctave = myTerminal ++ " -t octave -e octave"
      findOctave = title =? "octave"
      manageOctave = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
      spawnBtm = myTerminal ++ " -t btm -e btm"
      findBtm = title =? "btm"
      manageBtm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
      spawnKeepassXC = "keepassxc"
      findKeepassXC = className =? "KeePassXC"
      manageKeepassXC = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
      spawnMu4e = "~/.xmonad/scratchpad-mu4e.sh"
      findMu4e = title =? "scratch_mu4e"
      manageMu4e = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
      spawnHelp = myTerminal ++ " -t xmonad_helpmenu -e w3m ~/.xmonad/helpmenu.txt"
      findHelp = title =? "xmonad_helpmenu"
      manageHelp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
      spawnNuclear = "nuclear"
      findNuclear = className =? "nuclear"
      manageNuclear = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,                xK_Return), spawn $ XMonad.terminal conf)

    -- launch emacsclient
    , ((modm,                xK_a), spawn "emacsclient -c -a 'emacs'") 

    -- launch browser
    , ((modm,                xK_s), spawn "qutebrowser") 

    -- control brightness from kbd
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10")

    -- control volume from kbd
    , ((0, xF86XK_AudioLowerVolume), spawn "pamixer -d 10")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pamixer -i 10")
    , ((0, xF86XK_AudioMute), spawn "pamixer -t")

    -- control background music
    , ((0, xF86XK_AudioPlay), spawn "curl -X POST 'http://localhost:3100/nuclear/player/play-pause' -H 'accept: application/json'")
    , ((0, xF86XK_AudioPrev), spawn "curl -X POST 'http://localhost:3100/nuclear/player/previous' -H 'accept: application/json'")
    , ((0, xF86XK_AudioNext), spawn "curl -X POST 'http://localhost:3100/nuclear/player/next' -H 'accept: application/json'")

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run -nf '#282828' -nf '#ebdbb2' -sb '#458588' -sf '#ebdbb2' -fn 'UbuntuMono-R:regular:pixelsize=28' -l 4 -p '➤'")

    -- launch workspace switch dmenu script
    , ((modm,               xK_w     ), spawn "~/.xmonad/workspace-select.sh")

    -- close focused window
    , ((modm, xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_r     ), refresh)

    -- Move focus to the next window
    , ((mod1Mask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Swap the focused window and the master window
    , ((modm,               xK_m), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    , ((modm              , xK_f), namedScratchpadAction myScratchPads "ranger")
    , ((modm              , xK_x), namedScratchpadAction myScratchPads "keepassxc")
    , ((modm              , xK_z), namedScratchpadAction myScratchPads "terminal")
    , ((modm              , xK_b), namedScratchpadAction myScratchPads "btm")
    , ((modm              , xK_o), namedScratchpadAction myScratchPads "octave")
    , ((modm              , xK_e), namedScratchpadAction myScratchPads "mu4e")
    , ((modm              , xK_slash), namedScratchpadAction myScratchPads "helpmenu")
    , ((modm              , xK_n), namedScratchpadAction myScratchPads "nuclear")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    --, ((modm .|. shiftMask, xK_r     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_c     ), spawn "xmonad --recompile; xmonad --restart")

    -- Lock with xsecurelock
    , ((modm .|. shiftMask, xK_l     ), spawn "xsecurelock")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++


    --mod-[1..9], Switch to workspace N
    --mod-shift-[1..9], Move client to workspace N

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
spcPx = 6
mySpacing = spacingRaw False (Border spcPx spcPx spcPx spcPx) True (Border spcPx spcPx spcPx spcPx) True
myLayout = avoidStruts $ (mySpacing $ (tiled ||| Mirror tiled ||| Full))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 5/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "KeePassXC"      --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
    , className =? "nuclear"        --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
    , title =? "octave"             --> (customFloating $ W.RationalRect 0 0.03 1 0.5)
    , title =? "scratchpad"         --> (customFloating $ W.RationalRect 0 0.03 1 0.5)
    , title =? "ranger"             --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
    , title =? "btm"                --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
    , title =? "scratch_mu4e"       --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
    , title =? "xmonad_helpmenu"    --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
    , className =? "fl64.exe"       --> (customFloating $ W.RationalRect 0 0 1 1)
    , className =? "Brave-browser"  --> (customFloating $ W.RationalRect 0 0 1 1)
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = serverModeEventHook
------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook = return () 


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "~/.xmonad/startup.sh"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe ("xmobar /home/emmet/.config/xmobar/xmobarrc")
  xmonad $ docks def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP $ xmobarPP
          {
             ppOutput = \x ->  hPutStrLn xmproc x,
             ppTitle = xmobarColor "#458588" "" . shorten 10,
             ppCurrent = xmobarColor "#458588" "" . wrap ("<box type=Bottom Top width=2 mb=2 color=#438588>")"</box>"
,
             ppVisible = xmobarColor "#ebdbb2" "",
             ppHidden = xmobarColor "#a89984" "",
             ppOrder = \(ws:_) -> [ws],
             ppSep = " "
          },
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
