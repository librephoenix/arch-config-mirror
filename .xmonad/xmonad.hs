-- IMPORTS

import qualified Data.Map as M
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Dwindle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Spacing
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- CUSTOM COLORS
colorSchemeList, colorSchemePrettyList :: [String]
colorBgNormalList, colorFgNormalList :: [String]
color01NormalList, color01BrightList, color02NormalList, color02BrightList :: [String]
color03NormalList, color03BrightList, color04NormalList, color04BrightList :: [String]
color05NormalList, color05BrightList, color06NormalList, color06BrightList :: [String]
color07NormalList, color07BrightList, color08NormalList, color08BrightList :: [String]
colorFocusList, colorSecondaryList :: [String]
colorScheme, colorSchemePretty :: String
colorBgNormal, colorFgNormal :: [Char]
color01Normal, color01Bright, color02Normal, color02Bright :: String
color03Normal, color03Bright, color04Normal, color04Bright :: String
color05Normal, color05Bright, color06Normal, color06Bright :: String
color07Normal, color07Bright, color08Normal, color08Bright :: String
colorFocus, colorSecondary :: String
-- color scheme enumerations
gruvboxIndex, solarizedIndex, draculaIndex, tokyoNightIndex, oceanicNextIndex, ubuntuIndex :: Int
gruvboxIndex = 0

solarizedIndex = 1

draculaIndex = 2

tokyoNightIndex = 3

oceanicNextIndex = 4

ubuntuIndex = 5

-- choose a color scheme
myColorScheme = draculaIndex

colorSchemeList = ["gruvbox", "solarized", "dracula", "tokyo-night", "oceanic-next"]

colorSchemePrettyList = ["Gruvbox Dark", "Solarized Dark", "Dracula", "Tokyo Night", "Oceanic Next"]

colorBgNormalList = ["#282828", "#002b36", "#282a36", "#1a1b26", "#1b2b34"] -- normal bg

colorBgBrightList = ["#3b3838", "#113b3f", "#36343f", "#2a2b36", "#2b3b41"] -- lighter bg

trayerBgNormalList = ["0x00282828", "0x00002b36", "0x00282a36", "0x1a1b26", "0x1b2b34"] -- trayer tint

colorFgNormalList = ["#ebdbb2", "#839496", "#f8f8f2", "#a9b1d6", "#d8dee9"] -- normal fg

color01NormalList = ["#343428", "#073642", "#000000", "#32344a", "#29414f"] -- black

color01BrightList = ["#928374", "#002b36", "#555555", "#444b6a", "#405860"] -- bright black

color02NormalList = ["#cc241d", "#dc3ddf", "#ff5555", "#f7768e", "#ec5f67"] -- red

color02BrightList = ["#fb4934", "#cb4b16", "#ff1010", "#ff7a93", "#ff3130"] -- bright red

color03NormalList = ["#98971a", "#859900", "#50fa7b", "#9ece6a", "#99c794"] -- green

color03BrightList = ["#b8bb26", "#586e75", "#02fe03", "#b9f27c", "#66fa56"] -- bright green

color04NormalList = ["#d79921", "#b58900", "#f1fa8c", "#e0af68", "#fac863"] -- yellow

color04BrightList = ["#fabd2f", "#657b83", "#ffff02", "#ff9e64", "#ffca4f"] -- bright yellow

color05NormalList = ["#458588", "#268bd2", "#bd93f9", "#7aa2f7", "#6699cc"] -- blue

color05BrightList = ["#83a598", "#839496", "#4d31fd", "#7da6ff", "#4477ee"] -- bright blue

color06NormalList = ["#b16286", "#d33682", "#ff79c6", "#ad8ee6", "#c594c5"] -- magenta

color06BrightList = ["#d3869b", "#6c71c4", "#ff20d8", "#bb9af7", "#d864d8"] -- bright magenta

color07NormalList = ["#689d6a", "#2aa198", "#8be9fd", "#449dab", "#5fb3b3"] -- cyan

color07BrightList = ["#8ec07c", "#93a1a1", "#03feff", "#0db9d7", "#30d2d0"] -- bright cyan

color08NormalList = ["#a89984", "#eee8d5", "#bbbbbb", "#787c99", "#65737e"] -- white

color08BrightList = ["#ebdbb2", "#fdf6e3", "#ffffff", "#acb0d0", "#d8dee9"] -- bright white

colorFocusList = ["#458588", "#859900", "#ff79c6", "#ff9e64", "#c594c5"] -- focus and run launcher color

colorSecondaryList = ["#d79921", "#dc3ddf", "#bbbbbb", "#0db9d7", "#fac863"] -- secondary color

colorScheme = colorSchemeList !! myColorScheme

colorSchemePretty = colorSchemePrettyList !! myColorScheme

colorBgNormal = colorBgNormalList !! myColorScheme -- normal bg

colorBgBright = colorBgBrightList !! myColorScheme -- lighter bg

trayerBgNormal = trayerBgNormalList !! myColorScheme -- trayer tint

colorFgNormal = colorFgNormalList !! myColorScheme -- normal fg

color01Normal = color01NormalList !! myColorScheme -- black

color01Bright = color01BrightList !! myColorScheme -- bright black

color02Normal = color02NormalList !! myColorScheme -- red

color02Bright = color02BrightList !! myColorScheme -- bright red

color03Normal = color03NormalList !! myColorScheme -- green

color03Bright = color03BrightList !! myColorScheme -- bright green

color04Normal = color04NormalList !! myColorScheme -- yellow

color04Bright = color04BrightList !! myColorScheme -- bright yellow

color05Normal = color05NormalList !! myColorScheme -- blue

color05Bright = color05BrightList !! myColorScheme -- bright blue

color06Normal = color06NormalList !! myColorScheme -- magenta

color06Bright = color06BrightList !! myColorScheme -- bright magenta

color07Normal = color07NormalList !! myColorScheme -- cyan

color07Bright = color07BrightList !! myColorScheme -- bright cyan

color08Normal = color08NormalList !! myColorScheme -- white

color08Bright = color08BrightList !! myColorScheme -- bright white

colorFocus = colorFocusList !! myColorScheme -- focus and run launcher color

colorSecondary = colorSecondaryList !! myColorScheme

-- MY SETTINGS
-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = colorBgNormal
myFocusedBorderColor = colorFocus

-- Default apps
myTerminal, myBrowser :: String
myTerminal = "alacritty -o font.size=20"
myBrowser = "brave"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces =
  [ "<fn=1>\xf15c</fn>¹", -- document icon for writing
    "<fn=1>\xfa9e</fn>²", -- globe icon for browsing
    "<fn=1>\xf121</fn>³", -- dev icon for programming
    "<fn=1>\xf722</fn>⁴", -- music file icon for composition
    "<fn=1>\xf1fc</fn>⁵", -- paint icon for art
    "<fn=1>\xfa66</fn>⁶", -- video icon for recording/editing
    "<fn=1>\xf616</fn>⁷", -- money icon for finances
    "<fn=1>\xfce8</fn>⁸", -- rice icon for ricing
    "<fn=1>\xf11b</fn>⁹" -- gamepad icon for gaming
  ]

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm manageTerm,
    NS "ranger" spawnRanger findRanger manageRanger,
    NS "octave" spawnOctave findOctave manageOctave,
    NS "keepassxc" spawnKeepassXC findKeepassXC manageKeepassXC,
    NS "btm" spawnBtm findBtm manageBtm,
    NS "mu4e" spawnMu4e findMu4e manageMu4e,
    NS "helpmenu" spawnHelp findHelp manageHelp,
    NS "myuzi" spawnMyuzi findMyuzi manageMyuzi,
    NS "cfw" spawnCfw findCfw manageCfw,
    NS "pavucontrol" spawnPavucontrol findPavucontrol managePavucontrol,
    NS "discord" spawnDiscord findDiscord manageDiscord
  ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnRanger = myTerminal ++ " -t ranger -e ranger"
    findRanger = title =? "ranger"
    manageRanger = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnOctave = myTerminal ++ " -t octave -e octave"
    findOctave = title =? "octave"
    manageOctave = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnBtm = myTerminal ++ " -t btm -e btm"
    findBtm = title =? "btm"
    manageBtm = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnDiscord = "flatpak run com.discordapp.Discord"
    findDiscord = className =? "discord"
    manageDiscord = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnKeepassXC = "QT_QPA_PLATFORMTHEME=qt5ct keepassxc"
    findKeepassXC = className =? "KeePassXC"
    manageKeepassXC = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnMu4e = "~/.xmonad/scratchpad-mu4e.sh"
    findMu4e = title =? "scratch_mu4e"
    manageMu4e = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnHelp = myTerminal ++ " -t xmonad_helpmenu -e w3m ~/.xmonad/helpmenu.txt"
    findHelp = title =? "xmonad_helpmenu"
    manageHelp = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnMyuzi = "GDK_DPI_SCALE=1.5 myuzi"
    findMyuzi = className =? "Myuzi"
    manageMyuzi = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnCfw = "~/.config/xmobar/open-org-calendar.sh"
    findCfw = title =? "scratch_cfw"
    manageCfw = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.3
        t = 0.9 - h
        l = 0.65 - w
    spawnPavucontrol = "pavucontrol"
    findPavucontrol = className =? "Pavucontrol"
    managePavucontrol = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.3
        t = 0.9 - h
        l = 0.65 - w

-----------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ -- launch a terminal
      ((modm, xK_Return), spawn $ XMonad.terminal conf),
      -- launch emacsclient
      ((modm, xK_a), spawn "emacsclient -c -a 'emacs'"),
      -- launch browser
      ((modm, xK_s), spawn "brave"),
      -- control brightness from kbd
      ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +15"),
      ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 15-"),
      -- control kbd brightness from kbd
      ((0, xF86XK_KbdBrightnessUp), spawn "brightnessctl --device='asus::kbd_backlight' set +1 & xset r rate 350 100"),
      ((0, xF86XK_KbdBrightnessDown), spawn "brightnessctl --device='asus::kbd_backlight' set 1- & xset r rate 350 100"),
      ((shiftMask, xF86XK_MonBrightnessUp), spawn "brightnessctl --device='asus::kbd_backlight' set +1 & xset r rate 350 100"),
      ((shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl --device='asus::kbd_backlight' set 1- & xset r rate 350 100"),
      -- control volume from kbd
      ((0, xF86XK_AudioLowerVolume), spawn "pamixer -d 10"),
      ((0, xF86XK_AudioRaiseVolume), spawn "pamixer -i 10"),
      ((0, xF86XK_AudioMute), spawn "pamixer -t"),
      -- manage multiple monitors with kbd
      ((0, xF86XK_Explorer), spawn "/home/emmet/.local/bin/setup_external_monitor.sh"),
      ((0, xK_F8), spawn "/home/emmet/.local/bin/setup_external_monitor.sh"),
      -- launch dmenu
      ((modm, xK_semicolon), spawn ("dmenu_run -nb '" ++ colorBgNormal ++ "' -nf '" ++ color08Bright ++ "' -sb '" ++ colorFocus ++ "' -sf '" ++ color08Bright ++ "' -fn 'UbuntuMono-R:regular:pixelsize=28' -l 4 -p '➤'")),
      -- launch workspace switch dmenu script
      ((modm, xK_Tab), spawn ("~/.xmonad/workspace-select.sh '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")),
      -- launch app template dmenu script
      ((modm, xK_w), spawn ("~/.xmonad/template-select.sh '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")),
      -- launch virt-manager vm select dmenu script
      ((modm, xK_v), spawn ("~/.xmonad/vm-select.sh '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")),
      -- launch virt-manager vm select dmenu script
      ((modm .|. shiftMask, xK_v), spawn ("~/.xmonad/vm-app-select.sh '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")),
      -- close focused window
      ((modm, xK_q), kill),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_r), refresh),
      -- Move focus to the next window
      ((mod1Mask, xK_Tab), windows W.focusDown),
      -- Move focus to window below
      ((modm, xK_j), windowGo D False),
      -- Move focus to window above
      ((modm, xK_k), windowGo U False),
      -- Move focus to window left
      ((modm, xK_h), windowGo L False),
      -- Move focus to window right
      ((modm, xK_l), windowGo R False),
      -- Swap with window below
      ((modm .|. shiftMask, xK_j), windowSwap D False),
      -- Swap with window above
      ((modm .|. shiftMask, xK_k), windowSwap U False),
      -- Swap with window left
      ((modm .|. shiftMask, xK_h), windowSwap L False),
      -- Swap with window right
      ((modm .|. shiftMask, xK_l), windowSwap R False),
      -- Shrink the master area
      ((modm .|. controlMask, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modm .|. controlMask, xK_l), sendMessage Expand),
      -- Swap the focused window and the master window
      ((modm, xK_m), windows W.swapMaster),
      -- Push window back into tiling
      ((modm, xK_t), withFocused toggleFloat),
      -- Make window floating
      -- , ((modm .|. shiftMask, xK_t     ), withFocused $ windows . W.float)

      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      ((modm, xK_f), namedScratchpadAction myScratchPads "ranger"),
      ((modm, xK_x), namedScratchpadAction myScratchPads "keepassxc"),
      ((modm, xK_z), namedScratchpadAction myScratchPads "terminal"),
      ((modm, xK_b), namedScratchpadAction myScratchPads "btm"),
      ((modm, xK_d), namedScratchpadAction myScratchPads "discord"),
      ((modm, xK_o), namedScratchpadAction myScratchPads "octave"),
      ((modm, xK_e), namedScratchpadAction myScratchPads "mu4e"),
      ((modm, xK_slash), namedScratchpadAction myScratchPads "helpmenu"),
      ((modm, xK_n), namedScratchpadAction myScratchPads "myuzi"),
      ((modm, xK_c), namedScratchpadAction myScratchPads "cfw"),
      ((modm, xK_y), namedScratchpadAction myScratchPads "pavucontrol"),
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
      -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

      -- Quit xmonad
      -- , ((modm .|. shiftMask, xK_r     ), io (exitWith ExitSuccess))

      -- Restart xmonad
      -- , ((modm              , xK_c     ), spawn "xmonad --recompile; xmonad --restart")

      -- Lock with xsecurelock and suspend
      ((modm .|. shiftMask, xK_s), spawn "xsecurelock & systemctl suspend"),
      -- Run xmessage with a summary of the default keybindings (useful for beginners)
      ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N

      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --
      -- mod-{Left, Right}, Switch to physical/Xinerama screens 1 or 2
      -- mod-shift-{Left, Right}, Move client to screen 1 or 2
      --
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Left, xK_Right] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]
  where
    toggleFloat w =
      windows
        ( \s ->
            if M.member w (W.floating s)
              then W.sink w s
              else (W.float w (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)) s)
        )

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    --    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm .|. shiftMask, button1),
        ( \w ->
            focus w
              >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      ((modm, button1), dragWindow),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        ( \w ->
            focus w
              >> mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
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

myLayout = fullscreenFocus $ draggingVisualizer $ avoidStruts $ (mySpacing $ (mouseResizable ||| mouseResizableMirrored ||| Full))
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall 1 (5 / 100) (1 / 2)

    dwindled = Dwindle R CW 1.1 1.1

    mouseResizable =
      mouseResizableTile
        { masterFrac = 0.51,
          slaveFrac = 0.51,
          draggerType = BordersDragger
        }

    mouseResizableMirrored =
      mouseResizableTile
        { masterFrac = 0.51,
          slaveFrac = 0.51,
          draggerType = BordersDragger,
          isMirrored = True
        }

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
myManageHook =
  composeAll
    [ className =? "KeePassXC" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      className =? "Myuzi" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      title =? "octave" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      title =? "scratchpad" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      className =? "discord" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      title =? "ranger" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      title =? "btm" --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8),
      title =? "scratch_mu4e" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      title =? "scratch_cfw" --> (customFloating $ W.RationalRect 0.29 0.04 0.42 0.7),
      title =? "xmonad_helpmenu" --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9),
      className =? "fl64.exe" --> (customFloating $ W.RationalRect 0 0 1 1),
      className =? "Pavucontrol" --> (customFloating $ W.RationalRect 0.05 0.04 0.5 0.35),
      className =? "Syncthing GTK" --> (customFloating $ W.RationalRect 0.53 0.04 0.46 0.45),
      className =? "Zenity" --> (customFloating $ W.RationalRect 0.45 0.4 0.1 0.2),
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore,
      manageDocks
    ]

myFullscreenManageHook = fullscreenManageHook

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = serverModeEventHook

myFullscreenEventHook = fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawnOnce ("~/.xmonad/startup.sh '" ++ trayerBgNormal ++ "' '" ++ colorBgNormal ++ "' '" ++ color08Bright ++ "' '" ++ colorFocus ++ "' '" ++ color08Bright ++ "'")

myNavigation2DConfig = def {layoutNavigation = [("Tall", lineNavigation), ("Full", centerNavigation)]}

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe ("xmobar /home/emmet/.config/xmobar/" ++ colorScheme ++ "-xmobarrc")
  xmonad $
    withNavigation2DConfig myNavigation2DConfig $
      fullscreenSupportBorder $
        docks
          def
            { -- simple stuff
              terminal = myTerminal,
              focusFollowsMouse = myFocusFollowsMouse,
              clickJustFocuses = myClickJustFocuses,
              borderWidth = myBorderWidth,
              modMask = myModMask,
              workspaces = myWorkspaces,
              normalBorderColor = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor,
              -- key bindings
              keys = myKeys,
              mouseBindings = myMouseBindings,
              -- hooks, layouts
              layoutHook = myLayout,
              manageHook = myManageHook <+> myFullscreenManageHook <+> namedScratchpadManageHook myScratchPads,
              handleEventHook = myEventHook <+> myFullscreenEventHook <+> fadeWindowsEventHook,
              logHook =
                dynamicLogWithPP $
                  xmobarPP
                    { ppOutput = \x -> hPutStrLn xmproc x,
                      ppTitle = xmobarColor colorFocus "" . shorten 10,
                      ppCurrent = xmobarColor colorFocus "" . wrap ("<box type=Bottom Top width=2 mb=2 color=" ++ colorFocus ++ ">") "</box>",
                      ppVisible = xmobarColor colorFgNormal "",
                      ppHidden = xmobarColor color08Normal "",
                      ppOrder = \(ws : _) -> [ws],
                      ppSep = " "
                    },
              startupHook = myStartupHook
            }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "The default modifier key is 'alt'. Default keybindings:",
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
      "mod-button3  Set the window to floating mode and resize by dragging"
    ]
