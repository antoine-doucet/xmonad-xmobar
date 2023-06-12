import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Layout.Spiral

--import XMonad.Hooks.DynamicBars
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Layout.IndependentScreens

import System.IO (hPutStrLn, Handle)

import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Config.Desktop

import Prelude as P

currentTerminal = "alacritty --title Terminal " --"termite"

------------------------------------------------------------------------------

toggleFloat w = 
    windows (\s -> if M.member w (W.floating s)
                   then W.sink w s
--                   else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
                   else (W.float w (W.RationalRect 1 1 1 1) s))


customKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

---------------------------------------------------------------------------
    -- program spawn
           [ ((modMask, xK_i),      spawn "brave")
           , ((modMask, xK_d),      spawn "dmenu_run")
           , ((modMask, xK_k),      spawn "kiwix-desktop")
           , ((modMask, xK_Return), spawn currentTerminal)
--           , ((modMask .|. shiftMask, xK_Return), spawn "termite -c ~/.config/termite/light_config")
           , ((modMask .|. shiftMask, xK_Return), spawn "st")
--           , ((modMask, xK_w),       spawn "work_gambit_standalone")


           , ((modMask, xK_p),      spawn "~/Scripts/pdf_open")
           , ((modMask, xK_y),      spawn $ currentTerminal ++ "-e mpsyt")
           , ((modMask .|. shiftMask, xK_y), spawn "~/Scripts/mpsyt_rm_cache")
--           , ((modMask, xK_w),      spawn "~/Scripts/wineMenu/wine_programs.sh")
--           , ((modMask, xK_m),      spawn "python ~/Scripts/mail_fetch/mail_in.py")
           , ((modMask, xK_r),      spawn $ currentTerminal ++ "-e ranger")
           ]
---------------------------------------------------------------------------
    -- utilities
        ++ [ ((modMask .|. shiftMask, xK_q), kill)
           , ((modMask .|. shiftMask, xK_r),
                spawn ("xmonad --recompile;" ++ "xmonad --restart"))
           , ((modMask .|. shiftMask, xK_l),
                spawn "pkill -x X")
           , ((modMask .|. shiftMask, xK_space),
              withFocused toggleFloat)
           ]

---------------------------------------------------------------------------
    -- volume setting
        ++ [ ((0, 0x1008ff12), spawn "amixer set Master toggle")
           , ((0, 0x1008ff11), spawn "amixer -q sset Master 2%-")
           , ((0, 0x1008ff13), spawn "amixer -q sset Master 2%+")
           , ((modMask, xK_s), spawn "alsamix")
           ]

---------------------------------------------------------------------------
    -- brightness setting
        ++ [ ((0, 0x1008ff03), spawn "brightnessctl set  10%-")
           , ((0, 0x1008ff02), spawn "brightnessctl set +10%")
           ]

---------------------------------------------------------------------------
    -- workspace control
        ++ [ ((m .|. modMask, k), windows $ f i)
             | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
             , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
           ]

-------------------------------------------------------------------------------
    -- termite toggle color theme
        ++ [ ((modMask, xK_t), spawn "~/Scripts/termite/toggle_light") ]


-----------------------------------------------------------------
    -- mouse bindings
customMouseBindings (XConfig {XMonad.modMask = modMask}) =
    M.fromList $ 
           [ ((modMask, button1), 
                (\w -> focus w
                         >> mouseResizeWindow w))
           ]


-----------------------------------------------------------------
-- xmobar


xmobarStartup :: ScreenId -> IO Handle
xmobarStartup _ = spawnPipe "xmobar"

xmobarCleanup :: IO ()
xmobarCleanup = return ()

xmobarTop = statusBarPipe "xmobar -x 0 ~/.xmobar/xmobarrc_top" (pure ppTop)

xmobarBottom = statusBarPipe "xmobar -x 0 ~/.xmobar/xmobarrc_bottom" (pure ppBottom)

--barSpawner :: ScreenId -> IO StatusBarConfig
--barSpawner 0 = pure $ xmobarTop <> xmobarBottom
--barSpawner _ = mempty

-----------------------------------------------------------------
-- pp

customXmobarPP =
  xmobarPP
    { ppOrder = \(ws : l : t : ex) -> [ws] ++ ex ++ [t] -- remove layout info from default output
    }

wrapGreen l r  = wrap ("<fc=#00FF44>" ++ l) ("</fc>" ++ r)
wrapYellow l r = wrap ("<fc=#D5Ec13>" ++ l) ("</fc>" ++ r)
wrapBlue l r   = wrap ("<fc=#1De2df>" ++ l) ("</fc>" ++ r)


ppTop = 
  xmobarPP
    { ppCurrent = wrapBlue "|" "|"
    , ppHidden  = wrapYellow "" ""
    , ppSep     = " <- "
--    , ppSort    = getSortByXineramaRule
    , ppOrder   = \(ws:l:t:ex) -> (ws:t:ex)
    , ppTitle   = wrapBlue " < " " > "
    }

ppBottom = xmobarPP

-------------------------------------------------------------------------------
-- Hooks

-- Xterm used as a floating terminal for scripting
customManageHook = composeAll . concat $
              [ [ className =? "XTerm" --> doFloat ]
              , [ className =? "elem" --> doFloat ]
              , [ className =? "Path of Exile" --> doFloat ]
              ]
 
customLayoutHook = avoidStruts $
                     spacingRaw False
                                (Border 1 1 1 1) True
                                (Border 5 5 5 5) True
--                       $ layoutHook def
                         $ spiral (6/7)

myDesktopConfig = def --docks $ desktopConfig
  { terminal    = currentTerminal
  , borderWidth = 1
  , focusedBorderColor = "#100000"
  , normalBorderColor  = "#000000"

--  , mouseBindings = customMouseBindings
  , keys = customKeys

--  , startupHook = dynStatusBarStartup xmobarStartup xmobarCleanup

--  , handleEventHook = dynStatusBarEventHook xmobarStartup xmobarCleanup
  , layoutHook = customLayoutHook
  , manageHook = customManageHook <+> manageHook def
  , logHook = dynamicLog

  }

--main = xmonad $ dynamicSBs barSpawner myDesktopConfig
--main = xmonad $ withEasySB (xmobarBottom <> xmobarTop) defToggleStrutsKey myDesktopConfig
main = do
  xmTop <- xmobarTop 
  xmBottom <- xmobarBottom
  xmonad $ withEasySB (xmTop <> xmBottom) defToggleStrutsKey myDesktopConfig

