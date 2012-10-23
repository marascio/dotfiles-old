import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CycleWS
import Data.List
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program.
myTerminal = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- The width and color of the border.
myBorderWidth        = 1
myNormalBorderColor  = "gray"
myFocusedBorderColor = "red"

-- The modified key we prefer.
myModMask = mod4Mask

-- The workspaces and their names.
myWorkspaces = [ "1:shell", "2:code", "3:shell", "4:web", "5:vm", "6:mkt",
                 "7:chat", "8",  "9" ]

-- Key bindings.
myKeys = 
    [ ((myModMask   .|. shiftMask, xK_z),      spawn "xscreensaver-command -lock")
    , ((myModMask   .|. shiftMask, xK_f),      spawn "firefox")
    , ((myModMask   .|. shiftMask, xK_d),      spawn "VBoxManage startvm robocop-winxp-x86")
    , ((myModMask   .|. shiftMask, xK_m),      spawn "somafm.sh groovesalad")
    , ((controlMask .|. shiftMask, xK_b),      spawn "xdotool key --window $(cat /tmp/ttmake.wid) b")
    , ((controlMask .|. shiftMask, xK_c),      spawn "xdotool key --window $(cat /tmp/ttmake.wid) c")
    , ((controlMask .|. shiftMask, xK_d),      spawn "xdotool key --window $(cat /tmp/ttmake.wid) d")
    , ((controlMask .|. shiftMask, xK_k),      spawn "wid=$(xdotool getwindowfocus); xdotool windowactivate --sync 0x3a0000c key ctrl+shift+s windowactivate $wid")
    , ((myModMask   .|. shiftMask, xK_h),      sendMessage Shrink) -- alternate shrink keybinding
    , ((myModMask   .|. shiftMask, xK_l),      sendMessage Expand) -- alternate expand keybinding
    , ((myModMask   .|. shiftMask, xK_equal),  spawn "amixer set Master 10%+ unmute")
    , ((myModMask   .|. shiftMask, xK_minus),  spawn "amixer set Master 10%- unmute")
    , ((myModMask   .|. shiftMask, xK_0),      spawn "amixer set Master toggle")
    , ((myModMask,                 xK_Down),   nextWS)
    , ((myModMask,                 xK_Up),     prevWS)
    , ((myModMask   .|. shiftMask, xK_Down),   shiftToNext)
    , ((myModMask   .|. shiftMask, xK_Up),     shiftToPrev)
    , ((myModMask,                 xK_Right),  nextScreen)
    , ((myModMask,                 xK_Left),   prevScreen)
    , ((myModMask   .|. shiftMask, xK_Right),  shiftNextScreen)
    , ((myModMask   .|. shiftMask, xK_Left),   shiftPrevScreen)
    , ((myModMask,                 xK_z),      toggleWS)
    , ((myModMask   .|. shiftMask, xK_Down),   shiftToNext >> nextWS)
    , ((myModMask   .|. shiftMask, xK_Up),     shiftToPrev >> prevWS)
    , ((controlMask,               xK_Print),  spawn "sleep 0.2; scrot -s")
    , ((0,                         xK_Print),  spawn "scrot")
    ]
    ++
    [ ((m .|. myModMask, k), windows $ f i)
          | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Window rules. 
myManageHook = composeAll
    [ (className =? "VirtualBox" <&&> fmap ("robocop"  `isPrefixOf`) title) --> (doShift "5:win" <+> unfloat)
    --, (className =? "Firefox"    <&&> fmap ("Campfire" `isPrefixOf`) title) --> doShift "7:chat"
    , title      =? "Campfire"         --> doShift "7:chat"
    , title      =? "wikidiary"        --> doShift "1:shell"
    , className  =? "Firefox"          --> doShift "4:web"
    , resource   =? "NxCoreAccess.exe" --> doShift "6:mkt"
    , title      =? "xclock"           --> (doShift "6:mkt" <+> doFloat)
    , className  =? "Skype"            --> doFloat
    , isFullscreen                     --> doFullFloat
    , isDialog                         --> doCenterFloat
    ]
    where
        unfloat = ask >>= doF . W.sink

-- Run xmonad.
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/lrm/.xmobarrc"
    xmonad $ ewmh defaultConfig
        { terminal   = myTerminal
        , workspaces = myWorkspaces
        , focusFollowsMouse = myFocusFollowsMouse
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook    = dynamicLogWithPP $ xmobarPP
                           { ppOutput = hPutStrLn xmproc
                           , ppTitle  = xmobarColor "green" "" . shorten 50
                           }
        , modMask    = myModMask
        } `additionalKeys` myKeys

