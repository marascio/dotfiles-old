import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
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
myNormalBorderColor  = "#0f0f0f"
myFocusedBorderColor = "#000044"

-- The modified key we prefer.
myModMask = mod4Mask

-- The workspaces and their names.
myWorkspaces = [ "1:wiki", "2:code", "3:web", "4:comm", "5:win", "6:mkt", "7", 
                 "8",  "9" ]

-- Key bindings.
myKeys = 
    [ ((myModMask   .|. shiftMask, xK_z),      spawn "xscreensaver-command -lock")
    , ((myModMask   .|. shiftMask, xK_f),      spawn "firefox")
    , ((myModMask   .|. shiftMask, xK_d),      spawn "VBoxManage startvm robocop-winxp-x86")
    , ((myModMask   .|. shiftMask, xK_n),      spawn "nxcore")
    , ((controlMask .|. shiftMask, xK_b),      spawn "xdotool key --window $(cat /tmp/ttmake.wid) b")
    , ((controlMask .|. shiftMask, xK_c),      spawn "xdotool key --window $(cat /tmp/ttmake.wid) c")
    , ((controlMask .|. shiftMask, xK_d),      spawn "xdotool key --window $(cat /tmp/ttmake.wid) d")
    , ((myModMask   .|. shiftMask, xK_h),      sendMessage Shrink) -- alternate shrink keybinding
    , ((myModMask   .|. shiftMask, xK_l),      sendMessage Expand) -- alternate expand keybinding
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
    [ (className =? "VirtualBox" <&&> fmap ("robocop" `isPrefixOf`) title) --> (doShift "5:win" <+> unfloat)
    , title      =? "wikidiary"        --> doShift "1:wiki"
    , className  =? "Namoroka"         --> doShift "3:web" 
    , resource   =? "NxCoreAccess.exe" --> doShift "6:mkt"
    ]
    where
        unfloat = ask >>= doF . W.sink

-- Run xmonad.
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/lrm/.xmobarrc"
    xmonad $ defaultConfig
        { terminal   = myTerminal
        , workspaces = myWorkspaces
        , focusFollowsMouse = myFocusFollowsMouse
        -- , normalBorderColor = myNormalBorderColor
        -- , focusedBorderColor = myFocusedBorderColor
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook    = dynamicLogWithPP $ xmobarPP
                           { ppOutput = hPutStrLn xmproc
                           , ppTitle  = xmobarColor "green" "" . shorten 50
                           }
        , modMask    = myModMask
        } `additionalKeys` myKeys

