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
myWorkspaces = [ "1:wiki", "2:wiki", "3:comm", "4:web", "5:win", "6:",  "9:min" ]

-- Key bindings.
myKeys = 
    [ ((mod4Mask .|. shiftMask, xK_z),      spawn "xscreensaver-command -lock")
    , ((controlMask,           xK_Print),   spawn "sleep 0.2; scrot -s")
    , ((0,                     xK_Print),   spawn "scrot")
    ]
    ++
    [ ((m .|. mod4Mask, k), windows $ f i)
          | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Window rules. 
myManageHook = composeAll
    [ (className =? "VirtualBox" <&&> fmap ("robocop" `isPrefixOf`) title) --> (doShift "8:win" <+> unfloat)
    ,  title     =? "wikidiary" --> doShift "3:wiki"
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





