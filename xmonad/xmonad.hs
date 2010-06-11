import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


main = do
--    xmproc <- spawnPipe "/usr/bin/xmobar /home/lrm/.xmobarrc"
    xmonad $ defaultConfig
        { terminal   = myTerminal
        , workspaces = myWorkspaces
        , focusFollowsMouse = myFocusFollowsMouse
        -- , normalBorderColor = myNormalBorderColor
        -- , focusedBorderColor = myFocusedBorderColor
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
--        , logHook    = dynamicLogWithPP $ xmobarPP
--                           { ppOutput = hPutStrLn xmproc
--                           , ppTitle  = xmobarColor "green" "" . shorten 50
--                           }
        , modMask    = mod4Mask        -- Rebind Mod to the Windows key
        } `additionalKeys` myKeys

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

myTerminal = "urxvt"

myManageHook = composeAll
    [ className =? "VirtualBox" --> doFloat
    , title     =? "wikidiary"  --> doShift "3:wiki"
    ]

myWorkspaces = [ "1:shell", "2:code", "3:wiki", "4:mail", "5:comm", 
                 "6:web",   "7:vm",   "8:win",  "9:min" ]

myFocusFollowsMouse = False

myBorderWidth = 1
myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#000044"
