{- Joeys Xmonad config
Running Xmonad Darcs & Contrib Darcs
Requires atleast Xmonad 0.9 & mobar 0.8
Include .xmobarrc + ~/bin
 -}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders

myManageHook = composeAll []

{- myManageHook = composeAll
     $ map (\s -> resource =? ("atWorkspace" ++ s) --> doShift s) (s1 ++ s2)
    where
        s1 = map show [0..9]
        s2 = map (("F" ++) . show) [1..12]
-}

myLayoutHook = onWorkspace "media" myFullscreen $ onWorkspace "web" myBorderless $ myLayout
        where
                myFullscreen = noBorders ( Full )
                myBorderless = noBorders (avoidStruts  $  layoutHook defaultConfig)
                myLayout = avoidStruts  $  layoutHook defaultConfig

main = do
     xmproc <- spawnPipe "/usr/bin/xmobar /home/duncan/.xmobarrc"
     xmonad $ defaultConfig
        { workspaces = (map show $ [1..9] ++ [0]) ++ fnWorkspaces
        , manageHook = manageDocks <+> myManageHook
                                   <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#1793d1"
        , terminal = "urxvtc"
        , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 125
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys` myKeys

fnWorkspaces = (map (("F" ++) . show) [1..12])
fnWorkspacesKB = [xK_F1..xK_F12]
workspaceMap (k, w) = [ ((mod4Mask, k), windows $ W.greedyView w)
                      , ((shiftMask .|. mod4Mask, k), windows $ W.shift w) ]

myKeys = [ ((mod4Mask, xK_p), spawn "dmenu_run") ]
         ++ workspaceMap (xK_0, "0")
         ++ ((zip fnWorkspacesKB fnWorkspaces) >>= workspaceMap)
