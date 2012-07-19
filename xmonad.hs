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

modm = mod4Mask


modm = mod4Mask

myLayoutHook = onWorkspace "media" myFullscreen $ onWorkspace "web" myBorderless $ myLayout

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
                        , ppTitle = xmobarColor "green" "" . shorten 190
                        }
        , modMask = modm     -- Rebind Mod to the Windows key
        } `additionalKeys` myKeys

fnWorkspaces = (map (("F" ++) . show) [1..12])
fnWorkspacesKB = [xK_F1..xK_F12]
workspaceMap (k, w) = [ ((modm, k), windows $ W.greedyView w)
                      , ((shiftMask .|. modm, k), windows $ W.shift w) ]

myKeys = [ ((modm, xK_p), spawn "dmenu_run") ]
         ++ workspaceMap (xK_0, "0")
         ++ ((zip fnWorkspacesKB fnWorkspaces) >>= workspaceMap)
         ++
         [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
