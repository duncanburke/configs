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
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys` myKeys

fnWorkspaces = (map (("F" ++) . show) [1..12])
fnWorkspacesKB = [xK_F1..xK_F12]
workspaceMap (k, w) = [ ((mod4Mask, k), windows $ W.greedyView w)
                      , ((shiftMask .|. mod4Mask, k), windows $ W.shift w) ]

myKeys = [ ((mod4Mask, xK_p), spawn "dmenu_run") ]
         ++ workspaceMap (xK_0, "0")
         ++ ((zip fnWorkspacesKB fnWorkspaces) >>= workspaceMap)
         ++ [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
