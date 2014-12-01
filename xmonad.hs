import Control.Applicative
import System.IO (hPutStrLn)

import XMonad
import XMonad.Hooks.DynamicLog (xmobarPP, ppOutput, ppTitle,
                                dynamicLogWithPP,
                                shorten,
                                xmobarColor)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.StackSet (greedyView, shift, view)

main :: IO ()
main = do
     xmproc <- spawnPipe "/home/duncan/.cabal/bin/xmobar /home/duncan/.xmobarrc"
     let conf = defaultConfig {
           workspaces = (show <$> ([1..9] ++ [0] :: [Int])) ++ (take nFn fnWorkspaceIds),
           manageHook = manageDocks <+>
                        composeAll [] <+>
                        manageHook defaultConfig,
           layoutHook = avoidStruts $ layoutHook defaultConfig,
           normalBorderColor = "#000000",
           focusedBorderColor = "#1793d1",
           terminal = "urxvtc",
           logHook = dynamicLogWithPP $ xmobarPP {
             ppOutput = hPutStrLn xmproc,
             ppTitle = xmobarColor "green" "" . shorten 190},
           modMask = mod4Mask }
     xmonad $ (conf `additionalKeys` keyBindings)

type Keybinding = ((KeyMask, KeySym), X ())

keyBindings :: [Keybinding]
keyBindings = commandBindings ++
              workspaceBindings ++
              screenBindings

workspaceBindings :: [Keybinding]
workspaceBindings = workspaceMap xK_0 "0" ++
                    ((take nFn $ zip [xK_F1..] fnWorkspaceIds) >>= (uncurry workspaceMap))

commandBindings :: [Keybinding]
commandBindings = [((mod4Mask, xK_p), spawn "dmenu_run"),
                   ((mod4Mask, xK_z), spawn "mpc toggle"),
                   ((mod4Mask, xK_x), spawn "mpc next")]

screenBindings :: [Keybinding]
screenBindings = [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) |
                  (key, sc) <- zip screenKeys screenOrder,
                  (f, m) <- [(view, 0), (shift, shiftMask)]]

fnWorkspaceIds :: [WorkspaceId]
fnWorkspaceIds = (("F" ++) . show) <$> ([1..] :: [Int])

nFn :: Int
nFn = 12

workspaceMap :: KeySym -> WorkspaceId -> [Keybinding]
workspaceMap k w = [((mod4Mask, k), windows $ greedyView w),
                    ((shiftMask .|. mod4Mask, k),
                     windows $ shift w)]

screenKeys :: [KeySym]
screenKeys = [xK_h, xK_t, xK_n]

screenOrder :: [ScreenId]
screenOrder = [2,0,1]
