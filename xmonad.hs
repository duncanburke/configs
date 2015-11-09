import Control.Applicative
import System.IO (hPutStrLn)
import System.Exit

import qualified Data.Map as Map

import XMonad
import XMonad.Hooks.DynamicLog ( xmobarPP
                               , ppOutput
                               , ppTitle
                               , dynamicLogWithPP
                               , shorten
                               , xmobarColor )
import XMonad.Hooks.ManageDocks ( manageDocks
                                , avoidStruts
                                , docksEventHook )
import XMonad.Util.Run (spawnPipe)
import XMonad.StackSet ( greedyView
                       , shift
                       , view )
import qualified XMonad.StackSet as W
import XMonad.Layout.NoBorders (noBorders)

main :: IO ()
main = do
  xmproc <- spawnPipe "/home/duncan/.cabal/bin/xmobar /home/duncan/.xmobarrc"
  xmonad $ defaultConfig
    { workspaces = workspaceIds
    , layoutHook = layoutHook'
    , terminal = "urxvtc"
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#1793d1"
    , modMask = mod4Mask
    , keys = \conf -> Map.fromList $ keys' conf
    , logHook = dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 190 }
    , manageHook = manageHook'
    , handleEventHook = docksEventHook }

type Keybinding = ((KeyMask, KeySym), X ())

layoutHook' = noBorders $ avoidStruts $ (tiled ||| Full)
  where tiled   = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

manageHook' :: ManageHook
manageHook' = manageDocks
              <+> composeAll []

keys' :: XConfig Layout -> [Keybinding]
keys' conf = commandBindings conf
             ++ workspaceBindings
             ++ screenBindings

commandBindings :: XConfig Layout -> [Keybinding]
commandBindings conf@(XConfig {XMonad.modMask = modMask'}) =
  [ ((modMask' .|. shiftMask, xK_Return    ), spawn $ terminal conf)
  , ((modMask'              , xK_p         ), spawn "dmenu_run")
  , ((modMask'              , xK_semicolon ), spawn "mpc toggle")
  , ((modMask'              , xK_q         ), spawn "mpc next")
  , ((modMask' .|. shiftMask, xK_q         ), spawn "mpc prev")
  , ((modMask'              , xK_o         ), spawn "mpc random")
  , ((modMask'              , xK_a         ), spawn "mpc repeat")
  , ((modMask' .|. shiftMask, xK_a         ), spawn "mpc single")
  , ((modMask'              , xK_comma     ), spawn "toggle_mpdscribble")
  , ((modMask' .|. shiftMask, xK_j         ), kill)
  , ((modMask'              , xK_space     ), sendMessage NextLayout)
  , ((modMask' .|. shiftMask, xK_space     ), setLayout $ layoutHook conf)
  , ((modMask' .|. shiftMask, xK_apostrophe), io (exitWith ExitSuccess))
  , ((modMask'              , xK_apostrophe), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

  , ((modMask'              , xK_n         ), refresh)

  , ((modMask'              , xK_r         ), windows W.focusDown)
  , ((modMask'              , xK_c         ), windows W.focusUp)
  , ((modMask'              , xK_s         ), windows W.focusMaster)

  , ((modMask'              , xK_Return    ), windows W.swapMaster)
  , ((modMask' .|. shiftMask, xK_r         ), windows W.swapDown)
  , ((modMask' .|. shiftMask, xK_c         ), windows W.swapUp)

  , ((modMask'              , xK_g         ), sendMessage Shrink)
  , ((modMask'              , xK_l         ), sendMessage Expand)

  , ((modMask'              , xK_d         ), withFocused $ windows . W.sink)

  , ((modMask'              , xK_b         ), sendMessage (IncMasterN 1))
  , ((modMask'              , xK_m         ), sendMessage (IncMasterN (-1))) ]


screenBindings :: [Keybinding]
screenBindings = [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
                 | (key, sc) <- zip screenKeys screenOrder
                 , (f, m) <- [(view, 0), (shift, shiftMask)] ]

fnWorkspaceIds :: [WorkspaceId]
fnWorkspaceIds = (("F" ++) . show) <$> ([1..] :: [Int])

nFn :: Int
nFn = 12

workspaceMap :: KeySym -> WorkspaceId -> [Keybinding]
workspaceMap k w = [ ((mod4Mask, k), windows $ greedyView w)
                   , ((shiftMask .|. mod4Mask, k), windows $ shift w) ]

workspaceIds :: [WorkspaceId]
workspaceIds = (show <$> ([1..9] ++ [0] :: [Int]))
               ++ (take nFn fnWorkspaceIds)

workspaceKeys :: [KeySym]
workspaceKeys = [xK_1..xK_9]
                ++ [xK_0]
                ++ (take nFn [xK_F1..])

workspaceBindings :: [Keybinding]
workspaceBindings = zip workspaceKeys workspaceIds
                    >>= (uncurry workspaceMap)

screenKeys :: [KeySym]
screenKeys = [xK_h, xK_t, xK_n]

screenOrder :: [ScreenId]
screenOrder = [2, 0, 1]
