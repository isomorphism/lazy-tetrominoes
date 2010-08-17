module Main where
import Prelude hiding ((++), concat)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import System.Random
import qualified Data.IntMap as IM
import Graphics.DrawingCombinators
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import qualified Graphics.Rendering.OpenGL.GL as GL

main = do SDL.init [SDL.InitTimer, SDL.InitVideo]
          SDL.setVideoMode 800 800 32 [SDL.OpenGL]
          gameLoop =<< (newGame <$> getStdGen <*> openFont "Vera.ttf")
          SDL.quit

newEvents = moreEvents =<< SDL.pollEvent
    where moreEvents SDL.NoEvent = return []
          moreEvents ev = (ev:) <$> newEvents

keypresses (SDL.KeyDown sym) = Just $ SDL.symKey sym
keypresses _ = Nothing

gameLoop gs = do evs <- newEvents
                 let gs' = updateGame (catMaybes $ keypresses <$> evs) gs
                 GL.clear [GL.ColorBuffer]
                 clearRender $ renderGame gs'
                 SDL.glSwapBuffers
                 unless (any (== SDL.Quit) evs) (SDL.delay 20 >> gameLoop gs')

type Stack = IM.IntMap (IM.IntMap (Maybe Color))
type TilePos = (Int, Int)
data Piece = Piece { pos :: TilePos, pts :: [TilePos], clr :: Color }
data Game = Game { activePiece :: Maybe Piece
                 , queue       :: [Piece]
                 , stack       :: Stack
                 , score       :: Int
                 , countdown   :: Int
                 , dropped     :: Bool
                 , clearing    :: [Int]
                 , font        :: Font }

tetrominoes = [ mkPiece [(0,2),  (0,1),  (0,0),  (1,0) ] (0, 0.5, 1)
              , mkPiece [(0,2),  (0,1),  (0,0), (-1,0) ] (0.5, 0, 1)
              , mkPiece [(-2,0), (-1,0), (0,0),  (1,0) ] (1, 1, 1)
              , mkPiece [(0,0),  (0,1),  (1,1),  (1,0) ] (0.5, 0.5, 0.5)
              , mkPiece [(-1,0), (0,0),  (0,1),  (1,1) ] (0.5, 1, 0)
              , mkPiece [(1,0),  (0,0),  (0,1), (-1,1) ] (0, 1, 0.5)
              , mkPiece [(-1,0), (0,0),  (1,0),  (0,1) ] (0, 1, 1)
              ]

mkPiece p (r,g,b) = Piece (4 - minimum (fst <$> p),0) p (Color r g b 1)
movePiece xy pc = pc { pos = addPair xy (pos pc) }
rotatePiece pc = pc { pts = (\(x, y) -> (y, - x)) <$> pts pc }

stackH = 20
stackW = 10
emptyRow = IM.fromList $ zip [0..stackW-1] (repeat Nothing)
emptyStack = IM.fromList $ zip [0..stackH-1] (repeat emptyRow)
newGame gen ft = Game Nothing rndQueue emptyStack 0 (calcSpd 0) False [] ft
    where rndQueue = (tetrominoes !!) <$> randomRs (0, length tetrominoes - 1) gen
         
renderGame gm = translate (-1,1) %% scale (1/11) (-1/11) %% img
    where img = concat [renderBlink, renderExtra, renderPiece, renderBoard]
          renderPiece = maybe mempty pieceImg $ activePiece gm
          renderBoard = renderStack (stack gm) ++ emptyBoard
          renderExtra = concat [ if stackFull gm then renderGameover else mempty 
                               , write ("Level: " ++ show (calcLvl $ score gm)) (2, 8)
                               , write ("Score: " ++ show (score gm)) (2, 9)
                               , write "Next:" (2, 1)
                               , translate (-11, 2) %% (pieceImg . head $ queue gm)
                               ]
          renderGameover = (translate (1, 9) ++ scale 3 3)
              %% tint (Color 1 0 0 1) (write "GAME OVER" (0,0))
          renderStack stk = concat $ do (y, rw) <- IM.toList stk
                                        (x, tl) <- IM.toList rw
                                        case tl of
                                            Nothing -> []
                                            Just c  -> return . tint (darken c) $ tile (x, y)
          darken = modulate $ Color 0.5 0.5 0.5 1
          renderBlink = concat $ do y <- clearing gm
                                    x <- [0..stackW-1]
                                    let c = if odd (countdown gm `div` 2)
                                            then Color 1 1 1 0.8
                                            else Color 0 0 0 0.8
                                    return . tint c $ tile (x, y)
          write str pos = translate pos 
              %% translate (0, 1)
              %% scale 0.5 (-0.5)
              %% text (font gm) str

calcLvl n = 1 + div n 500
calcSpd n = max 2 $ 26 - calcLvl n

updateGame ks gm | stackFull gm = gm
                 | otherwise = upd gm
    where upd = clearLines . decCountdown . newPiece . dropPiece . handleKeys ks

decCountdown gm = gm { countdown = countdown gm - 1 }

updIfReady p f gm = if countdown gm <= 0 && p gm then f gm else gm

clearLines = updIfReady (not . null . clearing) removeLines
removeLines gm = let intact = IM.elems $ foldr IM.delete (stack gm) (clearing gm)
                     newRows = replicate (length $ clearing gm) emptyRow
                     renumbered = IM.fromList $ zip [0..stackH-1] (newRows ++ intact) 
                 in gm { stack = renumbered
                       , score = score gm + length (clearing gm) * 100
                       , clearing = []
                       , countdown = calcSpd (score gm) }

dropPiece = updIfReady (isJust . activePiece) pieceFall
pieceFall gm = case activePiece gm of
                   Nothing -> gm
                   Just pc -> let pc' = movePiece (0, 1) pc
                                  gm' = gm { activePiece = Just pc' }
                              in if blocked pc' (stack gm')
                                 then settle pc gm
                                 else upd gm'
    where upd g | dropped g = g { score = score g + 10, countdown = 1 }
                | otherwise = g { score = score g + 1
                                , countdown = calcSpd (score g) }

settle pc gm = let tPts = addPair (pos pc) <$> pts pc
                   addTile stk (x,y) = IM.adjust (IM.insert x (Just $ clr pc)) y stk
                   gm' = gm { activePiece = Nothing
                            , stack       = foldl addTile (stack gm) tPts
                            , dropped     = False
                            , countdown   = calcSpd (score gm) }
               in checkLines gm'

checkLines gm = let filled = IM.filter (all isJust . IM.elems) (stack gm)
                in gm { clearing = IM.keys filled, countdown = 20 }

newPiece gm = if isNothing (activePiece gm) && countdown gm <= 0
              then gm { countdown   = calcSpd (score gm)
                      , dropped     = False
                      , activePiece = Just . head $ queue gm
                      , queue       = tail $ queue gm }
              else gm

handleKeys ks gm = foldl handleKey gm ks

handleKey gm SDLK_SPACE = gm { dropped = True, countdown = 0 }
handleKey gm SDLK_DOWN = ifInControl pieceFall gm
handleKey gm SDLK_UP = ifInControl (tryMove rotatePiece) gm
handleKey gm SDLK_LEFT = ifInControl (tryMove $ movePiece (-1,0)) gm
handleKey gm SDLK_RIGHT = ifInControl (tryMove $ movePiece (1,0)) gm
handleKey gm _ = gm

ifInControl f gm | isJust (activePiece gm) && not (dropped gm) = f gm
                 | otherwise = gm

tryMove f gm = case activePiece gm of
                   Nothing -> gm
                   Just pc -> if blocked (f pc) (stack gm)
                              then gm
                              else gm { activePiece = Just (f pc) }

blocked pc stk = let pts' = addPair (pos pc) <$> pts pc
                     obs (x,y) = IM.lookup x =<< IM.lookup y stk
                 in any (maybe True isJust . obs) pts'

stackFull gm = any isJust (IM.elems $ stack gm IM.! 0)

pieceImg pc = tint (clr pc) (concat $ tile . addPair (pos pc) <$> pts pc)

emptyBoard = tint (Color 0 0 0 1) holes ++ bg
    where holes = concat [rect 11 1 10 20, rect 2 8 8 3, rect 2 1 8 6]
          bg = tint (Color 0.3 0.3 0.3 1) (rect 0 0 22 22)

rect x y w h = convexPoly [(x,y), (x+w,y), (x+w,y+h), (x,y+h)]

addPair (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

tile (x, y) = rect (11 + fromIntegral x) (1 + fromIntegral y) 1 1

x ++ y = mappend x y
concat ms = mconcat ms
