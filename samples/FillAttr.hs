{-
    A simple Win32-console demo program that marks words on the screen using different
    character attributes.
-}
module Main where

import Control.Applicative
import Control.Monad

import Data.Char
import Data.List

import Graphics.Win32.Misc (getStdHandle, sTD_OUTPUT_HANDLE, sTD_INPUT_HANDLE)

import System.Win32.Console.Extra
import System.Win32.Process (sleep)
import System.Win32.Types


main :: IO ()
main = do
    hOut <- getStdHandle sTD_OUTPUT_HANDLE
    hIn <- getStdHandle sTD_INPUT_HANDLE
    loop hOut hIn 0x0A
    where
        loop hOut hIn attr = do
            writeConsole W hOut "Type an infix to mark words, or empty string to end: "
            w <- filter (`notElem` "\r\n") <$> readConsole W hIn 255
            case w of
                "" -> sweepScreen hOut
                _  -> do
                    markWords hOut (isInfixOf w . map toLower) attr
                    let attr' = if attr >= 0x0F then 0x09 else succ attr
                    loop hOut hIn attr'
        

sweepScreen :: HANDLE -> IO ()
sweepScreen hOut = do
    info <- getConsoleScreenBufferInfo hOut

    let SMALL_RECT l t r b = bufWindow info
        w = toEnum . fromEnum $ r - l + 1
        attr = bufAttributes info
        newAttr = attr `div` 16 + 16*(attr `mod` 16)

    forM_ [t..b] $ \i -> do
        fillConsoleOutputAttribute hOut newAttr w $ COORD l i
        sleep 50
        fillConsoleOutputAttribute hOut attr w $ COORD l i


markWords :: HANDLE -> (String -> Bool) -> WORD -> IO ()
markWords hOut prd attr = do
    info <- getConsoleScreenBufferInfo hOut

    let SMALL_RECT l t r b = bufWindow info
        w = r - l + 1
        h = b - t + 1

    s <- readConsoleOutputCharacter W hOut (fromIntegral $ w * h) (COORD l t)
    let ls = findWords prd . locWords $ s

    forM_ ls $ \(i, len) -> fillConsoleOutputAttribute hOut attr (toEnum len) $
        COORD (fromIntegral $ l + (toEnum i) `mod` w) (fromIntegral $ t + (toEnum i) `div` w)


findWords :: (String -> Bool) -> [(Int, String)] -> [(Int, Int)]
findWords p = map (\(i, cs) -> (i, length cs)) . filter (p . snd)

locWords :: String -> [(Int, String)]
locWords = id
    . map (\cs -> (fst $ head cs, map snd cs))
    . filter (grouper . snd . head)
    . groupBy (\x y -> grouper (snd x) && grouper (snd y))
    . zip [0..]
    where
        grouper = isAlphaNum
