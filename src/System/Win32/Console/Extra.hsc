{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-
    | Module:       System.Win32.Console.Extra
      Copyright:    (c) 2016 siracusa
      License:      BSD3
      Maintainer:   siracusa <pvnsrc@gmail.com>
      Stability:    experimental
      Portability:  Windows

    This module is a direct translation of the Windows console types and functions as declared
    in the @wincon.h@ header file. This module aims to stick to the original API as closely as
    possible. However, some intermediate types had to be introduced (as Haskell doesn't support
    untagged unions) and function parameter types had to be changed to more primitive types (as
    the Haskell FFI doesn't support arbitrary data types).

    Some data types aren't supported as parameter types of foreign functions directly by the FFI.
    For these wrapper functions have been introduced that map the original types to pointers and
    call the C functions from the C wrapper file. These wrapped functions are marked with an @Hs@
    prefix to their original name. Direct foreign import functions are marked with a prefix @c_@.

    Additionally, some functions handling characters exist in an ASCII and a Unicode version. The
    Windows API uses suffixes @A@ and @W@ for the ASCII and Unicode versions, respectively. It
    also defines versions of those functions without any suffix which are mapped to the @A@ or
    @W@ versions, depending on whether the @UNICODE@ flag was defined at compilation or not. This
    module doesn't take that flag into account and instead provides bindings to both function
    variants.

    For functions handling possibly large chunks of data, there are also variants with a @With@
    suffix. These functions leave buffer allocation, writing, and/or reading to the user, such that
    more efficient versions can be implemented than the ones working on Haskell strings or lists.
-}
module System.Win32.Console.Extra (
    -- * Constants
    fOREGROUND_BLUE,
    fOREGROUND_GREEN,
    fOREGROUND_RED,
    fOREGROUND_INTENSITY,
    bACKGROUND_BLUE,
    bACKGROUND_GREEN,
    bACKGROUND_RED,
    bACKGROUND_INTENSITY,

    -- According to MSDN
    #if _WIN32_WINNT >= 0x0500
    cONSOLE_FULLSCREEN,
    cONSOLE_FULLSCREEN_HARDWARE,
    #endif

    -- According to MSYS
    #if _WIN32_WINNT >= 0x0501
    cONSOLE_FULLSCREEN_MODE,
    cONSOLE_WINDOWED_MODE,
    #endif

{-
    cTRL_C_EVENT,
    cTRL_BREAK_EVENT,
    cTRL_CLOSE_EVENT,
    cTRL_LOGOFF_EVENT,
    cTRL_SHUTDOWN_EVENT,
-}

    eNABLE_PROCESSED_INPUT,
    eNABLE_LINE_INPUT,
    eNABLE_ECHO_INPUT,
    eNABLE_WINDOW_INPUT,
    eNABLE_MOUSE_INPUT,
    eNABLE_INSERT_MODE,
    eNABLE_QUICK_EDIT_MODE,
    eNABLE_EXTENDED_FLAGS,
    eNABLE_AUTO_POSITION,

    eNABLE_PROCESSED_OUTPUT,
    eNABLE_WRAP_AT_EOL_OUTPUT,

    kEY_EVENT,
    mOUSE_EVENT,
    wINDOW_BUFFER_SIZE_EVENT,
    mENU_EVENT,
    fOCUS_EVENT,

    rIGHT_ALT_PRESSED,
    lEFT_ALT_PRESSED,
    rIGHT_CTRL_PRESSED,
    lEFT_CTRL_PRESSED,
    sHIFT_PRESSED,
    nUMLOCK_ON,
    sCROLLLOCK_ON,
    cAPSLOCK_ON,
    eNHANCED_KEY,

    fROM_LEFT_1ST_BUTTON_PRESSED,
    fROM_LEFT_2ND_BUTTON_PRESSED,
    fROM_LEFT_3RD_BUTTON_PRESSED,
    fROM_LEFT_4TH_BUTTON_PRESSED,
    rIGHTMOST_BUTTON_PRESSED,

    mOUSE_MOVED,
    dOUBLE_CLICK,
    mOUSE_WHEELED,

    -- According to MSYS (MSDN: 0x0501)
    #if _WIN32_WINNT >= 0x0500
    aTTACH_PARENT_PROCESS,
    #endif

    -- * Types
    CHAR,
    WCHAR,
    SHORT,

    UNICODE_ASCII_CHAR (..),
    CHAR_INFO (..),
    SMALL_RECT (..),
    CONSOLE_CURSOR_INFO (..),
    COORD (..),
    CONSOLE_SCREEN_BUFFER_INFO (..),

{-
    HANDLER_ROUTINE,
    makeHandlerRoutinePtr,
-}

    KEY_EVENT_RECORD (..),
    MOUSE_EVENT_RECORD (..),
    WINDOW_BUFFER_SIZE_RECORD (..),
    MENU_EVENT_RECORD (..),
    FOCUS_EVENT_RECORD (..),
    INPUT_RECORD (..),
    INPUT_RECORD_EVENT (..),

    -- * Functions
    c_AllocConsole,
    allocConsole,

    -- According to MSYS (MSDN: 0x0501)
    #if _WIN32_WINNT >= 0x0500
    c_AttachConsole,
    attachConsole,
    #endif

    c_CreateConsoleScreenBuffer,
    createConsoleScreenBuffer,

    c_HsFillConsoleOutputAttribute,
    fillConsoleOutputAttribute,

    c_HsFillConsoleOutputCharacterA,
    c_HsFillConsoleOutputCharacterW,
    fillConsoleOutputCharacter,

    c_FlushConsoleInputBuffer,
    flushConsoleInputBuffer,

    c_FreeConsole,
    freeConsole,

    c_GenerateConsoleCtrlEvent,
    generateConsoleCtrlEvent,

    c_GetConsoleCP,
    getConsoleCP,

    c_GetConsoleCursorInfo,
    getConsoleCursorInfo,

    c_GetConsoleMode,
    getConsoleMode,

    c_GetConsoleOutputCP,
    getConsoleOutputCP,

    c_GetScreenBufferInfo,
    getConsoleScreenBufferInfo,

    c_GetConsoleTitleA,
    c_GetConsoleTitleW,
    getConsoleTitle,

    #if _WIN32_WINNT >= 0x0500
    c_GetConsoleDisplayMode,
    getConsoleDisplayMode,
    #endif

    #if _WIN32_WINNT >= 0x0500
    c_GetConsoleWindow,
    getConsoleWindow,
    #endif

    #if _WIN32_WINNT >= 0x0501
    c_GetConsoleProcessList,
    getConsoleProcessList,
    #endif

    c_HsGetLargestConsoleWindowSize,
    getLargestConsoleWindowSize,

    c_GetNumberOfConsoleInputEvents,
    getNumberOfConsoleInputEvents,

    c_GetNumberOfConsoleMouseButtons,
    getNumberOfConsoleMouseButtons,

    c_PeekConsoleInputA,
    c_PeekConsoleInputW,
    peekConsoleInput,
    peekConsoleInputWith,

    c_ReadConsoleA,
    c_ReadConsoleW,
    readConsole,
    readConsoleWith,

    c_ReadConsoleInputA,
    c_ReadConsoleInputW,
    readConsoleInput,
    readConsoleInputWith,

    c_HsReadConsoleOutputAttribute,
    readConsoleOutputAttribute,
    readConsoleOutputAttributeWith,

    c_HsReadConsoleOutputCharacterA,
    c_HsReadConsoleOutputCharacterW,
    readConsoleOutputCharacter,
    readConsoleOutputCharacterWith,

    c_HsReadConsoleOutputA,
    c_HsReadConsoleOutputW,
    readConsoleOutput,

    c_HsScrollConsoleScreenBufferA,
    c_HsScrollConsoleScreenBufferW,
    scrollConsoleScreenBuffer,

    c_SetConsoleActiveScreenBuffer,
    setConsoleActiveScreenBuffer,

    c_SetConsoleCP,
    setConsoleCP,

{-
    c_SetConsoleCtrlHandler,
    setConsoleCtrlHandler,
-}

    c_SetConsoleCursorInfo,
    setConsoleCursorInfo,

    c_HsSetConsoleCursorPosition,
    setConsoleCursorPosition,

    -- According to MSYS (MSDN: 0x0500)
    #if _WIN32_WINNT >= 0x0501
    c_SetConsoleDisplayMode,
    setConsoleDisplayMode,
    #endif

    c_SetConsoleMode,
    setConsoleMode,

    c_SetConsoleOutputCP,
    setConsoleOutputCP,

    c_HsSetConsoleScreenBufferSize,
    setConsoleScreenBufferSize,

    c_SetConsoleTextAttribute,
    setConsoleTextAttribute,

    c_SetConsoleTitleA,
    c_SetConsoleTitleW,
    setConsoleTitle,

    c_SetConsoleWindowInfo,
    setConsoleWindowInfo,

    c_WriteConsoleA,
    c_WriteConsoleW,
    writeConsole,
    writeConsoleWith,

    c_WriteConsoleInputA,
    c_WriteConsoleInputW,
    writeConsoleInput,
    writeConsoleInputWith,

    c_HsWriteConsoleOutputA,
    c_HsWriteConsoleOutputW,
    writeConsoleOutput,

    c_HsWriteConsoleOutputAttribute,
    writeConsoleOutputAttribute,
    writeConsoleOutputAttributeWith,

    c_HsWriteConsoleOutputCharacterA,
    c_HsWriteConsoleOutputCharacterW,
    writeConsoleOutputCharacter,
    writeConsoleOutputCharacterWith,

    -- * Generic functions
    InputHandler,
    OutputHandler,
    A (..),
    W (..),
    TChar (
        suffix,
        toCTString,
        toCTStringLen,
        fromCTString,

        c_GetConsoleTitle,
        c_FillConsoleOutputCharacter,
        c_PeekConsoleInput,
        c_ReadConsole,
        c_ReadConsoleInput,
        c_ReadConsoleOutput,
        c_ReadConsoleOutputCharacter,
        c_ScrollConsoleScreenBuffer,
        c_SetConsoleTitle,
        c_WriteConsole,
        c_WriteConsoleInput,
        c_WriteConsoleOutput,
        c_WriteConsoleOutputCharacter
    )
) where


import Control.Applicative

import Data.Maybe (fromMaybe)
import Data.Char (chr, ord)

import Foreign
import Foreign.C

import Graphics.Win32.GDI.Types (HWND)

import System.Win32.Process (ProcessId)
import System.Win32.Types

##ifdef GHCI
import WinConsoleWrapper
##endif


#include "WinConsoleWrapper.h"

-- windowsVersion :: String
-- windowsVersion = (#const _WIN32_WINNT)


-- from <https://wiki.haskell.org/FFI_cook_book>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- * Constants

fOREGROUND_BLUE, fOREGROUND_GREEN, fOREGROUND_RED, fOREGROUND_INTENSITY,
    bACKGROUND_BLUE, bACKGROUND_GREEN, bACKGROUND_RED, bACKGROUND_INTENSITY :: WORD
fOREGROUND_BLUE      = (#const FOREGROUND_BLUE)
fOREGROUND_GREEN     = (#const FOREGROUND_GREEN)
fOREGROUND_RED       = (#const FOREGROUND_RED)
fOREGROUND_INTENSITY = (#const FOREGROUND_INTENSITY)
bACKGROUND_BLUE      = (#const BACKGROUND_BLUE)
bACKGROUND_GREEN     = (#const BACKGROUND_GREEN)
bACKGROUND_RED       = (#const BACKGROUND_RED)
bACKGROUND_INTENSITY = (#const BACKGROUND_INTENSITY)


-- According to MSDN
#if _WIN32_WINNT >= 0x0500
-- Older versions of MSYS doesn't seem to support these options, so we just hardcode them
cONSOLE_FULLSCREEN, cONSOLE_FULLSCREEN_HARDWARE :: DWORD
cONSOLE_FULLSCREEN          = 1
cONSOLE_FULLSCREEN_HARDWARE = 2
#endif

-- According to MSYS
#if _WIN32_WINNT >= 0x0501
cONSOLE_FULLSCREEN_MODE, cONSOLE_WINDOWED_MODE :: DWORD
cONSOLE_FULLSCREEN_MODE = (#const CONSOLE_FULLSCREEN_MODE)
cONSOLE_WINDOWED_MODE   = (#const CONSOLE_WINDOWED_MODE)
#endif

{-
cTRL_C_EVENT, cTRL_BREAK_EVENT, cTRL_CLOSE_EVENT, cTRL_LOGOFF_EVENT, cTRL_SHUTDOWN_EVENT :: DWORD
cTRL_C_EVENT        = (#const CTRL_C_EVENT)
cTRL_BREAK_EVENT    = (#const CTRL_BREAK_EVENT)
cTRL_CLOSE_EVENT    = (#const CTRL_CLOSE_EVENT)
cTRL_LOGOFF_EVENT   = (#const CTRL_LOGOFF_EVENT)
cTRL_SHUTDOWN_EVENT = (#const CTRL_SHUTDOWN_EVENT)
-}

-- Input modes
eNABLE_PROCESSED_INPUT, eNABLE_LINE_INPUT, eNABLE_ECHO_INPUT, eNABLE_WINDOW_INPUT,
    eNABLE_MOUSE_INPUT, eNABLE_INSERT_MODE, eNABLE_QUICK_EDIT_MODE,
    eNABLE_EXTENDED_FLAGS, eNABLE_AUTO_POSITION :: DWORD
eNABLE_PROCESSED_INPUT = (#const ENABLE_PROCESSED_INPUT)
eNABLE_LINE_INPUT      = (#const ENABLE_LINE_INPUT)
eNABLE_ECHO_INPUT      = (#const ENABLE_ECHO_INPUT)
eNABLE_WINDOW_INPUT    = (#const ENABLE_WINDOW_INPUT)
eNABLE_MOUSE_INPUT     = (#const ENABLE_MOUSE_INPUT)
eNABLE_INSERT_MODE     = (#const ENABLE_INSERT_MODE)
eNABLE_QUICK_EDIT_MODE = (#const ENABLE_QUICK_EDIT_MODE)
eNABLE_EXTENDED_FLAGS  = (#const ENABLE_EXTENDED_FLAGS)
eNABLE_AUTO_POSITION   = (#const ENABLE_AUTO_POSITION)

-- Output buffer modes
eNABLE_PROCESSED_OUTPUT, eNABLE_WRAP_AT_EOL_OUTPUT :: DWORD
eNABLE_PROCESSED_OUTPUT   = (#const ENABLE_PROCESSED_OUTPUT)
eNABLE_WRAP_AT_EOL_OUTPUT = (#const ENABLE_WRAP_AT_EOL_OUTPUT)

kEY_EVENT, mOUSE_EVENT, wINDOW_BUFFER_SIZE_EVENT, mENU_EVENT, fOCUS_EVENT :: DWORD
kEY_EVENT                = (#const KEY_EVENT)
mOUSE_EVENT              = (#const MOUSE_EVENT)
wINDOW_BUFFER_SIZE_EVENT = (#const WINDOW_BUFFER_SIZE_EVENT)
mENU_EVENT               = (#const MENU_EVENT)
fOCUS_EVENT              = (#const FOCUS_EVENT)

rIGHT_ALT_PRESSED, lEFT_ALT_PRESSED, rIGHT_CTRL_PRESSED, lEFT_CTRL_PRESSED, sHIFT_PRESSED,
    nUMLOCK_ON, sCROLLLOCK_ON, cAPSLOCK_ON, eNHANCED_KEY :: DWORD
rIGHT_ALT_PRESSED  = (#const RIGHT_ALT_PRESSED)
lEFT_ALT_PRESSED   = (#const LEFT_ALT_PRESSED)
rIGHT_CTRL_PRESSED = (#const RIGHT_CTRL_PRESSED)
lEFT_CTRL_PRESSED  = (#const LEFT_CTRL_PRESSED)
sHIFT_PRESSED      = (#const SHIFT_PRESSED)
nUMLOCK_ON         = (#const NUMLOCK_ON)
sCROLLLOCK_ON      = (#const SCROLLLOCK_ON)
cAPSLOCK_ON        = (#const CAPSLOCK_ON)
eNHANCED_KEY       = (#const ENHANCED_KEY)

fROM_LEFT_1ST_BUTTON_PRESSED, fROM_LEFT_2ND_BUTTON_PRESSED, fROM_LEFT_3RD_BUTTON_PRESSED,
    fROM_LEFT_4TH_BUTTON_PRESSED, rIGHTMOST_BUTTON_PRESSED :: DWORD
fROM_LEFT_1ST_BUTTON_PRESSED = (#const FROM_LEFT_1ST_BUTTON_PRESSED)
fROM_LEFT_2ND_BUTTON_PRESSED = (#const FROM_LEFT_2ND_BUTTON_PRESSED)
fROM_LEFT_3RD_BUTTON_PRESSED = (#const FROM_LEFT_3RD_BUTTON_PRESSED)
fROM_LEFT_4TH_BUTTON_PRESSED = (#const FROM_LEFT_4TH_BUTTON_PRESSED)
rIGHTMOST_BUTTON_PRESSED     = (#const RIGHTMOST_BUTTON_PRESSED)

mOUSE_MOVED, dOUBLE_CLICK, mOUSE_WHEELED :: DWORD
mOUSE_MOVED    = (#const MOUSE_MOVED)
dOUBLE_CLICK   = (#const DOUBLE_CLICK)
mOUSE_WHEELED  = (#const MOUSE_WHEELED)
{-
#if _WIN32_WINNT >= 0x0600
mOUSE_HWHEELED :: DWORD
mOUSE_HWHEELED = (#const MOUSE_HWHEELED)
#endif
-}

-- According to MSYS (MSDN: 0x0501)
#if _WIN32_WINNT >= 0x0500

aTTACH_PARENT_PROCESS :: DWORD
aTTACH_PARENT_PROCESS = (#const ATTACH_PARENT_PROCESS)

#endif


-- * Types

-- Should these go somewhere else?
type CHAR = CChar

type WCHAR = CWchar

type SHORT = CShort


{-
typedef union _UNICODE_ASCII_CHAR {
    WCHAR UnicodeChar;
    CHAR  AsciiChar;
} UNICODE_ASCII_CHAR;
-}
newtype UNICODE_ASCII_CHAR t = UnicodeAsciiChar { unicodeAsciiChar :: t }
                               deriving (Show, Read, Eq)

instance Storable (UNICODE_ASCII_CHAR CHAR) where
    sizeOf _ = (#size UNICODE_ASCII_CHAR)
    alignment _ = (#alignment UNICODE_ASCII_CHAR)
    peek ptr = UnicodeAsciiChar <$> (#peek UNICODE_ASCII_CHAR, AsciiChar) ptr
    poke ptr val = case val of
        UnicodeAsciiChar c -> (#poke UNICODE_ASCII_CHAR, AsciiChar) ptr c

instance Storable (UNICODE_ASCII_CHAR WCHAR) where
    sizeOf _ = (#size UNICODE_ASCII_CHAR)
    alignment _ = (#alignment UNICODE_ASCII_CHAR)
    peek ptr = UnicodeAsciiChar <$> (#peek UNICODE_ASCII_CHAR, UnicodeChar) ptr
    poke ptr val = case val of
        UnicodeAsciiChar c -> (#poke UNICODE_ASCII_CHAR, UnicodeChar) ptr c


{-
typedef struct _CHAR_INFO {
	union {
		WCHAR UnicodeChar;
		CHAR AsciiChar;
	} Char;
	WORD Attributes;
} CHAR_INFO, *PCHAR_INFO;
-}
data CHAR_INFO t = CHAR_INFO {
        infoChar       :: UNICODE_ASCII_CHAR t,
        infoAttributes :: WORD
    } deriving (Show, Read)

instance Storable (UNICODE_ASCII_CHAR t) => Storable (CHAR_INFO t) where
    sizeOf _ = (#size CHAR_INFO)
    alignment _ = (#alignment CHAR_INFO)
    peek ptr = CHAR_INFO <$> (#peek CHAR_INFO, Char) ptr
                         <*> (#peek CHAR_INFO, Attributes) ptr
    poke ptr val = do
        (#poke CHAR_INFO, Char)       ptr $ infoChar val
        (#poke CHAR_INFO, Attributes) ptr $ infoAttributes val


{-
typedef struct _SMALL_RECT {
	SHORT Left;
	SHORT Top;
	SHORT Right;
	SHORT Bottom;
} SMALL_RECT, *PSMALL_RECT;
-}
data SMALL_RECT = SMALL_RECT {
        rectLeft   :: SHORT,
        rectTop    :: SHORT,
        rectRight  :: SHORT,
        rectBottom :: SHORT
    } deriving (Show, Read, Eq)

instance Storable SMALL_RECT where
    sizeOf _ = (#size SMALL_RECT)
    alignment _ = (#alignment SMALL_RECT)
    peek ptr = SMALL_RECT <$> (#peek SMALL_RECT, Left) ptr
                          <*> (#peek SMALL_RECT, Top) ptr
                          <*> (#peek SMALL_RECT, Right) ptr
                          <*> (#peek SMALL_RECT, Bottom) ptr
    poke ptr val = do
        (#poke SMALL_RECT, Left)   ptr $ rectLeft val
        (#poke SMALL_RECT, Top)    ptr $ rectTop val
        (#poke SMALL_RECT, Right)  ptr $ rectRight val
        (#poke SMALL_RECT, Bottom) ptr $ rectBottom val


{-
typedef struct _CONSOLE_CURSOR_INFO {
	DWORD	dwSize;
	BOOL	bVisible;
} CONSOLE_CURSOR_INFO,*PCONSOLE_CURSOR_INFO;
-}
data CONSOLE_CURSOR_INFO = CONSOLE_CURSOR_INFO {
        curSize    :: DWORD,
        curVisible :: BOOL
    } deriving (Show, Read, Eq)

instance Storable CONSOLE_CURSOR_INFO where
    sizeOf _ = (#size CONSOLE_CURSOR_INFO)
    alignment _ = (#alignment CONSOLE_CURSOR_INFO)
    peek ptr = CONSOLE_CURSOR_INFO <$> (#peek CONSOLE_CURSOR_INFO, dwSize) ptr
                                   <*> (#peek CONSOLE_CURSOR_INFO, bVisible) ptr
    poke ptr val = do
        (#poke CONSOLE_CURSOR_INFO, dwSize)   ptr $ curSize val
        (#poke CONSOLE_CURSOR_INFO, bVisible) ptr $ curVisible val


{-
typedef struct _COORD {
	SHORT X;
	SHORT Y;
} COORD, *PCOORD;
-}
data COORD = COORD {
        coordX :: SHORT,
        coordY :: SHORT
    } deriving (Show, Read, Eq)

instance Storable COORD where
    sizeOf _ = (#size COORD)
    alignment _ = (#alignment COORD)
    peek ptr = COORD <$> (#peek COORD, X) ptr
                     <*> (#peek COORD, Y) ptr
    poke ptr val = do
        (#poke COORD, X) ptr $ coordX val
        (#poke COORD, Y) ptr $ coordY val

{-
-- TODO
typedef struct _CONSOLE_FONT_INFO {
	DWORD nFont;
	COORD dwFontSize;
} CONSOLE_FONT_INFO, *PCONSOLE_FONT_INFO;
-}

{-
typedef struct _CONSOLE_SCREEN_BUFFER_INFO {
	COORD	dwSize;
	COORD	dwCursorPosition;
	WORD	wAttributes;
	SMALL_RECT srWindow;
	COORD	dwMaximumWindowSize;
} CONSOLE_SCREEN_BUFFER_INFO,*PCONSOLE_SCREEN_BUFFER_INFO;
-}
data CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO {
        bufSize           :: COORD,
        bufCursorPosition :: COORD,
        bufAttributes     :: WORD,
        bufWindow         :: SMALL_RECT,
        bufMaxWindowSize  :: COORD
    } deriving (Show, Read, Eq)

instance Storable CONSOLE_SCREEN_BUFFER_INFO where
    sizeOf _ = (#size CONSOLE_SCREEN_BUFFER_INFO)
    alignment _ = (#alignment CONSOLE_SCREEN_BUFFER_INFO)
    peek ptr = CONSOLE_SCREEN_BUFFER_INFO <$> (#peek CONSOLE_SCREEN_BUFFER_INFO, dwSize) ptr
                                          <*> (#peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition) ptr
                                          <*> (#peek CONSOLE_SCREEN_BUFFER_INFO, wAttributes) ptr
                                          <*> (#peek CONSOLE_SCREEN_BUFFER_INFO, srWindow) ptr
                                          <*> (#peek CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize) ptr
    poke ptr val = do
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwSize)              ptr $ bufSize val
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition)    ptr $ bufCursorPosition val
        (#poke CONSOLE_SCREEN_BUFFER_INFO, wAttributes)         ptr $ bufAttributes val
        (#poke CONSOLE_SCREEN_BUFFER_INFO, srWindow)            ptr $ bufWindow val
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize) ptr $ bufMaxWindowSize val


{-
{-
typedef BOOL(CALLBACK *PHANDLER_ROUTINE)(DWORD);
-}
type HANDLER_ROUTINE = DWORD -> IO BOOL

foreign import ccall "wrapper"
    makeHandlerRoutinePtr :: HANDLER_ROUTINE -> IO (FunPtr HANDLER_ROUTINE)
-}


{-
typedef struct _KEY_EVENT_RECORD {
	BOOL bKeyDown;
	WORD wRepeatCount;
	WORD wVirtualKeyCode;
	WORD wVirtualScanCode;
	union {
		WCHAR UnicodeChar;
		CHAR AsciiChar;
	} uChar;
	DWORD dwControlKeyState;
}
#ifdef __GNUC__
/* gcc's alignment is not what win32 expects */
 PACKED
#endif
KEY_EVENT_RECORD;
-}
data KEY_EVENT_RECORD t = KEY_EVENT_RECORD {
        keyEventKeyDown         :: BOOL,
        keyEventRepeatCount     :: WORD,
        keyEventVirtualKeyCode  :: WORD,
        keyEventVirtualScanCode :: WORD,
        keyEventChar            :: UNICODE_ASCII_CHAR t,
        keyEventControlKeystate :: DWORD
    } deriving (Show, Read, Eq)

instance Storable (UNICODE_ASCII_CHAR t) => Storable (KEY_EVENT_RECORD t) where
    sizeOf _ = (#size KEY_EVENT_RECORD)
    alignment _ = (#alignment KEY_EVENT_RECORD)
    peek ptr =
        KEY_EVENT_RECORD <$> (#peek KEY_EVENT_RECORD, bKeyDown) ptr
                         <*> (#peek KEY_EVENT_RECORD, wRepeatCount) ptr
                         <*> (#peek KEY_EVENT_RECORD, wVirtualKeyCode) ptr
                         <*> (#peek KEY_EVENT_RECORD, wVirtualScanCode) ptr
                         <*> (#peek KEY_EVENT_RECORD, uChar) ptr
                         <*> (#peek KEY_EVENT_RECORD, dwControlKeyState) ptr
    poke ptr val = do
        (#poke KEY_EVENT_RECORD, bKeyDown)          ptr $ keyEventKeyDown val
        (#poke KEY_EVENT_RECORD, wRepeatCount)      ptr $ keyEventRepeatCount val
        (#poke KEY_EVENT_RECORD, wVirtualKeyCode)   ptr $ keyEventVirtualKeyCode val
        (#poke KEY_EVENT_RECORD, wVirtualScanCode)  ptr $ keyEventVirtualScanCode val
        (#poke KEY_EVENT_RECORD, uChar)             ptr $ keyEventChar val
        (#poke KEY_EVENT_RECORD, dwControlKeyState) ptr $ keyEventControlKeystate val


{-
typedef struct _MOUSE_EVENT_RECORD {
	COORD dwMousePosition;
	DWORD dwButtonState;
	DWORD dwControlKeyState;
	DWORD dwEventFlags;
} MOUSE_EVENT_RECORD;
-}
data MOUSE_EVENT_RECORD = MOUSE_EVENT_RECORD {
        mousePosition        :: COORD,
        mouseButtonState     :: DWORD,
        mouseControlKeyState :: DWORD,
        mouseEventFlags      :: DWORD
    } deriving (Show, Read, Eq)

instance Storable MOUSE_EVENT_RECORD where
    sizeOf _ = (#size MOUSE_EVENT_RECORD)
    alignment _ = (#alignment MOUSE_EVENT_RECORD)
    peek ptr =
        MOUSE_EVENT_RECORD <$> (#peek MOUSE_EVENT_RECORD, dwMousePosition) ptr
                           <*> (#peek MOUSE_EVENT_RECORD, dwButtonState) ptr
                           <*> (#peek MOUSE_EVENT_RECORD, dwControlKeyState) ptr
                           <*> (#peek MOUSE_EVENT_RECORD, dwEventFlags) ptr
    poke ptr val = do
        (#poke MOUSE_EVENT_RECORD, dwMousePosition)   ptr $ mousePosition val
        (#poke MOUSE_EVENT_RECORD, dwButtonState)     ptr $ mouseButtonState val
        (#poke MOUSE_EVENT_RECORD, dwControlKeyState) ptr $ mouseControlKeyState val
        (#poke MOUSE_EVENT_RECORD, dwEventFlags)      ptr $ mouseEventFlags val


{-
typedef struct _WINDOW_BUFFER_SIZE_RECORD {	COORD dwSize; } WINDOW_BUFFER_SIZE_RECORD;
-}
data WINDOW_BUFFER_SIZE_RECORD = WINDOW_BUFFER_SIZE_RECORD {
        bufSizeNew :: COORD
    } deriving (Show, Read, Eq)

instance Storable WINDOW_BUFFER_SIZE_RECORD where
    sizeOf _ = (#size WINDOW_BUFFER_SIZE_RECORD)
    alignment _ = (#alignment WINDOW_BUFFER_SIZE_RECORD)
    peek ptr = WINDOW_BUFFER_SIZE_RECORD <$> (#peek WINDOW_BUFFER_SIZE_RECORD, dwSize) ptr
    poke ptr val = (#poke WINDOW_BUFFER_SIZE_RECORD, dwSize) ptr $ bufSizeNew val

{-
typedef struct _MENU_EVENT_RECORD {	UINT dwCommandId; } MENU_EVENT_RECORD,*PMENU_EVENT_RECORD;
-}
data MENU_EVENT_RECORD = MENU_EVENT_RECORD {
        menuCommandId :: UINT
    } deriving (Show, Read, Eq)

instance Storable MENU_EVENT_RECORD where
    sizeOf _ = (#size MENU_EVENT_RECORD)
    alignment _ = (#alignment MENU_EVENT_RECORD)
    peek ptr = MENU_EVENT_RECORD <$> (#peek MENU_EVENT_RECORD, dwCommandId) ptr
    poke ptr val = (#poke MENU_EVENT_RECORD, dwCommandId) ptr $ menuCommandId val


{-
typedef struct _FOCUS_EVENT_RECORD { BOOL bSetFocus; } FOCUS_EVENT_RECORD;
-}
data FOCUS_EVENT_RECORD = FOCUS_EVENT_RECORD {
        focusSetFocus :: BOOL
    } deriving (Show, Read, Eq)

instance Storable FOCUS_EVENT_RECORD where
    sizeOf _ = (#size FOCUS_EVENT_RECORD)
    alignment _ = (#alignment FOCUS_EVENT_RECORD)
    peek ptr = FOCUS_EVENT_RECORD <$> (#peek FOCUS_EVENT_RECORD, bSetFocus) ptr
    poke ptr val = (#poke FOCUS_EVENT_RECORD, bSetFocus) ptr $ focusSetFocus val


{-
typedef struct _INPUT_RECORD {
	WORD EventType;
	union {
		KEY_EVENT_RECORD KeyEvent;
		MOUSE_EVENT_RECORD MouseEvent;
		WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent;
		MENU_EVENT_RECORD MenuEvent;
		FOCUS_EVENT_RECORD FocusEvent;
	} Event;
} INPUT_RECORD,*PINPUT_RECORD;
-}
data INPUT_RECORD t = INPUT_RECORD {
        inputEventType :: WORD,
        inputEvent     :: INPUT_RECORD_EVENT t
    } deriving (Show, Read, Eq)

data INPUT_RECORD_EVENT t = InputKeyEvent (KEY_EVENT_RECORD t)
                          | InputMouseEvent MOUSE_EVENT_RECORD
                          | InputWindowBufferSizeEvent WINDOW_BUFFER_SIZE_RECORD
                          | InputMenuEvent MENU_EVENT_RECORD
                          | InputFocusEvent FOCUS_EVENT_RECORD
                            deriving (Show, Read, Eq)

instance Storable (KEY_EVENT_RECORD t) => Storable (INPUT_RECORD t) where
    sizeOf _ = (#size INPUT_RECORD)
    alignment _ = (#alignment INPUT_RECORD)
    peek ptr = do
        evType <- (#peek INPUT_RECORD, EventType) ptr
        let evTypeD = (toEnum . fromEnum :: WORD -> DWORD) evType
        event <- case evType of
            _ | evTypeD == kEY_EVENT                -> InputKeyEvent              <$> (#peek INPUT_RECORD, Event.KeyEvent) ptr
            _ | evTypeD == mOUSE_EVENT              -> InputMouseEvent            <$> (#peek INPUT_RECORD, Event.MouseEvent) ptr
            _ | evTypeD == wINDOW_BUFFER_SIZE_EVENT -> InputWindowBufferSizeEvent <$> (#peek INPUT_RECORD, Event.WindowBufferSizeEvent) ptr
            _ | evTypeD == mENU_EVENT               -> InputMenuEvent             <$> (#peek INPUT_RECORD, Event.MenuEvent) ptr
            _ | evTypeD == fOCUS_EVENT              -> InputFocusEvent            <$> (#peek INPUT_RECORD, Event.FocusEvent) ptr
            _ -> error $ "peek (INPUT_RECORD): Unknown event type " ++ show evType
        return $ INPUT_RECORD evType event
    poke ptr val = do
        (#poke INPUT_RECORD, EventType) ptr $ inputEventType val
        -- Add sanity checks?
        case inputEvent val of
            InputKeyEvent ev ->              (#poke INPUT_RECORD, Event.KeyEvent) ptr ev
            InputMouseEvent ev ->            (#poke INPUT_RECORD, Event.MouseEvent) ptr ev
            InputWindowBufferSizeEvent ev -> (#poke INPUT_RECORD, Event.WindowBufferSizeEvent) ptr ev
            InputMenuEvent ev ->             (#poke INPUT_RECORD, Event.MenuEvent) ptr ev
            InputFocusEvent ev ->            (#poke INPUT_RECORD, Event.FocusEvent) ptr ev



-- * Functions

{-
BOOL WINAPI AllocConsole(void);
-}
foreign import stdcall "windows.h AllocConsole"
    c_AllocConsole :: IO BOOL

allocConsole :: IO ()
allocConsole = failIfFalse_ "AllocConsole" c_AllocConsole

-- According to MSYS (MSDN: 0x0501)
#if _WIN32_WINNT >= 0x0500

{-
BOOL WINAPI AttachConsole(
  _In_ DWORD dwProcessId
);
-}
foreign import stdcall "windows.h AttachConsole"
    c_AttachConsole :: DWORD -> IO BOOL

attachConsole :: Maybe ProcessId -> IO ()
attachConsole mbPID = failIfFalse_ "AttachConsole" $
    c_AttachConsole $ fromMaybe aTTACH_PARENT_PROCESS mbPID

#endif

{-
HANDLE WINAPI CreateConsoleScreenBuffer(
  _In_             DWORD               dwDesiredAccess,
  _In_             DWORD               dwShareMode,
  _In_opt_   const SECURITY_ATTRIBUTES *lpSecurityAttributes,
  _In_             DWORD               dwFlags,
  _Reserved_       LPVOID              lpScreenBufferData
);
-}
-- TODO: Handle security attributes
foreign import stdcall "windows.h CreateConsoleScreenBuffer"
    c_CreateConsoleScreenBuffer :: DWORD -> DWORD -> LPVOID {- Ptr SECURITY_ATTRIBUTES -} -> DWORD -> LPVOID -> IO HANDLE

createConsoleScreenBuffer :: DWORD -> DWORD -> IO HANDLE
createConsoleScreenBuffer access share =
    failIf (== iNVALID_HANDLE_VALUE) "CreateConsoleScreenBuffer" $
        c_CreateConsoleScreenBuffer access share nullPtr cONSOLE_TEXTMODE_BUFFER nullPtr
    where
        -- Only supported console screen buffer type for @ConsoleCreateScreenBuffer@.
        cONSOLE_TEXTMODE_BUFFER :: DWORD
        cONSOLE_TEXTMODE_BUFFER = (#const CONSOLE_TEXTMODE_BUFFER)


{-
BOOL WINAPI FillConsoleOutputAttribute(
  _In_  HANDLE  hConsoleOutput,
  _In_  WORD    wAttribute,
  _In_  DWORD   nLength,
  _In_  COORD   dwWriteCoord,
  _Out_ LPDWORD lpNumberOfAttrsWritten
);
-}
foreign import ccall "WinConsoleWrapper.h HsFillConsoleOutputAttribute"
    c_HsFillConsoleOutputAttribute :: HANDLE -> WORD -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL

fillConsoleOutputAttribute :: HANDLE -> WORD -> DWORD -> COORD -> IO DWORD
fillConsoleOutputAttribute hdl attr len pos =
    returnWith_ $ \ptrN ->
        with pos $ \ptrPos ->
            failIfFalse_ "FillConsoleOutputAttribute" $
                c_HsFillConsoleOutputAttribute hdl attr len ptrPos ptrN

{-
BOOL WINAPI FillConsoleOutputCharacter(
  _In_  HANDLE  hConsoleOutput,
  _In_  TCHAR   cCharacter,
  _In_  DWORD   nLength,
  _In_  COORD   dwWriteCoord,
  _Out_ LPDWORD lpNumberOfCharsWritten
);
-}
foreign import ccall "WinConsoleWrapper.h HsFillConsoleOutputCharacterA"
    c_HsFillConsoleOutputCharacterA :: HANDLE -> CHAR -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL

foreign import ccall "WinConsoleWrapper.h HsFillConsoleOutputCharacterW"
    c_HsFillConsoleOutputCharacterW :: HANDLE -> WCHAR -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL

fillConsoleOutputCharacter :: TChar c t => c -> HANDLE -> t -> DWORD -> COORD -> IO DWORD
fillConsoleOutputCharacter t hdl char len pos =
    returnWith_ $ \ptrN ->
        with pos $ \ptrPos ->
            failIfFalse_ ("FillConsoleOutputCharacter" ++ suffix t) $
                c_FillConsoleOutputCharacter t hdl char len ptrPos ptrN



{-
BOOL WINAPI FlushConsoleInputBuffer(
  _In_ HANDLE hConsoleInput
);
-}
foreign import stdcall "windows.h FlushConsoleInputBuffer"
    c_FlushConsoleInputBuffer :: HANDLE -> IO BOOL

flushConsoleInputBuffer :: HANDLE -> IO ()
flushConsoleInputBuffer hdl = failIfFalse_ "FlushConsoleInputBuffer" $ c_FlushConsoleInputBuffer hdl


{-
BOOL WINAPI FreeConsole(void);
-}
foreign import stdcall "windows.h FreeConsole"
    c_FreeConsole :: IO BOOL

freeConsole :: IO ()
freeConsole = failIfFalse_ "FreeConsole" c_FreeConsole


{-
BOOL WINAPI GenerateConsoleCtrlEvent(
  _In_ DWORD dwCtrlEvent,
  _In_ DWORD dwProcessGroupId
);
-}
foreign import stdcall "windows.h GenerateConsoleCtrlEvent"
    c_GenerateConsoleCtrlEvent :: DWORD -> DWORD -> IO BOOL

generateConsoleCtrlEvent :: DWORD -> DWORD -> IO ()
generateConsoleCtrlEvent ev gid =
    failIfFalse_ "GenerateConsoleCtrlEvent" $ c_GenerateConsoleCtrlEvent ev gid


{-
UINT WINAPI GetConsoleCP(void);
-}
foreign import stdcall "windows.h GetConsoleCP"
    c_GetConsoleCP :: IO UINT

getConsoleCP :: IO UINT
getConsoleCP = c_GetConsoleCP


{-
BOOL WINAPI GetConsoleCursorInfo(
  _In_  HANDLE               hConsoleOutput,
  _Out_ PCONSOLE_CURSOR_INFO lpConsoleCursorInfo
);
-}
foreign import stdcall "windows.h GetConsoleCursorInfo"
    c_GetConsoleCursorInfo :: HANDLE -> Ptr CONSOLE_CURSOR_INFO -> IO BOOL

getConsoleCursorInfo :: HANDLE -> IO CONSOLE_CURSOR_INFO
getConsoleCursorInfo hdl =
    returnWith_ $ \ptrInfo ->
        failIfFalse_ "GetConsoleCursorInfo" $ c_GetConsoleCursorInfo hdl ptrInfo


{-
BOOL WINAPI GetConsoleMode(
  _In_  HANDLE  hConsoleHandle,
  _Out_ LPDWORD lpMode
);
-}
foreign import stdcall "windows.h GetConsoleMode"
    c_GetConsoleMode :: HANDLE -> LPDWORD -> IO BOOL

getConsoleMode :: HANDLE -> IO DWORD
getConsoleMode hdl =
    returnWith_ $ \ptrMode ->
        failIfFalse_ "GetConsoleMode" $ c_GetConsoleMode hdl ptrMode


{-
UINT WINAPI GetConsoleOutputCP(void);
-}
foreign import stdcall "windows.h GetConsoleOutputCP"
    c_GetConsoleOutputCP :: IO UINT

getConsoleOutputCP :: IO UINT
getConsoleOutputCP = c_GetConsoleOutputCP


{-
BOOL WINAPI GetConsoleScreenBufferInfo(
  _In_  HANDLE                      hConsoleOutput,
  _Out_ PCONSOLE_SCREEN_BUFFER_INFO lpConsoleScreenBufferInfo
);
-}
foreign import stdcall "windows.h GetConsoleScreenBufferInfo"
    c_GetScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO BOOL

getConsoleScreenBufferInfo :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFO
getConsoleScreenBufferInfo hdl =
    returnWith_ $ \ptrBuf ->
        failIfFalse_ "GetConsoleScreenBufferInfo" $ c_GetScreenBufferInfo hdl ptrBuf


{-
DWORD WINAPI GetConsoleTitle(
  _Out_ LPTSTR lpConsoleTitle,
  _In_  DWORD  nSize
);
-}
foreign import stdcall "windows.h GetConsoleTitleA"
    c_GetConsoleTitleA :: LPSTR -> DWORD -> IO DWORD

foreign import stdcall "windows.h GetConsoleTitleW"
    c_GetConsoleTitleW :: LPWSTR -> DWORD -> IO DWORD

getConsoleTitle :: TChar c t => c -> IO String
getConsoleTitle t = allocaArray0 maxConsoleTitleLen $ \ptrCStr -> do
    -- In case of empty titles, GetConsoleTitle returns 0 too, so we don't know if the string is empty
    -- or an error has occured. We set the last error to 0 and check if it has changed afterwards.
    -- If GetLastError still returns 0 after the call to GetConsoleTitle, we assume the title is empty.
    c_setLastError 0
    n <- c_GetConsoleTitle t ptrCStr $ toEnum maxConsoleTitleLen + 1
    s <- case n of
        0 -> do
            err <- getLastError
            case err of
                0 -> return ""
                _ -> errorWin ("GetConsoleTitle" ++ suffix t)
        _ -> peekCTString t ptrCStr
    -- Check length s == n?
    return s


-- According to MSDN and MSYS
#if _WIN32_WINNT >= 0x0500

{-
BOOL WINAPI GetConsoleDisplayMode(
  _Out_ LPDWORD lpModeFlags
);
-}
foreign import stdcall "windows.h GetConsoleDisplayMode"
    c_GetConsoleDisplayMode :: LPDWORD -> IO BOOL

getConsoleDisplayMode :: IO DWORD
getConsoleDisplayMode =
    returnWith_ $ \ptrMode ->
        failIfFalse_ "GetConsoleDisplayMode" $ c_GetConsoleDisplayMode ptrMode

#endif

-- According to MSDN and MSYS
#if _WIN32_WINNT >= 0x0500

{-
HWND WINAPI GetConsoleWindow(void);
-}
foreign import stdcall "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO HWND

getConsoleWindow :: IO (Maybe HWND)
getConsoleWindow = ptrToMaybe <$> c_GetConsoleWindow

#endif

-- According to MSDN and MSYS
#if _WIN32_WINNT >= 0x0501
{-
DWORD WINAPI GetConsoleProcessList(
  _Out_ LPDWORD lpdwProcessList,
  _In_  DWORD   dwProcessCount
);
-}
foreign import stdcall "windows.h GetConsoleProcessList"
    c_GetConsoleProcessList :: LPDWORD -> DWORD -> IO DWORD

getConsoleProcessList :: IO [DWORD]
getConsoleProcessList = do
    -- Try with buffer size of maxLen first ...
    mbIds <- allocaArray maxLen $ \ptrBuf -> do
        n <- failIf (== 0) "GetConsoleProcessList" $ c_GetConsoleProcessList ptrBuf (toEnum maxLen)
        case () of
            _ | fromEnum n > maxLen -> return $ Left n
            _                       -> Right <$> peekArray (fromEnum n) ptrBuf

    -- ... and if that fails, try again with a properly sized buffer
    case mbIds of
        Left n ->
            allocaArray (fromEnum n) $ \ptrBuf -> do
                n' <- failIf (== 0) "GetConsoleProcessList" $ c_GetConsoleProcessList ptrBuf n
                peekArray (fromEnum n') ptrBuf
        Right ids -> return ids
    where
        maxLen = 16

#endif


{-
COORD WINAPI GetLargestConsoleWindowSize(
  _In_ HANDLE hConsoleOutput
);
-}
foreign import ccall "WinConsoleWrapper.h HsGetLargestConsoleWindowSize"
    c_HsGetLargestConsoleWindowSize :: HANDLE -> Ptr COORD -> IO ()

getLargestConsoleWindowSize :: HANDLE -> IO COORD
getLargestConsoleWindowSize hdl =
    alloca $ \ptrSize ->
        failIf (== COORD 0 0) "GetLargestConsoleWindowSize" $ do
            c_HsGetLargestConsoleWindowSize hdl ptrSize
            peek ptrSize


{-
BOOL WINAPI GetNumberOfConsoleInputEvents(
  _In_  HANDLE  hConsoleInput,
  _Out_ LPDWORD lpcNumberOfEvents
);
-}
foreign import stdcall "windows.h GetNumberOfConsoleInputEvents"
    c_GetNumberOfConsoleInputEvents :: HANDLE -> Ptr DWORD -> IO BOOL

getNumberOfConsoleInputEvents :: HANDLE -> IO DWORD
getNumberOfConsoleInputEvents hdl =
    returnWith_ $ \ptrN ->
        failIfFalse_ "GetNumberOfConsoleInputEvents" $ c_GetNumberOfConsoleInputEvents hdl ptrN


{-
BOOL WINAPI GetNumberOfConsoleMouseButtons(
  _Out_ LPDWORD lpNumberOfMouseButtons
);
-}
foreign import stdcall "windows.h GetNumberOfConsoleMouseButtons"
    c_GetNumberOfConsoleMouseButtons :: LPDWORD -> IO BOOL

getNumberOfConsoleMouseButtons :: IO DWORD
getNumberOfConsoleMouseButtons =
    returnWith_ $ \ptrN ->
        failIfFalse_ "GetNumberOfConsoleMouseButtons" $ c_GetNumberOfConsoleMouseButtons ptrN


{-
BOOL WINAPI PeekConsoleInput(
  _In_  HANDLE        hConsoleInput,
  _Out_ PINPUT_RECORD lpBuffer,
  _In_  DWORD         nLength,
  _Out_ LPDWORD       lpNumberOfEventsRead
);
-}
foreign import stdcall "windows.h PeekConsoleInputA"
    c_PeekConsoleInputA :: HANDLE -> Ptr (INPUT_RECORD CHAR) -> DWORD -> LPDWORD -> IO BOOL

foreign import stdcall "windows.h PeekConsoleInputW"
    c_PeekConsoleInputW :: HANDLE -> Ptr (INPUT_RECORD WCHAR) -> DWORD -> LPDWORD -> IO BOOL


peekConsoleInput :: TChar c t => c -> HANDLE -> DWORD -> IO [INPUT_RECORD t]
peekConsoleInput t hdl len = peekConsoleInputWith t hdl len $ \(ptrBuf, n) -> peekArray (fromEnum n) ptrBuf

peekConsoleInputWith :: TChar c t => c -> HANDLE -> DWORD -> OutputHandler (Ptr (INPUT_RECORD t), DWORD)
peekConsoleInputWith t hdl len handler =
    allocaArray (fromEnum len) $ \ptrBuf ->
        alloca $ \ptrN -> do
            failIfFalse_ ("PeekConsoleInput" ++ suffix t) $ c_PeekConsoleInput t hdl ptrBuf len ptrN
            n <- peek ptrN
            handler (ptrBuf, n)


{-
BOOL WINAPI ReadConsole(
  _In_     HANDLE  hConsoleInput,
  _Out_    LPVOID  lpBuffer,
  _In_     DWORD   nNumberOfCharsToRead,
  _Out_    LPDWORD lpNumberOfCharsRead,
  _In_opt_ LPVOID  pInputControl
);
-}
foreign import stdcall "windows.h ReadConsoleA"
    c_ReadConsoleA :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPVOID -> IO BOOL

foreign import stdcall "windows.h ReadConsoleW"
    c_ReadConsoleW :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPVOID -> IO BOOL


readConsole :: TChar c t => c -> HANDLE -> DWORD -> IO String
readConsole t hdl len = readConsoleWith t hdl len $ peekCTStringLen t

readConsoleWith :: TChar c t => c -> HANDLE -> DWORD -> OutputHandler (Ptr t, DWORD)
readConsoleWith t hdl len handler =
    allocaArray (fromEnum len) $ \ptrCStr ->
        alloca $ \ptrN -> do
            failIfFalse_ ("ReadConsole" ++ suffix t) $
                c_ReadConsole t hdl (castPtr ptrCStr) len ptrN nullPtr
            n <- peek ptrN
            handler (ptrCStr, n)


{-
BOOL WINAPI ReadConsoleInput(
  _In_  HANDLE        hConsoleInput,
  _Out_ PINPUT_RECORD lpBuffer,
  _In_  DWORD         nLength,
  _Out_ LPDWORD       lpNumberOfEventsRead
);
-}
foreign import stdcall "windows.h ReadConsoleInputA"
    c_ReadConsoleInputA :: HANDLE -> Ptr (INPUT_RECORD CHAR) -> DWORD -> LPDWORD -> IO BOOL

foreign import stdcall "windows.h ReadConsoleInputW"
    c_ReadConsoleInputW :: HANDLE -> Ptr (INPUT_RECORD WCHAR) -> DWORD -> LPDWORD -> IO BOOL


readConsoleInput :: TChar c t => c -> HANDLE -> DWORD -> IO [INPUT_RECORD t]
readConsoleInput t hdl len = readConsoleInputWith t hdl len $ \(ptr, n) -> peekArray (fromEnum n) ptr

readConsoleInputWith :: TChar c t => c -> HANDLE -> DWORD -> OutputHandler (Ptr (INPUT_RECORD t), DWORD)
readConsoleInputWith t hdl len handler =
    allocaArray (fromEnum len) $ \ptrBuf ->
        alloca $ \ptrN -> do
            failIfFalse_ ("ReadConsoleInput" ++ suffix t) $ c_ReadConsoleInput t hdl ptrBuf len ptrN
            n <- peek ptrN
            handler (ptrBuf, n)


{-
BOOL WINAPI ReadConsoleOutputAttribute(
  _In_  HANDLE  hConsoleOutput,
  _Out_ LPWORD  lpAttribute,
  _In_  DWORD   nLength,
  _In_  COORD   dwReadCoord,
  _Out_ LPDWORD lpNumberOfAttrsRead
);
-}
foreign import ccall "WinConsoleWrapper.h HsReadConsoleOutputAttribute"
    c_HsReadConsoleOutputAttribute :: HANDLE -> Ptr WORD -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL

readConsoleOutputAttribute :: HANDLE -> DWORD -> COORD -> IO [WORD]
readConsoleOutputAttribute hdl len pos =
    readConsoleOutputAttributeWith hdl len pos $ \(ptrBuf, n) -> peekArray (fromEnum n) ptrBuf

readConsoleOutputAttributeWith :: HANDLE -> DWORD -> COORD -> OutputHandler (Ptr WORD, DWORD)
readConsoleOutputAttributeWith hdl len pos handler =
    allocaArray (fromEnum len) $ \ptrBuf ->
        alloca $ \ptrN ->
            with pos $ \ptrPos -> do
                failIfFalse_ "ReadConsoleOutputAttribute" $
                    c_HsReadConsoleOutputAttribute hdl ptrBuf len ptrPos ptrN
                n <- peek ptrN
                handler (ptrBuf, n)


{-
BOOL WINAPI ReadConsoleOutputCharacter(
  _In_  HANDLE  hConsoleOutput,
  _Out_ LPTSTR  lpCharacter,
  _In_  DWORD   nLength,
  _In_  COORD   dwReadCoord,
  _Out_ LPDWORD lpNumberOfCharsRead
);
-}
foreign import ccall "WinConsoleWrapper.h HsReadConsoleOutputCharacterA"
    c_HsReadConsoleOutputCharacterA :: HANDLE -> LPSTR -> DWORD -> Ptr COORD -> LPDWORD -> IO BOOL

foreign import ccall "WinConsoleWrapper.h HsReadConsoleOutputCharacterW"
    c_HsReadConsoleOutputCharacterW :: HANDLE -> LPWSTR -> DWORD -> Ptr COORD -> LPDWORD -> IO BOOL


readConsoleOutputCharacter :: TChar c t => c -> HANDLE -> DWORD -> COORD -> IO String
readConsoleOutputCharacter t hdl len pos =
    readConsoleOutputCharacterWith t hdl len pos $ peekCTStringLen t

readConsoleOutputCharacterWith :: TChar c t => c -> HANDLE -> DWORD -> COORD -> OutputHandler (Ptr t, DWORD)
readConsoleOutputCharacterWith t hdl len pos handler =
    with pos $ \ptrPos ->
        allocaArray (fromEnum len) $ \ptrCStr ->
            alloca $ \ptrN -> do
                failIfFalse_ ("ReadConsoleOutputCharacter" ++ suffix t) $
                    c_ReadConsoleOutputCharacter t hdl ptrCStr len ptrPos ptrN
                n <- peek ptrN
                handler (ptrCStr, n)

{-
BOOL WINAPI ReadConsoleOutput(
  _In_    HANDLE      hConsoleOutput,
  _Out_   PCHAR_INFO  lpBuffer,
  _In_    COORD       dwBufferSize,
  _In_    COORD       dwBufferCoord,
  _Inout_ PSMALL_RECT lpReadRegion
);
-}
foreign import ccall "WinConsoleWrapper.h HsReadConsoleOutputA"
    c_HsReadConsoleOutputA :: HANDLE -> Ptr (CHAR_INFO CHAR) -> Ptr COORD -> Ptr COORD -> Ptr SMALL_RECT -> IO BOOL

foreign import ccall "WinConsoleWrapper.h HsReadConsoleOutputW"
    c_HsReadConsoleOutputW :: HANDLE -> Ptr (CHAR_INFO WCHAR) -> Ptr COORD -> Ptr COORD -> Ptr SMALL_RECT -> IO BOOL

readConsoleOutput :: TChar c t => c -> HANDLE -> Ptr (CHAR_INFO t) -> COORD -> COORD -> SMALL_RECT -> IO SMALL_RECT
readConsoleOutput t hdl ptrBuf bufSiz bufPos rect =
    with bufSiz $ \ptrSize ->
        with bufPos $ \ptrPos ->
            with rect $ \ptrRect -> do
                failIfFalse_ ("ReadConsoleOutput" ++ suffix t) $
                    c_ReadConsoleOutput t hdl ptrBuf ptrSize ptrPos ptrRect
                peek ptrRect


{-
BOOL WINAPI ScrollConsoleScreenBuffer(
  _In_           HANDLE     hConsoleOutput,
  _In_     const SMALL_RECT *lpScrollRectangle,
  _In_opt_ const SMALL_RECT *lpClipRectangle,
  _In_           COORD      dwDestinationOrigin,
  _In_     const CHAR_INFO  *lpFill
);
-}
foreign import ccall "WinConsoleWrapper.h HsScrollConsoleScreenBufferA"
    c_HsScrollConsoleScreenBufferA :: HANDLE -> Ptr SMALL_RECT -> Ptr SMALL_RECT -> Ptr COORD -> Ptr (CHAR_INFO CHAR) -> IO BOOL

foreign import ccall "WinConsoleWrapper.h HsScrollConsoleScreenBufferW"
    c_HsScrollConsoleScreenBufferW :: HANDLE -> Ptr SMALL_RECT -> Ptr SMALL_RECT -> Ptr COORD -> Ptr (CHAR_INFO WCHAR) -> IO BOOL

scrollConsoleScreenBuffer :: TChar c t => c -> HANDLE -> SMALL_RECT -> Maybe SMALL_RECT -> COORD -> CHAR_INFO t -> IO ()
scrollConsoleScreenBuffer t hdl rect mbClip pos fill =
    with rect $ \ptrRect ->
        withMaybe mbClip $ \ptrClip ->
            with pos $ \ptrPos ->
                with fill $ \ptrFill ->
                    failIfFalse_ "ScrollConsoleScreenBufferW" $
                        c_ScrollConsoleScreenBuffer t hdl ptrRect ptrClip ptrPos ptrFill


{-
BOOL WINAPI SetConsoleActiveScreenBuffer(
  _In_ HANDLE hConsoleOutput
);
-}
foreign import stdcall "windows.h SetConsoleActiveScreenBuffer"
    c_SetConsoleActiveScreenBuffer :: HANDLE -> IO BOOL

setConsoleActiveScreenBuffer :: HANDLE -> IO ()
setConsoleActiveScreenBuffer hdl =
    failIfFalse_ "SetConsoleActiveScreenBuffer" $ c_SetConsoleActiveScreenBuffer hdl


{-
BOOL WINAPI SetConsoleCP(
  _In_ UINT wCodePageID
);
-}
foreign import stdcall "windows.h SetConsoleCP"
    c_SetConsoleCP :: UINT -> IO BOOL

setConsoleCP :: UINT -> IO ()
setConsoleCP = failIfFalse_ "SetConsoleCP" . c_SetConsoleCP

{-
{-
BOOL WINAPI SetConsoleCtrlHandler(
  _In_opt_ PHANDLER_ROUTINE HandlerRoutine,
  _In_     BOOL             Add
);
-}
foreign import stdcall "windows.h SetConsoleCtrlHandler"
    c_SetConsoleCtrlHandler :: FunPtr HANDLER_ROUTINE -> BOOL -> IO BOOL

-- TODO: Free the FunPtr after the handler has been removed via "freeHaskellFunPtr"
setConsoleCtrlHandler :: FunPtr HANDLER_ROUTINE -> BOOL -> IO ()
setConsoleCtrlHandler ptrHandler add = failIfFalse_ "SetConsoleCtrlHandler" $ do
    c_SetConsoleCtrlHandler ptrHandler add
-}

{-
BOOL WINAPI SetConsoleCursorInfo(
  _In_       HANDLE              hConsoleOutput,
  _In_ const CONSOLE_CURSOR_INFO *lpConsoleCursorInfo
);
-}
foreign import stdcall "windows.h SetConsoleCursorInfo"
    c_SetConsoleCursorInfo :: HANDLE -> Ptr CONSOLE_CURSOR_INFO -> IO BOOL

setConsoleCursorInfo :: HANDLE -> CONSOLE_CURSOR_INFO -> IO ()
setConsoleCursorInfo hdl info =
    with info $ \ptrInfo ->
        failIfFalse_ "SetConsoleCursorInfo" $ c_SetConsoleCursorInfo hdl ptrInfo


{-
BOOL WINAPI SetConsoleCursorPosition(
  _In_ HANDLE hConsoleOutput,
  _In_ COORD  dwCursorPosition
);
-}
foreign import ccall "WinConsoleWrapper.h HsSetConsoleCursorPosition"
    c_HsSetConsoleCursorPosition :: HANDLE -> Ptr COORD -> IO BOOL

setConsoleCursorPosition :: HANDLE -> COORD -> IO ()
setConsoleCursorPosition hdl pos =
    with pos $ \ptrPos -> do
        failIfFalse_ "SetConsoleCursorPosition" $ c_HsSetConsoleCursorPosition hdl ptrPos


-- According to MSYS (MSDN: 0x0500)
#if _WIN32_WINNT >= 0x0501

{-
BOOL WINAPI SetConsoleDisplayMode(
  _In_      HANDLE hConsoleOutput,
  _In_      DWORD  dwFlags,
  _Out_opt_ PCOORD lpNewScreenBufferDimensions
);
-}
foreign import stdcall "windows.h SetConsoleDisplayMode"
    c_SetConsoleDisplayMode :: HANDLE -> DWORD -> Ptr COORD -> IO BOOL

setConsoleDisplayMode :: HANDLE -> DWORD -> IO COORD
setConsoleDisplayMode hdl flags =
    returnWith_ $ \ptrSize ->
        failIfFalse_ "SetConsoleDisplayMode" $ c_SetConsoleDisplayMode hdl flags ptrSize

#endif


{-
BOOL WINAPI SetConsoleMode(
  _In_ HANDLE hConsoleHandle,
  _In_ DWORD  dwMode
);
-}
foreign import stdcall "windows.h SetConsoleMode"
    c_SetConsoleMode :: HANDLE -> DWORD -> IO BOOL

setConsoleMode :: HANDLE -> DWORD -> IO ()
setConsoleMode hdl mode = failIfFalse_ "SetConsoleMode" $ c_SetConsoleMode hdl mode


{-
BOOL WINAPI SetConsoleOutputCP(
  _In_ UINT wCodePageID
);
-}
foreign import stdcall "windows.h SetConsoleOutputCP"
    c_SetConsoleOutputCP :: UINT -> IO BOOL

setConsoleOutputCP :: UINT -> IO ()
setConsoleOutputCP = failIfFalse_ "SetConsoleOutputCP" . c_SetConsoleOutputCP


{-
BOOL WINAPI SetConsoleScreenBufferSize(
  _In_ HANDLE hConsoleOutput,
  _In_ COORD  dwSize
);
-}
foreign import ccall "WinConsoleWrapper.h HsSetConsoleScreenBufferSize"
    c_HsSetConsoleScreenBufferSize :: HANDLE -> Ptr COORD -> IO BOOL

setConsoleScreenBufferSize :: HANDLE -> COORD -> IO ()
setConsoleScreenBufferSize hdl size =
    with size $ \ptrSize -> do
        failIfFalse_ "SetConsoleScreenBufferSize" $ c_HsSetConsoleScreenBufferSize hdl ptrSize


{-
BOOL WINAPI SetConsoleTextAttribute(
  _In_ HANDLE hConsoleOutput,
  _In_ WORD   wAttributes
);
-}
foreign import stdcall "windows.h SetConsoleTextAttribute"
    c_SetConsoleTextAttribute :: HANDLE -> WORD -> IO BOOL

setConsoleTextAttribute :: HANDLE -> WORD -> IO ()
setConsoleTextAttribute hdl attr =
    failIfFalse_ "SetConsoleTextAttribute" $ c_SetConsoleTextAttribute hdl attr


{-
BOOL WINAPI SetConsoleTitle(
  _In_ LPCTSTR lpConsoleTitle
);
-}
foreign import stdcall "windows.h SetConsoleTitleA"
    c_SetConsoleTitleA :: LPCSTR -> IO BOOL

foreign import stdcall "windows.h SetConsoleTitleW"
    c_SetConsoleTitleW :: LPCWSTR -> IO BOOL

setConsoleTitle :: TChar c t => c -> String -> IO ()
setConsoleTitle t s = withCTString t s $ \ptrCStr -> do
    failIfFalse_ ("SetConsoleTitle" ++ suffix t) $ c_SetConsoleTitle t ptrCStr


{-
BOOL WINAPI SetConsoleWindowInfo(
  _In_       HANDLE     hConsoleOutput,
  _In_       BOOL       bAbsolute,
  _In_ const SMALL_RECT *lpConsoleWindow
);
-}
foreign import stdcall "windows.h SetConsoleWindowInfo"
    c_SetConsoleWindowInfo :: HANDLE -> BOOL -> Ptr SMALL_RECT -> IO BOOL

setConsoleWindowInfo :: HANDLE -> BOOL -> SMALL_RECT -> IO ()
setConsoleWindowInfo hdl isAbs rect =
    with rect $ \ptrRect ->
        failIfFalse_ "SetConsoleWindowInfo" $ c_SetConsoleWindowInfo hdl isAbs ptrRect


{-
BOOL WINAPI WriteConsole(
  _In_             HANDLE  hConsoleOutput,
  _In_       const VOID    *lpBuffer,
  _In_             DWORD   nNumberOfCharsToWrite,
  _Out_            LPDWORD lpNumberOfCharsWritten,
  _Reserved_       LPVOID  lpReserved
);
-}
foreign import stdcall "windows.h WriteConsoleA"
    c_WriteConsoleA :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPVOID -> IO BOOL

foreign import stdcall "windows.h WriteConsoleW"
    c_WriteConsoleW :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPVOID -> IO BOOL

writeConsole :: TChar c t => c -> HANDLE -> String -> IO DWORD
writeConsole t hdl s = writeConsoleWith t hdl $ withCTStringLen t s

writeConsoleWith :: TChar c t => c -> HANDLE -> InputHandler (Ptr t, DWORD) -> IO DWORD
writeConsoleWith t hdl withBuffer =
    returnWith_ $ \ptrN ->
        withBuffer $ \(ptrBuf, len) ->
            failIfFalse_ ("WriteConsole" ++ suffix t) $
                c_WriteConsole t hdl (castPtr ptrBuf) len ptrN nullPtr


{-
BOOL WINAPI WriteConsoleInput(
  _In_        HANDLE       hConsoleInput,
  _In_  const INPUT_RECORD *lpBuffer,
  _In_        DWORD        nLength,
  _Out_       LPDWORD      lpNumberOfEventsWritten
);
-}
foreign import stdcall "windows.h WriteConsoleInputA"
    c_WriteConsoleInputA :: HANDLE -> Ptr (INPUT_RECORD CHAR) -> DWORD -> LPDWORD -> IO BOOL

foreign import stdcall "windows.h WriteConsoleInputW"
    c_WriteConsoleInputW :: HANDLE -> Ptr (INPUT_RECORD WCHAR) -> DWORD -> LPDWORD -> IO BOOL

writeConsoleInput :: TChar c t => c -> HANDLE -> [INPUT_RECORD t] -> IO DWORD
writeConsoleInput t hdl evs =
    writeConsoleInputWith t hdl $ \act -> withArrayLen evs $ \len ptr -> act (ptr, toEnum len)

writeConsoleInputWith :: TChar c t => c -> HANDLE -> InputHandler (Ptr (INPUT_RECORD t), DWORD) -> IO DWORD
writeConsoleInputWith t hdl withBuffer =
    returnWith_ $ \ptrN ->
        withBuffer $ \(ptrBuf, len) ->
            failIfFalse_ ("WriteConsoleInput" ++ suffix t) $ c_WriteConsoleInput t hdl ptrBuf len ptrN


{-
BOOL WINAPI WriteConsoleOutput(
  _In_          HANDLE      hConsoleOutput,
  _In_    const CHAR_INFO   *lpBuffer,
  _In_          COORD       dwBufferSize,
  _In_          COORD       dwBufferCoord,
  _Inout_       PSMALL_RECT lpWriteRegion
);
-}
foreign import ccall "WinConsoleWrapper.h HsWriteConsoleOutputA"
    c_HsWriteConsoleOutputA :: HANDLE -> Ptr (CHAR_INFO CHAR) -> Ptr COORD -> Ptr COORD -> Ptr SMALL_RECT -> IO BOOL

foreign import ccall "WinConsoleWrapper.h HsWriteConsoleOutputW"
    c_HsWriteConsoleOutputW :: HANDLE -> Ptr (CHAR_INFO WCHAR) -> Ptr COORD -> Ptr COORD -> Ptr SMALL_RECT -> IO BOOL

writeConsoleOutput :: TChar c t => c -> HANDLE -> Ptr (CHAR_INFO t) -> COORD -> COORD -> SMALL_RECT -> IO SMALL_RECT
writeConsoleOutput t hdl ptrBuf bufSiz bufPos rect =
    with bufSiz $ \ptrSize ->
        with bufPos $ \ptrPos ->
            with rect $ \ptrRect -> do
                failIfFalse_ ("WriteConsoleOutput" ++ suffix t) $
                    c_WriteConsoleOutput t hdl ptrBuf ptrSize ptrPos ptrRect
                peek ptrRect


{-
BOOL WINAPI WriteConsoleOutputAttribute(
  _In_        HANDLE  hConsoleOutput,
  _In_  const WORD    *lpAttribute,
  _In_        DWORD   nLength,
  _In_        COORD   dwWriteCoord,
  _Out_       LPDWORD lpNumberOfAttrsWritten
);
-}
foreign import ccall "WinConsoleWrapper.h HsWriteConsoleOutputAttribute"
    c_HsWriteConsoleOutputAttribute :: HANDLE -> Ptr WORD -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL

writeConsoleOutputAttribute :: HANDLE -> COORD -> [WORD]-> IO DWORD
writeConsoleOutputAttribute hdl pos attrs =
    writeConsoleOutputAttributeWith hdl pos $ \act -> withArrayLen attrs $ \len ptr -> act (ptr, toEnum len)

writeConsoleOutputAttributeWith :: HANDLE -> COORD -> InputHandler (Ptr WORD, DWORD) -> IO DWORD
writeConsoleOutputAttributeWith hdl pos withBuffer =
    returnWith_ $ \ptrN ->
        with pos $ \ptrPos -> do
            withBuffer $ \(ptrBuf, len) ->
                failIfFalse_ "WriteConsoleOutputAttribute" $
                    c_HsWriteConsoleOutputAttribute hdl ptrBuf len ptrPos ptrN


{-
BOOL WINAPI WriteConsoleOutputCharacter(
  _In_  HANDLE  hConsoleOutput,
  _In_  LPCTSTR lpCharacter,
  _In_  DWORD   nLength,
  _In_  COORD   dwWriteCoord,
  _Out_ LPDWORD lpNumberOfCharsWritten
);
-}
foreign import ccall "WinConsoleWrapper.h HsWriteConsoleOutputCharacterA"
    c_HsWriteConsoleOutputCharacterA :: HANDLE -> LPCSTR -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL

foreign import ccall "WinConsoleWrapper.h HsWriteConsoleOutputCharacterW"
    c_HsWriteConsoleOutputCharacterW :: HANDLE -> LPCWSTR -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL

writeConsoleOutputCharacter :: TChar c t => c -> HANDLE -> String -> COORD -> IO DWORD
writeConsoleOutputCharacter t hdl s pos =
    writeConsoleOutputCharacterWith t hdl pos $ withCTStringLen t s

writeConsoleOutputCharacterWith :: TChar c t => c -> HANDLE -> COORD -> InputHandler (Ptr t, DWORD) -> IO DWORD
writeConsoleOutputCharacterWith t hdl pos withBuffer =
    returnWith_ $ \ptrN ->
        with pos $ \ptrPos -> do
            withBuffer $ \(ptrBuf, len) ->
                failIfFalse_ ("WriteConsoleOutputCharacter" ++ suffix t) $
                    c_WriteConsoleOutputCharacter t hdl ptrBuf len ptrPos ptrN



-- * Generic functions

{-
    | An action used for providing buffer input data. The action @f r@ provides a properly sized
      buffer to which the input data has been written, passing the relevant buffer information
      of type @i@ to the continuation function @r@. @r@ will never write to the buffer or use the
      buffer after it has finished.
-}
type InputHandler i = forall a. (i -> IO a) -> IO a

{-
    | An action, similar to 'InputHandler', but used for retrieving buffer output data.
-}
type OutputHandler o = forall a. (o -> IO a) -> IO a


-- | Parameter type used for the ASCII variants of functions handling strings
data A = A

-- | Parameter type used for the Unicode variants of functions handling strings
data W = W

{-
    | Helper class for functions with ASCII and Unicode character variants.

      NOTE: The conversion functions from and to Haskell strings don't take into account the
      code page currently set. This is left for later versions of that package.
-}
class (Storable t, Storable (UNICODE_ASCII_CHAR t), Eq t, Enum t) => TChar c t | c -> t where
    -- | The suffix associated with the character variant
    suffix          :: c -> String
    -- | Conversion from Haskell strings to Windows strings
    toCTString      :: c -> String -> [t]
    toCTString t = fst . toCTStringLen t
    toCTStringLen   :: c -> String -> ([t], Int)
    -- | Conversion from Windows strings to Haskell strings
    fromCTString    :: c -> [t] -> String

    c_FillConsoleOutputCharacter :: c -> HANDLE -> t -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL
    c_GetConsoleTitle :: c -> Ptr t -> DWORD -> IO DWORD
    c_PeekConsoleInput :: c -> HANDLE -> Ptr (INPUT_RECORD t) -> DWORD -> LPDWORD -> IO BOOL
    c_ReadConsole :: c -> HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPVOID -> IO BOOL
    c_ReadConsoleInput :: c -> HANDLE -> Ptr (INPUT_RECORD t) -> DWORD -> LPDWORD -> IO BOOL
    c_ReadConsoleOutput :: c -> HANDLE -> Ptr (CHAR_INFO t) -> Ptr COORD -> Ptr COORD -> Ptr SMALL_RECT -> IO BOOL
    c_ReadConsoleOutputCharacter :: c -> HANDLE -> Ptr t -> DWORD -> Ptr COORD -> LPDWORD -> IO BOOL
    c_ScrollConsoleScreenBuffer :: c -> HANDLE -> Ptr SMALL_RECT -> Ptr SMALL_RECT -> Ptr COORD -> Ptr (CHAR_INFO t) -> IO BOOL
    c_SetConsoleTitle :: c -> Ptr t -> IO BOOL
    c_WriteConsole :: c -> HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPVOID -> IO BOOL
    c_WriteConsoleInput :: c -> HANDLE -> Ptr (INPUT_RECORD t) -> DWORD -> LPDWORD -> IO BOOL
    c_WriteConsoleOutput :: c -> HANDLE -> Ptr (CHAR_INFO t) -> Ptr COORD -> Ptr COORD -> Ptr SMALL_RECT -> IO BOOL
    c_WriteConsoleOutputCharacter :: c -> HANDLE -> Ptr t -> DWORD -> Ptr COORD -> Ptr DWORD -> IO BOOL


instance TChar A CHAR where
    suffix A = "A"
    toCTStringLen A = \s -> let s' = map castCharToCChar s in (s', length s')
    fromCTString A = map castCCharToChar

    c_GetConsoleTitle A = c_GetConsoleTitleA
    c_FillConsoleOutputCharacter A = c_HsFillConsoleOutputCharacterA
    c_PeekConsoleInput A = c_PeekConsoleInputA
    c_ReadConsole A = c_ReadConsoleA
    c_ReadConsoleInput A = c_ReadConsoleInputA
    c_ReadConsoleOutput A = c_HsReadConsoleOutputA
    c_ReadConsoleOutputCharacter A = c_HsReadConsoleOutputCharacterA
    c_ScrollConsoleScreenBuffer A = c_HsScrollConsoleScreenBufferA
    c_SetConsoleTitle A = c_SetConsoleTitleA
    c_WriteConsole A = c_WriteConsoleA
    c_WriteConsoleInput A = c_WriteConsoleInputA
    c_WriteConsoleOutput A = c_HsWriteConsoleOutputA
    c_WriteConsoleOutputCharacter A = c_HsWriteConsoleOutputCharacterA


instance TChar W WCHAR where
    suffix W = "W"
    toCTStringLen W = \s -> let s' = charsToCWchars s in (s', length s')
    fromCTString W = cWcharsToChars

    c_GetConsoleTitle W = c_GetConsoleTitleW
    c_FillConsoleOutputCharacter W = c_HsFillConsoleOutputCharacterW
    c_PeekConsoleInput W = c_PeekConsoleInputW
    c_ReadConsole W = c_ReadConsoleW
    c_ReadConsoleInput W = c_ReadConsoleInputW
    c_ReadConsoleOutput W = c_HsReadConsoleOutputW
    c_ReadConsoleOutputCharacter W = c_HsReadConsoleOutputCharacterW
    c_ScrollConsoleScreenBuffer W = c_HsScrollConsoleScreenBufferW
    c_SetConsoleTitle W = c_SetConsoleTitleW
    c_WriteConsole W = c_WriteConsoleW
    c_WriteConsoleInput W = c_WriteConsoleInputW
    c_WriteConsoleOutput W = c_HsWriteConsoleOutputW
    c_WriteConsoleOutputCharacter W = c_HsWriteConsoleOutputCharacterW



-- * Helper stuff

maxConsoleTitleLen :: Int
maxConsoleTitleLen = 1023


returnWith_ :: Storable a => (Ptr a -> IO b) -> IO a
returnWith_ act = alloca $ \ptr -> act ptr >> peek ptr

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe mb f = case mb of
    Nothing -> f nullPtr
    Just a  -> with a f


peekCTString :: TChar c t => c -> Ptr t -> IO String
peekCTString t ptr = peekArray0 (toEnum 0) ptr >>= return . fromCTString t

peekCTStringLen :: TChar c t => c -> (Ptr t, DWORD) -> IO String
peekCTStringLen t (ptr, len) = peekArray (fromEnum len) ptr >>= return . fromCTString t

withCTString :: TChar c t => c -> String -> (Ptr t -> IO a) -> IO a
withCTString t s = withArray0 (toEnum 0) $ toCTString t s

withCTStringLen :: TChar c t => c -> String -> ((Ptr t, DWORD) -> IO a) -> IO a
withCTStringLen t s act = let (s', n) = toCTStringLen t s
                          in withArray s' $ \ptr -> act (ptr, toEnum n)


-- Stolen from the Foreign.C.String module because these functions aren't exported, unfortunately

cWcharsToChars :: [CWchar] -> [Char]
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []

charsToCWchars :: [Char] -> [CWchar]
charsToCWchars = foldr utf16Char [] . map ord
 where
  utf16Char c wcs
    | c < 0x10000 = fromIntegral c : wcs
    | otherwise   = let c' = c - 0x10000 in
                    fromIntegral (c' `div` 0x400 + 0xd800) :
                    fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs



-- * Other

{-
void WINAPI SetLastError(
  _In_ DWORD dwErrCode
);
-}
foreign import stdcall "windows.h SetLastError"
    c_setLastError :: DWORD -> IO ()
