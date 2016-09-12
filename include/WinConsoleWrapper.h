#ifndef WIN_CONSOLE_WRAPPER
#define WIN_CONSOLE_WRAPPER

#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef union _UNICODE_ASCII_CHAR {
    WCHAR UnicodeChar;
    CHAR  AsciiChar;
} UNICODE_ASCII_CHAR;

void HsGetLargestConsoleWindowSize(HANDLE h, COORD* s);
BOOL HsReadConsoleOutputAttribute(HANDLE h, LPWORD a, DWORD l, COORD* p, LPDWORD n);
BOOL HsSetConsoleCursorPosition(HANDLE h, COORD* p);
BOOL HsWriteConsoleOutputAttribute(HANDLE h, const WORD* a, DWORD l, COORD* p, LPDWORD n);
BOOL HsFillConsoleOutputAttribute(HANDLE h, WORD a, DWORD l, COORD* p, LPDWORD n);

BOOL HsFillConsoleOutputCharacterA(HANDLE h, CHAR c, DWORD l, COORD* p, LPDWORD n);
BOOL HsFillConsoleOutputCharacterW(HANDLE h, WCHAR c, DWORD l, COORD* p, LPDWORD n);

BOOL HsWriteConsoleOutputCharacterA(HANDLE h, LPCSTR s, DWORD l, COORD* p, LPDWORD n);
BOOL HsWriteConsoleOutputCharacterW(HANDLE h, LPCWSTR s, DWORD l, COORD* p, LPDWORD n);

BOOL HsReadConsoleOutputCharacterA(HANDLE h, LPSTR s, DWORD l, COORD* p, LPDWORD n);
BOOL HsReadConsoleOutputCharacterW(HANDLE h, LPWSTR s, DWORD l, COORD* p, LPDWORD n);

BOOL HsScrollConsoleScreenBufferA(HANDLE h, SMALL_RECT* s, SMALL_RECT* c, COORD* d, CHAR_INFO* f);
BOOL HsScrollConsoleScreenBufferW(HANDLE h, SMALL_RECT* s, SMALL_RECT* c, COORD* d, CHAR_INFO* f);

BOOL HsSetConsoleScreenBufferSize(HANDLE h, COORD* s);

BOOL HsReadConsoleOutputA(HANDLE h, PCHAR_INFO b, COORD* s, COORD* p, PSMALL_RECT r);
BOOL HsReadConsoleOutputW(HANDLE h, PCHAR_INFO b, COORD* s, COORD* p, PSMALL_RECT r);

BOOL HsWriteConsoleOutputA(HANDLE h, const CHAR_INFO* b, COORD* s, COORD* p, PSMALL_RECT r);
BOOL HsWriteConsoleOutputW(HANDLE h, const CHAR_INFO* b, COORD* s, COORD* p, PSMALL_RECT r);

#ifdef __cplusplus
}
#endif

#endif
