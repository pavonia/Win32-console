#include "WinConsoleWrapper.h"

#ifdef __cplusplus
extern "C" {
#endif

BOOL HsReadConsoleOutputAttribute(HANDLE h, LPWORD a, DWORD l, COORD* p, LPDWORD n) {
    return ReadConsoleOutputAttribute(h, a, l, *p, n);
}

BOOL HsWriteConsoleOutputAttribute(HANDLE h, const WORD* a, DWORD l, COORD* p, LPDWORD n) {
    return WriteConsoleOutputAttribute(h, a, l, *p, n);
}

void HsGetLargestConsoleWindowSize(HANDLE h, COORD* s) {
    *s = GetLargestConsoleWindowSize(h);
}

BOOL HsSetConsoleCursorPosition(HANDLE h, COORD* p) {
    return SetConsoleCursorPosition(h, *p);
}

BOOL HsFillConsoleOutputAttribute(HANDLE h, WORD a, DWORD l, COORD* p, LPDWORD n) {
    return FillConsoleOutputAttribute(h, a, l, *p, n);
}


BOOL HsFillConsoleOutputCharacterA(HANDLE h, CHAR c, DWORD l, COORD* p, LPDWORD n) {
    return FillConsoleOutputCharacterA(h, c, l, *p, n);
}

BOOL HsFillConsoleOutputCharacterW(HANDLE h, WCHAR c, DWORD l, COORD* p, LPDWORD n) {
    return FillConsoleOutputCharacterW(h, c, l, *p, n);
}


BOOL HsWriteConsoleOutputCharacterA(HANDLE h, LPCSTR s, DWORD l, COORD* p, LPDWORD n) {
    return WriteConsoleOutputCharacterA(h, s, l, *p, n);
}

BOOL HsWriteConsoleOutputCharacterW(HANDLE h, LPCWSTR s, DWORD l, COORD* p, LPDWORD n) {
    return WriteConsoleOutputCharacterW(h, s, l, *p, n);
}


BOOL HsReadConsoleOutputCharacterA(HANDLE h, LPSTR s, DWORD l, COORD* p, LPDWORD n) {
    return ReadConsoleOutputCharacterA(h, s, l, *p, n);
}

BOOL HsReadConsoleOutputCharacterW(HANDLE h, LPWSTR s, DWORD l, COORD* p, LPDWORD n) {
    return ReadConsoleOutputCharacterW(h, s, l, *p, n);
}

BOOL HsScrollConsoleScreenBufferA(HANDLE h, SMALL_RECT* s, SMALL_RECT* c, COORD* d, CHAR_INFO* f) {
    return ScrollConsoleScreenBufferA(h, s, c, *d, f);
}

BOOL HsScrollConsoleScreenBufferW(HANDLE h, SMALL_RECT* s, SMALL_RECT* c, COORD* d, CHAR_INFO* f) {
    return ScrollConsoleScreenBufferW(h, s, c, *d, f);
}


BOOL HsSetConsoleScreenBufferSize(HANDLE h, COORD* s) {
    return SetConsoleScreenBufferSize(h, *s);
}

BOOL HsReadConsoleOutputA(HANDLE h, PCHAR_INFO b, COORD* s, COORD* p, PSMALL_RECT r) {
    return ReadConsoleOutputA(h, b, *s, *p, r);
}
BOOL HsReadConsoleOutputW(HANDLE h, PCHAR_INFO b, COORD* s, COORD* p, PSMALL_RECT r) {
    return ReadConsoleOutputW(h, b, *s, *p, r);
}

BOOL HsWriteConsoleOutputA(HANDLE h, const CHAR_INFO* b, COORD* s, COORD* p, PSMALL_RECT r) {
    return WriteConsoleOutputA(h, b, *s, *p, r);
}
BOOL HsWriteConsoleOutputW(HANDLE h, const CHAR_INFO* b, COORD* s, COORD* p, PSMALL_RECT r) {
    return WriteConsoleOutputW(h, b, *s, *p, r);
}

#ifdef __cplusplus
}
#endif
