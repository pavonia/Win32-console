# Win32-console

## Compilation Notes

Make sure to pass `--hsc2hs-options="-D_WIN32_WINNT=XXX"` to `cabal configure`, where `XXX` is a value denoting your Windows version (see [here](https://msdn.microsoft.com/de-de/library/windows/desktop/aa383745%28v=vs.85%29.aspx) for the list of possible values).
