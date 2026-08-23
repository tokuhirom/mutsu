# Win32 IO paths preserve caller separators at boundaries

Win32 path operations now use the Win32 separator for UNC-root parts and
preserve a caller's trailing separator when rendering path values, including
drive roots such as `C:/`.
