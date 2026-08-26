## Terminal::ANSI

ANSI Escape Sequences

This module exports functions for emitting ANSI escape sequences.

It also maintains some state about the screen, such as a currently active scroll region.

It can optionally maintain the state of all of the characters on the screen, by writing to a virtual screen.

## Exported Functions

[alt-font($n where { ... })](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L255) Alternate font n (between 11 and 19)

[atomically(&callable)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L274) Combine a series of outputs into one.

[blink()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L240) Blink

[bold()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L220) Bold (increased intensity)

[clear-screen()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L49) Clear the screen.

[cursor-down($amt = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L134) Move cursor down.

[cursor-left($amt = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L144) Move cursor left.

[cursor-next-line($n = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L149) Move cursor to the beginning of the next line, n lines down.

[cursor-off()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L119) Turn off cursor (civis).

[cursor-on()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L124) Turn on cursor (cnorm).

[cursor-prev-line($n = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L154) Move cursor to the beginning of the previous line, n lines up.

[cursor-right($amt = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L139) Move cursor right.

[cursor-up($amt = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L129) Move cursor up.

[disable-output()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L294) Suppress all output

[enable-output()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L300) Enable output

[enable-virtual()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L284) Send commands to a virtual screen

[erase-to-end-of-line()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L205) Erase to end of line.

[faint()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L225) Faint (lower intensity)

[hide-cursor()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L99) Hide the cursor.

[home()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L60) Move to home (0,0).

[italic()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L230) italic

[move-to($l, $c = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L55) Move the cursor to line, column.

[normal-video()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L210) Normal video: same as text-reaset

[parse-input($str)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L305) Return a description of the given input sequence

[print-at($r, $c, $str)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L159) Atomic move + print.

[reset-scroll-region()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L72) Reset the scroll region.

[restore-cursor()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L195) restore the cursor position.

[restore-screen()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L114) Restore screen (rmcup).

[reverse-video()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L245) Reverse video

[save-cursor()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L190) Save the cursor position.

[save-screen()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L109) Save screen state (smcup).

[scroll-down($n = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L79) Scroll down by an amount.

[scroll-up($n = 1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L89) Scroll up by an amount.

[set-bg-color($n)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L175) Set bg color to $n.

[set-bg-default()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L185) Set bg color to default

[set-bg-rgb-color($r, $g, $b)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L180) set bg color to $n.

[set-fg-color($n)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L165) set fg color to $n.

[set-fg-rgb-color($r, $g, $b)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L170) set fg color to $n.

[set-scroll-region($top, $bottom)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L65) Set scroll region to top, bottom.

[show-cursor()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L104) Show the cursor.

[start-of-line()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L200) Move to start of line.

[strike()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L250) Crossed out (striked)

[terminal-ansi-throttle($rate = 0.1)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L269) Slow down the output for debugging

[text-reset()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L215) Text reset (same as normal-video)

[tget(&callable)](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L260) Get strings instead of printing them.

[underline()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L235) Underline

[virtual-screen()](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/0.0.23/lib/Terminal/ANSI.rakumod#L289) Get the virtual screen receiving commands
