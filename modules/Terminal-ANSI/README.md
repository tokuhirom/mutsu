## Terminal::ANSI

This is a library of ANSI escape sequences.

### Example

    use Terminal::ANSI;

    save-screen;
    clear-screen;
    home;

    hide-cursor;

    sub scroll($row,$col = 1, $height = 7) {
      print-at $row,$col, "━" x 20;
      print-at $row + $height,$col, "━" x 20;
      my $on = $row;
      for 20000, 19999 ... 0 {
        atomically {
          set-scroll-region($row + 1,$row + $height - 1);
          scroll-up if $on >= $row + $height - 1;
          print-at ++$on min $row + $height - 1, $col, "counting down..$_";
        }
      }
    }

    my $p1 = start scroll(2);
    my $p2 = start scroll(12,10,10);
    my $p3 = start scroll(24);
    await Promise.allof($p1,$p2,$p3);
    sleep 1;

    reset-scroll-region;
    show-cursor;
    restore-screen;

See the [eg/](https://git.sr.ht/~bduggan/raku-terminal-ansi/tree/master/eg) directory for more examples.

## Description

The functions in this module print ANSI escape sequences to stdout.
There are functions for cursor movement, scroll regions, colors, and
other screen functionality.  There aren't functions for any of the
esoteric escape codes.

The `atomically` function suppresses printing, and instead concatenates
the output and emits the entire line at the end.  This is helpful for
multi-threaded situations, where the output of several functions needs
to be in one piece.

There is also an OO interface, as well as a virtual screen, to mock
emitting of escape codes (useful for testing).

## Documentation

* This README can be found online here:

  https://man.sr.ht/~bduggan/raku-terminal-ansi/README.md

* [Terminal::ANSI](https://man.sr.ht/~bduggan/raku-terminal-ansi/lib/Terminal/ANSI.md) has documentation for all of the available functions.

* [Terminal::ANSI:OO](https://man.sr.ht/~bduggan/raku-terminal-ansi/lib/Terminal/ANSI/OO.md) has the OO interface.

## Bugs

Feel free to email bug reports to the author, or you can file a ticket at

[https://todo.sr.ht/~bduggan/raku-terminal-ansi](https://todo.sr.ht/~bduggan/raku-terminal-ansi)

## Author

Brian Duggan (bduggan at matatu.org)

