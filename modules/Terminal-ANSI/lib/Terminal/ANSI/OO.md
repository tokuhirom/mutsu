## NAME

Terminal::ANSI::OO -- Object-oriented interface to Terminal::ANSI


## SYNOPSIS

    use Terminal::ANSI::OO :t;

    put t.clear-screen;
    put t.red ~ "red" ~ t.text-reset;
    put t.bright-blue ~ "bright blue" ~ t.text-reset;
    put t.color(1) ~ 'color 1';
    put t.color(0,0,255) ~ 'blue';
    put t.bg-color(255,0,0) ~ 'red bg';


## DESCRIPTION

Terminal::ANSI::OO provides an OO interface to Terminal::ANSI. An object can
return all of the escape codes, rather then printing them to stdout. Passing
":t" will create and store such an object in the "t" variable.

All of the functions in Terminal::ANSI are available as methods, as well as
t.$color, t.bright-$color, and t.bg-$color, where $color is one of: black, red,
green, yellow, blue, magenta, cyan, or white.

Also t.color(N) and t.bg-color(N) (where N is between 0 and 255), will return
the escape code for changing to color N.

Also t.color(R,G,B) and t.bg-color(R,G,B) (where R,G,B are between 0 and 255),
will return the escape code for changing to rgb color R,G,B.


