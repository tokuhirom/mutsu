unit class ParamTypeProvider;

has $.label;

our subset ParamTypeSmall is export of Int where * < 10;
our enum ParamTypeColour is export <ParamRed ParamGreen>;
