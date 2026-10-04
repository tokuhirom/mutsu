unit module SubsetHiddenType;
my class Hidden { }
subset HiddenOnly of Any is export where { $_ ~~ Hidden };
sub make-hidden is export { Hidden.new }
class Holder is export { has HiddenOnly $.h = make-hidden(); }
