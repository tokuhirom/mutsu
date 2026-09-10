unit module EvalContextWidget;

# A package-qualified reference to this sub (`EvalContextWidget::make`) is the
# thing #7836 is about: it is in scope for a compunit that `use`d this module,
# and must stay in scope for a string EVAL'd on that compunit's behalf.
our sub make($n) is export {
    die "bad widget" if $n < 0;
    $n
}

our constant WIDGET-LIMIT = 7;
