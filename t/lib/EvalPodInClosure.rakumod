unit class EvalPodInClosure;

method direct($t) { "=begin pod\n\n$t\n\n=end pod\n; \$=pod[0]".EVAL }
method mapped(@x) { @x.map({ "=begin pod\n\n$_\n\n=end pod\n; \$=pod[0]".EVAL }).list }
method via-private(@x) { @x.map({ self!podify($_) }).list }
method !podify($t) { "=begin pod\n\n$t\n\n=end pod\n; \$=pod[0]".EVAL }
