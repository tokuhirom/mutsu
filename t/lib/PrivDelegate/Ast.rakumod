unit class PrivDelegate::Ast;
method !terms($m) { (a => $m,) }
method node($m) { self!terms($m).Hash }
