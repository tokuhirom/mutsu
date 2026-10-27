use PrivDelegate::Ast;
unit class PrivDelegate::Acts;
method build handles<node> { PrivDelegate::Ast }
method universal($m) { $.build.node($m) }
