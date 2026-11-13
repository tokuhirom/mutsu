use Test;
use lib 't/lib';
use LexicalTypeOptionalDefault;

# An omitted optional parameter typed by a lexical (`my`) class binds that
# class's type object, so calling `.new` on the default works -- also when
# the class has a qualified name and is declared after the routine, and when
# the routine lives in a module (Pod::To::HTML's `TOC::Calculator :$toc`).

plan 3;

my class TOC::Calc {...}
class R {
    method render(TOC::Calc :$toc) { $toc.new(:x(1)).go }
    method name(TOC::Calc :$toc) { $toc.^name }
}
my class TOC::Calc {
    has $.x;
    method go { "go " ~ $!x }
}

is R.render, 'go 1', 'default of a lexical qualified-name class can be instantiated';
is R.name, 'TOC::Calc', 'the default is the class type object';
is LexicalTypeOptionalDefault.new.render, 'go 1', 'works for a routine of a module';
