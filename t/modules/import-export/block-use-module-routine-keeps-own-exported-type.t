use Test;
use lib 't/lib';

# A module routine a block-scoped `use` imported reads its own `Widget`, the
# class the module exports, even when the program declares a `my class` of the
# same name. The program's type-only key is no declaration of the module
# routine's frame (ADR-8751). Checked against rakudo.

plan 6;

my class Widget { method who { "program-widget" } }

{
    use BlockUseExportedWidget;
    my $who = make-widget().who;
    is $who, 'module-widget', "the module routine's own class, called by name";
    my $name = make-widget().^name;
    is $name, 'BlockUseExportedWidget::Widget', '...and its qualified type name';
    my &m = &make-widget;
    my $via-code = m().who;
    is $via-code, 'module-widget', '...and through a code object';
    my $nested;
    for 1 { $nested = make-widget().who }
    is $nested, 'module-widget', '...and from a nested block';
}

is Widget.new.who, 'program-widget', "the program's class is untouched after the block";
sub program-routine { Widget.new.who }
{
    use BlockUseExportedWidget;
    my $own = program-routine();
    is $own, 'program-widget', "the program's own routine still reads the program's class";
}
