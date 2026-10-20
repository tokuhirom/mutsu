use Test;
use lib $*PROGRAM.parent(2).add('lib-prefix-class-parent');

# From the CSS::Module distribution (CSS::Module::CSS3::Fonts::AtFontFace):
# `also is Pfx::Sub::Gram` in a class whose method body does `use Pfx` (a class
# that is the head of the parent's qualified name) resolved the parent to `Any`.
plan 4;

use Holder;
is Holder.^mro.map(*.^name).head(2).join(' '), 'Holder Pfx::Sub::Gram', 'parent is the grammar';
ok Holder.^mro.map(*.^name).grep('Grammar'), 'Grammar is inherited through it';
is Holder.m, 'hi', 'the method body use works';
ok Holder.parse('abc', :rule<word>), 'inherited token is callable';
