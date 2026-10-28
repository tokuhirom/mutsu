use Test;

plan 3;

# Net::BGP (ecosystem): `use Net::BGP::Conversions-Pre201812:if( $*PERL.compiler.version < v2018.12);`
# -- whitespace after the `(` of the pragma's `:if(` adverb.
use if;

constant YES = True;
constant NO = False;

use Totally::Nonexistent::A:if( NO );
pass 'leading and trailing space, false condition skips the load';

use Totally::Nonexistent::B:if(  $*RAKU.compiler.version < v1.0 );
pass 'leading space before a comparison';

use Test:if( YES );
pass 'leading space, true condition loads';
