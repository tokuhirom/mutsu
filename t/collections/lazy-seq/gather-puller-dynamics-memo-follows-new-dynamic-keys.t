use v6;
use Test;

# The scan that gives a lazily pulled gather its puller's dynamic variables
# (#15112) reads a per-tier memo of dynamic keys (#9170). A dynamic variable
# declared AFTER an earlier force, in a frame that already holds hundreds of
# locals, must still reach the next force; one that is gone must not.

plan 6;

my $lv0 = 0; my $lv1 = 1; my $lv2 = 2; my $lv3 = 3; my $lv4 = 4; my $lv5 = 5; my $lv6 = 6; my $lv7 = 7; my $lv8 = 8; my $lv9 = 9; my $lv10 = 10; my $lv11 = 11; my $lv12 = 12; my $lv13 = 13; my $lv14 = 14; my $lv15 = 15; my $lv16 = 16; my $lv17 = 17; my $lv18 = 18; my $lv19 = 19; my $lv20 = 20; my $lv21 = 21; my $lv22 = 22; my $lv23 = 23; my $lv24 = 24; my $lv25 = 25; my $lv26 = 26; my $lv27 = 27; my $lv28 = 28; my $lv29 = 29; my $lv30 = 30; my $lv31 = 31; my $lv32 = 32; my $lv33 = 33; my $lv34 = 34; my $lv35 = 35; my $lv36 = 36; my $lv37 = 37; my $lv38 = 38; my $lv39 = 39; my $lv40 = 40; my $lv41 = 41; my $lv42 = 42; my $lv43 = 43; my $lv44 = 44; my $lv45 = 45; my $lv46 = 46; my $lv47 = 47; my $lv48 = 48; my $lv49 = 49; my $lv50 = 50; my $lv51 = 51; my $lv52 = 52; my $lv53 = 53; my $lv54 = 54; my $lv55 = 55; my $lv56 = 56; my $lv57 = 57; my $lv58 = 58; my $lv59 = 59; my $lv60 = 60; my $lv61 = 61; my $lv62 = 62; my $lv63 = 63; my $lv64 = 64; my $lv65 = 65; my $lv66 = 66; my $lv67 = 67; my $lv68 = 68; my $lv69 = 69; my $lv70 = 70; my $lv71 = 71; my $lv72 = 72; my $lv73 = 73; my $lv74 = 74; my $lv75 = 75; my $lv76 = 76; my $lv77 = 77; my $lv78 = 78; my $lv79 = 79; my $lv80 = 80; my $lv81 = 81; my $lv82 = 82; my $lv83 = 83; my $lv84 = 84; my $lv85 = 85; my $lv86 = 86; my $lv87 = 87; my $lv88 = 88; my $lv89 = 89; my $lv90 = 90; my $lv91 = 91; my $lv92 = 92; my $lv93 = 93; my $lv94 = 94; my $lv95 = 95; my $lv96 = 96; my $lv97 = 97; my $lv98 = 98; my $lv99 = 99; my $lv100 = 100; my $lv101 = 101; my $lv102 = 102; my $lv103 = 103; my $lv104 = 104; my $lv105 = 105; my $lv106 = 106; my $lv107 = 107; my $lv108 = 108; my $lv109 = 109; my $lv110 = 110; my $lv111 = 111; my $lv112 = 112; my $lv113 = 113; my $lv114 = 114; my $lv115 = 115; my $lv116 = 116; my $lv117 = 117; my $lv118 = 118; my $lv119 = 119; my $lv120 = 120; my $lv121 = 121; my $lv122 = 122; my $lv123 = 123; my $lv124 = 124; my $lv125 = 125; my $lv126 = 126; my $lv127 = 127; my $lv128 = 128; my $lv129 = 129; my $lv130 = 130; my $lv131 = 131; my $lv132 = 132; my $lv133 = 133; my $lv134 = 134; my $lv135 = 135; my $lv136 = 136; my $lv137 = 137; my $lv138 = 138; my $lv139 = 139; my $lv140 = 140; my $lv141 = 141; my $lv142 = 142; my $lv143 = 143; my $lv144 = 144; my $lv145 = 145; my $lv146 = 146; my $lv147 = 147; my $lv148 = 148; my $lv149 = 149; my $lv150 = 150; my $lv151 = 151; my $lv152 = 152; my $lv153 = 153; my $lv154 = 154; my $lv155 = 155; my $lv156 = 156; my $lv157 = 157; my $lv158 = 158; my $lv159 = 159; my $lv160 = 160; my $lv161 = 161; my $lv162 = 162; my $lv163 = 163; my $lv164 = 164; my $lv165 = 165; my $lv166 = 166; my $lv167 = 167; my $lv168 = 168; my $lv169 = 169; my $lv170 = 170; my $lv171 = 171; my $lv172 = 172; my $lv173 = 173; my $lv174 = 174; my $lv175 = 175; my $lv176 = 176; my $lv177 = 177; my $lv178 = 178; my $lv179 = 179; my $lv180 = 180; my $lv181 = 181; my $lv182 = 182; my $lv183 = 183; my $lv184 = 184; my $lv185 = 185; my $lv186 = 186; my $lv187 = 187; my $lv188 = 188; my $lv189 = 189; my $lv190 = 190; my $lv191 = 191; my $lv192 = 192; my $lv193 = 193; my $lv194 = 194; my $lv195 = 195; my $lv196 = 196; my $lv197 = 197; my $lv198 = 198; my $lv199 = 199; my $lv200 = 200; my $lv201 = 201; my $lv202 = 202; my $lv203 = 203; my $lv204 = 204; my $lv205 = 205; my $lv206 = 206; my $lv207 = 207; my $lv208 = 208; my $lv209 = 209; my $lv210 = 210; my $lv211 = 211; my $lv212 = 212; my $lv213 = 213; my $lv214 = 214; my $lv215 = 215; my $lv216 = 216; my $lv217 = 217; my $lv218 = 218; my $lv219 = 219; my $lv220 = 220; my $lv221 = 221; my $lv222 = 222; my $lv223 = 223; my $lv224 = 224; my $lv225 = 225; my $lv226 = 226; my $lv227 = 227; my $lv228 = 228; my $lv229 = 229; my $lv230 = 230; my $lv231 = 231; my $lv232 = 232; my $lv233 = 233; my $lv234 = 234; my $lv235 = 235; my $lv236 = 236; my $lv237 = 237; my $lv238 = 238; my $lv239 = 239; my $lv240 = 240; my $lv241 = 241; my $lv242 = 242; my $lv243 = 243; my $lv244 = 244; my $lv245 = 245; my $lv246 = 246; my $lv247 = 247; my $lv248 = 248; my $lv249 = 249; my $lv250 = 250; my $lv251 = 251; my $lv252 = 252; my $lv253 = 253; my $lv254 = 254; my $lv255 = 255; my $lv256 = 256; my $lv257 = 257; my $lv258 = 258; my $lv259 = 259; my $lv260 = 260; my $lv261 = 261; my $lv262 = 262; my $lv263 = 263; my $lv264 = 264; my $lv265 = 265; my $lv266 = 266; my $lv267 = 267; my $lv268 = 268; my $lv269 = 269; my $lv270 = 270; my $lv271 = 271; my $lv272 = 272; my $lv273 = 273; my $lv274 = 274; my $lv275 = 275; my $lv276 = 276; my $lv277 = 277; my $lv278 = 278; my $lv279 = 279; my $lv280 = 280; my $lv281 = 281; my $lv282 = 282; my $lv283 = 283; my $lv284 = 284; my $lv285 = 285; my $lv286 = 286; my $lv287 = 287; my $lv288 = 288; my $lv289 = 289; my $lv290 = 290; my $lv291 = 291; my $lv292 = 292; my $lv293 = 293; my $lv294 = 294; my $lv295 = 295; my $lv296 = 296; my $lv297 = 297; my $lv298 = 298; my $lv299 = 299;

my @seen;
sub dyn-peek() { @seen.push($*Probe // 'UNDEF'); 1 }
sub mk() { lazy gather { take dyn-peek() } }

# Forced with no dynamic variable declared anywhere yet.
@seen = ();
my $a = mk();
$a[0];
is @seen.join(','), 'UNDEF', 'no dynamic variable declared: the body sees none';

# A dynamic variable that appears later reaches a later force.
@seen = ();
my $b = mk();
{ my $*Probe = 'late'; $b[0] }
is @seen.join(','), 'late', 'a dynamic variable declared after earlier forces is seen';

# ... in a second frame too, with a different value.
@seen = ();
my $c = mk();
{ my $*Probe = 'again'; $c[0] }
is @seen.join(','), 'again', 'the next puller decides';

# Once its frame has left, it is gone again.
@seen = ();
my $d = mk();
$d[0];
is @seen.join(','), 'UNDEF', 'a puller without it sees none';

# A routine frame declaring it, called in a loop (new overlay each call).
sub pull-with($v) { my $*Probe = $v; my $g = mk(); $g[0] }
@seen = ();
pull-with($_) for <x y z>;
is @seen.join(','), 'x,y,z', 'each call pulls under its own value';

# A nested pull: inner frame shadows, outer restored.
@seen = ();
{
    my $*Probe = 'outer';
    my $o = mk();
    { my $*Probe = 'inner'; my $i = mk(); $i[0] }
    $o[0];
}
is @seen.join(','), 'inner,outer', 'nested pullers each see their own';
