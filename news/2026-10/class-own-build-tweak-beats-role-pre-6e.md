# A class's own BUILD/TWEAK replaces the role's before 6.e

The construction plan treated every language revision other than `c` as 6.e, so under the default 6.d a role's `BUILD`/`TWEAK` still ran next to the class's own same-named submethod. Rakudo runs only the class's before 6.e. The revision test is now "6.e or later", for both the class and the role. This was the reason every Testo test got an empty description (its role `TWEAK` ran after the class's and reset `$!desc`).
