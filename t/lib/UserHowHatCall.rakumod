# Companion of t/oo/mop/mop-user-how-hat-call.t (ecosystem: Red's MetamodelX::Red::Model).
class MetamodelX::UserHowHat is Metamodel::ClassHOW {
    method who-am-i($obj) { $obj.defined ?? "instance" !! "type object" }
    multi method kind($obj, :$with where not .defined) { "plain:" ~ ($obj.defined ?? "D" !! "U") }
    multi method kind(Str :$with!, |c) { "str" }
}
my module EXPORTHOW { package DECLARE { constant userhowhat = MetamodelX::UserHowHat } }
