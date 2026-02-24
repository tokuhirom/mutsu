use Test;

plan 4;

is-deeply "€Šž".encode("windows-1252").list, Buf.new(0x80, 0x8A, 0x9E).list,
    "encode windows-1252";
is Buf.new(0x80, 0x8A, 0x9E).decode("windows-1252"), "€Šž",
    "decode windows-1252";

is-deeply "Привет".encode("windows-1251").list,
    Buf.new(0xCF, 0xF0, 0xE8, 0xE2, 0xE5, 0xF2).list, "encode windows-1251";
is Buf.new(0xCF, 0xF0, 0xE8, 0xE2, 0xE5, 0xF2).decode("windows-1251"), "Привет",
    "decode windows-1251";
