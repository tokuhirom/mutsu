unit module RakuASTRegexSourceFixture;

grammar Cached {
    token TOP { 'a' | "b" }
}

our sub anonymous() { token { 'a' | "b" } }
