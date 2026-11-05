# $?LINE in a regex code block is the line it is written on

`$?LINE` inside a regex literal's code block counted from the start of the regex, and inside the code of an anonymous `token`/`regex`/`rule` term died "Variable '$?LINE' is not declared". The fragment parse of a code block now starts from the line its copy came from, and the anonymous term folds `$?LINE` in its code blocks when it is declared.
