/**
## syntax
```BCNF
pattern := variable | string | number | '[' ((pattern ',')* (pattern | '...' variable))? ']' | '{' evaluation '}'
evaluation := evaluation bi-op evaluation | uni-op evaluation |  '(' evaluation ')' | pattern
term
    := unit
    | pattern
    | term '&' term
    | term '|' term
    | '~' term
    | '(' term ')'
decl := pattern '=>' term
decls = (decl ';')*
```

## example
code:
```patQL
["fruits", "apple", 3] => unit;
["fruits", "banana", 4] => unit;

?main => ["fruits", ?kind, ?amount] & ["greater than", ?amount, 3];
```
result:
```patQL
["fruits", "banana", 4]
```
*/
pub mod grammar;
