# ABOUT

- context-free: all variables are global and have no lifetime
- dynamic weak typing: language operates with types `Bool` and `Int`, and these can be implicitly converted into each other

# BUILD

```
kompile paracl.k  # build parser and interpreter
krun input.paracl # run imterpreter
```

# SNIPPETS

## VARIABLE ASSIGNMENT
```
var = 10;
var = true;
```

## WHILE LOOP
```
// infinite loop
while (1) {
    ...
}

// count to 10
count = 0;
while (count < 10) {
    ++count;
}

// execute exactly once
r = 0;
do {
    r++;
} while (false)
```

## FOR LOOP
```
// empty loop counting to 0
for (a = 10; a > 0; --a) {
}
```

## IF STATEMENT
```
// always do
if (1) {
    ...
}

// another example, evaluated to if (true)
a1 = 3
if (1 && a1 || false + 10) {
    ...
}
```