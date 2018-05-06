# While language

*Charlie Harding, April 2018*

This project started out based on the While language specified in [Semantics with Applications](https://dl.acm.org/citation.cfm?id=129085), with the addition of procedures, blocks and arrays. It uses variable scoping by means of a list of maps, where each map is from string to integer, so from variable to value. Procedures are dealt with in the same way. Variables are created globally, unless they are defined as `var` at the head of a block. The innermost copy of a variable is always accessed and modified, and there is no access to outer, shadowed copies. Procedures are always created locally, even if this shadows another procedure with the same name. Their scope is given by the define-site not the call-site, so the only local variables accessible are those existing at the point it is defined, plus their arguments.

Any variable can also be used as an array of arbitrary depth, with equal scope to the variable itself. E.g. `{ var xs = 0; xs[1][2][3] := 4; }` is valid. Arrays may share a name with variables, and `x[1]` is distinct from `x[1][y]` for all `y`. It is convention that arrays store their length in the underlying variable, but this is not enforced. It is, however, the location of any automatically-provided arrays, such as varargs and `$`.

Procedures can be defined with the `proc` keyword, followed by an identifier name, argument list, and statement/block. The argument list consists of required arguments, optional arguments (with default values given after a colon), and finally an optional vararg parameter, signified by a postfix at-sign. These are then called with a statement consisting of the method name followed by any comma-separated arguments in brackets. If the procedure accepts a vararg, then any arguments past the final optional argument will be passed in as an array. Alternatively, an array can be passed in, again by using the at-sign syntax. If the at-sign is followed by an expression, the number will represent the length of the array. Otherwise, the array's underlying variable will be used. A shallow copy (1D) of the array will be copied for use in the procedure. Within the procedure, the array's length will be available in the array's underlying variable. As procedures do not have the facility to return yet, convention is to place the return value in a variable with the same name as the procedure.

```
while> proc x (a, b, c: b + 1, arr@) {
 ... >    if arr == 0 then arr[0] = 0 else;
 ... >    x = a * b + c + arr - arr[0];
 ... > }
while> x(1,2);x
5
while> x(1,2,0,1,2,3,4,5);x
6
while> array[0] = 3; array[1] = 2;
while> x(1,2,0,array@2);x
1
while> x(1,2,0,array@1);x
0
while> array=1;
while> # The following two lines are equivalent:
while> x(1,2,0,array@array);x
0
while> x(1,2,0,array@);x
0
```

The REPL can be run by compiling the While.hs file with `ghc -main-is While.main While`, and then running `./While`. This will provide a prompt, `while >`. Type statements, followed by a new-line, and they will be executed, or type an expression to print its value. To type multiple statements, use semicolons as within the source code, but for single simple commands the semicolon can be omitted. An expression can be added to a line after the final semicolon, to execute and immediately print. If you type an incomplete statement, a continuation prompt `>` will be shown, allowing you to continue your command. This involved modifying Yoda to provide the function `parsePartial`, which returns `Nothing` if no string has been matched yet, but with further text a match could be made, or `Just` a list containing any complete matches. `Just []` means that the string is not a valid match or prefix.

The While interpreter can also execute files, e.g. `./While fact.while 5` will print the factorial of 5. These files must be a chain of statements. Input is handled by `$` storing `argc`, and `$` is an array of the arguments themselves, as ints. Output is handled the same way: the first `$` items stored in the array `$` are printed. This means that a blank program will simply echo its arguments.

The language has been extended to support the prefix operator `-`, as well as the binary operators `|`, `!=`, `<`, `>` and `>=`. It also allows any arithmetic operator to be used with the equals sign to produce an update assignment. Blocks are represented with braces, and parentheses have been removed from the statement level, so they only remain at the expression level. Semicolons are now required after every statement, except after a block or a single statement in the middle of a syntactic construct. For example, in the statement `if true then x := 4 else skip;`, it is optional to include a semicolon between the assignment of x and the word `else`. I have also added `repeat`-`while` loops, and allowed `until` everywhere `while` can be used.

The language also allows comments: `#` up to the end of the line.