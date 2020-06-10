# mips masc

generate mips from human code

example:

```zig
\\.text
\\j main

fn gcd(a: i32, b: i32) i32 {
    if b == 0 {return a;}
    return gcd(b, a%b);
}

\\main:

var gcd_value = gcd(25, 15);

$v0 = 1;
$a0 = gcd_value;
\\syscall
!clear $call;
```

|
v

```mips
.text                                             # .text
j main                                            # j main
# ====================
# jal call_gcd
# args:
#    $a0: a - i32
#    $a1: b - i32
# return:
#    $v0: i32
# ====================
call_gcd:                                         # fn gcd(a: i32, b: i32) i32{
    # save used s registers to stack
    subiu $sp, $sp, 4                             #     $sp = &$sp[-1]
    sw $ra, 0($sp)                                #     $sp[0] = $ra
                                                  #
    move $t0 $a0                                  #     a = $a0
    move $t1 $a1                                  #     b = $a1
    # body
    bnez $t1, if_end                              #     if b == 0 {
        move $v0 $t0                              #         .      a
        j deinit_gcd                              #         return ^;
    if_end:                                       #     }
    move $a0 $t1                                  #     .               b
    rem $a1, $t0 $t1                              #     |                  a % b
    jal call_gcd                                  #     |           gcd(^, ^^^^^)
    move $v0, $v0                                 #     return$v0 = ^^^^^^^^^^^^^
                                                  #
deinit_gcd:                                       # cleanup:
    # reload used s registers from stack
    lw $ra, 0($sp)                                #     $ra = $sp[0]
    addiu $sp, $sp, 4                             #     $sp = &$sp[1]
jr $ra                                            # }
main:                                             # main:
li $a0 25                                         # .                   25
li $a1 15                                         # |                       15
jal call_gcd                                      # |               gcd(^^, ^^)
move $t0, $v0                                     # var gcd_value = ^^^^^^^^^^^
li $v0 1                                          # $v0 = 1
move $a0 $t0                                      # $a0 = gcd_value
syscall                                           # syscall
```

## syntax

inspired by zig.

[examples](https://github.com/pfgithub/masc/tree/master/src/tests).

statement:

-   `var varname: TYPE = EXPRESSION;` - define variable
-   `variable | register = EXPRESSION;` - set variable. eg `myvar = 3;` or `$v0 = 5;`
-   `save EXPRESSION = EXPRESSION` - save into memory. eg `save myptr.* = 25;`. you may wonder why this is not just `myptr.* = 25;` and the answer to that is there is no
    good reason and that way would be better but isn't supported because only variable | register is supported and I wasn't thinking enough when implementing this.
-   `inline? fn function_name(arg_name: TYPE, ...) { STATEMENT... }` - define ¿inline? function. make sure to jump over functions.
    functions will save any needed things to the stack (and unneeded because they will always save \$ra even if they don't call any functions oops)
-   `loop { STATEMENT ... }` - loop forever. break/continue out of the loop.
-   `if EXPRESSION operator EXPRESSION { STATEMENT... }` - expression: `<=`, `<`, `==`, `!=`, `>`, `>=`.
-   `!clear $ra, $call, ...` - tell the register allocator that these variables cannot be used. `$call` expands into $t0-7, $a0-4, $ra, $v0-1. this way, using a variable across a syscall makes it go into \$s0-7.
-   `\\inline assembly...` - inline assembly currently has no option to use variables. make sure to !clear any used registers after inline assembly. eg: `\syscall`
-   `return EXPRESSION?;` - return a value from a function
-   `break;` - break from a loop
-   `continue;` - continue in a loop

types:

-   `u32`, `i32`, `u8` - unsigned and signed integer types
-   `[*]type` - pointer to array (indexable, math supported)
-   `*type` - pointer to one (not indexable, no math)
-

expression:

-   `variable_name`
-   `@TYPE:data_name` - get something from the .data section with name data_name and type TYPE. example: `@i32:len` or `@[*]i32:x[25]`. masc doesn't help with defining items in the .data section, do it yourself.
-   `$register_name` - use a register
-   `function_name(EXPRESSION, ...)` - call a function
-   `EXPRESSION + - * / ^ % EXPRESSION` - math. eg `1 + 1` or `5 * 3 + 6 % 8`. `^` is binary xor.
-   `EXPRESSION[EXPRESSION]` - index array, eg `somearray[5]`
-   `EXPRESSION.*` - get value of pointer, eg `somepointer.*`
-   `&EXPRESSION` - address of expression, eg `&somepointer[2]`
-   `undefined` - anything. eg `var varname: u32 = undefined;`
-   `25` - any number, eg `-8` or `5325`. no binary literals, hex literals, or floating point numbers supported.

comments:

-   `// asdfnjdksalk` - comment that will not be visible in the output code
-   `/* asdfnjdks */` - comment that will not be visible in the output code
-   `\\# comment that will be visible in the output code` (inline assembly "#")
-   `\\` - newline that will be visible in the output code

there's probably more that I'm missing. look at the examples.

## source code structure

don't look at the source code it's very bad because it constantly does string manipulation instead of being resonable and storing useful data.

## issues

lots. glhf if you want to actually use this.
