Task 1
======
We use Rust. For details on the infrastructure, refer to the README.


Task 2
======
Done.


Task 3
======

(1) MiniJava Program
----------------
```java
class Foo {
    public Foo[] bar;

    public static void main(String[] args) {
        System.out.println(42);
    }
}
```

(2) On the Turing-Completeness of MiniJava
--------------------------------------

MiniJava is Turing-complete because it can simulate a universal Turing machine
using the following gadgets:

**Infinite Tape:**

Unlimited memory is achieved via the 32-bit `int` type as a local variable and
using recursion (*Method* and *MethodInvocation*) to "allocate" more memory on
the stack. The size of the stack (and thus the available memory) is not bounded
(at least in theory), since there are no features that allow bounded
enumeration of the available memory locations, such as storing the address of a
variable in a fixed-length integer (like `&varname` in C, which is `<=
2^(8*sizeof(void*))`, a static number).

Alternatively, a infinite tape can be implemented using a vector class backed
by a heap allocated array.

**head, state register, final states/accepting states and transfer function:**

MiniJava provides both conditional expressions (*IfStatement*), as well as an
iteration construct (*WhileStatement*) that can be used to implement the logic
of the turing machine. All other data structures can easily be implemented using
variables and arrays without special logic.

(3) Subset of Java
--------------

Yes (of Java <= SE8), because the ignored characters ("Leerraum"), the Comment-patterns and the keywords
(at least for Java <= SE8, see note below) are a subset of those of Java; because the grammar yields a sublanguage
of that of Java; and for the semantics only additional constraints are given to the semantics
of Java, so any valid MiniJava program is a valid Java (<= SE8) program.

However, since Java SE9, the underscore `_` [1] is a reserved keyword and
cannot be used as an identifier:

> The underscore may be used in identifiers formed of two or more characters,
> but it cannot be used as a one-character identifier due to being a keyword.
> - Java SE11 Specification, 3.8 Identifiers [2] 


Therefore, the following program is a valid MiniJava Program, but not a valid
Java program as of Java SE9.

```java
class _ {
   public static void main(String[] args) {
   }
}
```

This is why MiniJava is not a subset of Java SE9 and later.


(4) Word Problems
-------------
- Program 1: Yes
- Program 2: No, because accessing the argument to *MainMethod* is forbidden
- Program 3: No, because only a single `public static void` method is allowed, the *MainMethod*,
  which is required to have a single argument `String[] IDENT`.


[1]: https://docs.oracle.com/javase/specs/jls/se9/html/jls-3.html#jls-3.9
[2]: https://docs.oracle.com/javase/specs/jls/se11/html/jls-3.html#jls-3.8
