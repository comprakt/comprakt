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
MiniJava is turing complete, since it provides both conditional expressions (*IfStatement*),
as well as an iteration construct (*WhileStatement*). Unlimited memory is achieved via the
32-bit `int` type as a local variable and using recursion (*Method* and *MethodInvocation*)
to "allocate" more memory. Since there is no mechanism for getting the address of a variable
(like e.g. `&varname` in C), the size of the Stack is not bounded, and can grow arbitrarily
(at least in theory).

(3) Subset of Java
--------------
Yes, because the ignored characters ("Leerraum"), the Comment-patterns and the keywords
(<=SE8, see note below) are a subset of those of Java; because the grammar yields a sublanguage
of that of Java; and for the semantics only additional constraints are given to the semantics
of Java, so any valid MiniJava program is a valid Java (<=SE8) program.

However, since Java release 9 [1], the underscore `_` is a reserved keyword, which it is not
in MiniJava, so that MiniJava is no longer a subset of Java.

(4) Word Problems
-------------
- Program 1: Yes
- Program 2: No, because accessing the argument to *MainMethod* is forbidden
- Program 3: No, because only a single `public static void` method is allowed, the *MainMethod*,
  which is required to have a single argument `String[] IDENT`.


[1]: https://docs.oracle.com/javase/specs/jls/se10/html/jls-3.html#jls-3.9
