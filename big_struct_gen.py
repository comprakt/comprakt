import itertools
import re
import sys

ident = re.compile("^[a-zA-Z_]\w*$")
blacklist = re.compile("abstract|assert|boolean|break|byte|case|catch|char|class|const|continue|default|double|do|else|enum|extends|false|finally|final|float|for|goto|if|implements|import|instanceof|interface|int|long|native|new|null|package|private|protected|public|return|short|static|strictfp|super|switch|synchronized|this|throws|throw|transient|true|try|void|volatile|while")

chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

num_fields = 8193
if len(sys.argv) == 2:
    num_fields = int(sys.argv[1])

def gen_names(n):
    j = 0
    i = 0
    while i < n:
        j += 1
        for x in itertools.product(chars, repeat=j):
            s = "".join(x)
            if ident.match(s) != None and blacklist.match(s) == None:
                i += 1
                yield s
            if i == n:
                return

with open("foo.mj", 'w') as file:
    last = ""
    file.write(
"""class Foo {
    """
    )
    j = 1
    k = -2
    for name in gen_names(num_fields):
        file.write("public A {};".format(name))
        last = name
    file.write(
"""
    public static void main(String[] args) {{
        Foo foo = new Foo();
        foo.{0} = new A();
        foo.{0}.a = 1;
        System.out.println(foo.{0}.a);
    }}
}}

class A {{
    public int a;
}}
""".format(last)
    )
