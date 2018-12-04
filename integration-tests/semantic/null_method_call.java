class DummyClass {
    public void foo() { }
}

class NullMethodCall {
    public static void main(String[] args) {
        int input = System.in.read();
        DummyClass i;
        if (input == 23) {
            i.foo();
        } else {
            i = new DummyClass();
            i.foo();
        }

    }
}
