class NullArrayAccess {
    public static void main(String[] args) {
        int input = System.in.read();
        int[] i = null;
        if (input == 23) {
            System.out.println(i[23]);
        } else {
            i = new int[24];
            System.out.println(i[24]);
        }
    }
}
