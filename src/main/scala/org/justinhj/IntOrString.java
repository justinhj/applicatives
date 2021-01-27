package org.justinhj;

class IntOrString {
    private IntOrString() {}
    class IsInt extends IntOrString {
        int i;
        IsInt(int i) { this.i = i; }
    }
    class IsString extends IntOrString {
        String s;
        IsString(String s) { this.s = s; }
    }
}
