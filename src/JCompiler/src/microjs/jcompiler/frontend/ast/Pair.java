package microjs.jcompiler.frontend.ast;

import java_cup.runtime.ComplexSymbolFactory.Location;

public class Pair extends BinOp {
    public Pair(Expr car, Expr cdr, Location startPos, Location endPos) {
        super("_CONS",car,cdr,startPos,endPos); 
    }
}