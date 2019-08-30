package microjs.jcompiler.middleend.kast;

import java.util.List;

import java_cup.runtime.ComplexSymbolFactory.Location;

public class KWhile extends KStatement {
    private KExpr cond;
    private KStatement kbody;
    
    public KWhile(KExpr cond, KStatement kbody, Location startPos, Location endPos) {
    	super(startPos, endPos);		
    	this.cond = cond;
    	this.kbody = kbody;
    }
    
    @Override
    public void accept(KASTVisitor visitor) {
    	visitor.visit(this);
    }

	public KExpr getCond() {
		return cond;
	}
	
	public KStatement getBody() {
		return kbody;
	}
}
