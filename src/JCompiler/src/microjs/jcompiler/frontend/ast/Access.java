package microjs.jcompiler.frontend.ast;

import microjs.jcompiler.middleend.kast.KExpr;
import microjs.jcompiler.middleend.kast.KCall;
import microjs.jcompiler.middleend.kast.KEVar;
import microjs.jcompiler.utils.DotGraph;

import java.util.ArrayList;
import java.util.List;
import java_cup.runtime.ComplexSymbolFactory.Location;

public class Access extends Expr {
	private String name;
	private Expr expr;
	public Access(Expr expr, String name, Location startPos, Location endPos) {
		super(startPos, endPos);
		this.name = "_"+name.toUpperCase(); 
		this.expr = expr;
	}
	@Override
	public KCall expand() {
	List<KExpr> args = new ArrayList<KExpr>();
	args.add(expr.expand());
	return new KCall(new KEVar(name, getStartPos(), getEndPos()), args,getStartPos(), getEndPos());
	}

	@Override
	protected void prettyPrint(StringBuilder buf) {
    	expr.prettyPrint(buf);
    	buf.append(".");
    	buf.append(name.substring(1).toLowerCase());
	}
	
	@Override
    protected String buildDotGraph(DotGraph graph){
    	return null;
    }
}