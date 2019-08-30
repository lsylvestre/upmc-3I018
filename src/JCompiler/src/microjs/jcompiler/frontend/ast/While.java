package microjs.jcompiler.frontend.ast;

import java.util.List;
import java_cup.runtime.ComplexSymbolFactory.Location;
import microjs.jcompiler.middleend.kast.KWhile;
import microjs.jcompiler.middleend.kast.KSeq;
import microjs.jcompiler.middleend.kast.KStatement;
import microjs.jcompiler.utils.DotGraph;

public class While extends Statement {
    private Expr cond;
    private List<Statement> body;

    public While(Expr cond, List<Statement> body, Location startPos, Location endPos) {
    	super(startPos, endPos);		
    	this.cond = cond;
    	this.body = body;
    }
    
    @Override
    public KWhile expand() {
        Location whileStartPos  = getStartPos();
        Location bodyEndPos    = getEndPos();
        List<KStatement> kbody = Statement.expandStatements(body);
        KStatement kbody_s = KSeq.buildKSeq(kbody,whileStartPos, bodyEndPos);
        return new KWhile(cond.expand(), kbody_s, getStartPos(), getEndPos());
    }
    
	@Override
	protected String buildDotGraph(DotGraph graph) {
		/*String WhileNode = graph.addNode("While");
		String condNode = cond.buildDotGraph(graph);
		graph.addEdge(WhileNode, condNode, "cond");
		String bodyNode = body.buildDotGraph(graph);
		graph.addEdge(WhileNode, bodyNode, "body");
	
		return WhileNode;*/
        return null;
	}
    
    @Override
    protected void prettyPrint(StringBuilder buf, int indent_level) {
    	indent(buf, indent_level);
    	buf.append("while (");
    	cond.prettyPrint(buf);
    	buf.append(") {\n");
    	Statement.prettyPrintStatements(buf, body, indent_level + 1);
    	indent(buf, indent_level);
    	buf.append("}");
    }
}
