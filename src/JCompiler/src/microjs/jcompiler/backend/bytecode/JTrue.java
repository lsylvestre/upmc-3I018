package microjs.jcompiler.backend.bytecode;

import microjs.jcompiler.backend.Serializer;

public class JTrue extends BCInstr {
	private String ref;
	
	public JTrue(String ref) {
		this.ref = ref;
	}
	
	@Override
	public String getOpcodeName() {
		return "JTRUE";
	}
	
	@Override
	public int getOpcode() {
		return 11;
	}

	@Override
	public void genBytecode(Serializer gen) {
		gen.encode(getOpcode());
		gen.encode(gen.fetchLabel(ref));
	}
	
	@Override
	public int getSize() {
		return 2;
	}

	
	@Override
	public String toString() {
		return "  JTRUE " + ref;
	}
}
