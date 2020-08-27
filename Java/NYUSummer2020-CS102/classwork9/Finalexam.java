package classwork9;

import net.datastructures.Entry;
import net.datastructures.Position;

public class Finalexam {
	
	public class TreeNode<T> {
		   private T value = null;
		   private TreeNode[] children  = new TreeNode[100];
		   private int childCount = 0;
		    TreeNode(T value) {
		        this.value = value;
		    }
		    
		    public T getValue() {return value;}
		    public TreeNode[] getChildren() {return this.children;}
		    public int getChildCount() {return this.childCount;}

		    public TreeNode<T> addChild(T value) {
		        TreeNode<T> newChild = new TreeNode<T>(value);
		        children[childCount++] = newChild;
		        return newChild;
		    }

	
	
}


