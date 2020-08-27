package classwork9;

public class main {
	public static void main(String[] args) {
		class Node {
			int data;
			Node next;
		}
		
		public static total() {
			Node n;
			n = header;
			int total = 0; 
			while (n.next != null) {
				n.next;
				total += n.data;
			}
			return total;
		}
	}

}

public static void Switch(DLLNode<T> first) {
	DLLNode<T> second = first.getForward();
	first.setForward(second.getForward());
	second.getForward().setBack(first);
	first.getBackward().setForward(second);
	second.setBack(first.getBackward());
	first.setBack(second);
	second.setForward(first);
	
}