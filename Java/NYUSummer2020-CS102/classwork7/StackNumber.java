package classwork7;

import net.datastructures.*;

public class StackNumber {
	public static void main(String args[]) {
		
		ArrayStack<Integer> R = new ArrayStack<Integer>();
		ArrayStack<Integer> S = new ArrayStack<Integer>();
		ArrayStack<Integer> T = new ArrayStack<Integer>();
		
		for(int i = 0; i < 10; i++) {
			int randR = (int) (Math.random() * 100);
			R.push(randR);
			int randS = (int) (Math.random() * 100);
			S.push(randS);
			int randT = (int) (Math.random() * 100);
			T.push(randT);
		}


		
		System.out.println("Before: ");
		System.out.println("R: " + R);
		System.out.println("S: " + S);
		System.out.println("T: " + T);
		
		int size = S.size() + T.size();

		while (T.isEmpty() == false) {
			Integer temp = T.pop();
			R.push(temp);
		}

		while (S.isEmpty() == false) {
			Integer temp = S.pop();
			R.push(temp);
		}

		
		for (int i = 0; i < size; i++) {
			Integer temp = R.pop();
			S.push(temp);
		}
		
		
		System.out.println("After: ");
		System.out.println("R: " + R);
		System.out.println("S: " + S);
		System.out.println("T: " + T);
	}

}
