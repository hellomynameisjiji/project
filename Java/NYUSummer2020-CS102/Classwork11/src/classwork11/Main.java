// Team members: Alan Shi, Alex Brzozowski
package classwork11;
import java.util.Random;
import java.util.*;

public class Main {
	public static void main(String[] args) { 
		// part a
		Random rand = new Random();
		int sum = 0;
		
		for (int i = 0; i < 10; i++) {
			TreeMap<String, Integer>  t = new TreeMap();
			rand.setSeed(System.currentTimeMillis());
			do {
				int k = rand.nextInt(26)+65;
				char key = (char) k;
				String s=String.valueOf(key);
				t.put(s,k-64);

			} while (t.size() != 26);
			
			sum += t.getHeight(t.root());
		}
		
		System.out.println("Average height: " + (sum / 10));
		
//		System.out.println(t.put1("s", 3));
		
		// part b
		
		rand.setSeed(System.currentTimeMillis());
		TreeMap<String, Integer>  t = new TreeMap();
		do {
			int k = rand.nextInt(26)+65;
			char key = (char) k;
			String s=String.valueOf(key);
			t.put(s,k-64);

		} while (t.size() != 26);

		int count = 0;
		int k = 65;
		do {
			char key = (char) k;
			String s=String.valueOf(key);
			System.out.println(s + ": " + t.searchDepth(t.root(), s, 0));
			k++;
		} while (++count != 26);
	}

}
