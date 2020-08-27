package classwork8;

import net.datastructures.ArrayQueue;
import net.datastructures.LinkedStack;

public class CapitalGains {
	public static void main(String[] args) {
		ArrayQueue<Integer> shares = new ArrayQueue<Integer>(10);
		ArrayQueue<Integer> price= new ArrayQueue<Integer>(10);
		
		System.out.println(fifo(shares, price, "B,30,100"));
		System.out.println(fifo(shares, price, "B,40,110"));
		System.out.println(fifo(shares, price, "S,50,120"));
		System.out.println(fifo(shares, price, "S,20,125"));
		
		
		LinkedStack<Integer> sharesL = new LinkedStack<Integer>();
		LinkedStack<Integer> priceL = new LinkedStack<Integer>();
		System.out.println(lifo(sharesL, priceL, "B,30,100"));
		System.out.println(lifo(sharesL, priceL, "B,40,110"));
		System.out.println(lifo(sharesL, priceL, "S,50,120"));
		System.out.println(lifo(sharesL, priceL, "S,20,125"));
	}
	
	public static int fifo(ArrayQueue<Integer> shares, ArrayQueue<Integer> price, String s) {
		int cost;
		int gains = 0;
		String[] input = s.split(",");
		
		if (input[0].equals("B")) {
			shares.enqueue(Integer.parseInt(input[1]));
			price.enqueue(Integer.parseInt(input[2]));
		}
		
		if (input[0].equals("S")) {
			if (Integer.parseInt(input[1]) > shares.first()) {
				int newFirst = shares.first();
				int difference = Integer.parseInt(input[1]) - newFirst;
				cost = shares.dequeue() * price.dequeue() + difference * price.first();
				gains = Integer.parseInt(input[1]) * Integer.parseInt(input[2]) - cost;
				newFirst = shares.first() - difference; 
				shares.enqueue(newFirst);
				for (int i = 0; i < shares.size()-1; i++) {
					shares.enqueue(shares.dequeue());
				}
			}
			if (Integer.parseInt(input[1]) <= shares.first()) {
				int newFirst = shares.first();
				int difference = newFirst - Integer.parseInt(input[1]);
				cost = Integer.parseInt(input[1]) * price.first();
				gains = Integer.parseInt(input[1]) * Integer.parseInt(input[2]) - cost;
				newFirst = difference;
				
				shares.enqueue(newFirst);
				for (int i = 0; i < shares.size(); i++) {
					shares.enqueue(shares.dequeue());
				}
			}
		}
		return gains;
	}
	
	public static int lifo(LinkedStack<Integer> shares, LinkedStack<Integer> price,String s) {
		int cost;
		int gains = 0;
		String[] input = s.split(",");
		
		if (input[0].equals("B")) {
			shares.push(Integer.parseInt(input[1]));
			price.push(Integer.parseInt(input[2]));
		}
		
		if (input[0].equals("S")) {
			if (Integer.parseInt(input[1]) > shares.top()) {
				int newLast = shares.top();
				int difference = Integer.parseInt(input[1]) - newLast;
				cost = shares.pop() * price.pop() + difference * price.top();
				gains = Integer.parseInt(input[1]) * Integer.parseInt(input[2]) - cost;
				newLast = shares.top() - difference; 
				shares.push(newLast);
				for (int i = 0; i < shares.size(); i++) {
					shares.push(shares.pop());
				}
			}
			if (Integer.parseInt(input[1]) <= shares.top()) {
				int newLast = shares.top();
				int difference = newLast - Integer.parseInt(input[1]);
				cost = Integer.parseInt(input[1]) * price.top();
				gains = Integer.parseInt(input[1]) * Integer.parseInt(input[2]) - cost;
				newLast = difference;
				shares.push(newLast);
				for (int i = 0; i < shares.size(); i++) {
					shares.push(shares.pop());
				}
			}
		}
		return gains;
	}
}

