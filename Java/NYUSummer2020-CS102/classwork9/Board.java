package classwork9;

import net.datastructures.*;
import java.util.Random;

public class Board {
	public static void main(String[] args) { 
		Random rand = new Random();
		
		for (int i = 0; i <= 100; i++) {
			rand.setSeed(i);
		    PositionalList<Integer> board = new LinkedPositionalList<>();
		    
		    for (int j = 0; j < 50; i++) {
		    	int c = (int)(Math.random() * 3) + 1;
		        board.addLast(c);
		    }
		     

			
		}

	     
	}
	    	
	public static int rollDice() {
		 int dice = (int)(Math.random() * 6) + 1;
	     return dice;
	}
	

	public static void playGame(LinkedPositionalList<Integer> board, Position<Integer> p1, Position<Integer> p2, int score1, int score2) {
		Position<Integer> p1 = board.first();
		Position<Integer> p2 = board.first();
		
		int cumDice1 = 0;
		int cumDice2 = 0;
		
		do {
			int dice = rollDice();
			for (int i = 0; i < dice; i++) {
				p1 = board.after(marker);
			}
			
			if (marker ) {
				score += marker.getElement();
			}
		} while (cumDice1 <= 50);

	 	
		if (marker == null) {
			marker = list.first();
			for (int i = 0; i < dice - 1;  i++) {
				marker = data.after(marker);
			}
		} else {
			for (int j = 0; j < dice; j ++) {
				marker = data.after(marker);
			}
		}
		
		if (marker == others) {
			score -= marker.getElement();
		} else
			score += marker.getElement();
	}
	
}
	  




