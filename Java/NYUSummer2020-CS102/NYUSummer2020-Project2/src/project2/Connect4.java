package project2;

import java.util.Scanner;

public class Connect4 {
	static final int R = 1, B = 2;
	static final int NUM_COLUMNS = 4;
	static final int NUM_IN_ROW = 4;
	static Scanner input = new Scanner(System.in);
	static int firstplayer;
	static int p1 = 0, p2 = 0, ties = 0;
	static long cnt = 0;

	public static void main(String[] args) {
/**
		for (int i=0; i<3; i++) {
			int[ ][ ] list = new int[NUM_COLUMNS][NUM_COLUMNS]; // Create a board
			firstplayer = R;
			p1=0;p2=0;cnt=0;ties=0;
		    switch(i) {
		    case 0: list[2][0]=R; break; //  first column
		    case 1: list[2][1]=R; break; //  second column
		    case 2: list[2][2]=R; break; //  third column  
		    }
		   

			Play(list , B);
			System.out.println ("NetWins for column " + i + ": " + (p1-p2));
			System.out.println ("Number of recursion calls: " + cnt);
			System.out.println ("Red Wins: " + p1 + " Blue Wins: "+ p2);
			
		}
		*/
		for (int i=0; i<4; i++) {
			int[ ][ ] list = new int[NUM_COLUMNS][NUM_COLUMNS]; // Create a board
			firstplayer = R;
			p1=0;p2=0;cnt=0;ties=0;
		    switch(i) {
		    case 0: list[3][0]=R; break; //  first column
		    case 1: list[3][1]=R; break; //  second column
		    case 2: list[3][2]=R; break; //  third column  
		    case 3: list[3][3]=R; break; //  third column  
		    }
		   

			Play(list , B);
			System.out.println ("COLUMN " + (i+1));
			System.out.println ("P1 Net Wins: " + (p1 - p2));
			System.out.println ("BLUE Wins: " + p1 + " RED Wins: " + p2);
			System.out.println ("Total Recursive Calls: " + cnt);	
		}
		
	}
	
	public static int Play(int[][] inlist, int clr) { // inlist: current board state, clr: next player
		cnt++;
		int res=checkBoard(inlist,clr);
		// 0 - board full, 1- X wins  2 = O wins   3-keep playing
		if (res < 3) { 
			if (res == 0) {ties++;return 0;
			} else {
				if (res == firstplayer) {p1++; return 1;} else {p2++; return -1;}
			}
		}	
		res = 0;

		// for each space that can be the next move
		//    make a copy of board (next lines)	

		//   update the board for this move

			for (int col = 0; col < NUM_COLUMNS; col++  ){
				for (int row = NUM_COLUMNS - 1; row >= 0; row--  ){
				if (inlist[row][col] == 0) { // If not a complete game, clone the current board array
					if (isValid(inlist, row, col)) {
						int[][] clonelist = new int[NUM_COLUMNS][NUM_COLUMNS];
						for (int x = 0;x <NUM_COLUMNS;x++  ){
							for (int y = 0; y<NUM_COLUMNS;y++  ){
								clonelist[x][y] = inlist[x][y] ;
							}
						}
						 
						clonelist[row][col] = clr;		// the next player played
						Play(clonelist, 3- clr); // Recursive call with all possible moves for the curr player
					}
				}
			}
		}
		//  recursively call Play
		return res;
	}
	public static boolean isFull(int[][] inlist){
		boolean empty = true;
		for (int i = 0 ; i<NUM_COLUMNS ; i++ ) {
			for (int i2 = 0 ; i2<NUM_COLUMNS ; i2++ ) {
				if (inlist[i][i2] ==0   ) { empty = false; break;} 
			}
		}
		return empty;
	}
	public static boolean isValid(int[][] inlist, int row, int col) {
		for (int i = row + 1; i < NUM_COLUMNS; i++ ) {
			if (inlist[i][col] == 0) {return false; } 
		}
		return true;
	}
	public static int checkBoard(int[][] inlist ,int clr){
		int chkclr = 3-clr;
		for (int i = 0 ; i<NUM_COLUMNS; i++ ) {
			int colcnt = 0;
			for (int j=0; j<NUM_COLUMNS; j++) {
				if (inlist[i][j] == chkclr) {
					colcnt++;
					if (colcnt == NUM_IN_ROW)  { return chkclr;}	 
				}  else {
					colcnt =0;
				}
			}
		}
		for (int i = 0 ; i<NUM_COLUMNS; i++ ) {
			int colcnt = 0;
			for (int j=0; j<NUM_COLUMNS; j++) {
				if (inlist[j][i] == chkclr) {
					colcnt++;
					if (colcnt == NUM_IN_ROW)  { return chkclr;}	 
				}  else {
					colcnt =0;
				}
			}
		}
		int colcnt = 0;
		for (int i = 0 ; i<NUM_COLUMNS; i++ ) {
			if (inlist[i][i] == chkclr) {
				colcnt++;
				if (colcnt == NUM_IN_ROW)  {return chkclr;}	 
			}  else {
				colcnt =0;
			}
		}
		colcnt = 0;
		for (int i = 0 ; i<NUM_COLUMNS; i++ ) {
			if (inlist[NUM_COLUMNS-1-i][i] == chkclr) {
				colcnt++;
				if (colcnt == NUM_IN_ROW)  { return chkclr;}	 
			}  else {
				colcnt =0;
			}
		}
		if (isFull(inlist)) {  return 0; 
		} else {
			return 3;
		}


	}
	public static void printlist(int[][] inlist) {
		for (int i =0; i<inlist.length; i++) {
			for (int j =0; j<inlist.length; j++) {
				System.out.print(inlist[i][j]+" ");
			}
			System.out.println();
		}
		System.out.println();
	}

}
