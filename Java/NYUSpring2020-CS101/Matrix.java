/***************************************************

  Name: Jihyeon Hyeong
  Date: MAR 22, 2020
  Homework #11-12
  
  Program name:        Matrix
  Program description: Generate a matrix having the dimensions specified by the user.
  						Do matrix operations (Transpose, Sum by Column, Reverse Matrix).
  
****************************************************/

import java.util.Scanner;

public class Matrix {
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		int row;
		int column;

		// Prompt the user for the number of rows

		do {
			System.out.println("How many rows will you have in your matrix?: ");
			row = input.nextInt();

			if (row > 4 || row < 1)		// Validate the input
				System.out.print("Number should be between 1 and 4. ");
		} while (row > 4 || row < 1);


		// Prompt the user for the number of columns
		do {
			System.out.println("How many columns will you have in your matrix?: ");
			column = input.nextInt();

			if (column > 4 || column < 1) 	// Validate the input
				System.out.print("Number should be between 1 and 4. ");
		} while (column > 4 || column < 1);

		// Generate a matrix having the dimensions specified by the user
		int[][] matrix = new int[row][column];

		// Assign integers to each element of the matrix by incrementing by one for each column
		for (int i = 0; i < row; i++) {	
			for (int j = 0; j < column; j++) {
				matrix[i][0] = 10 * i;
				matrix[i][j] += matrix[i][0] + j;  
			}
		}
		
		printMatrix(matrix);
		String menuChoice;

		do {
			System.out.printf("P %-14s - Print the contents of the matrix", "Print matrix");
			System.out.printf("\nR %-14s - Reverse all elements in every row of the matrix", "Reverse rows");
			System.out.printf("\nS %-14s - Calculate the sum of the values in each column", "columnSum");
			System.out.printf("\nT %-14s - Rows become columns (and vice versa)", "transpose");		
			System.out.printf("\nQ %-14s - Exit the program", "Quit");
			System.out.println("\nWhat would you like to do? Enter P, R, S, T, or Q to quit: ");	

			menuChoice = input.next().toUpperCase();

			switch(menuChoice) {
				case "P": printMatrix(matrix);
						break;

				case "R": printMatrix(reverseRows(matrix));
						break;				

				case "S": printMatrix(columnSum(matrix));
						break;				

				case "T": printMatrix(transpose(matrix));
						break;
			} 
	} while (!menuChoice.equals("Q"));
	input.close();	
}

	public static void printMatrix(int[][] matrix) {
		for (int row = 0; row < matrix.length; row++) {
			for (int column = 0; column < matrix[row].length; column++) {
				System.out.printf("%3d", matrix[row][column]);
			}
			System.out.println();
		}
	}

	public static int[][] transpose(int[][] matrix) {
		int[][] transMatrix = new int[matrix[0].length][matrix.length];

		for (int row = 0; row < matrix.length; row++) {
			for (int column = 0; column < matrix[0].length; column++) {
				transMatrix[column][row] = matrix[row][column];
			}
		}

		return transMatrix;
	}

	public static int[][] columnSum(int[][] matrix) {
		int[] columnSum = new int[matrix[0].length];

		for (int column = 0; column < matrix[0].length; column++) {
			int total = 0;
			for (int row = 0; row < matrix.length; row++) {
				total += matrix[row][column]; 
			}
			columnSum[column] = total;
		}

		int[][] columnSum2D = new int[1][columnSum.length];

		for (int column = 0; column < columnSum.length; column++) {
			columnSum2D[0][column] = columnSum[column];
		}

		return columnSum2D;
	}

	public static int[][] reverseRows(int[][] matrix) {
		int[][] reverseMatrix = new int[matrix.length][matrix[0].length];

		for (int row = 0; row < matrix.length; row++) {
			for (int column = 0; column < matrix[0].length; column++) {
				reverseMatrix[row][column] = matrix[row][matrix[0].length - column - 1];
			}
		}

		return reverseMatrix;
	}
}