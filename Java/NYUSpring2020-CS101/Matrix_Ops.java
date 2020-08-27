/***************************************************

  Name: Jihyeon Hyeong
  Date: APRIL 8, 2020
  Homework #17
  
  Program name:        Matrix_Ops with More Utilities
  Program description: Generate a matrix having the dimensions specified by the user.
                        Do matrix operations (Transpose, Sum by Column, Reverse, AddReverse, AddNumber Matrix).
  
****************************************************/

import java.util.Scanner;

public class Matrix_Ops {
    public static void main(String[] args){

        // Define variables for rows and columns selection
        int rows = 0;
        int cols = 0;

        // Create Scanner object
        Scanner input = new Scanner(System.in);

        // TODO: Get rows and columns and validate the inputs
        do {
            System.out.println("How many rows will you have in your matrix?: ");
            rows = input.nextInt();

            if (rows > 4 || rows < 1)     // Validate the input
                System.out.print("Number should be between 1 and 4. ");
        } while (rows > 4 || rows < 1);


        // Prompt the user for the number of columns
        do {
            System.out.println("How many columns will you have in your matrix?: ");
            cols = input.nextInt();

            if (cols > 4 || cols < 1)   // Validate the input
                System.out.print("Number should be between 1 and 4. ");
        } while (cols > 4 || cols < 1);

        // Create a matrix object using the Matrix class 
        Matrix m1 = new Matrix(rows, cols);

        // Print the newly created matrix, m1
        m1.printMatrix();

        // Create matrix objects for the transposed, column summed, 
        // and reversed rows results:
        Matrix tmat, cmat, rmat, arMat, anMat;

        // Display menu, get user's menu selection, and call the 
        // appropriate instance method in the Matrix class
        String userInput = " ";
        do {
            printMenu();
            userInput = input.next().toUpperCase();
            System.out.println();

            switch(userInput) {
            case("T"): 
                tmat = m1.transpose();
                tmat.printMatrix();
                break; 
            case("C"): 
                cmat = m1.columnSum();
                cmat.printMatrix();
                break; 
            case("R"): 
                rmat = m1.reverseRows();
                rmat.printMatrix();
                break; 
            case("P"): 
                m1.printMatrix();
                break;
            case("Q"): 
                System.out.println("\n  Exiting.\n");
                input.close();
                break;
            case("AR"):
                arMat = m1.add(m1.reverseRows());
                arMat.printMatrix();
                break;
            case("AN"):
                System.out.println("Enter a number to add: ");
                int numToAdd = input.nextInt();
                m1.setNum(numToAdd);

                anMat = m1.add();
                anMat.printMatrix();
                break;
            default:
                System.out.println("\n  Error, invalid input.");
                break;
            }
        } while (!userInput.equals("Q"));
    }
    
    public static void printMenu() {
        System.out.println("\n");
        System.out.println("________________________________________________________________________\n");
        System.out.println("  T  transpose   - Rows become colums (and vice versa)");
        System.out.println("  C  columnSum   - Caclulate the sum of the values in each column");
        System.out.println("  R  reverseRows - Reverse all elements in every row of the matrix");
        System.out.println("  P  printMatrix - Print the original matrix");
        System.out.println("  AR addReverse  - Add reverse of matrix to original matrix");
        System.out.println("  AN addNum      - Add a number to each element of the original matrix");
        System.out.println("  Q  quit        - Exit the program");
        System.out.println("________________________________________________________________________\n");

        System.out.print("\n  Enter: T, C, R, P, AR, AN, or Q =>  ");
    }     
}


class Matrix  {
    int[][] mat;  // Declare mat as an instance variable 
    private int numToAdd = 0;  // Decare numToAdd as a private instance variable

    // Constructor that initializes the matrix to 0 1 2, then 10 11 12, then 20 21 22, ...
    Matrix(int rows, int columns) {
        mat = new int [rows][columns];

        // TODO: Initialize mat
        for (int i = 0; i < rows; i++) { 
            for (int j = 0; j < columns; j++) {
                mat[i][0] = 10 * i;
                mat[i][j] += mat[i][0] + j;  
            }
        }        

    }

    // Constructor that initializes the entire matrix to the value passed in.
    Matrix(int rows, int columns, int value) {
        mat = new int [rows][columns];

        // TODO: Initialize mat with value
        for (int i = 0; i < rows; i++) { 
            for (int j = 0; j < columns; j++) {
                mat[i][j] = value;  
            }
        }        
        
    }

    // Setter to set the input number to be added to original matrix
    public void setNum(int numToAdd) {
        this.numToAdd = numToAdd;
    }


    void printMatrix() {
        // TOOD: Print out matrix mat
        for (int rows = 0; rows < mat.length; rows++) {
            for (int columns = 0; columns < mat[rows].length; columns++) {
                System.out.printf("%3d", mat[rows][columns]);
            }
            System.out.println();
        }
    }


    Matrix transpose() {
        // Create the matrix, mtran, that will hold the results
        Matrix mtran = new Matrix(mat[0].length, mat.length);

        // TODO: Transpose matrix mat into matrix mtran and return mtran
        for (int rows = 0; rows < mat.length; rows++) {
            for (int columns = 0; columns < mat[0].length; columns++) {
                mtran.mat[columns][rows] = mat[rows][columns];
            }
        }        
        return mtran;
    }

    Matrix columnSum() {
        // Create the matrix, mcolsum, that will hold the results-this matrix has 1 row
        // Initialize each element of mcolsum to 0 using the appropriate constructor
        Matrix mcolsum = new Matrix(1, mat[0].length, 0);

        // TODO: Sum the columns of matrix mat into matrix mcolsum and return mcolsum
        for (int columns = 0; columns < mat[0].length; columns++) {
            for (int rows = 0; rows < mat.length; rows++) {
                mcolsum.mat[0][columns] += mat[rows][columns]; 
            }
        }

        return mcolsum;
    }

    Matrix reverseRows() {
        // Create the matrix, mrev, that will hold the results
        Matrix mrev = new Matrix(mat.length, mat[0].length);

        // TODO: Reverse the rows of matrix mat into matrix mrev and return mrev
        for (int rows = 0; rows < mat.length; rows++) {
            for (int columns = 0; columns < mat[0].length; columns++) {
                mrev.mat[rows][columns] = mat[rows][mat[0].length - columns - 1];
            }
        }

        return mrev;
    }

    Matrix add(Matrix theMat) {
        Matrix sum = new Matrix(mat.length, mat[0].length);
        // TODO: New code to add two matrices goes here
        for (int rows = 0; rows < mat.length; rows++) {
            for (int columns = 0; columns < mat[0].length; columns++) {
                sum.mat[rows][columns] = mat[rows][columns] + theMat.mat[rows][columns];
            }
        }

        return sum;
    }

    Matrix add() {
        Matrix inc = new Matrix(mat.length, mat[0].length);

        // TODO: New code to add two matrices goes here
        for (int rows = 0; rows < mat.length; rows++) {
            for (int columns = 0; columns < mat[0].length; columns++) {
                inc.mat[rows][columns] = mat[rows][columns] + numToAdd;
            }
        }

        return inc;
    }
}
