/***************************************************

  Name: Jihyeon Hyeong
  Date: MAR 06, 2020
  Homework #8
  
  Program name:        HexUtility
  Program description: Accepts hexadecimal numbers as input.
                       Valid input examples: F00D, 000a, 1010, FFFF, bye, BYE
                       Enter BYE (case insensitive) to exit the program.
  
****************************************************/

import java.util.Scanner;

public class HexUtility {

  public static void main(String[] args) {

    // Maximum length of input string
    final byte INPUT_LENGTH = 4;

    String userInput = "";                   // Initialize to null string
    Scanner input = new Scanner(System.in);

    // Process the inputs until BYE is entered
    do {
      // Input a 4 digit hex number
      System.out.print("\nEnter a hexadecimal string, or enter BYE to quit:  ");
      userInput = input.next().toUpperCase();		// Turn it to uppoercase

      // Process the input
      switch (userInput) {

        case "BYE": System.out.printf("\nGoodbye!");		
                    //if the string typed was “BYE” (not case sensitive) - exit the program.
            
        default:  if (userInput.length() != INPUT_LENGTH) {		
                    // Input length is incorrect
                    System.out.printf("      The input %s is the wrong length, it should be %d characters long.\n", userInput, INPUT_LENGTH);
                    break;
                  }

                  // Input length is correct (test whether it's valid)
                  if (isValidHex(userInput)) {  //Call method isValidHex
                    // The input is a valid hexadecimal string

                    long decVal = hex2Dec(userInput, INPUT_LENGTH);
                    String binVal = hex2Bin(userInput, INPUT_LENGTH); // Add this call for step b): hex2Bin(userInput, INPUT_LENGTH)
                    System.out.printf("      0x%s = %s in decimal and %s in binary. \n", userInput, decVal, binVal);
                  }

                  else {
                    // String is either the wrong length or is not a valid hexadecimal string
                    System.out.printf("      The input %s is not a valid hexadecimal value.\n", userInput);
                  }
                  break;
        }
    } while (!userInput.equals("BYE"));

    input.close();
  }
 
  // Method to validate the input.
  // This method returns true or false to the caller (main).
  // This method accepts one parameter - the user input.
  public static boolean isValidHex(String userIn) {
    boolean isValid = false;
        
    // The length is correct, now check that all characters are legal hexadecimal digits
    for (int i = 0; i < 4; i++) {
      char thisChar = userIn.charAt(i);

      // Is the character a decimal digit (0..9)? If so, advance to the next character
      if (Character.isDigit(thisChar)) {
        isValid = true;
      }

      else {
        // Character is not a decimal digit (0..9), is it a valid hexadecimal digit (A..F)?
        if ('A' <= thisChar && thisChar <= 'F') {
          isValid = true;
        }
        else {
          // Found an invalid digit, no need to check other digits, exit this loop
          isValid = false;
          
        }
      }
    }
        
    // Returns true if the string is a valid hexadecimal string, false otherwise
    return isValid;
  }

  // Method to conver the hex number to decimal
  public static long hex2Dec(String hexString, byte inputLen) {
    // Convert hexadecimal string to decimal, e.g. F00D = 61453 in decimal
  	long decVal = 0;

    for (int i = 0; i < inputLen; i++) {      
      char thisChar = hexString.charAt(i);
      int thisCharInt;

      // Is the character a decimal digit (0..9)? If so, then '-48' since ASCII code for '0' is 48 
        if (Character.isDigit(thisChar)) {
          thisCharInt = (int) thisChar - 48;
        }

      else {
        // Character is not a decimal digit (0..9), then '-65' since ASCII code for 'A' is 65
          thisCharInt = (int) thisChar - 55;
        }

        decVal += thisCharInt * ( Math.pow(16 , inputLen - 1 - i ) );
    }

      return decVal;

  }

  public static String hex2Bin(String hexString, byte inputLen) {
    // Initialize binString to empty string
    String binString = ""; 

    String[] binValue = {"0000", "0001", "0010", "0011",
                         "0100", "0101", "0110", "0111",
                         "1000", "1001", "1010", "1011",
                         "1100", "1101", "1110", "1111"};

    // Convert hexString to a binary string
    for (int i = 0; i < inputLen; i++) {      
      char thisChar = hexString.charAt(i);
      int thisCharInt;

    // Is the character a decimal digit (0..9)? If so, then '-48' since ASCII code for '0' is 48 
      if (Character.isDigit(thisChar)) {
          thisCharInt = (int) thisChar - 48;
        }
 
      else {
        // Character is not a decimal digit (0..9), then '-65' since ASCII code for 'A' is 65
        thisCharInt = (int) thisChar - 55;
       }

        binString += (String) binValue[thisCharInt];
    }

    return binString;
  }
}