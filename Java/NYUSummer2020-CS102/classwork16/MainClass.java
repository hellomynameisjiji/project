package classwork16;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Scanner;

public class MainClass {
	
	static int TrumpVotes = 0;
	static int BidenVotes = 0;

	public static void main(String[] args) {
		// File Formats: 
		//  data/ElectoralVotes.csv :  State, Number of electoral votes
		//  data/ElectionData 2012.csv:  State, # of Obama votes, # of Romney votes
		File selectedfile = new File ("data/ElectoralVotes.csv");		
		// Confirm if the file exists -- if not, prompt error
		Scanner fileInput = null;
		try {
			fileInput = new Scanner(selectedfile);
		}
		catch (FileNotFoundException e1) {
			System.out.println("\n[ERROR]: Cannot find the electoral votes file.");
		}		
		// Scan each entry until no more exist  
		while (fileInput.hasNextLine()) {
			// Read the next line in the file
			String buffline = fileInput.nextLine();
			// Split the line into an array
			String[] data_line = buffline.split(",");
			// Store the information  in your HashMap
		}
		
		
		// Use above logic to read each state's actual vote total
		// For each state, find out who won
		// Look up in HashMap number of electoral votes for the state
		// Award electoral votes for that state to the winning candidate
		
		
		
		
		System.out.println("Trump Votes: " + TrumpVotes);
		System.out.println("Biden Votes: " + BidenVotes);
		

	}

}
