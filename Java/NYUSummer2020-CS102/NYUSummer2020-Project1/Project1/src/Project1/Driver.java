package Project1;
import net.datastructures.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;


public class Driver {
	public static void main(String[] args) {
		SinglyLinkedList<Process> awaitingList = new SinglyLinkedList<Process>();

		// Part A
		File myFile = new File("/Users/hyeongjihyeon/Downloads/part_a.txt");
		System.out.println("[Part A] Result with a given example: ");
		read(myFile, awaitingList);
		System.out.println("\n");
		
		// Part A - test with my own test file
		File myFile2 = new File("/Users/hyeongjihyeon/Downloads/part_a own_test.txt");
		System.out.println("[Part A] Result with my own test file: ");
		read(myFile2, awaitingList);
		System.out.println("\n");
		
		// Part A - test with the provided test file
		File myFile3 = new File("/Users/hyeongjihyeon/Downloads/Test Cases - Assignment 1 _2_.txt");
		System.out.println("[Part A] Result with the provided test file: ");
		read(myFile3, awaitingList);
		System.out.println("\n");
		
		// Part B
		System.out.println("[Part B] Result: ");
		read(20, awaitingList);

	}

	
	public static void read(File myFile, SinglyLinkedList<Process> awaitingList) {
		// Read a file
		try {
			Scanner fileInput = new Scanner(myFile);
			
			while (fileInput.hasNextLine()) {
				String line = fileInput.nextLine();
				String[] splitLine = line.split(";");
				
				for (int i = 0; i < splitLine.length; i++) {
					Process myProcess = new Process(splitLine[i]);
					awaitingList.addLast(myProcess);
					}
				System.out.println("Number of cycles needed to empty the list of processes: " + executeA(awaitingList));	
			}
						      
		      fileInput.close();

		} catch (FileNotFoundException e) {
		     System.out.println("Can't open the file, sorry.");
		}

	}
	
	public static void read(int size, SinglyLinkedList<Process> awaitingList) {
		for (int i = 0; i < size; i++) {
			Process myProcess = new Process();
			awaitingList.addLast(myProcess);
			}
		
		executeB(awaitingList);
	}
	
	public static int executeA(SinglyLinkedList<Process> list) {
		int numberOfCycles = 1;
		SinglyLinkedList<Process> executedList = new SinglyLinkedList<Process>();
		ArrayList<String> availableResource = initResource();
		
		while (list.isEmpty() == false) {
			Process activeProcess = list.first();
			
			if (activeProcess.executable(availableResource)) {
				if (activeProcess.getFirst().equals("A")) {
					activeProcess.useFirst();
					availableResource.remove("A");
				}
				if (activeProcess.getFirst().equals("B")) {
					activeProcess.useFirst();
					availableResource.remove("B");
				}
				if (activeProcess.getFirst().equals("C")) {
					activeProcess.useFirst();
					availableResource.remove("C");
				}

				executedList.addLast(activeProcess);
				list.removeFirst();


			} else {
				numberOfCycles++;
				
				availableResource = initResource();
			}
		}
		return numberOfCycles;
	
	}
	
	
	public static void executeB(SinglyLinkedList<Process> list) {
		int numberOfCycles = 1;
		SinglyLinkedList<Process> executedList = new SinglyLinkedList<Process>();
		ArrayList<String> availableResource = initResource();
		
		while (list.isEmpty() == false) {
			Process activeProcess = list.first();
			
			if (activeProcess.executable(availableResource)) {
				if (activeProcess.getFirst().equals("A")) {
					activeProcess.useFirst();
					availableResource.remove("A");
				}
				if (activeProcess.getFirst().equals("B")) {
					activeProcess.useFirst();
					availableResource.remove("B");
				}
				if (activeProcess.getFirst().equals("C")) {
					activeProcess.useFirst();
					availableResource.remove("C");
				}

				executedList.addLast(activeProcess);
				list.removeFirst();
			} else {
				numberOfCycles++;
				Process addedProcess1 = new Process();
				list.addLast(addedProcess1);
				Process addedProcess2 = new Process();
				list.addLast(addedProcess2);

				availableResource = initResource();
				
				
				if (numberOfCycles % 100 == 0) {
					System.out.println("Length of processes at cycle " + numberOfCycles + ": " + list.size());
				}		
			}
			
			if (numberOfCycles >= 1000 && list.isEmpty() == false) {
		        break;
			}
		}
		
		if (numberOfCycles >= 1000) {
			System.out.println(" ");
			System.out.println("Number of cycles is over 1000. ");
			System.out.println("Number of processes left: " + list.size());
		} else {
			System.out.println("Number of cycles needed to empty the list of processes: " + numberOfCycles);
		}
	}
	
	public static ArrayList<String> initResource() {
		ArrayList<String> availableResource = new ArrayList<String>();
		availableResource.add("A");
		availableResource.add("B");
		availableResource.add("C");
		
		return availableResource; 
	}
	
	
}
