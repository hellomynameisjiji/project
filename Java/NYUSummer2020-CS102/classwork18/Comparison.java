package classwork18;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Random;

public class Comparison {
	public static void main(String[] args ) {
		int[] numbers = new int[100000];
		CompareSort comp = null;
		
		Random rand = new Random();
		for (int i = 0; i < numbers.length; i++) {
			numbers[i] = rand.nextInt(1000000)+1 ;
		}
		int[] numbers1 = numbers.clone();
		int[] numbers2 = numbers.clone();
			// Array class sort


			long start = System.currentTimeMillis();
		
			Arrays.sort(numbers);
		
			long end = System.currentTimeMillis();		
			long dur = end - start;
			System.out.format("Sort time using %s is %d", "array sort", dur);
			System.out.println();
			// Selection sort
			start = System.currentTimeMillis();

			selectionSort(numbers1);
			
			end = System.currentTimeMillis();		
			dur = end - start;
			System.out.format("Sort time using %s is %d", "selection sort", dur);
			System.out.println();

			
			// Merge sort
			start = System.currentTimeMillis();

			mergeSort(numbers2, comp);
			
			end = System.currentTimeMillis();		
			dur = end - start;
			System.out.format("Sort time using %s is %d", "merge sort", dur);
		
	}
		
	static void selectionSort(int arr[]) {
		int n = arr.length;
		
		for (int i = 0; i < n-1; i++) {
			int min_idx = i;
			for (int j = i+1; j < n; j++) {
				if (arr[j] < arr[min_idx]) 
					min_idx = j;
				
				int temp = arr[min_idx];
				arr[min_idx] = arr[i];
				arr[i] = temp;
			}
		}
	}
	
	  public static void merge(int[] S1, int[] S2, int[] S, Comparator<Integer> comp) {
		    int i = 0, j = 0;
		    while (i + j < S.length) {
		      if (j == S2.length || (i < S1.length && comp.compare(S1[i], S2[j]) < 0))
		        S[i+j] = S1[i++];                     // copy ith element of S1 and increment i
		      else
		        S[i+j] = S2[j++];                     // copy jth element of S2 and increment j
		    }
		  }	
	
	public static void mergeSort(int[] S, Comparator<Integer> comp) {
		int n = S.length;
	    if (n < 2) return;                        // array is trivially sorted
	    // divide
	    int mid = n/2;
	    int[] S1 = Arrays.copyOfRange(S, 0, mid);   // copy of first half
	    int[] S2 = Arrays.copyOfRange(S, mid, n);   // copy of second half
	    // conquer (with recursion)
	    mergeSort(S1, comp);                      // sort copy of first half
	    mergeSort(S2, comp);                      // sort copy of second half
	    // merge results
	    merge(S1, S2, S, comp);               // merge sorted halves back into original
	  }
	

}


