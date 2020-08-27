package classwork6;

public class Recursion {
	public static void main(String[] args) {
		printNumbers(4);
		
		System.out.println("Cat Eyes: " + catEyes(7));
		
		System.out.println("power: " + power(2, 3));
		
		System.out.println("Factorial: " + factorial(5));
		
		System.out.println("Mobile Speakers: " + mobileSpeakers(4));
		
		System.out.println("Sum of digits: " + sum(135));
		
		System.out.println("Number of the digit 5: " + sum(135));
		
		int[] array = {11, 34, 56, 77, 88, 99, 123, 456, 2345};
		System.out.println("BinarySearch: " + binarySearch(array, 456, 0, array.length - 1));
		
		String s = "ABC";
		permutations(s.toCharArray(), 0);
		
		int input = 13513;
		System.out.println(sum2(input));
	}
	
	public static void printNumbers(int num) {
		if (num == 0) {	// base case
			return;
		} else {
			System.out.println(num);
			printNumbers(num - 1);
		}
	}
	
	public static int catEyes(int cats) {
		if (cats == 0) {
			return 0;
		} else {
			return 2 + catEyes(cats - 1);
		}
	}
	
	public static int power(int base, int num) {
		if (num == base) {
			return base;
		} else {
			return base * power(base, num - 1);
		}
	}
	
	public static int factorial(int num) {
		if (num == 0 || num == 1) {
			return 1;
		} else {
			return num * factorial(num - 1);
		}
	}
	
	public static int fibonacci(int num) {
		if (num == 0) {
			return 0;
		} if (num == 1) {
			return 1;
		} else {
			return fibonacci(num - 1) + fibonacci(num - 2);
		}
	}
	
	public static int mobileSpeakers(int mobiles) {
		if (mobiles == 0) {
			return 0;
		} else {
			if (mobiles % 2 == 0) {
				return 2 + mobileSpeakers(mobiles - 1);
			} else {
				return 1 + mobileSpeakers(mobiles - 1);
			}
		}
	}
	
	public static int sum(int num) {
		if (num <= 9) {
			return num;
		} else {
			return num % 10 + sum(num / 10);
		}
	}

	public static int count5(int num) {
		if (num <= 9) {
			if (num == 5) {
				return 1;
			} else {
				return 0;
			} 
		} else {
			if (num % 10 == 5) {
				return 1 + count5(num / 10);
			} else {
				return 0 + count5(num / 10);
			}
		}
	}
	
	public static int binarySearch(int[] array, int key, int start, int end) {
		if (start <= end) {
			int middle = (start + end) / 2;
			if (array[middle] > key) {
				return binarySearch(array, key, start, middle-1);
			} else if (array[middle] < key) {
				return binarySearch(array, key, middle + 1, end);
			} else {
				return middle;
			}
		} else {
			return -1;
		}
	}
	
	// permutations
	public static void swap(char[] ch, int i, int j) {
		char temp = ch[i];
		ch[i] = ch[j];
		ch[j] = temp;
	}

	public static void permutations (char[] ch, int currentIndex) {
		if (currentIndex == ch.length - 1) {
			System.out.println(String.copyValueOf(ch));
		}
		
		for (int i = currentIndex; i < ch.length; i++) {
			swap(ch, currentIndex, i);
			permutations(ch, currentIndex + 1);
			swap(ch, currentIndex, i);
		}
	}


	public static int sum2(int input) {
		if (input / 10 == 0) {
			return input % 10;
		} else {
			return (input % 10) + sum2(input / 10);
		}
	}


	
}

