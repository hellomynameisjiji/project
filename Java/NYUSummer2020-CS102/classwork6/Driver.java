package classwork6;

public class Driver {
	public static void main(String[] args) {
		int[] list = new int[7];
		for (int i = 0; i < 7; i++) {
			list[i] = 0;
		}
		
		seat(list, 0, 0);
		for (int i = 0; i < list.length; i++) {
			System.out.print(list[i]);			
		}
	}
	
	public static void seat(int[] list, int index, int count) {
		if (index == 7) {
				System.out.println(count);								
		} else {
			for (int i = 1; i < 6; i++) {
				if (index == 0) {
					list[index] = i;
				}
				if (index != 0) {
					if (i < 4 && 0 < i) {
						if (list[index - 1] != 0 && list[index - 1] < 4) {
							continue;
						}
						list[index] = i;
					}
					if (i > 3) {
						if (list[index - 1] > 3) {
							continue;
						}
						list[index] = i;
					}
				}
				else list[index] = (char)'E';
			}
			seat(list, index+1, count+1);
		}
	}
}