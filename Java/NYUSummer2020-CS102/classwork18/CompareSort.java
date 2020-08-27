package classwork18;

import java.util.Comparator;

public class CompareSort implements Comparator<Integer> {

	@Override
	public int compare(Integer s1, Integer s2) {
		// TODO Auto-generated method stub
		if (s1.compareTo(s2)<0){return -1;}
		if (s1.compareTo(s2)>0){return  1;}
		return s1.compareTo(s2);
	}

}
