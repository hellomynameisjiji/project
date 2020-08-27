package classwork17;

import java.util.Random;

public class Driver {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
	    Random r = new Random(3);
        ProbeHashMap<Integer,Integer> map = new ProbeHashMap<Integer,Integer>(25);
        for (int i = 1; i<=15;i++) {
            map.put(r.nextInt(1000)+1, i);
        }
        for (   Integer  key  : map.keySet()) {
            System.out.println(key + "-" + map.get(key));
            map.findSlot(map.get(key), key);
        }
        
        
    }

}


