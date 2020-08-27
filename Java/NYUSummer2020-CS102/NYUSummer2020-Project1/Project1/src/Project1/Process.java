package Project1;
import java.util.ArrayList;
import java.util.Random;
import Project1.Process;

public class Process {
	private ArrayList<String> resource;
	private int size;
	
	public Process() {
		this.size = 0;
		this.resource = extractResource();
	}	

	public Process(String data) {
		this.size = 0;
		this.resource = extractResource(data);
	}
	
	
	public ArrayList<String> extractResource(String input) {
		String[] inputSplit = input.split("\\(|\\|\\ |\\,|\\)");
		ArrayList<String> resource = new ArrayList<String>();
		
		for (int i = 0; i < inputSplit.length; i++) {
			if (inputSplit[i].equals("A")) {
				resource.add(size, "A");
				size++;
			}
			if (inputSplit[i].equals("B")) {
				resource.add(size, "B");
				size++;
			} 
			if (inputSplit[i].equals("C")) {
				resource.add(size, "C");
				size++;
				}
			}
		
		return resource;
	}

	
	public ArrayList<String> extractResource() {
		Random r = new Random();
		ArrayList<String> resource = new ArrayList<String>();
	    int num = r.nextInt(6) + 1;
	    
	    if((num & 4) == 4){
	        resource.add(size, "A");
	        size++;
	    }
	    if((num & 2) == 2){
	    	resource.add(size, "B");
	    	size++;
	    }
	    if((num & 1) == 1){
	    	resource.add(size, "C");
	    	size++;
	    }
	    
	    return resource;
	}
	
	public String useFirst() {
		if (size == 0) {
			return "-1";
		}
		String temp = resource.get(0);
		resource.remove(0);
		size--;
		return temp;
	}
	
	public String use() {
		if (size == 0) {
			return "-1";
		}
		
		String temp = new String();
		int k = 0;
		while (size != 0) {
			temp += resource.get(k);
			resource.remove(k);
			size--;
		}
		return temp;
	}
	
	public String getFirst() {
		if (size == 0) {
			return "-1";
		}
		String temp = resource.get(0);
		return temp;
	}
	
	public String get() {
		if (size == 0) {
			return "-1";
		}
		
		String temp = new String();
		for (int i = 0; i < size; i++)
			temp += resource.get(i);
		
		return temp;
	}
	
	public boolean executable(ArrayList<String> resourceList) {
		int count = 0;
		
		for (int i = 0; i < resource.size(); i++) {
			for (int j = 0; j < resourceList.size(); j++) {
				if (resource.get(i).equals(resourceList.get(j)))
					count++;
			}
		}
		
		return count >= size;
	}

		
}

