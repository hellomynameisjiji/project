//**********************************************************************
//ShoppingCart.java
//
//Represents a shopping cart as an array of items
//**********************************************************************

import java.text.NumberFormat;

public class ShoppingCart
{
	private int itemCount;     // total number of items in the cart
	private double totalPrice; // total price of items in the cart
	private int capacity;		// current cart capacity
	private Item[] cart;       // the actual array of items to store things in the cart 
	// -----------------------------------------------------------
	//  Creates an empty shopping cart with a capacity of 3 items.
	// -----------------------------------------------------------
	public ShoppingCart()
	{
		capacity = 3;
		itemCount = 0;
		totalPrice = 0.0;
		cart = new Item[capacity];
	}
// -------------------------------------------------------
//  Adds an item to the shopping cart.
// -------------------------------------------------------
	public void addToCart(String itemName, double price, int quantity)
	{
		cart[itemCount++] = new Item(itemName, price, quantity);
		totalPrice += price * quantity;
		if(itemCount == capacity) // if full, increase the size of the cart
			increaseSize();
	}
// -------------------------------------------------------
//  Returns the contents of the cart together with
//  summary information.
// -------------------------------------------------------
	public String toString() // this method is called when an object needs to be "printed" 
							 // (when System.out.println() is called, this overriden method is called)
	{
		NumberFormat fmt = NumberFormat.getCurrencyInstance();
		String contents = "\nShopping Cart\n";
		contents += "\nItem\tPrice\tQty\tTotal\n";
		for (int i = 0; i < itemCount; i++)
			contents += cart[i].toString() + "\n";
			contents += "\nTotal Price: " + fmt.format(totalPrice);
			contents += "\n";
			return contents;
	}
// ------------------------------------------------------------
//  Increases the capacity of the shopping cart by doubling it.
// ------------------------------------------------------------
	private void increaseSize()
	{ 
		Item[] tempItem = new Item[capacity];
		capacity *= 2; // double the size

		for (int i=0; i< itemCount; i++) 
		{
			tempItem[i] = cart[i];
		}
		cart = new Item[capacity];
		for (int i=0; i< itemCount; i++)
		{
			cart[i] = tempItem[i];
		}
	
		// The above can also be accomplished by using:
		// System.arraycopy(...)
	}
	/**
	 * @return Returns the totalPrice.
	 */
	public double getTotalPrice() {
		return totalPrice;
	}
}