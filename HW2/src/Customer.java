import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
/**
 * Yakov Kazinets and Tae Kim
 * I pledge my honor that I have abided by the Stevens Honor System.
 * 
 * 
 * 
 * 
 * */
public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd = new Random();
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {//<----------------------------------
        // TODO
    	this.bakery = bakery;
    	this.shoppingCart = new ArrayList<BreadType>();
    	this.shopTime = 1 + rnd.nextInt(1);
    	this.checkoutTime = 1 + rnd.nextInt(1);
    	fillShoppingCart();
    }

    /**
     * Run tasks for the customer
     */
    public void run() {//<----------------------------------
        for(BreadType bread: shoppingCart) {
        	
        	//entry
        	if (bread == BreadType.RYE) {
        		try {
        			bakery.rye_shelf.acquire();
        			Thread.sleep(shopTime);
        		} catch (InterruptedException ie) {
        			ie.printStackTrace();
        		}
        		//System.out.println("Customer " + hashCode() + " is buying " + bread.toString());
        		bakery.takeBread(bread);
        		bakery.rye_shelf.release();
        	} else if (bread == BreadType.SOURDOUGH){
        		try {
        			bakery.sour_shelf.acquire();
        			Thread.sleep(shopTime);
        		} catch (InterruptedException ie) {
        			ie.printStackTrace();
        		}
        		//System.out.println("Customer " + hashCode() + " is buying " + bread.toString());
        		bakery.takeBread(bread);
        		bakery.sour_shelf.release();
        	} else if (bread == BreadType.WONDER){
        		try {
        			bakery.wonder_shelf.acquire();
        			Thread.sleep(shopTime);
        		} catch (InterruptedException ie) {
        			ie.printStackTrace();
        		}
        		//System.out.println("Customer " + hashCode() + " is buying " + bread.toString());
        		bakery.takeBread(bread);
        		bakery.wonder_shelf.release();
        	}
        		
        }
        try {
    		bakery.cashiers.acquire();
    		//System.out.println("Customer " + hashCode() + " is checking out ");
    		Thread.sleep(checkoutTime);
    		bakery.addSales(getItemsValue());
    		//System.out.println(this.toString());
    		
    	} catch (InterruptedException ie) {
    		ie.printStackTrace();
    	}
    	bakery.cashiers.release();
    	System.out.println("Customer " + hashCode() + " has left the bakery");
    	//toString();
        
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}