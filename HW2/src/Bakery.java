import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
/**
 * Yakov Kazinets and Tae Kim
 * I pledge my honor that I have abided by the Stevens Honor System.
 * 
 * 
 * 
 * 
 * */
public class Bakery implements Runnable {
    private static final int TOTAL_CUSTOMERS = 2000000;
    private static final int ALLOWED_CUSTOMERS = 500;
    private static final int FULL_BREAD = 20;
    private Map<BreadType, Integer> availableBread;
    private ExecutorService executor;
    private float sales = 0;

    // TODO //<-----------------------------------------------------------------------------------------
    static Semaphore rye_shelf = new Semaphore(500);
    static Semaphore sour_shelf = new Semaphore(500);
    static Semaphore wonder_shelf = new Semaphore(500);
    static Semaphore cashiers = new Semaphore(500);
    //static Semaphore mutex = new Semaphore(1);
    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Run all customers in a fixed thread pool
     */
    public void run() {
    	
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);
        
        // TODO //<-----------------------------------------------------------------------------------------
        executor = Executors.newFixedThreadPool(ALLOWED_CUSTOMERS);
        System.out.println("Opening Bakery");
        for (int i = 0; i < TOTAL_CUSTOMERS; i++) {
        	Customer c = new Customer(this);
        	executor.execute(c);
        }
        
        executor.shutdown();
        try {
        	executor.awaitTermination(TOTAL_CUSTOMERS,TimeUnit.SECONDS);
        	System.out.println("Closing Bakery");
            System.out.println("Bakery made $" + sales + " today" );
        } catch (InterruptedException ie) {
        	ie.printStackTrace();
        }
        
        
       
        
    }
}