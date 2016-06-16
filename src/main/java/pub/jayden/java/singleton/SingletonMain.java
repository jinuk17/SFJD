package pub.jayden.java.singleton;

import java.math.BigDecimal;

/**
 * Created by jaydenuk on 2016. 4. 8..
 */

public class SingletonMain {

    public static void main(String[] args) {
        final Product product = new Product(1L, "AAAA", new BigDecimal("12"));

        System.out.println(product);

        product.setName("BBBB");

        System.out.println(product);
    }
}
