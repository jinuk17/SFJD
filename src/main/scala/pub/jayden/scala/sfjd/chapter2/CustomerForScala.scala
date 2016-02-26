package pub.jayden.scala.sfjd.chapter2

import scala.beans.BeanProperty

/**
 * Created by jaydenuk on 2016. 2. 24..
 */

class CustomerForScala1(var customerId: Int, var zip: String) {
}


class CustomerForScala2(@BeanProperty var customerId: Int, @BeanProperty var zip: String) {
}

object CustomerForScala{
  def main(args: Array[String]) {
    val customer1 = new CustomerForScala1(1, "123 45")

    customer1.customerId = 100;
    println(customer1.customerId)


    val customer2 = new CustomerForScala2(1, "123 45")

    customer2.getCustomerId
    customer2.setCustomerId(333)

    customer2.setZip("BeanProperty")

    println(customer2.zip)

  }
}
