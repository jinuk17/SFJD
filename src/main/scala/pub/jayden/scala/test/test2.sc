object Linearization{

  class C1 {
    def m: Unit = {
      print("C1 ")
    }
  }

  trait T1 extends C1 {
    override def m: Unit = {
      print("T1 ")
      super.m
    }
  }

  trait T2 extends C1 {
    override def m: Unit = {
      print("T2 ")
      super.m
    }
  }

  trait T3 extends C1 {
    override def m: Unit = {
      print("T3 ")
      super.m
    }
  }

  class C2A extends T2 {
    override def m: Unit = {
      print("C2A ")
      super.m
    }
  }

  class C2 extends C2A with T1 with T2 with T3 {
    override def m: Unit = {
      print("C2 ")
      super.m
    }
  }

  def calcLinearization(obj: C1, name: String) = {
    print(s"$name: ")
    obj.m
    print("AnyRef ")
    print("Any ")
  }

  calcLinearization(new C2, "C2")

}