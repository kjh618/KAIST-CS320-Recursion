object Main extends App {

  // fac and mkRec with Scala Recursion
  val facScalaRec: Int => Int = (n) => {
    if (n == 0) 1 else n * facScalaRec(n - 1)
  }

  val mkScalaRec: ((Int => Int) => Int => Int) => Int => Int = (bodyProc) => {
    lazy val f: Int => Int = (x) => bodyProc(f)(x)

    bodyProc(f)
  }

  val facMkScalaRec = mkScalaRec(
    (fac) => (n) => if (n == 0) 1 else n * fac(n - 1)
  )


  // fac and mkRec with "My" Recursion
  trait recFun extends (recFun => Int => Int) // "type recFun = recFun => Int => Int"

  val facRec: Int => Int = {
    val facX: recFun = (facY: recFun) => { // facX: recFun == recFun => Int => Int
      val fac: Int => Int = (x) => facY(facY)(x)

      (n) => if (n == 0) 1 else n * fac(n - 1)
    }

    facX(facX)
  }
  /*
  {with {fac
    {with {facX {fun {facY}
      {with {fac {fun {x} {{facY facY} x}}}

      {fun {n} {if0 n 1 {* n {fac {- n 1}}}  }}}}}

    {facX facX}  }}

  {fac 10}  }
  */

  val mkRec: ((Int => Int) => Int => Int) => Int => Int = (bodyProc) => {
    val fX: recFun = (fY: recFun) => { // fX: recFun == recFun => Int => Int
      val f: Int => Int = (x) => fY(fY)(x)

      bodyProc(f)
    }

    fX(fX)
  }

  val facMkRec = mkRec(
    (fac) => (n) => if (n == 0) 1 else n * fac(n - 1)
  )
  /*
  {with {mk-rec {fun {body-proc}
    {with {fX {fun {fY}
      {with {f {fun {x} {{fY fY} x}}}

      {body-proc f}  }}}

    {fX fX}  }}}

  {with {fac {mk-rec
    {fun {fac} {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}  }}

  {fac 10}  }}
  */

  //  val mkRecWrong: ((Int => Int) => Int => Int) => Int => Int = (bodyProc) => {
  //    val fX: recFun = (fY: recFun) => { // fX: recFun == recFun => Int => Int
  //      bodyProc(fY) // The type of fY does not match. (found: recFun, required: Int => Int)
  //    }
  //
  //    fX(fX)
  //  }


  // Tests
  println(facScalaRec(10))
  println(facMkScalaRec(10))

  println(facRec(10))
  println(facMkRec(10))
}