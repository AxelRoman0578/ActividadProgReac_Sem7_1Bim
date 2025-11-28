// 1. Extension para funciones matemáticas
extension (f: Double => Double)
  def integracion(a: Double, b: Double): Double =
    val h = (b - a) / 2
    val x0 = a
    val x1 = (a + b) / 2
    val x2 = b
    (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
    //fx0 limite inferior, fx1 limite intermedio, fx2 limite superior

// 2. Extension para calcular el error absoluto
extension (valorObtenido: Double)
  def errorAbs(valorEsperado: Double): Double =
    math.abs(valorEsperado - valorObtenido)

// 3. Cálculo de cada integral y su error
def resultado1: (Double, Double) =
  val f = (x: Double) => -x*x + 8*x - 12
  val res = f.integracion(3, 5)
  val err = res.errorAbs(7.33)
  (res, err)
def resultado2: (Double, Double) =
  val f = (x: Double) => 3*x*x
  val res = f.integracion(0, 2)
  val err = res.errorAbs(8.0)
  (res, err)
def resultado3: (Double, Double) =
  val f = (x: Double) => x + 2*x*x - x*x*x + 5*math.pow(x,4)
  val res = f.integracion(-1, 1)
  val err = res.errorAbs(3.333)
  (res, err)
def resultado4: (Double, Double) =
  val f = (x: Double) => (2*x + 1)/(x*x + x)
  val res = f.integracion(1, 2)
  val err = res.errorAbs(1.09861)
  (res, err)
def resultado5: (Double, Double) =
  val f = (x: Double) => math.exp(x)
  val res = f.integracion(0, 1)
  val err = res.errorAbs(1.71828)
  (res, err)
def resultado6: (Double, Double) =
  val f = (x: Double) => 1/(x - 1)
  val res = f.integracion(2, 3)
  val err = res.errorAbs(0.828427)
  (res, err)
def resultado7: (Double, Double) =
  val f = (x: Double) => 1/(1 + x*x)
  val res = f.integracion(0, 1)
  val err = res.errorAbs(0.785398)
  (res, err)
  
resultado1
resultado2
resultado3
resultado4
resultado5
resultado6
resultado7