package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

/**
 * Pruebas de contrato de la versión secuencial: una por función, con los
 * valores de los dos ejemplos del enunciado. No se modifican.
 *
 * La rúbrica pide además cinco casos propios por función, distintos de estos y
 * de los ejemplos del enunciado. Van al final de este archivo o en un archivo
 * nuevo de esta misma carpeta; las dos formas valen.
 */
@RunWith(classOf[JUnitRunner])
class SimilitudTest extends AnyFunSuite {

  val eps = 1e-9

  // Ejemplo 1 del enunciado, con las palabras ya normalizadas para que estas
  // pruebas no dependan unas de otras.
  val d0: Documento = ("d0", Vector("el", "gato", "duerme", "en", "el", "sofa"))
  val d1: Documento = ("d1", Vector("el", "gato", "duerme", "en", "la", "alfombra"))
  val d2: Documento = ("d2", Vector("la", "lluvia", "moja", "el", "parque"))
  val c1: Corpus = Vector(d0, d1, d2)

  // Ejemplo 2 del enunciado.
  val e0: Documento = ("e0", Vector("la", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e1: Documento = ("e1", Vector("toda", "funcion", "recursiva", "termina",
    "cuando", "llega", "al", "caso", "base"))
  val e2: Documento = ("e2", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "tareas", "independientes"))
  val e3: Documento = ("e3", Vector("el", "paralelismo", "divide", "el",
    "trabajo", "en", "hilos", "independientes"))
  val c2: Corpus = Vector(e0, e1, e2, e3)

  test("normalizar: baja a minúsculas y parte en palabras") {
    assert(normalizar("El Gato Duerme") == Vector("el", "gato", "duerme"))
  }

  test("fragmentos: son las ventanas consecutivas, en orden") {
    val ts = Vector("a", "b", "c", "d")
    assert(fragmentos(ts, 2) ==
      Vector(Vector("a", "b"), Vector("b", "c"), Vector("c", "d")))
  }

  test("huella: el ejemplo 1 tiene 4, 4 y 3 fragmentos") {
    assert(huella(d0, 3).size == 4)
    assert(huella(d1, 3).size == 4)
    assert(huella(d2, 3).size == 3)
  }

  test("huellas: hay una huella por documento") {
    assert(huellas(c1, 3).length == 3)
  }

  test("jaccard: ejemplo 2, las dos parejas dan 0.75 y 0.5") {
    assert(math.abs(jaccard(huella(e0, 3), huella(e1, 3)) - 0.75) < eps)
    assert(math.abs(jaccard(huella(e2, 3), huella(e3, 3)) - 0.5) < eps)
  }

  test("parejas: hay n (n - 1) / 2") {
    assert(parejas(5).length == 10)
  }

  test("similitudes: el ejemplo 1 da 1/3, 0 y 0") {
    val hs = huellas(c1, 3)
    val ms = similitudes(hs, parejas(3)).map(_._3)
    assert(math.abs(ms(0) - 2.0 / 6.0) < eps && ms(1) == 0.0 && ms(2) == 0.0)
  }

  test("indiceInvertido: el fragmento inicial de d0 está en d0 y en d1") {
    val idx = indiceInvertido(huellas(c1, 3))
    assert(idx(Vector("el", "gato", "duerme")) == Set(0, 1))
  }

  test("parejasCandidatas: en el ejemplo 2 quedan dos de seis") {
    val idx = indiceInvertido(huellas(c2, 3))
    assert(parejasCandidatas(idx) == Vector((0, 1), (2, 3)))
  }

  test("ranking: ordena de mayor a menor y trunca a p") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.4), (0, 2, 0.1), (1, 2, 0.9))
    assert(ranking(ms, 0.0, 10).map(_._3) == Vector(0.9, 0.4, 0.1))
    assert(ranking(ms, 0.0, 2).length == 2)
  }

  test("detectar: el ejemplo 2 reporta (0, 1) con 0.75 y (2, 3) con 0.5") {
    val r = detectarDirecto(c2, 3, 0.3, 5)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1), (2, 3)))
    assert(math.abs(r(0)._3 - 0.75) < eps && math.abs(r(1)._3 - 0.5) < eps)
  }

  test("detectar: las dos soluciones coinciden en los ejemplos") {
    assert(detectarDirecto(c1, 3, 0.2, 5) == detectarConIndice(c1, 3, 0.2, 5))
    assert(detectarDirecto(c2, 3, 0.3, 5) == detectarConIndice(c2, 3, 0.3, 5))
  }
  ///INTEGRANTE 1
  test("normalizar: quita la puntuación y baja a minúsculas") {
    assert(normalizar("Hola, Mundo!") == Vector("hola", "mundo"))
  }

  test("normalizar: espacios, tabuladores y saltos de línea solo separan") {
    assert(normalizar("  uno   dos\t tres\n") == Vector("uno", "dos", "tres"))
  }

  test("normalizar: los dígitos se conservan como palabras") {
    assert(normalizar("Version 2 del 2026-11-19") ==
      Vector("version", "2", "del", "2026", "11", "19"))
  }

  test("normalizar: una cadena vacía o de puros signos no tiene palabras") {
    assert(normalizar("") == Vector())
    assert(normalizar("¡¿?! ... --") == Vector())
  }

  test("normalizar: el guion y el apóstrofe separan, y las tildes y la eñe se conservan") {
    assert(normalizar("don't stop-me") == Vector("don", "t", "stop", "me"))
    assert(normalizar("Año Nuevo: ¡Canción!") == Vector("año", "nuevo", "canción"))
  }

  // fragmentos ----------------------------------------------------------------

  test("fragmentos: con k = 3 sobre cinco palabras salen tres, en orden") {
    assert(fragmentos(Vector("a", "b", "c", "d", "e"), 3) ==
      Vector(Vector("a", "b", "c"), Vector("b", "c", "d"), Vector("c", "d", "e")))
  }

  test("fragmentos: si hay menos de k palabras no sale ninguno") {
    assert(fragmentos(Vector("a", "b"), 3) == Vector())
    assert(fragmentos(Vector(), 1) == Vector())
  }

  test("fragmentos: con exactamente k palabras sale uno, igual al vector completo") {
    assert(fragmentos(Vector("x", "y", "z"), 3) == Vector(Vector("x", "y", "z")))
  }

  test("fragmentos: con k = 1 cada palabra es su propio fragmento") {
    assert(fragmentos(Vector("a", "b", "c"), 1) ==
      Vector(Vector("a"), Vector("b"), Vector("c")))
  }

  test("fragmentos: salen t - k + 1 aunque las palabras se repitan") {
    val casos = for (t <- 0 to 40; k <- 1 to 10) yield (t, k)
    assert(casos.forall { case (t, k) =>
      fragmentos(Vector.fill(t)("a"), k).length == math.max(0, t - k + 1) })
  }

  // huella --------------------------------------------------------------------

  test("huella: el conjunto de fragmentos de cuatro palabras con k = 2") {
    assert(huella(("h0", Vector("a", "b", "c", "d")), 2) ==
      Set(Vector("a", "b"), Vector("b", "c"), Vector("c", "d")))
  }

  test("huella: un fragmento repetido cuenta una sola vez") {
    // a b a b a tiene cuatro fragmentos, pero solo dos distintos.
    val h = huella(("h1", Vector("a", "b", "a", "b", "a")), 2)
    assert(h == Set(Vector("a", "b"), Vector("b", "a")))
    assert(h.size == 2)
  }

  test("huella: con menos de k palabras es vacía y con exactamente k tiene un fragmento") {
    assert(huella(("h2", Vector("a", "b")), 3) == Set())
    assert(huella(("h3", Vector("a", "b", "c")), 3) == Set(Vector("a", "b", "c")))
  }

  test("huella: su tamaño es a lo sumo t - k + 1, con igualdad si no hay repetidos") {
    val distintas: Documento = ("h4", Vector("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"))
    assert(huella(distintas, 3).size == 8)
    val alAzar = documentoAlAzar("h5", 50, 5)
    assert((1 to 6).forall(k => huella(alAzar, k).size <= 50 - k + 1))
  }

  test("huella: no depende del id del documento, pero sí del orden de las palabras") {
    val palabras = Vector("uno", "dos", "tres", "cuatro")
    assert(huella(("x", palabras), 2) == huella(("y", palabras), 2))
    assert(huella(("x", palabras), 2) != huella(("x", palabras.reverse), 2))
  }

  // huellas -------------------------------------------------------------------

  test("huellas: un corpus vacío no tiene huellas") {
    assert(huellas(Vector(), 3) == Vector())
  }

  test("huellas: la huella de cada documento, en el orden del corpus") {
    val corpus: Corpus = Vector(
      ("a", Vector("x", "y", "z")),
      ("b", Vector("y", "z", "w")))
    assert(huellas(corpus, 2) == Vector(
      Set(Vector("x", "y"), Vector("y", "z")),
      Set(Vector("y", "z"), Vector("z", "w"))))
  }

  test("huellas: un documento corto deja una huella vacía en su misma posición") {
    val corpus: Corpus = Vector(
      ("d0", Vector("a", "b", "c", "d", "e")),
      ("d1", Vector("a", "b")),
      ("d2", Vector("a")))
    assert(huellas(corpus, 2).map(h => h.size) == Vector(4, 1, 0))
  }

  test("huellas: dos copias del mismo documento tienen huellas iguales") {
    val d: Documento = ("d", Vector("p", "q", "r", "s", "t"))
    val hs = huellas(Vector(d, ("copia", d._2)), 3)
    assert(hs(0) == hs(1))
  }

  test("huellas: coincide con calcular cada huella por separado en un corpus al azar") {
    val corpus = corpusAlAzar(15, 40, 6)
    assert(List(1, 2, 3).forall { k =>
      huellas(corpus, k) == corpus.map(d =>
        d._2.sliding(k).filter(w => w.length == k).map(w => w.toVector).toSet) })
  }
  ///INTEGRANTE 2

  /** Una huella a partir de fragmentos escritos como "a b c". */
  def i2hu(fs: String*): Huella = fs.map(_.split(" ").toVector).toSet

  /** Huella calculada sin usar las funciones de otros integrantes. */
  def i2HuellaLocal(d: Documento, k: Int): Huella =
    d._2.sliding(k).filter(_.length == k).map(_.toVector).toSet

  def i2HuellasLocales(c: Corpus, k: Int): Vector[Huella] =
    c.map(i2HuellaLocal(_, k))

  // --- jaccard ---------------------------------------------------------------

  test("jaccard: huellas disjuntas dan 0") {
    assert(jaccard(i2hu("a b", "b c"), i2hu("x y", "y z")) == 0.0)
  }

  test("jaccard: huellas iguales dan 1") {
    val h = i2hu("a b", "b c", "c d")
    assert(jaccard(h, h) == 1.0)
  }

  test("jaccard: dos huellas vacías dan 0 y no NaN") {
    val r = jaccard(Set(), Set())
    assert(r == 0.0 && !r.isNaN)
  }

  test("jaccard: una huella vacía y otra no dan 0") {
    assert(jaccard(Set(), i2hu("a b")) == 0.0)
    assert(jaccard(i2hu("a b"), Set()) == 0.0)
  }

  test("jaccard: intersección 2 y unión 5 dan 0.4") {
    val a = i2hu("a b", "b c", "c d")
    val b = i2hu("b c", "c d", "d e", "e f")
    assert(math.abs(jaccard(a, b) - 0.4) < eps)
  }

  test("jaccard: una huella contenida en otra da |A| / |B|") {
    val a = i2hu("a b", "b c")
    val b = i2hu("a b", "b c", "c d", "d e")
    assert(math.abs(jaccard(a, b) - 0.5) < eps)
  }

  test("jaccard: es simétrica y el orden de los fragmentos no cuenta") {
    val a = i2hu("a b", "b c", "c d")
    val b = i2hu("c d", "z z", "a b")
    assert(jaccard(a, b) == jaccard(b, a))
    assert(math.abs(jaccard(a, b) - 0.5) < eps)
  }

  test("jaccard: un fragmento con las mismas palabras en otro orden es distinto") {
    assert(jaccard(i2hu("a b"), i2hu("b a")) == 0.0)
  }

  test("jaccard: queda en [0, 1] y cumple la definición en corpus al azar") {
    val hs = i2HuellasLocales(corpusAlAzar(20, 80, 10), 2)
    val todas = for {a <- hs; b <- hs} yield (a, b)
    assert(todas.forall { case (a, b) =>
      val j = jaccard(a, b)
      val esperado = a.intersect(b).size.toDouble / a.union(b).size
      j >= 0.0 && j <= 1.0 && math.abs(j - esperado) < eps
    })
  }

  // --- parejas ---------------------------------------------------------------

  test("parejas: con 0 y con 1 documentos no hay parejas") {
    assert(parejas(0) == Vector())
    assert(parejas(1) == Vector())
  }

  test("parejas: un número negativo de documentos da el vector vacío") {
    assert(parejas(-3) == Vector())
  }

  test("parejas: con 2 documentos solo está (0, 1)") {
    assert(parejas(2) == Vector((0, 1)))
  }

  test("parejas: con 4 documentos salen las seis, en orden") {
    assert(parejas(4) ==
      Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("parejas: hay n (n - 1) / 2 para n de 0 a 40") {
    assert((0 to 40).forall(n => parejas(n).length == n * (n - 1) / 2))
  }

  test("parejas: todas cumplen 0 <= i < j < n y ninguna se repite") {
    val n = 25
    val ps = parejas(n)
    assert(ps.forall { case (i, j) => 0 <= i && i < j && j < n })
    assert(ps.distinct.length == ps.length)
  }

  test("parejas: salen en orden lexicográfico") {
    val ps = parejas(15)
    assert(ps == ps.sorted)
  }

  test("parejas: el último es (n - 2, n - 1) y el primero (0, 1)") {
    assert(parejas(9).head == ((0, 1)) && parejas(9).last == ((7, 8)))
  }

  // --- similitudes -----------------------------------------------------------

  val i2h0: Huella = i2hu("a b", "b c", "c d")
  val i2h1: Huella = i2hu("b c", "c d", "d e", "e f")
  val i2h2: Huella = i2hu("x y")
  val i2hv: Vector[Huella] = Vector(i2h0, i2h1, i2h2)

  test("similitudes: sin parejas da el vector vacío") {
    assert(similitudes(i2hv, Vector()) == Vector())
  }

  test("similitudes: valores conocidos de las tres parejas") {
    val ms = similitudes(i2hv, parejas(3))
    assert(ms.map(m => (m._1, m._2)) == Vector((0, 1), (0, 2), (1, 2)))
    assert(math.abs(ms(0)._3 - 0.4) < eps)
    assert(ms(1)._3 == 0.0 && ms(2)._3 == 0.0)
  }

  test("similitudes: conserva el orden de las parejas que se le pasan") {
    val ps: Vector[Pareja] = Vector((1, 2), (0, 1), (0, 2))
    val ms = similitudes(i2hv, ps)
    assert(ms.map(m => (m._1, m._2)) == ps)
    assert(ms(0)._3 == 0.0 && math.abs(ms(1)._3 - 0.4) < eps)
  }

  test("similitudes: una sola pareja") {
    val ms = similitudes(i2hv, Vector((0, 1)))
    assert(ms.length == 1 && math.abs(ms(0)._3 - 0.4) < eps)
  }

  test("similitudes: una huella vacía da 0 con todas") {
    val vacia: Huella = Set()
    val ms = similitudes(Vector(i2h0, vacia, i2h1), parejas(3))
    assert(ms(0)._3 == 0.0 && ms(2)._3 == 0.0)
    assert(!ms.exists(_._3.isNaN))
  }

  test("similitudes: cada valor coincide con jaccard de sus huellas") {
    val hs = i2HuellasLocales(corpusAlAzar(25, 60, 8), 2)
    val ms = similitudes(hs, parejas(hs.length))
    assert(ms.length == hs.length * (hs.length - 1) / 2)
    assert(ms.forall { case (i, j, s) => s == jaccard(hs(i), hs(j)) })
  }

  test("similitudes: un documento copiado a medias queda muy por encima del azar") {
    val c = corpusConCopias(10, 200, 50, 1, 0.5)
    val hs = i2HuellasLocales(c, 3)
    val ms = similitudes(hs, parejas(c.length))
    val mejor = ms.maxBy(_._3)
    assert((mejor._1, mejor._2) == ((0, 10)))
  }

  ///INTEGRANTE 3

  /** Una huella a partir de fragmentos escritos como "a b c". */
  def ixhu(fs: String*): Huella = fs.map(_.split(" ").toVector).toSet

  /** Un fragmento escrito como "a b c". */
  def ixfr(f: String): Fragmento = f.split(" ").toVector

  /** Huella calculada sin usar las funciones de otros integrantes. */
  def ixHuellaLocal(d: Documento, k: Int): Huella =
    d._2.sliding(k).filter(_.length == k).map(_.toVector).toSet

  def ixHuellasLocales(c: Corpus, k: Int): Vector[Huella] =
    c.map(ixHuellaLocal(_, k))

  // --- indiceInvertido -------------------------------------------------------

  test("indiceInvertido: sin huellas el índice es vacío") {
    assert(indiceInvertido(Vector()) == Map())
  }

  test("indiceInvertido: una huella vacía no aporta entradas y las posiciones se respetan") {
    val hs: Vector[Huella] = Vector(Set(), ixhu("a b"), Set())
    assert(indiceInvertido(hs) == Map(ixfr("a b") -> Set(1)))
  }

  test("indiceInvertido: un fragmento en tres huellas queda con los tres") {
    val hs = Vector(ixhu("a b", "x y"), ixhu("a b"), ixhu("a b", "z z"))
    val idx = indiceInvertido(hs)
    assert(idx(ixfr("a b")) == Set(0, 1, 2))
    assert(idx(ixfr("x y")) == Set(0) && idx(ixfr("z z")) == Set(2))
  }

  test("indiceInvertido: ejemplo armado a mano con todas sus entradas") {
    val hs = Vector(ixhu("a b", "b c"), ixhu("b c", "c d"), ixhu("c d"))
    assert(indiceInvertido(hs) == Map(
      ixfr("a b") -> Set(0),
      ixfr("b c") -> Set(0, 1),
      ixfr("c d") -> Set(1, 2)))
  }

  test("indiceInvertido: g está en la huella i si y solo si i está en idx(g)") {
    val hs = ixHuellasLocales(corpusAlAzar(25, 50, 10), 2)
    val idx = indiceInvertido(hs)
    val todosLosFragmentos = hs.flatten.toSet
    assert(idx.keySet == todosLosFragmentos)
    assert(idx.values.forall(_.nonEmpty))
    assert(hs.indices.forall(i =>
      todosLosFragmentos.forall(g => hs(i).contains(g) == idx(g).contains(i))))
  }

  // --- parejasCandidatas -----------------------------------------------------

  test("parejasCandidatas: un índice vacío no tiene parejas") {
    assert(parejasCandidatas(Map()) == Vector())
  }

  test("parejasCandidatas: si cada fragmento está en un solo documento no hay candidatas") {
    val idx: Indice = Map(ixfr("a b") -> Set(0), ixfr("b c") -> Set(1),
      ixfr("c d") -> Set(2))
    assert(parejasCandidatas(idx) == Vector())
  }

  test("parejasCandidatas: una pareja que sale de varios fragmentos aparece una sola vez") {
    val idx: Indice = Map(ixfr("a b") -> Set(0, 1), ixfr("b c") -> Set(0, 1),
      ixfr("c d") -> Set(0, 1))
    assert(parejasCandidatas(idx) == Vector((0, 1)))
  }

  test("parejasCandidatas: salen con i < j y ordenadas, sin importar el orden de los conjuntos") {
    val idx: Indice = Map(
      ixfr("a b") -> Set(3, 0),
      ixfr("b c") -> Set(2, 1),
      ixfr("c d") -> Set(4, 0, 3))
    assert(parejasCandidatas(idx) == Vector((0, 3), (0, 4), (1, 2), (3, 4)))
  }

  test("parejasCandidatas: son exactamente las parejas con huellas que se intersectan") {
    val hs = ixHuellasLocales(corpusAlAzar(30, 40, 20), 3)
    val esperado = (for {
      i <- hs.indices
      j <- i + 1 until hs.length
      if hs(i).intersect(hs(j)).nonEmpty
    } yield (i, j)).toVector
    assert(parejasCandidatas(indiceInvertido(hs)) == esperado)
  }
  ///INTEGRANTE 4 - Juan stevan cataño

  // Pruebas ranking

  test("ranking: filtra por umbral incluyendo igualdad exacta y descarta menores") {
    val meds = Vector(
      (0, 1, 0.75),
      (0, 2, 0.50),
      (1, 2, 0.4999),
      (1, 3, 0.10)
    )
    val res = ranking(meds, 0.50, 10)
    assert(res == Vector((0, 1, 0.75), (0, 2, 0.50)))
  }

  test("ranking: ordena de mayor a menor y desempata de forma determinista por indices") {
    val meds = Vector(
      (2, 4, 0.60),
      (0, 3, 0.60),
      (1, 2, 0.90),
      (0, 1, 0.60)
    )
    val res = ranking(meds, 0.50, 10)
    assert(res == Vector((1, 2, 0.90), (0, 1, 0.60), (0, 3, 0.60), (2, 4, 0.60)))
  }

  test("ranking: trunca exactamente a los primeros p elementos") {
    val meds = Vector(
      (0, 1, 0.20),
      (0, 2, 0.85),
      (1, 2, 0.70),
      (2, 3, 0.95)
    )
    val res = ranking(meds, 0.10, 2)
    assert(res == Vector((2, 3, 0.95), (0, 2, 0.85)))
  }

  test("ranking: retorna vacio ante entrada vacia o cuando ninguna supera el umbral") {
    val meds = Vector((0, 1, 0.45), (1, 2, 0.89))
    assert(ranking(Vector.empty, 0.1, 5) == Vector.empty)
    assert(ranking(meds, 0.90, 5) == Vector.empty)
  }

  test("ranking: maneja tope p <= 0 y umbral 0.0 correctamente") {
    val meds = Vector((0, 1, 0.0), (1, 2, 0.3))
    assert(ranking(meds, 0.0, 0) == Vector.empty)
    assert(ranking(meds, 0.0, 2) == Vector((1, 2, 0.3), (0, 1, 0.0)))
  }


  // Pruebas detectarDirecto


  test("detectarDirecto: identifica documentos identicos con similitud 1.0") {
    val c = Vector(
      ("D0", normalizar("El sol brilla sobre el mar azul")),
      ("D1", normalizar("El sol brilla sobre el mar azul")),
      ("D2", normalizar("La luna ilumina la noche oscura"))
    )
    val res = detectarDirecto(c, 3, 0.5, 5)
    assert(res == Vector((0, 1, 1.0)))
  }

  test("detectarDirecto: ubica en primer lugar la copia parcial sembrada con corpusConCopias") {
    val c = corpusConCopias(6, 40, 25, 1, 0.85)
    val res = detectarDirecto(c, 3, 0.3, 3)
    assert(res.nonEmpty)
    assert(res.head._1 == 0 && res.head._2 == 6)
  }

  test("detectarDirecto: documentos mas cortos que k producen huella vacia y similitud 0") {
    val c = Vector(
      ("D0", Vector("hola", "mundo")),
      ("D1", Vector("hola", "mundo")),
      ("D2", Vector("solo"))
    )
    assert(detectarDirecto(c, 4, 0.01, 5) == Vector.empty)
  }

  test("detectarDirecto: corpus vacio o de un solo documento retorna ranking vacio") {
    val c1 = Vector(("D0", normalizar("Un unico texto en el repositorio")))
    assert(detectarDirecto(Vector.empty, 3, 0.1, 5) == Vector.empty)
    assert(detectarDirecto(c1, 3, 0.1, 5) == Vector.empty)
  }

  test("detectarDirecto: aplica umbral y tope p sobre similitudes escalonadas") {
    val c = Vector(
      ("D0", Vector("a", "b", "c", "d")),
      ("D1", Vector("a", "b", "c", "x")),
      ("D2", Vector("a", "b", "y", "z")),
      ("D3", Vector("a", "b", "c", "d"))
    )
    val res = detectarDirecto(c, 2, 0.5, 2)
    assert(res == Vector((0, 3, 1.0), (0, 1, 0.5)))
  }


  // Pruebas detectarConIndice


  test("detectarConIndice: produce el mismo ranking que detectarDirecto para umbral > 0") {
    val c = Vector(
      ("D0", normalizar("los algoritmos funcionales evitan el estado mutable")),
      ("D1", normalizar("los algoritmos funcionales usan recursion e inmutabilidad")),
      ("D2", normalizar("la musica clasica requiere disciplina y practica diaria")),
      ("D3", normalizar("el estado mutable dificulta la concurrencia segura"))
    )
    val directo = detectarDirecto(c, 2, 0.1, 10)
    val conIndice = detectarConIndice(c, 2, 0.1, 10)
    assert(conIndice == directo)
  }

  test("detectarConIndice: retorna vacio cuando ningun par comparte fragmentos") {
    val c = Vector(
      ("D0", normalizar("primavera verano otono invierno")),
      ("D1", normalizar("mercurio venus tierra marte")),
      ("D2", normalizar("triangulo cuadrado pentagono hexagono"))
    )
    assert(detectarConIndice(c, 2, 0.01, 5) == Vector.empty)
  }

  test("detectarConIndice: calcula Jaccard exacto con repeticion interna de fragmentos") {
    val c = Vector(
      ("D0", Vector("uno", "dos", "uno", "dos", "uno")),
      ("D1", Vector("uno", "dos", "tres"))
    )
    val res = detectarConIndice(c, 2, 0.3, 5)
    assert(res.length == 1)
    assert(res.head._1 == 0 && res.head._2 == 1)
    assert(math.abs(res.head._3 - (1.0 / 3.0)) < 1e-9)
  }

  test("detectarConIndice: maneja documentos con t < k mezclados con documentos validos") {
    val c = Vector(
      ("D0", Vector("corto")),
      ("D1", normalizar("arquitectura moderna de software distribuido")),
      ("D2", normalizar("arquitectura moderna de sistemas concurrentes")),
      ("D3", Vector("dos", "palabras"))
    )
    val res = detectarConIndice(c, 3, 0.2, 5)
    assert(res == detectarDirecto(c, 3, 0.2, 5))
    assert(res.map(m => (m._1, m._2)) == Vector((1, 2)))
  }

  test("detectarConIndice: coincide con detectarDirecto en corpusConCopias") {
    val c = corpusConCopias(8, 50, 30, 2, 0.75)
    val resIndice = detectarConIndice(c, 3, 0.25, 5)
    val resDirecto = detectarDirecto(c, 3, 0.25, 5)
    assert(resIndice == resDirecto)
  }


}
