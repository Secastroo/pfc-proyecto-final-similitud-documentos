package proyecto

import scala.util.Random

/**
 * Detección de similitud entre documentos — versión secuencial.
 *
 * Cada documento se resume por el conjunto de sus fragmentos de k palabras
 * consecutivas, y dos documentos se comparan por el índice de Jaccard de esos
 * conjuntos. El índice invertido evita comparar las parejas que no comparten
 * ningún fragmento, cuya similitud es cero de antemano.
 *
 * Complete cada función. Las firmas y los tipos no se modifican.
 */
object Similitud {

  // Un fragmento es la secuencia de k palabras consecutivas.
  type Fragmento = Vector[String]

  // Un documento es una pareja con el id y sus palabras normalizadas.
  type Documento = (String, Vector[String])
  type Corpus = Vector[Documento]

  // La huella de un documento es el conjunto de sus fragmentos.
  type Huella = Set[Fragmento]

  // El indice invertido asocia a cada fragmento los documentos en los que
  // aparece, identificados por su posicion en el corpus.
  type Indice = Map[Fragmento, Set[Int]]

  // Una pareja de documentos, siempre con i < j, y una pareja medida.
  type Pareja = (Int, Int)
  type Medicion = (Int, Int, Double)

  // ---------------------------------------------------------------------------
  // Generación de entradas (ya implementadas, no se modifican)
  // ---------------------------------------------------------------------------

  val random = new Random()

  def documentoAlAzar(id: String, t: Int, v: Int): Documento =
    (id, Vector.fill(t)("p" + random.nextInt(v)))

  def corpusAlAzar(n: Int, t: Int, v: Int): Corpus =
    Vector.tabulate(n)(i => documentoAlAzar("D" + i, t, v))

  def copiaParcial(d: Documento, id: String, f: Double, v: Int): Documento = {
    val tomadas = (d._2.length * f).toInt
    (id, d._2.take(tomadas) ++
      Vector.fill(d._2.length - tomadas)("p" + random.nextInt(v)))
  }

  def corpusConCopias(n: Int, t: Int, v: Int, c: Int, f: Double): Corpus = {
    val base = corpusAlAzar(n, t, v)
    base ++ Vector.tabulate(c)(i => copiaParcial(base(i), "C" + i, f, v))
  }

  // ---------------------------------------------------------------------------
  // Exploración de entradas (ya implementadas, no se modifican)
  // ---------------------------------------------------------------------------

  def idDoc(d: Documento): String = d._1
  def palabrasDoc(d: Documento): Vector[String] = d._2
  def largoDoc(d: Documento): Int = d._2.length

  // ---------------------------------------------------------------------------
  // Aquí empieza su trabajo
  // ---------------------------------------------------------------------------

  /**
   * Pasa el texto a minúsculas, convierte en separador todo carácter que no
   * sea letra o dígito y devuelve el vector de palabras no vacías.
   */
  def normalizar(texto: String): Vector[String] = {
    // Todo lo que no es letra o dígito se vuelve un espacio, que separa palabras.
    val limpio = texto.toLowerCase.map(c => if (c.isLetterOrDigit) c else ' ')
    // Al partir por espacios pueden quedar cadenas vacías: se descartan.
    limpio.split(' ').toVector.filter(palabra => palabra.nonEmpty)
  }
  /**
   * Los t - k + 1 fragmentos de k palabras consecutivas, en el orden en que
   * aparecen; el vector vacío si el documento tiene menos de k palabras.
   */
  def fragmentos(ts: Vector[String], k: Int): Vector[Fragmento] =
    // Caso base: no alcanzan k palabras para un fragmento (o k no es positivo).
    if (k <= 0 || ts.length < k) Vector()
    // Paso: el fragmento que empieza en la primera palabra, seguido de los que
    // salen de quitarla.
    else ts.take(k) +: fragmentos(ts.tail, k)

  /** El conjunto de fragmentos del documento: un repetido cuenta una vez. */
  def huella(d: Documento, k: Int): Huella =
    fragmentos(palabrasDoc(d), k).toSet
  /** Una huella por documento del corpus, en el mismo orden. */
  def huellas(c: Corpus, k: Int): Vector[Huella] =
    for (d <- c) yield huella(d, k)
  /**
   * Índice de Jaccard de dos huellas, el tamaño de la intersección sobre el de
   * la unión. Dos huellas vacías dan cero y no dividen por cero.
   */
  def jaccard(h1: Huella, h2: Huella): Double ={
    // |A ∩ B| se cuenta una sola vez y |A ∪ B| = |A| + |B| - |A ∩ B|.
    val interseccion = h1.count(h2.contains)
    val union = h1.size + h2.size - interseccion
    if (union == 0) 0.0 else interseccion.toDouble / union
  }
  /** Las n (n - 1) / 2 parejas (i, j) con i < j, en orden. */
  def parejas(n: Int): Vector[Pareja] =
    (for {i <- 0 until n
      j <- i + 1 until n
    } yield (i, j)).toVector

  /** La similitud de cada pareja que se le pase, en el mismo orden. */
  def similitudes(hs: Vector[Huella], ps: Vector[Pareja]): Vector[Medicion] =
    ps.map { case (i, j) => (i, j, jaccard(hs(i), hs(j))) }
  /**
   * El mapa de cada fragmento al conjunto de documentos donde aparece, con los
   * documentos identificados por su posición en el vector de huellas.
   */
  def indiceInvertido(hs: Vector[Huella]): Indice = {
    val pares = for {
      (h, i) <- hs zip (0 until hs.length)
      g <- h
    } yield (g, i)
    pares.groupBy(_._1).map { case (g, ps) => (g, ps.map(_._2).toSet) }
  }

  /**
   * Las parejas que comparten al menos un fragmento, sin repetir y ordenadas.
   * Una misma pareja puede salir de varias entradas del índice.
   */
  def parejasCandidatas(idx: Indice): Vector[Pareja] = {
    val todas = for {
      docs <- idx.values.toVector
      i <- docs.toVector
      j <- docs.toVector
      if i < j
    } yield (i, j)
    todas.distinct.sorted
  }

  /**
   * Las mediciones que alcanzan el umbral, de mayor a menor similitud y
   * truncadas a p. Los empates se rompen por posición, de modo que el
   * resultado no dependa del orden en que llegaron las mediciones.
   */
  def ranking(ss: Vector[Medicion], umbral: Double, p: Int): Vector[Medicion] = {
    if (p <= 0) Vector.empty
    else {
      ss.filter(m => m._3 >= umbral)
        .sortBy(m => (-m._3, m._1, m._2))
        .take(p)
    }
  }

  /** Solución directa: compara las n (n - 1) / 2 parejas. */
  def detectarDirecto(c: Corpus, k: Int, umbral: Double, p: Int): Vector[Medicion] = {
    val hs = huellas(c, k)
    val ps = parejas(c.length)
    val ss = similitudes(hs, ps)
    ranking(ss, umbral, p)
  }

  /** Solución con índice: compara solo las parejas candidatas. */
  def detectarConIndice(c: Corpus, k: Int, umbral: Double,  p: Int): Vector[Medicion] = {
    val hs = huellas(c, k)
    val idx = indiceInvertido(hs)
    val ps = parejasCandidatas(idx)
    val ss = similitudes(hs, ps)
    ranking(ss, umbral, p)
  }
}
