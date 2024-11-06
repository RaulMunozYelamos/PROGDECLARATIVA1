package prac1

import scala.annotation.tailrec

trait ArbolHuffman {

  def peso(): Int = this match {
    case HojaHuff(caracter, pesoHoja)=> pesoHoja
    case RamaHuff(nodoizq, nododch) => nodoizq.peso() + nododch.peso()
  }

  def caracteres(): List[Char] = this match {
    case RamaHuff(nodoizq, nododch) => nodoizq.caracteres() ::: nododch.caracteres()
    case HojaHuff(caracter, _) => List(caracter)
  }







  private def decoAux[A](arbolA: ArbolHuffman, bits: List[A], cadena: String): String = (this, bits) match {
    case (HojaHuff(caracter, pesoHoja), _) => arbolA.decoAux(arbolA, bits, caracter + cadena)
    case (_, Nil) => cadena
    case (RamaHuff(izquierda, derecha), h :: t) => if (h.equals(1)) derecha.decoAux(arbolA, t, cadena) else izquierda.decoAux(arbolA, t, cadena)

  }

  def decodificar[A](bits: List[A]): String = {
    this.decoAux(this, bits, "").reverse
  }


  def buscarcaracter(c:Char):Boolean=this match {
    case RamaHuff(nodoizq, nododch) => nodoizq.buscarcaracter(c) || nododch.buscarcaracter(c)
    case HojaHuff(caracter, pesoHoja) if caracter==c=>true
    case HojaHuff(caracter, pesoHoja) if caracter!=c => false
  }



  def codificar(cadena:String): List[Int] = {
    val listaChars= cadena.toList
    listaChars.flatMap(char => this.codificarchar(char, List()))
  }



  def codificarchar(char:Char, listaBits:List[Int]):List[Int] = this match {
    case RamaHuff(nodoizq,nododch) if nodoizq.buscarcaracter(char)=> nodoizq.codificarchar(char, List(0):::listaBits)
    case RamaHuff(nodoizq,nododch) if nododch.buscarcaracter(char)=> nododch.codificarchar(char, List(1):::listaBits)
    case HojaHuff(caracter, pesoHoja) if caracter==char => listaBits.reverse ///recursividad con la cola para buscar el resto de caracteres
  }

}


case class RamaHuff(nodoizq: ArbolHuffman, nododch: ArbolHuffman) extends ArbolHuffman

case class HojaHuff(caracter: Char, pesoHoja: Int) extends ArbolHuffman


object ArbolHuffman{
    def main(args: Array[String]): Unit = {
      val arbolWiki: ArbolHuffman = RamaHuff( // https://en.wikipedia.org/wiki/File:Huffman_tree_2.svg
        RamaHuff(
          RamaHuff(
            HojaHuff(' ', 7),
            RamaHuff(
              HojaHuff('a', 4),
              HojaHuff('e', 4)
            )
          ),
          RamaHuff(
            RamaHuff(
              HojaHuff('f', 3),
              RamaHuff(
                HojaHuff('h', 2),
                HojaHuff('i', 2)
              )
            ),
            RamaHuff(
              HojaHuff('m', 2),
              HojaHuff('t', 2)
            )
          )
        ),
        RamaHuff(
          RamaHuff(
            RamaHuff(
              HojaHuff('n', 2),
              HojaHuff('s', 2)
            ),
            RamaHuff(
              HojaHuff('l', 1),
              HojaHuff('o', 1)
            )
          ),
          RamaHuff(
            RamaHuff(
              HojaHuff('p', 1),
              HojaHuff('r', 1)
            ),
            RamaHuff(
              HojaHuff('u', 1),
              HojaHuff('x', 1)
            )
          )
        )
      )
      println(arbolWiki.peso())
      println(arbolWiki.caracteres())
      val listabits = arbolWiki.codificar("u")
      println(listabits)
      val lista= arbolWiki.decodificar(listabits)
      println(lista)
      println(ListaCharsADistFrec(cadenaAListChars("22333666666")))
      println(DistribFrecAListaHojas(ListaCharsADistFrec(cadenaAListChars("22333666666"))))
      println(combinar(DistribFrecAListaHojas(ListaCharsADistFrec(cadenaAListChars("22333666666")))))
      println(repetirHasta(combinar,esListaSingleton)(DistribFrecAListaHojas(ListaCharsADistFrec(cadenaAListChars("22333666666")))))
    }

  //def crearArbolHuffman(cadena:String):ArbolHuffman= {

    //ListaCharsADistFrec y sus auxiliares
    def contarcaracter(caracter:Char, contador:Int, listaBuscar:List[Char]): Int = listaBuscar match {
      case h::Nil if h!=caracter=> contador
      case h::Nil if h==caracter=> contador+1
      case h::t if h!=caracter=> contarcaracter(caracter, contador, t)
      case h::t if h==caracter => contarcaracter(caracter, contador+1, t)
    }

    def tuplaCaracter(caracter:Char, listaBuscar:List[Char]): (Char, Int) =
      (caracter, contarcaracter(caracter, 0, listaBuscar))

    def ListaCharsADistFrec(listachar:List[Char]):List[(Char, Int)]= listachar match {
      case h::Nil => List(tuplaCaracter(h, listachar))
      case h::t => List(tuplaCaracter(h,listachar)) ::: ListaCharsADistFrec(t)
    }

    //DistribFrecAListaHojas y sus funciones auxiliares

    private def tuplaAHoja(tupla:(Char,Int)):HojaHuff=
      HojaHuff(tupla._1, tupla._2)


    def DistribFrecAListaHojas(frec:List[(Char, Int)]):List[HojaHuff] = {
      val frecordenada= frec.sortBy(_._2)
      frecordenada.map(tuplaAHoja)
    }


  def listaCharsACadena(listaCaracteres: List[Char]): String = {
    @tailrec
    def listaCharsCadenaAux(lista: List[Char], cadena: String): String = lista match {
      case Nil => cadena.reverse
      case head :: tail => listaCharsCadenaAux(tail, head + cadena)
    }
    listaCharsCadenaAux(listaCaracteres, "")
  }
    //Creación del árbol codificado a partir de la lista de hojas




    def crearRamaHuff(izq:ArbolHuffman, dch: ArbolHuffman): RamaHuff = {
       RamaHuff(izq,dch)
    }




    def combinar(nodos:List[ArbolHuffman]):List[ArbolHuffman] = {
      def auxCombinar(rama: RamaHuff, lista: List[ArbolHuffman]): List[ArbolHuffman] = lista match{
        case Nil => List(rama)
        case h::t if (rama.peso()<=h.peso()) => rama :: lista
        case h::t if (rama.peso()>h.peso()) => h :: auxCombinar(rama,t )
      }
    if (nodos.length<1) return Nil
    if (nodos.length==1) return List(nodos.head)
    auxCombinar(crearRamaHuff(nodos.head, nodos.tail.head), nodos.tail)
    }

    def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista match {
      case h::t => if (lista.isEmpty) true else false
      case _ => false
    }

  def cadenaAListChars(cadena: String): List[Char] = {
    cadena.toList
  }




    //cada vez que queremos utilizarla meter en f la funcion combinar y en g la funcion eslistasingleton
    def repetirHasta(f: List[ArbolHuffman] => List[ArbolHuffman], g: List[ArbolHuffman]=> Boolean)(listaNodos: List[ArbolHuffman]): List[ArbolHuffman] =   {
      if (g(listaNodos)) return listaNodos
      else repetirHasta(f,g)(f(listaNodos))
    }


  //}

  //CONSTRUCTOR DEL OBJETO

  // apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)

}




// Pregunta 1 --> Estructura, que hay que entregar, si hay q poner abstract class
// Pregunta 2 --> Que es el apply, donde se pone y pa que sirve



