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

  def cadenaAListChars(cadena: String): List[Char] = {
    cadena.toList
  }


  def listaCharsACadena(listaCaracteres: List[Char]): String = {
    @tailrec
    def listaCharsCadenaAux(lista: List[Char], cadena: String): String = lista match {
      case Nil => cadena.reverse
      case head :: tail => listaCharsCadenaAux(tail, head + cadena)
    }
    listaCharsCadenaAux(listaCaracteres, "")
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
    val listaChars= cadenaAListChars(cadena)
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

    }



  def crearArbolHuffman(cadena:String):ArbolHuffman= {

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

    def tuplaAHoja(tupla:(Char,Int)):HojaHuff=
      new HojaHuff(tupla._1, tupla._2)


    def DistribFrecAListaHojas(frec:List[(Char, Int)]):List[HojaHuff] = {
      val frecordenada= frec.sortBy(_._2)
      frecordenada.map(tuplaAHoja)
    }

    //Creación del árbol codificado a partir de la lista de hojas




    def crearRamaHuff(izq:ArbolHuffman, dch: ArbolHuffman): RamaHuff = {
       RamaHuff(izq,dch)
    }


    def ordenpeso(listaarbol:List[ArbolHuffman]):List[ArbolHuffman] =listaarbol match {
      case h1::h2::tail => if h1.peso()>h2.peso
    }

    def combinar(nodos:List[ArbolHuffman]):List[ArbolHuffman] =  nodos match {
      case Nil => List()
      case h1::Nil =>List(h1)
      case h1::h2::Nil => List(crearRamaHuff(h1,h2))
      case h1::h2::tail =>
        val listaHuff = List(crearRamaHuff(h1,h2))::: tail
        combinar(ordenpeso(listaHuff))///FALTA QUE ORDENE
    }
  }
}




/////
