import scala.annotation.tailrec
import scala.collection.IterableOnce.iterableOnceExtensionMethods



trait ArbolHuffman {



    case class RamaHuff(nodoizq: ArbolHuffman, nododch: ArbolHuffman) extends ArbolHuffman

    case class HojaHuff(caracter: Char, pesoHoja: Int) extends ArbolHuffman

    def peso(): Int = this match {
      case RamaHuff(nodoizq, nododch) => nodoizq.peso() + nododch.peso()
      case HojaHuff(caracter, pesoHoja)=> pesoHoja

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



      def decodificar(bits: List[Int], arbol: ArbolHuffman): String = {
      @tailrec
      def decoAux(arbolA: ArbolHuffman, arbolB: ArbolHuffman, bits: List[Int], cadena: String): String = (arbolB, bits) match {
        case (HojaHuff(char, peso), _) => decoAux(arbolA, arbolA, bits, char + cadena)
        case (_, Nil) => cadena
        case (RamaHuff(nodoizq, nododch), head :: tail) => if (head.equals('1')) decoAux(arbolA, nododch, tail, cadena) else decoAux(arbolA, nodoizq, tail, cadena)
      }

      decoAux(arbol, arbol, bits, "")
    }


    def buscarcaracter(c:Char):Boolean=this match {
      case RamaHuff(nodoizq, nododch) => nodoizq.buscarcaracter(c) || nododch.buscarcaracter(c)
      case HojaHuff(caracter, pesoHoja) if caracter==c=>true
      case HojaHuff(caracter, pesoHoja) if caracter!=c => false
    }



    def codificar(cadena:String): List[Int] = {
      val listaChars= cadenaAListChars(cadena)
      //listaChars.flatMap(codificarchar())

      val listafinal=codificarchar(listaChars.head, List()) :::codificar(listaCharsACadena(listaChars.tail))
      listafinal
    }



    def codificarchar(char:Char, listaBits:List[Int]):List[Int] = this match {
      case RamaHuff(nodoizq,nododch) if nodoizq.buscarcaracter(char)=> nodoizq.codificarchar(char, List(0):::listaBits)
      case RamaHuff(nodoizq,nododch) if nododch.buscarcaracter(char)=> nododch.codificarchar(char, List(1):::listaBits)
      case HojaHuff(caracter, pesoHoja) if caracter==char => listaBits.reverse ///recursividad con la cola para buscar el resto de caracteres
    }



  def crearArbolHuffman(cadena:String):ArbolHuffman= {

    /*    def contarcaracter(caracter:Char, contador:Int, listaBuscar:List[Char]): Int = listaBuscar match {
      case h::Nil if h!=caracter=> contador
      case h::Nil if h==caracter=> contador+1
      case h::t if h!=caracter=>contarcaracter(caracter, contador, t)
      case h::t if h==caracter =>contarcaracter(caracter, contador+1, t)
    }

     */

    /*
    def ListaCharsADistFrec(listachar:List[Char]):List[(Char, Int)]= {
      def auxListaCharADistFrec(listacadena:List[Char]):List[(Char, Int)] = listacadena match{
        case head::tail => List((head, contarcaracter(head, 0, listacadena))) ::: auxListaCharADistFrec(tail)


      }
    }

     */




  }

}

