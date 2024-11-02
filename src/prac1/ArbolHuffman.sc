import scala.annotation.tailrec


type Bit = 0 | 1
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


    def decodificar(bits: List[Bit], arbol: ArbolHuffman): String = {
      @tailrec
      def decoAux(arbolA: ArbolHuffman, arbolB: ArbolHuffman, bits: List[Bit], cadena: String): String = (arbolB, bits) match {
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
      codificarB(cadenaAListChars(cadena), List())
    }

    def codificarB(listchar:List[Char], listaBits:List[Int]):List[Int] = this match {
      case RamaHuff(nodoizq,nododch) if nodoizq.buscarcaracter(listchar.head)=> nodoizq.codificarB(listchar, List(0):::listaBits)
      case RamaHuff(nodoizq,nododch) if nododch.buscarcaracter(listchar.head)=> nododch.codificarB(listchar, List(1):::listaBits)
      case HojaHuff(caracter, pesoHoja) if this.buscarcaracter(listchar.head) => listaBits ///recursividad con la cola para buscar el resto de caracteres
    }


}

