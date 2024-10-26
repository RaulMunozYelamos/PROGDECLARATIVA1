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





    def codificar(cadena:String):List[Bit] ={

      @tailrec
      def codificaraux(cadenaAListChars:List[Char], lista:List[Bit]):List[Bit]=this match {
        case RamaHuff(nodoizq,nododch) if nodoizq.buscarcaracter(cadenalistchar.head)=> codificaraux(cadena,0:::lista)
        case RamaHuff(nodoizq,nododch) if nododch => codificaraux(cadena.tail, 1:::lista)
        case HojaHuff(caracter, pesoHoja)=> buscarcaracter(cadena.head)

      }
    }


}

