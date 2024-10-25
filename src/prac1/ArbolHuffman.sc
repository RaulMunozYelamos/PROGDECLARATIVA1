import scala.annotation.tailrec

object ArbolHuffman
  trait ArbolHuffman
  type Bit =0 || 1
  case class RamaHuff(nodoizq: ArbolHuffman , nododch: ArbolHuffman) extends ArbolHuffman
  case class HojaHuff(caracter: Char, peso:Int) extends ArbolHuffman

  def peso(arbol:ArbolHuffman): Int = arbol match {
    case RamaHuff(nodoizq, nododch) => peso(nodoizq) + peso(nododch)
    case HojaHuff(_, peso)=> peso
  }

  def caracteres(arbol: ArbolHuffman): List[Char] = arbol match {
    case RamaHuff(nodoizq,nododch) => caracteres(nodoizq) ::: caracteres(nododch)
    case HojaHuff(caracter, _)=> List(caracter)
  }

  def cadenaAListChars(cadena: String):List [Char] =
    cadena.toList


  def listaCharsACadena(listaCaracteres:List[Char]):String = {
    @tailrec
    def listaCharsCadenaAux (lista:List[Char], cadena: String): String = lista match{
      case Nil => cadena.reverse
      case head::tail => listaCharsCadenaAux(tail, head+cadena)

    }
    listaCharsCadenaAux(listaCaracteres, "")
  }



def decodificar (bits:List[Bit], arbol:ArbolHuffman):String = {
  def decoAux(arbolA: ArbolHuffman, arbolB:ArbolHuffman, bits:List[Bit], cadena:String) : String = (arbolB, bits) match{
    case (HojaHuff(char, peso), _) => decoAux(arbolA, arbolA, bits, char+cadena)
    case (_,Nil) => cadena
    case (RamaHuff(nodoizq, nododch), h::t) => if (h.equals('1')) decoAux(arbolA, nododch, t, cadena) else decoAux(arbolA, nodoizq, t, cadena)
  }
  decoAux(arbol,arbol, bits, "")
}


