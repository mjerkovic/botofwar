import collection.mutable
import java.util.Random
import scala.Predef._
import scala.Some

class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}

class ControlFunction {
  def respond(input: String) : String = {
    val (opcode, paramMap) = CommandParser(input)
    if (opcode == "React") {
      val view = View(paramMap("view"))
      if (paramMap("generation").toInt == 0) {
        new MasterBot().react(paramMap, view)
      } else {
        new MiniBot().react(paramMap, view)
      }
    } else
      ""
  }
}

trait Bot {
  def react(paramMap: Map[String, String], view: View) : String
}

class MasterBot extends Bot {

    val rnd = new Random()
    
    override def react(paramMap: Map[String, String], view: View) : String = {
      val cmd = view.offsetToNearest('m') match {
          case Some(offset) =>
              return "Spawn(direction=" + offset + ",target=m,targetHeading="+ offset +"energy=100)|Status(text=m)|"
          case None =>
            ""
      }
      val heading = findHeading(view, rnd)
      cmd + "Move(direction=" + heading + ")"
    }
    
    private def findHeading(view: View, rnd: Random) = {
        view.offsetToNearest(BotCharacter.Zugar) match {
            case Some(offset) =>
                offset.signum
            case None => {
                var score: Double = -0.1
                var heading: XY = XY.Zero
                XY.directions.foreach(x => {
                    val cellScore = Desirability.getDesirability(view.cellAtRelPos(x))
                    if (cellScore > score) {
                        score = cellScore
                        heading = x
                    }      
                })
                heading                
            }
        }
    }
    
}

class MiniBot extends Bot {
    override def react(paramMap: Map[String, String], view: View) : String = {
      view.offsetToNearest(paramMap("target")(0)) match {
            case Some(offset) =>
                "Move(direction=" + XY(paramMap("targetHeading")) + ")"
            case None => 
                "Explode(size=10)"
        }    
    }
}

object BotCharacter {
    val EmptyCell = '_'
    val Zugar = 'P'
    val Fluppet = 'B'
    val Me = 'M'
    val EnemyBot = 'm'
    val Wall = 'W'
    val Unknown = '?'
    
}

object Desirability {
    val desirability = new mutable.HashMap[Char, Double]
    desirability += (BotCharacter.EmptyCell -> 0.1)
    
    def getDesirability(botChar: Char) = {
        botChar match {
            case BotCharacter.EmptyCell =>
                0.1
            case BotCharacter.Zugar =>
                0.7
            case BotCharacter.Fluppet =>
                0.9
            case _ =>
                0.0
        }
    }
}

case class XY(x: Int, y: Int) {
    def isNonZero = x!=0 || y!=0
    def isZero = x==0 && y==0
    def isNonNegative = x>=0 && y>=0

    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x+dx, y)
    def addToY(dy: Int) = XY(x, y+dy)

    def +(pos: XY) = XY(x+pos.x, y+pos.y)
    def -(pos: XY) = XY(x-pos.x, y-pos.y)
    def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)

    def distanceTo(pos: XY) : Double = (this-pos).length
    def length : Double = math.sqrt(x*x + y*y)

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)    

    override def toString = x + ":" + y

}
object XY {
    val Zero = XY(0,0)
    val One =  XY(1,1)
    val Right      = XY( 1,  0)
    val RightUp    = XY( 1, -1)
    val Up         = XY( 0, -1)
    val UpLeft     = XY(-1, -1)
    val Left       = XY(-1,  0)
    val LeftDown   = XY(-1,  1)
    val Down       = XY( 0,  1)
    val DownRight  = XY( 1,  1)
    
    val directions = List(Right, RightUp, Up, UpLeft, Left, LeftDown, Down, DownRight)
    
    def apply(s: String) : XY = {
        val xy = s.split(':').map(_.toInt)
        XY(xy(0), xy(1))
    }
    
    def random(rnd: Random) = XY(rnd.nextInt(3) - 1, rnd.nextInt(3) - 1)
    
}

object CommandParser {
    def apply(command: String) = {
        def splitParam(param: String) = {
            val segments = param.split('=')
            if( segments.length != 2 )
                throw new IllegalStateException("invalid key/value pair: " + param)
            (segments(0),segments(1))
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)

        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map( splitParam ).toMap
        (segments(0), keyValuePairs)
    }
}

case class View(cells: String) {
    val size = math.sqrt(cells.length).toInt
    val center = XY(size/2, size/2)

    def apply(relPos: XY) = cellAtRelPos(relPos)

    def indexFromAbsPos(absPos: XY) = absPos.x + (absPos.y * size)
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.apply(indexFromAbsPos(absPos))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells(indexFromRelPos(relPos))
    def offsetToNearest(c: Char) = {
        var nearestPosOpt : Option[XY] = None
        var nearestDistance = Double.MaxValue
        for(i <- 0 until cells.length) {
            if(c == cells(i)) {
                val pos = absPosFromIndex(i)
                val distanceToCenter = pos.distanceTo(center)
                if(distanceToCenter < nearestDistance) {
                    nearestDistance = distanceToCenter
                    nearestPosOpt = Some(pos - center)
                }
            }
        }
        nearestPosOpt
    }
}