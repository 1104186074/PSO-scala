package PSO_scalar

/**
 * Created by jnu on 2015/12/9.
 */
//求函數最小值
object mainPSO {
  def main(args: Array[String]): Unit = {

    val popSize=5
    val c1=1.49
    val c2=1.49
    val w=0.5
    val maxgen=100
    val dim=2
    val xminmax=Array[Double](-8,8,-10,10)
    val result=(new PSO).fun(new Fitness,popSize,c1,c2,w,maxgen,dim,xminmax)
    print(result(0))
    result.foreach(println)
  }
}
