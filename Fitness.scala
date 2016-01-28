package PSO_scalar

/**
 * Created by jnu on 2015/12/10.
 */

import scala.math._
class Fitness {
  /**
   *定义适应度函数计算方法，以x^3-y^2函数为例
   * @param x 输入变量
   * @return   优化函数的值
   */
  def function(x:Array[Double]):Double ={
    val m=x.length//变量x的个数
    val X=x
    var out:Double=0
    out=pow(X(0),3)-pow(X(1),2)
//    for(i<-0 until m){
//    var temp=20*exp(-0.2*sqrt((1/m)*pow(X,2)))+exp((1/m)*cos(2*Pi*X(i)))
//      //(X.map(-0.2*sqrt(1/m)*pow(_,2))).reduce()
//    }
//    out=(20+exp(1))+out
    out
  }
  def function2(x:Array[Double]):Double = {
    val m = x.length //变量x的个数
    var out: Double = 0
    out = -5*sin(x(0))*sin(x(1))*sin(x(2))*sin(x(3))*sin(x(4))-sin(5*x(0))*sin(5*x(1))*sin(5*x(2)) *sin(5*x(3))*sin(5*sin(4))+8
    1/out
  }
}
