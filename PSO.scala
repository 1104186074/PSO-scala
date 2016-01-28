package PSO_scalar

/**
 * Created by jnu on 2015/12/8.
 */

import scala.util.Random

class PSO {
  /**
   *粒子群算法实现主程序
   * @param fitness 适应度函数
   * @param popSize  种群个数
   * @param c1   学习因子1，默认为1.5
   * @param c2   学习因子2，默认为1.5
   * @param wei    惯性权重
   * @param maxGen  最大迭代次数
   * @param Dim   变量x的维度
   *              输出最佳x和fitness
   */

  def fun(fitness: Fitness,popSize:Int,c1:Double,c2:Double,wei:Double,maxGen:Int,Dim:Int,xminmax:Array[Double]): Array[Any] ={
    val matrix_X=Array.ofDim[Double](popSize,Dim)//生成候选解集X
    val matrix_V=Array.ofDim[Double](popSize,Dim)//生成候选解集速度V
    val xpbest=Array.ofDim[Double](popSize,Dim)//每个粒子的位置
    val fitness_result=new Array[Double](popSize)
    for (i<-0 until popSize){
      for (j<-0 until Dim){
        matrix_X(i)(j)=(new Random).nextDouble()//0-10之间的double随机数
        matrix_V(i)(j)=(new Random).nextDouble()//0-10之间的double随机数
      }
      //计算初始化每个粒子的适应度
      fitness_result(i)=fitness.function(matrix_X(i))
      xpbest(i)=matrix_X(i)
    }
    //全局最优函数值的位置xgbest
    var xgbest=matrix_X(0)
    //存储所有目标函数全局最小值，便于化收敛图,初始值设为1000
    val gbest=new Array[Double](maxGen)
    //通过循环比较找出全局最优函数值的位置，更新xgbest
    for (k<-1 until popSize){
      if (fitness.function(matrix_X(k))<fitness.function(xgbest)){
        xgbest=matrix_X(k)
      }
    }
    //更新粒子的位置和速度，找出个体最优函数值
    for (m<-0 until maxGen){
      for ( n<-0 until popSize){
        matrix_V(n)=velocity(wei,c1,c2,xpbest(n),matrix_V(n),xgbest)
        matrix_X(n)=sumAndsubtraction(matrix_X(n),matrix_V(n),true)
        //边界条件
        matrix_X(n)=boundaryRepair(matrix_X(n),xminmax,Dim )
        //通过比较找出个体最优函数值及其位置
        if (fitness.function(matrix_X(n))<fitness_result(n)){
          fitness_result(n)=fitness.function(matrix_X(n))
          xpbest(n)=matrix_X(n)
        }
        //通过比较找出群体最优函数值及其位置
        if (fitness_result(n)<fitness.function(xgbest)){
          xgbest=xpbest(n)
        }
      }
      gbest(m)=fitness.function(xgbest)
    }
    xgbest.foreach(println)
   val out= Array(xgbest,fitness.function(xgbest))
    out
  }


  //数和矩阵相乘
  def multiplication(double: Double,array: Array[Double]):Array[Double]={
    array.map(_*double)
  }

  //矩阵相加减法
  /**
   *
   * @param array1 矩阵1
   * @param array2 矩阵2
   * @param b true为加法，false为减法
   * @return
   */
  def sumAndsubtraction(array1: Array[Double],array2: Array[Double],b: Boolean):Array[Double]={
    var result=new Array[Double](array1.length)
    if (array1.length==array2.length) {
      if (b.equals(true)) {
        for (i <- 0 until array1.length) {
          result(i) = array1(i) + array2(i)
        }
      }
      else {
        for (i <- 0 until array1.length) {
          result(i) = array1(i) - array2(i)
        }
      }
    }
    result
  }
  //更新粒子速度
  def velocity(wei:Double,c1:Double,c2:Double,xpbest:Array[Double],matrix_n:Array[Double],xgbest:Array[Double]): Array[Double] ={
    val temp1=multiplication(wei,matrix_n)
    val temp2=sumAndsubtraction(xpbest,matrix_n,false)
    val temp3=sumAndsubtraction(xgbest,matrix_n,false)
    val temp4=multiplication(c1*(new Random).nextDouble(),temp2)
    val temp5=multiplication(c2*(new Random).nextDouble(),temp3)
    val temp6=sumAndsubtraction(temp4,temp5,true)
    val temp7=sumAndsubtraction(temp1,temp6,true)
    temp7
  }
  //边界修正
  def boundaryRepair(matrix_X:Array[Double],xminmax:Array[Double],Dim:Int):Array[Double]={
    for (i<-0 until Dim){
       if (matrix_X(i)<xminmax(2*i)){
         matrix_X(i)=xminmax(2*i)
       }
      if (matrix_X(i)>xminmax(2*i+1)){
        matrix_X(i)=xminmax(2*i+1)
      }
    }
    matrix_X
  }
}

