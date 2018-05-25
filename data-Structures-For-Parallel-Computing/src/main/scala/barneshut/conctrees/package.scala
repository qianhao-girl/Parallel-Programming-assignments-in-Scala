package barneshut

package object conctrees {

  // 隐式类：（Scala 2.0 后添加的的新类型），即被implicit修饰的类。作用是对类的加强

  implicit class ConcOps[T](val self: Conc[T]) extends AnyVal {  // 注意foreach,<> 是Conc[T]类的optional方法，要使用直接导入需要的作用域内（须无冲突）
    def foreach[U](f: T => U) = Conc.traverse(self, f)
    def <>(that: Conc[T]) = Conc.concatTop(self.normalized, that.normalized)
  }

}

 // 例子：
/*  import scala.io.Source
    import java.io.File

    object Context_Helper{                    // 隐式类只能在trait/object/class 的内部定义，且构造函数只能带有一个非隐式参数
      implicit class lmpInt(val x: Int){      // implicit case class:  非法，case class with immutable variable could pattern match,是因为自动生成了额外的方法
        def add(x2: Int) = x1 + x2
      }                                       // 为Int类增加了add方法

      implicit class FileEnhance(file: File) {
        def read() = Source.fromFile(file.getPath).mkString  //为File类增加的方法
      }
    }

    object implicit_Class{
      import Context_Helper._            // 将隐式类们引入该无冲突的作用域。无冲突： 在同一作用域内没有有任何方法、成员、或对象与隐式类同名，所以不能用于Case类

      def main(args: Array[String]){
        println( 1.add(2))

        println(new File('D:\\...').read())
      }
    }
    */ //分析：
                // 1. 1.add(2) 编译器不会直接报错，而是在




// 隐式类型转换：（自动类型转换），与显示类型转换（强制类型转换）相对，指不需要代码书写，由系统自动完成的类型转换。
