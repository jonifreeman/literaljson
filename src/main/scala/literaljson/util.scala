package literaljson

import java.lang.reflect._

object Util {
  def parametrizedType(f: Field): Class[_] = {
    val ptype = f.getGenericType.asInstanceOf[ParameterizedType]
    ptype.getActualTypeArguments()(0).asInstanceOf[Class[_]]
  }
}
