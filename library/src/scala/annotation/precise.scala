package scala.annotation

/** This annotation can only be used on an upper bound of a type parameter, it implies that this reference will keep
 *  the most precise type without growing arbitrarily, collapsing unions of singletons to a singleton where they have
*   the same underlying types
 */
final class precise extends StaticAnnotation
