package fix

import scalafix._
import scala.meta._

// Entry point for macro annotation, this can come from another sbt project.
object Macros {
  def expand(defn: Defn.Class): Tree = q"class ${defn.name}(${Term.Name(defn.name.value.toLowerCase)}: String)"
}

// This is only for testing purposes.
case class Scaladataclasses_v1(mirror: Mirror) extends SemanticRewrite(mirror) {
  def rewrite(ctx: RewriteCtx): Patch = {
    ctx.reporter.info(ctx.tree.syntax)
    ctx.reporter.info(ctx.tree.structure)
    val patch = ctx.tree.collect {
      // adapt to your needs here.
      case cls: Defn.Class =>
        ctx.removeTokens(cls.tokens) +
          ctx.addLeft(cls.tokens.head, Macros.expand(cls).syntax)
    }.asPatch
    ctx.reporter.info(apply(ctx, patch))
    patch
  }
}

