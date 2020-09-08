package com.github.gpoirier

import cats._
import cats.data._
import cats.implicits._
import monocle.Lens

package object barrage {
  implicit class LensOps[S1, S2](private val self: Lens[S1, S2]) extends AnyVal {
    def composeWith[F[_]: Monad, A](m: StateT[F, S2, A]): StateT[F, S1, A] = StateT[F, S1, A] { s1 =>
      m.run(self.get(s1)) map {
        case (s2, a) =>
          self.set(s2)(s1) -> a
      }
    }
  }
}
