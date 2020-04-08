package com.akolov.formi

package lenses

import org.log4s.getLogger

trait Lens[P, E, A] { self =>
  def get(p: P): Either[E, A]
  def set(p: P, a: A): Either[E, P]

  def modify(p: P)(f: A => A): Either[E, P] = {
    get(p).flatMap { curr =>
      set(p, curr)
    }
  }

  def map[B](fbp: B => P)(fpb: P => B): Lens[B, E, A] = new Lens[B, E, A] {
    override def get(b: B): Either[E, A] = self.get(fbp(b))

    override def set(b: B, a: A): Either[E, B] = self.set(fbp(b), a).map(fpb)
  }
}

object Lens {
  val logger = getLogger

  def compose[P, E, A, B](lp: Lens[P, E, A], la: Lens[A, E, B]): Lens[P, E, B] = new Lens[P, E, B] {

    override def get(p: P): Either[E, B] =
      for {
        a <- lp.get(p)
        _ = logger.debug(s"Got ${a} from ${p}")
        r <- la.get(a)
        _ = logger.debug(s"Got ${r} from ${a}")
      } yield r

    override def set(p: P, b: B): Either[E, P] = {
      for {
        a <- lp.get(p)
        _ = logger.debug(s"Got ${a} from ${p}")
        na <- la.set(a, b)
        _ = logger.debug(s"Set ${b} in $a, got ${na}")
        np <- lp.set(p, na)
        _ = logger.debug(s"Set ${na} in $p, got ${np}")
      } yield np
    }
  }
}
