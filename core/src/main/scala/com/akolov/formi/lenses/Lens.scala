package com.akolov.formi

package lenses

import org.log4s.getLogger

/**
  Lens for formi documents with schema
  Creating a lens may succeed or fail
  Every lens operation may succeed or fail
  */
trait Lens[P, E, A] { self =>
  def get(p: P): Either[E, A]

  def set(p: P, a: A): Either[E, P]

  def modify(p: P)(f: A => A): Either[E, P] = {
    get(p).flatMap { curr =>
      set(p, f(curr))
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
        r <- la.get(a)
      } yield r

    override def set(p: P, b: B): Either[E, P] =
      for {
        a <- lp.get(p)
        na <- la.set(a, b)
        np <- lp.set(p, na)
      } yield np
  }
}
