package com.evolutiongaming.bootcamp.cats

import cats.Functor

object Homework {

  trait Applicative[F[_]] extends Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(a => f(a, _)))(fb)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(as => as)

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    // implement methods using each other
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(a => f(a)))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma) { a =>
        map(mb) { b =>
          f(a, b)
        }
      }
  }

}
