package edu.gatech.dt87.scalaverse.planner

/**
 * Created by drewtaylor on 8/26/14.
 */
case class Tree[A](root: A, children: List[Tree[A]])